###############################################
# Plotting Main and Inset Maps for Repeated Accessibility - RB4
###############################################
#
# Created by Flavia Trigo, based on Hendrik's scripts available in zenodo.
# Created on 21/08/2025
# Updated/edited by Gemini
# Last updated/edited on 21/Aug/2026
# 
# PURPOSE OF THIS SCRIPT:
# To load specific repeated accessibility raster data and corresponding vector
# layers, process them, generate a main map and an inset map, and combine
# them into a single figure for export. 
#
# DATA HANDOFF: This script imports the peak RA date exported by Script 10, 
# instantly generating an 11-day window (+/- 5 days) around it without 
# requiring heavy spatial recalculations.
#
# ACKNOWLEDGEMENT:
# This script is an updated and revised version of the work originally produced by:
#
# Hendrik, P., Brey, T., Konijnenberg, R., & Teschke, K. (2022). A tool to evaluate accessibility
# due to sea-ice cover: a case study of the Weddell Sea, Antarctica.
# Antarctic Science, 34(1), 97-104. DOI: https://doi.org/10.1017/S0954102021000523
#
###############################################
# Initial cleanup
rm(list = ls()); if(!is.null(dev.list())) dev.off(); cat("\014")

###############################################
#
#                         Install and load needed packages
#
###############################################
if (!require("pacman")) install.packages("pacman")
# Load all necessary packages, including lubridate for the date window
pacman::p_load("fs", "tidyverse", "terra", "sf", "tmap", "stringr", "purrr", "grid", "cols4all", "tmaptools", "lubridate")

################################################################################
# SECTION 1: LOAD DATA
################################################################################
message("\n--- Configuration ---")

# Anchor the paths to the current working directory set by setwd()
project_root = getwd()

# Get the current year to automatically route inputs, outputs, and labels
current_year = as.character(format(Sys.time(), "%Y"))
current_year = 2025
# --- Define paths ---
rb4_folder_path = fs::path(project_root, "04_cleaned_data", current_year, "new_CRS_results_amsr2_48_6_4")
final_results_path = fs::path(rb4_folder_path, "02_final_RA_rasters")
base_data_path = fs::path(project_root, "03_original_data", "00_basemaps")
cleaned_data_path = fs::path(project_root, "04_cleaned_data", current_year)

# --- Load Handoff Data (The Peak Date) ---
handoff_file = fs::path(rb4_folder_path, paste0("RB4_peak_date_handoff_", current_year, ".rds"))
if(!fs::file_exists(handoff_file)) stop("Peak date handoff file not found! Please run Script 10 first.")

max_date = readRDS(handoff_file)
message("Successfully imported peak date handoff: ", format(max_date, "%d %B"))

# --- Create the 11-day window (+/- 5 days) ---
window_dates = max_date + lubridate::days(-5:5)
window_patterns = paste0("month-", sprintf("%02d", lubridate::month(window_dates)), 
                         "_day-", sprintf("%02d", lubridate::day(window_dates)))


# --- Load vector data ---
message("\nLoading vector data...")
depth_550 = st_read(fs::path(base_data_path, "Bathymetry_IBCSO", "550m_outline.gpkg"), quiet = TRUE)
antarctica = st_read(fs::path(base_data_path, "QuantarcticaV3", "Land", "QuantarcticaV3_Land_EPSG102020.shp"), quiet = TRUE)
ice_shelf = st_read(fs::path(base_data_path, "QuantarcticaV3", "IceShelf", "QuantarcticaV3_IceShelf_EPSG102020.shp"), quiet = TRUE)
rb_48_6_4 = st_read(fs::path(base_data_path, "research_blocks", "RB_48_6_4.shp"), quiet = TRUE)

# --- Load longline data ---
message("Loading longline point data...")
longline_filename = paste0("longline_486_4_", current_year, ".gpkg")
longline_486_4 = st_read(fs::path(cleaned_data_path, longline_filename), quiet = TRUE)

# --- Find and load raster data ---
message("\nFiltering rasters for the 11-day window...")
all_quality_files = fs::dir_ls(final_results_path, glob = "*RA_Quality_*.tif")
quality_files = all_quality_files[stringr::str_detect(all_quality_files, paste(window_patterns, collapse = "|"))]

message("Found ", length(quality_files), " 'RA_Quality' files within the +/- 5 day window.")
raster_list = purrr::map(quality_files, terra::rast)


################################################################################
# SECTION 2: MANIPULATION OF DATA FOR PLOTTING
################################################################################

message("\nProcessing and manipulating data for plotting...")

# --- Process raster data ---
new_layer_names = quality_files |>
  fs::path_file() |>
  stringr::str_extract("month-\\d{2}_day-\\d{2}") |>
  stringr::str_replace("_day-", "_d") |>
  stringr::str_replace("month-", "m")

names(raster_list) = new_layer_names
template_raster = raster_list[[1]]

ra_stack = raster_list |>
  purrr::map(~terra::resample(.x, template_raster)) |>
  terra::rast()

ra_mean = mean(ra_stack, na.rm = TRUE)
message("Raster stack and mean successfully created.")

# --- Process vector data for main map ---
target_crs = crs(ra_mean)
antarctica_reproj = st_transform(antarctica, target_crs)
ice_shelf_reproj = st_transform(ice_shelf, target_crs)
rb_48_6_4_reproj = st_transform(rb_48_6_4, target_crs)
longline_486_4_reproj = st_transform(longline_486_4, target_crs)
depth_550_reproj = st_transform(depth_550, target_crs)

longline_current_year = longline_486_4_reproj |>
  dplyr::filter(year == as.numeric(current_year))

zoom_bbox = rb_48_6_4_reproj |>
  st_buffer(dist = 70000) |>
  st_bbox()

bbox_coords = st_bbox(c(xmin = 161622.0, xmax = 604195.4, ymin = 2012215.5, ymax = 2300664.1),
                      crs = st_crs(ice_shelf_reproj))
bbox_sf = st_as_sfc(bbox_coords)


################################################################################
# SECTION 3: PLOTTING
################################################################################

message("\nGenerating maps...")

legend_breaks = c(0, 20, 40, 50, 60, 70, 80, 90, 100)
legend_labels = c("0 - 20 %", "21 - 40 %", "41 - 50 %", "51 - 60 %", "61 - 70 %", "71 - 80 %", "81 - 90 %", "91 - 100 %")
palette_ra = c4a("brewer.rd_yl_bu", n = 8) 

# Dynamically construct the legend title using the calculated window dates
date_range_str = paste0(format(min(window_dates), "%d %b"), " - ", format(max(window_dates), "%d %b"))
legend_title = paste0("Average RA over 3 years\n(", date_range_str, "; 2012-", current_year, ")")

# ==========================================
# MAP 1: Main Map (All Longline Data)
# ==========================================
main_map = tm_shape(antarctica_reproj, bbox = zoom_bbox) +
  tm_polygons(fill = "antiquewhite2", col_alpha = 0.9, fill_alpha = 0.9) +
  tm_shape(ra_mean) +
  tm_raster(
    col.scale = tm_scale_intervals(breaks = legend_breaks, values = palette_ra, labels = legend_labels),
    col.legend = tm_legend(title = legend_title)
  ) +
  tm_shape(rb_48_6_4_reproj) +
  tm_borders(col = "black", lwd = 1.9) +
  tm_shape(depth_550_reproj)+
  tm_borders(col = "black", lwd = 1.0, lty = "solid") +
  tm_shape(longline_486_4_reproj) +
  tm_dots(col = "black", size = 0.1) + 
  tm_shape(ice_shelf_reproj) +
  tm_polygons(fill = "snow2", col_alpha = 0.9, fill_alpha = 1) +
  tm_add_legend(type = "polygons",
                labels = c("Ice Shelf", "Antarctica", "48.6_4"),
                fill = c("snow2", "antiquewhite2", "white"),
                lty = c("solid", "solid", "solid"),
                border.col = "black",
                title = "") +
  tm_add_legend(type = "symbols", labels = c("Longline (All Years)"), fill = c("black")) +
  tm_layout(legend.position = c("right", "bottom")) +
  tm_graticules(alpha=0.3, labels.where = c("bottom", "right")) +
  tm_scalebar(position = c("left", "bottom"), text.size = 0.7)

# ==========================================
# MAP 2: Current Year Only Map
# ==========================================
main_map_current = tm_shape(antarctica_reproj, bbox = zoom_bbox) +
  tm_polygons(fill = "antiquewhite2", col_alpha = 0.9, fill_alpha = 0.9) +
  tm_shape(ra_mean) +
  tm_raster(
    col.scale = tm_scale_intervals(breaks = legend_breaks, values = palette_ra, labels = legend_labels),
    col.legend = tm_legend(title = legend_title)
  ) +
  tm_shape(rb_48_6_4_reproj) +
  tm_borders(col = "black", lwd = 1.9) +
  tm_shape(depth_550_reproj)+
  tm_borders(col = "black", lwd = 1.0, lty = "solid") +
  tm_shape(longline_current_year) +  # USING FILTERED DATA
  tm_dots(col = "black", size = 0.15) + # Made slightly larger/red to pop out
  tm_shape(ice_shelf_reproj) +
  tm_polygons(fill = "snow2", col_alpha = 0.9, fill_alpha = 1) +
  tm_add_legend(type = "polygons",
                labels = c("Ice Shelf", "Antarctica", "48.6_4"),
                fill = c("snow2", "antiquewhite2", "white"),
                lty = c("solid", "solid", "solid"),
                border.col = "black",
                title = "") +
  tm_add_legend(type = "symbols", labels = paste0("Longline (", current_year, ")"), fill = c("black")) + 
  tm_layout(legend.position = c("right", "bottom")) +
  tm_graticules(alpha=0.3, labels.where = c("bottom", "right")) +
  tm_scalebar(position = c("left", "bottom"), text.size = 0.7)


# ==========================================
# Inset Map
# ==========================================
message("Generating inset map...")
inset_map = tm_shape(antarctica_reproj) +
  tm_polygons(fill = "antiquewhite2", col_alpha = 0.2, fill_alpha = 0.9) +
  tm_text(text = "Antarctica", size = 0.8, options = opt_tm_text(just = "center")) +
  tm_shape(ice_shelf_reproj) +
  tm_polygons(fill = "snow2", col_alpha = 0.3, fill_alpha = 0.9) +
  tm_shape(bbox_sf) +
  tm_borders(col = "red", lwd = 2) +
  tm_layout(
    frame = TRUE,
    bg.color = "white"
  )

################################################################################
# SECTION 4: COMBINE AND SAVE PLOTS
################################################################################

message("\nCombining and saving maps...")

# Define output directory dynamically
output_dir = fs::path(project_root, "05_results", current_year, "figures")
fs::dir_create(output_dir) # Create directory if it doesn't exist

# --- Save Map 1 (All Years) ---
file_all_png = fs::path(output_dir, paste0("RB_48_6_4_repeat_access_2012-", current_year, ".png"))
file_all_pdf = fs::path(output_dir, paste0("RB_48_6_4_repeat_access_2012-", current_year, ".pdf"))

png(filename = file_all_png, width = 11, height = 8.5, units = "in", res = 300)
print(main_map)
inset_vp = viewport(x = 0.05, y = 0.90, width = 0.2, height = 0.2, just = c("left", "top"))
print(inset_map, vp = inset_vp)
dev.off() 

pdf(file = file_all_pdf, width = 11, height = 8.5)
print(main_map)
print(inset_map, vp = inset_vp)
dev.off() 

# --- Save Map 2 (Current Year Only) ---
file_current_png = fs::path(output_dir, paste0("RB_48_6_4_repeat_access_", current_year, ".png"))
file_current_pdf = fs::path(output_dir, paste0("RB_48_6_4_repeat_access_", current_year, ".pdf"))

png(filename = file_current_png, width = 11, height = 8.5, units = "in", res = 300)
print(main_map_current)
print(inset_map, vp = inset_vp)
dev.off()

pdf(file = file_current_pdf, width = 11, height = 8.5)
print(main_map_current)
print(inset_map, vp = inset_vp)
dev.off()

message("\n=======================================================")
message("SUCCESS! Maps combined and saved.")
message("All Years Map saved to:\n  ", file_all_png)
message("Current Year (", current_year, ") Map saved to:\n  ", file_current_png)
message("=======================================================\n")

message("Script 11 finished.")