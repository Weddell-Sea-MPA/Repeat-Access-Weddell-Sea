###############################################
# Process Mean Raster and Calculate Point Percentages - FRZ & RB4
###############################################
#
# Created by Flavia C B Trigo
# Created on 02/09/2025
# Updated/edited by Gemini
# Last updated/edited on 21/Aug/2026
#
# PURPOSE OF THIS SCRIPT:
# A unified script to calculate the overlap between sea ice accessibility 
# (mean RA rasters) and longline fishing starting points. 
# 
# DATA HANDOFF: It reads the peak RA dates exported by Scripts 06 and 10, 
# creates an 11-day window (+/- 5 days) around them, averages those rasters, 
# and calculates the fishing overlap percentages for both the FRZ and RB4 zones.
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
pacman::p_load("fs", "tidyverse", "terra", "sf", "stringr", "purrr", "cols4all", "lubridate")

################################################################################
# SECTION 1: CONFIGURATION & FUNCTION DEFINITION
################################################################################
message("\n--- Configuration ---")

# Anchor the paths to the current working directory set by setwd()
project_root = getwd()
current_year = as.character(format(Sys.time(), "%Y"))

# Define output directories
df_output_dir = fs::path(project_root, "05_results", current_year, "dataframes")
fig_output_dir = fs::path(project_root, "05_results", current_year, "figures")
fs::dir_create(df_output_dir)
fs::dir_create(fig_output_dir)

# --- Define the core analysis function ---
process_overlap <- function(region_name, raster_folder, handoff_filename, longline_file, plot_title, output_prefix) {
  
  message(sprintf("\n======================================================="))
  message(sprintf("Starting analysis for: %s", region_name))
  message(sprintf("=======================================================\n"))
  
  # 1. Load longline data
  message("1. Loading longline point data...")
  longline_path = fs::path(project_root, "04_cleaned_data", current_year, longline_file)
  if (!fs::file_exists(longline_path)) stop("Longline file not found: ", longline_path)
  longline_points = st_read(longline_path, quiet = TRUE)
  
  # 2. Import Data Handoff and define dynamic window
  message("2. Importing peak date handoff and defining the 11-day window...")
  raster_dir = fs::path(project_root, "04_cleaned_data", current_year, raster_folder)
  handoff_path = fs::path(raster_dir, handoff_filename)
  
  if(!fs::file_exists(handoff_path)) stop("Peak date handoff file not found! Please run the line plot script first.")
  
  max_date = readRDS(handoff_path)
  message("   Successfully imported peak date: ", format(max_date, "%d %B"))
  
  # Create the 11-day window (+/- 5 days)
  window_dates = max_date + lubridate::days(-5:5)
  window_patterns = paste0("month-", sprintf("%02d", lubridate::month(window_dates)), 
                           "_day-", sprintf("%02d", lubridate::day(window_dates)))
  
  # Filter the file list to ONLY include files within this dynamic 11-day window
  final_results_path = fs::path(raster_dir, "02_final_RA_rasters")
  all_quality_files = fs::dir_ls(final_results_path, glob = "*RA_Quality_*.tif")
  quality_files = all_quality_files[stringr::str_detect(all_quality_files, paste(window_patterns, collapse = "|"))]
  
  message(sprintf("   Using 11-day window: %s to %s", format(min(window_dates), "%d %b"), format(max(window_dates), "%d %b")))
  message("   Found ", length(quality_files), " 'RA_Quality' files within this window.")
  
  # Stack the FILTERED rasters and calculate mean
  raster_stack = terra::rast(purrr::map(quality_files, terra::rast))
  mean_raster = mean(raster_stack, na.rm = TRUE)
  
  # 3. Transform points to match raster CRS
  message("3. Transforming longline data to match raster projection...")
  longline_points = st_transform(longline_points, crs = crs(mean_raster))
  
  # 4. Extract values and calculate percentages
  message("4. Extracting values and calculating category percentages...")
  point_values = terra::extract(mean_raster, longline_points)
  
  breaks = c(0, 20, 40, 50, 60, 70, 80, 90, 100)
  category_labels = c("0-20", "21-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100")
  
  category_percentages = point_values %>%
    rename(value = 2) %>%
    filter(!is.na(value)) %>%
    mutate(category_label = cut(value, breaks = breaks, labels = category_labels, include.lowest = TRUE, right = TRUE)) %>%
    group_by(category_label) %>%
    summarise(count = n()) %>%
    complete(category_label = category_labels, fill = list(count = 0)) %>%
    mutate(percentage = (count / sum(count)) * 100)
  
  # 5. Save CSV
  csv_filename = paste0(output_prefix, "_mean_raster_category_percentages_", current_year, ".csv")
  csv_path = fs::path(df_output_dir, csv_filename)
  write.csv(category_percentages, csv_path, row.names = FALSE)
  message("   Results saved to: ", csv_filename)
  
  # 6. Create and save Bar Plot
  message("5. Generating and saving the bar plot...")
  palette_ra = c4a("brewer.rd_yl_bu", n = 8)
  
  category_plot = ggplot(category_percentages, aes(x = category_label, y = percentage, fill = category_label)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = palette_ra) +
    geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, size = 3.5) +
    labs(x = "Mean RA (%)", y = plot_title) +
    theme_bw() +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
  
  plot_filename = paste0(output_prefix, "_mean_raster_category_plot_", current_year, ".png")
  plot_path = fs::path(fig_output_dir, plot_filename)
  ggsave(plot_path, category_plot, width = 10, height = 7)
  message("   Plot saved to: ", plot_filename)
  
  message(sprintf("✅ Completed analysis for %s\n", region_name))
}

################################################################################
# SECTION 2: EXECUTE ANALYSIS FOR BOTH REGIONS
################################################################################

# --- Run Analysis for FRZ (using RB5 longline data) ---
process_overlap(
  region_name = "FRZ (RB5 Data)",
  raster_folder = "results_amsr2_FRZ",
  handoff_filename = paste0("FRZ_peak_date_handoff_", current_year, ".rds"),
  longline_file = paste0("longline_486_5_", current_year, ".gpkg"),
  plot_title = paste0("RB48.6_5 Longline Starting Points (", current_year, ") (%)"),
  output_prefix = "FRZ_RB5"
)

# --- Run Analysis for RB4 (using RB4 longline data) ---
process_overlap(
  region_name = "RB4",
  raster_folder = "new_CRS_results_amsr2_48_6_4",
  handoff_filename = paste0("RB4_peak_date_handoff_", current_year, ".rds"),
  longline_file = paste0("longline_486_4_", current_year, ".gpkg"),
  plot_title = paste0("RB48.6_4 Longline Starting Points (", current_year, ") (%)"),
  output_prefix = "RB_48_6_4"
)

message("#####################################################")
message("  Script Complete. All overlaps calculated and plotted.")
message("#####################################################")