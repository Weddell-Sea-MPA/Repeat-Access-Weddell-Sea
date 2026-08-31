################################################################################
# Plot 5-day Trailing Mean Repeated Accessibility - RB4
################################################################################
#
# Created by Flavia Trigo
# Created on 27/08/2025
# Updated/edited by Gemini
# Last updated/edited on 21/Aug/2026
#
# PURPOSE OF THIS SCRIPT:
# To process "RA_Quality" rasters for RB4, calculate a 5-day trailing mean and
# standard deviation raster for each day using terra::app, and create a
# time-series plot showing the trend of the global averages.
#
# DATA HANDOFF: This script now exports the calculated peak date as an .rds 
# file so that Script 11 (Map Plot) can perfectly sync its 11-day window 
# without recalculating the math.
#
# ACKNOWLEDGEMENT:
# This script is an updated and revised version of the work originally produced by:
#
# Hendrik, P., Brey, T., Konijnenberg, R., & Teschke, K. (2022). A tool to evaluate accessibility
# due to sea-ice cover: a case study of the Weddell Sea, Antarctica.
# Antarctic Science, 34(1), 97-104. DOI: https://doi.org/10.1017/S0954102021000523
#
################################################################################
# Initial cleanup
rm(list = ls()); if(!is.null(dev.list())) dev.off(); cat("\014")

###############################################
#
#                         Install and load needed packages
#
###############################################
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  "fs", "tidyverse", "terra", "stringr", "purrr", "lubridate", "ggplot2", "progressr"
)

################################################################################
# SECTION 1: FIND AND ORGANISE DATA FILES
################################################################################
message("\n--- Configuration ---")

# Anchor the paths to the current working directory set by setwd()
project_root = getwd()

# Get the current year to automatically route inputs, outputs, and labels
current_year = as.character(format(Sys.time(), "%Y"))
current_year = 2025
# Define path to the raster data
rb4_folder_path = fs::path(project_root, "04_cleaned_data", current_year, "new_CRS_results_amsr2_48_6_4")
final_results_path = fs::path(rb4_folder_path, "02_final_RA_rasters")

message("Finding all 'RA_Quality' (mean) raster files...")

quality_mean_files = fs::dir_ls(final_results_path, glob = "*RA_Quality_month*.tif")

if (length(quality_mean_files) == 0) {
  stop("No 'RA_Quality' files found. Please check the path and run the calculation script first.")
}

message("Found ", length(quality_mean_files), " total files to process.")

file_info = tibble(path_mean = quality_mean_files) |>
  mutate(
    month = as.integer(str_match(path_mean, "month-(\\d{2})")[,2]),
    day = as.integer(str_match(path_mean, "day-(\\d{2})")[,2]),
    date = make_date(2023, month, day),
    doy = yday(date)
  ) |>
  drop_na(doy)

message("Successfully parsed ", nrow(file_info), " files.")


################################################################################
# SECTION 2: CALCULATE 5-DAY TRAILING STATISTICS USING TERRA::APP
################################################################################

message("\nCalculating 5-day moving average statistics")

# Ensure the file list is sorted by day of the year
daily_files_sorted = file_info |> arrange(doy)

# Define the window size
window_size = 5

# Create a list of day indices to iterate over
day_indices = seq(from = window_size, to = nrow(daily_files_sorted))

# Set up the progress bar
progressr::with_progress({
  p = progressr::progressor(steps = length(day_indices))
  
  rolling_stats = purrr::map_dfr(day_indices, function(i) {
    window_paths = daily_files_sorted$path_mean[(i - window_size + 1):i]
    accessibility_stack = terra::rast(window_paths)
    
    mean_accessibility = global(accessibility_stack, "mean", na.rm = T)$mean %>% mean
    sd_accessibility = global(accessibility_stack, "sd", na.rm = T)$sd %>% mean
    
    p()
    tibble(
      doy = daily_files_sorted$doy[i],
      mean_ra = mean_accessibility,
      sd_ra = sd_accessibility
    )
  })
})

message("Calculation complete. Data is ready for plotting.")

################################################################################
# SECTION 3: PLOT THE RESULTS WITH GGPLOT2
################################################################################

message("\nGenerating the plot...")

# --- Find the day and value of the highest 5-day moving average average ---
max_index = which.max(rolling_stats$mean_ra)
day_of_max_ra = rolling_stats$doy[max_index]
max_ra_value = rolling_stats$mean_ra[max_index]

# Create labels for annotations
max_ra_date = as.Date(day_of_max_ra - 1, origin = "2023-01-01")
date_label = format(max_ra_date, "%d %B")
value_label = paste0("Max 5-day Mean RA = ", round(max_ra_value, 2), "%")

message(paste("Day of maximum 5-day moving average accessibility:", date_label, " (Day", day_of_max_ra, ")"))

# Define breaks and labels for month-based axes
month_breaks = yday(make_date(2023, 1:12, 1))
month_labels = month.abb
x_axis_title = paste0("Days of the Year (1st January to 31st December; 2012-", current_year, ")")

# --- PLOT: Standard Deviation ---
plot_sd = rolling_stats %>%
  mutate(ymax = mean_ra + sd_ra, ymin = mean_ra - sd_ra) %>%
  mutate(ymin = ifelse(ymin < 0, 0, ymin)) %>%
  ggplot(aes(x = doy, y = mean_ra)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = "grey80", alpha = 0.6) +
  annotate("rect", xmin = day_of_max_ra - 5, xmax = day_of_max_ra + 5, ymin = 0, ymax = 100, fill = "lightsteelblue", alpha = 0.5) +
  geom_line(colour = "black", linewidth = 1) +
  geom_vline(xintercept = day_of_max_ra, linetype = "dashed", colour = "grey30") +
  geom_hline(yintercept = max_ra_value, linetype = "dashed", colour = "grey30") +
  annotate("text", x = day_of_max_ra + 7, y = 75, label = date_label, angle = 90, colour = "grey20", size = 3, vjust = 0.5) +
  annotate("text", x = 360, y = max_ra_value + 3, label = value_label, colour = "grey20", size = 3, hjust = 1) +
  scale_x_continuous(name = x_axis_title, breaks = month_breaks, labels = month_breaks, expand = c(0, 0), 
                     sec.axis = sec_axis(~., name=NULL, breaks=month_breaks, labels=month_labels)) +
  scale_y_continuous(name = "RB 48.6_4 5-day Moving Average RA (%)", breaks = seq(0, 100, 10), limits = c(0, 100)) +
  theme_light() +
  theme(axis.title = element_text(size = 11), panel.grid.major = element_line(linetype = "dashed", colour = "grey90"), panel.grid.minor = element_blank())

################################################################################
# SECTION 4: SAVE OUTPUTS (Plot & Data Handoff)
################################################################################

message("\nSaving the plot and data handoff file...")

# Define output directory dynamically based on the current year
output_dir = fs::path(project_root, "05_results", current_year, "figures")
fs::dir_create(output_dir)

# 1. Save the Handoff File for Script 11
handoff_file = fs::path(rb4_folder_path, paste0("RB4_peak_date_handoff_", current_year, ".rds"))
saveRDS(max_ra_date, file = handoff_file)

# 2. Save the Plot
output_filename = paste0("RB_48_6_4_5day_mov_averag_RA_2012-", current_year, ".png")
file_sd = fs::path(output_dir, output_filename)
ggsave(filename = file_sd, plot = plot_sd, width = 11, height = 7, units = "in", dpi = 300)

message("\n=======================================================")
message("SUCCESS! 5-day Trailing Mean plot created.")
message("Peak Date Handoff saved to:\n  ", handoff_file)
message("Plot saved to:\n  ", file_sd)
message("=======================================================\n")

message("Script 10 finished.")