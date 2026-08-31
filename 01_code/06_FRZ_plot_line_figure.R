################################################################################
# Plot 5-day Trailing Mean Repeated Accessibility - FRZ
################################################################################
#
# Created by Flavia C B Trigo
# Created on 27/08/2025
# Updated/edited by Gemini
# Last updated/edited on 20/Aug/2026
#
# PURPOSE OF THIS SCRIPT:
# To process "RA_Quality" rasters, calculate a 5-day trailing mean and
# standard deviation for each day's global average, and create a
# time-series plot showing the annual trend.
#
# DATA HANDOFF: This script now exports the calculated peak date as an .rds 
# file so that Script 07 (Map Plot) can perfectly sync its 11-day window 
# without recalculating the math.
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
  "fs",        # For modern file system operations
  "tidyverse", # For data manipulation (dplyr, ggplot2, etc.)
  "terra",     # For high-performance raster processing
  "stringr",   # For string manipulation
  "purrr",     # For functional programming
  "lubridate", # For easy date and time manipulation
  "ggplot2",   # For creating plots
  "progressr"  # For progress bars
)

################################################################################
# SECTION 1: FIND AND ORGANISE DATA FILES
################################################################################
message("\n--- Configuration ---")

project_root = getwd()
current_year = as.character(format(Sys.time(), "%Y"))

# Define paths
frz_folder_path = fs::path(project_root, "04_cleaned_data", current_year, "results_amsr2_FRZ")
final_results_path = fs::path(frz_folder_path, "02_final_RA_rasters")

message("\nFinding all 'RA_Quality' raster files...")
quality_mean_files = fs::dir_ls(final_results_path, glob = "*RA_Quality_month*.tif")

if (length(quality_mean_files) == 0) {
  stop("No 'RA_Quality' files found. Please check the path and run Script 05 first.")
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
# SECTION 2: CALCULATE 5-DAY TRAILING STATISTICS
################################################################################
message("\nCalculating 5-day moving average statistics...")

daily_files_sorted = file_info |> arrange(doy)
window_size = 5
day_indices = seq(from = window_size, to = nrow(daily_files_sorted))

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

max_index = which.max(rolling_stats$mean_ra)
day_of_max_ra = rolling_stats$doy[max_index]
max_ra_value = rolling_stats$mean_ra[max_index]

max_ra_date = as.Date(day_of_max_ra - 1, origin = "2023-01-01")
date_label = format(max_ra_date, "%d %B")
value_label = paste0("Max 5-day Mean RA = ", round(max_ra_value, 2), "%")

message(paste("Day of maximum 5-day moving average accessibility:", date_label, " (Day", day_of_max_ra, ")"))

month_breaks = yday(make_date(2023, 1:12, 1))
month_labels = month.abb
x_axis_title = paste0("Days of the Year (1st January to 31st December; 2012-", current_year, ")")

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
  scale_y_continuous(name = "FRZ 5-day Moving Average RA (%)", breaks = seq(0, 100, 10), limits = c(0, 100)) +
  theme_light() +
  theme(axis.title = element_text(size = 11), panel.grid.major = element_line(linetype = "dashed", colour = "grey90"), panel.grid.minor = element_blank())

################################################################################
# SECTION 4: SAVE OUTPUTS (Plot & Data Handoff)
################################################################################
message("\nSaving the plot and data handoff file...")

output_dir = fs::path(project_root, "05_results", current_year, "figures")
fs::dir_create(output_dir)

# 1. Save the Handoff File for Script 07
handoff_file = fs::path(frz_folder_path, paste0("FRZ_peak_date_handoff_", current_year, ".rds"))
saveRDS(max_ra_date, file = handoff_file)

# 2. Save the Plot
output_filename = paste0("FRZ_5day_mov_averag_RA_2012-", current_year, ".png")
file_sd = fs::path(output_dir, output_filename)
ggsave(filename = file_sd, plot = plot_sd, width = 11, height = 7, units = "in", dpi = 300)

message("\n=======================================================")
message("SUCCESS! 5-day Trailing Mean plot created.")
message("Peak Date Handoff saved to:\n  ", handoff_file)
message("Plot saved to:\n  ", file_sd)
message("=======================================================\n")
message("Script 06 finished.")