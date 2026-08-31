################################################################################
# CHECK MISSING DATES, FILL GAPS, AND EXTRACT METADATA #########################
#
# Created by FT
# Created on 02/Aug/2025
# Updated/edited by FT
# Last updated/edited on 17/Aug/2026
#
# PURPOSE OF THIS SCRIPT:
# 1. To analyse a directory of daily GeoTIFF files and identify any missing
#    dates within the overall date range.
# 2. To fill gaps for single or consecutive missing days by interpolating from
#    the rasters that bookend the data gap.
# 3. To extract key metadata from each GeoTIFF file (original and new) in parallel.
# 4. To save the results as summary data frames and a plot.
#
# DISCLAIMER:
# This script was edited and commented with the assistance of Google's Gemini (version 2.5 Pro).
# And updated with Version 3.1 PRO.
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
rm(list = ls());
if(!is.null(dev.list())) dev.off();
cat("\014")

################################################################################
#
#                         Install and load needed packages
#
################################################################################
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  "tidyverse", # A collection of packages for data manipulation and visualisation.
  "lubridate", # Simplifies working with dates and times.
  "fs",        # Provides a consistent, cross-platform interface for file system operations.
  "terra",     # A modern, high-performance package for raster and vector data.
  "future",    # Establishes the framework for parallel processing.
  "furrr",     # Combines 'purrr' functional programming with 'future' parallelism.
  "progressr"  # Provides a universal framework for progress bars.
)

progressr::handlers(global = TRUE)
progressr::handlers("progress")

################################################################################
#
#                                  Configuration
#
################################################################################
message("\n--- Configuration ---")

# Anchor the paths to the current working directory set by setwd()
project_root = getwd()

# Gets the current year to automatically create year-specific output folders and file names
current_year = as.character(format(Sys.time(), "%Y"))

# --- Define input and output paths safely based on the working directory
tif_path = fs::path(project_root, "03_original_data", "downloads_amsr2_WithLandMask_v5.4")

# Paths where the new dataset and results will be stored, dynamically routed by year.
storage_path = fs::path(project_root, "04_cleaned_data", current_year, "amsr2_WithLandMask_withMissingDates_v5.4")
output_path_data = fs::path(project_root, "05_results", current_year, "dataframes", "metadata_amsr2_WithLandMask_v5.4")
output_path_figs = fs::path(project_root, "05_results", current_year, "figures", "sanity_checks")

# --- Create output directories if they don't exist
fs::dir_create(storage_path)
fs::dir_create(output_path_data)
fs::dir_create(output_path_figs)

message("Configuration loaded. Using data from: ", tif_path)
message("Cleaned data with filled gaps will be stored in: ", storage_path)

################################################################################
#
#                         Part 1: Analyse Missing Dates
#
################################################################################
message("\n--- Part 1: Analysing Missing Dates ---")

files = fs::dir_ls(path = tif_path, glob = "*.tif")
files = fs::path_real(files)

dates_df = tibble::tibble(file = fs::path_file(files)) |>
  dplyr::mutate(
    date_str = stringr::str_extract(file, "\\d{8}"),
    date = lubridate::ymd(date_str)
  ) |>
  dplyr::arrange(date)

full_dates = tibble::tibble(
  date = seq(min(dates_df$date, na.rm = TRUE), max(dates_df$date, na.rm = TRUE), by = "day")
)

missing_days_df = full_dates |>
  dplyr::left_join(dates_df, by = "date") |>
  dplyr::mutate(missing = is.na(file))

message("Date analysis complete. Found ", sum(missing_days_df$missing), " missing days.")

################################################################################
#
#             Part 1.5: Fill Missing Dates & Create Complete Dataset
#
################################################################################
message("\n--- Part 1.5: Filling Missing Dates ---")

missing_days_no_leap = missing_days_df |>
  dplyr::filter(!(lubridate::month(date) == 2 & lubridate::day(date) == 29))

gaps_df = missing_days_no_leap |>
  dplyr::arrange(date) |>
  dplyr::mutate(gap_id = cumsum(missing != dplyr::lag(missing, default = first(missing)))) |>
  dplyr::filter(missing == TRUE)

gaps_to_process = gaps_df |>
  dplyr::group_by(gap_id) |>
  dplyr::summarise(
    start_date = min(date),
    end_date = max(date),
    dates_in_gap = list(date),
    .groups = "drop"
  )

message("Found ", nrow(gaps_to_process), " contiguous gap(s) of missing data to fill.")

files_df = tibble::tibble(file_path = files) |>
  dplyr::mutate(
    date = lubridate::ymd(stringr::str_extract(fs::path_file(file_path), "\\d{8}"))
  )

progressr::with_progress({
  p = progressr::progressor(steps = nrow(gaps_to_process))
  
  purrr::pwalk(gaps_to_process, function(gap_id, start_date, end_date, dates_in_gap) {
    p(message = paste("Processing gap starting", start_date))
    
    prev_date = start_date - lubridate::days(1)
    next_date = end_date + lubridate::days(1)
    
    prev_file = files_df |> dplyr::filter(date == prev_date) |> dplyr::pull(file_path)
    next_file = files_df |> dplyr::filter(date == next_date) |> dplyr::pull(file_path)
    
    if (length(prev_file) == 1 && length(next_file) == 1) {
      prev_rast = terra::rast(prev_file)
      next_rast = terra::rast(next_file)
      
      mean_rast = (prev_rast + next_rast) / 2
      
      purrr::walk(dates_in_gap, function(missing_date) {
        date_str = format(missing_date, "%Y%m%d")
        template_name = fs::path_file(prev_file)
        new_filename = stringr::str_replace(template_name, "\\d{8}", date_str)
        
        output_file_path = fs::path(storage_path, new_filename)
        terra::writeRaster(mean_rast, output_file_path, overwrite = TRUE)
      })
      
    } else {
      warning(
        "Cannot fill gap starting ", start_date,
        ". The file before the gap or after the gap could not be found.",
        call. = FALSE
      )
    }
  })
})

message("Finished filling gaps.")

message("Copying original files to the new complete dataset directory...")
fs::file_copy(path = files, new_path = storage_path, overwrite = TRUE)
message("All original files copied.")

################################################################################
#
#             Part 2: Extract Metadata from COMPLETE Dataset
#
################################################################################
message("\n--- Part 2: Extracting Metadata from the new, complete dataset ---")

files = fs::dir_ls(path = storage_path, glob = "*.tif")
files = fs::path_real(files)

workers_to_use = max(1, future::availableCores() - 1)
future::plan(future::multisession, workers = workers_to_use)
message("Using ", workers_to_use, " parallel workers for metadata extraction.")

extract_metadata_fast = function(tif_file_path) {
  r = terra::rast(tif_file_path)
  
  tibble::tibble(
    tif_name = fs::path_file(tif_file_path),
    date = lubridate::ymd(stringr::str_extract(fs::path_file(tif_file_path), "\\d{8}")),
    spatial_projection = terra::crs(r, proj = TRUE), 
    lower_left_origin_X = terra::xmin(r),            
    lower_left_origin_Y = terra::ymin(r),            
    count_row = terra::nrow(r),                      
    count_col = terra::ncol(r),                      
    res_x = terra::res(r)[1],                        
    res_y = terra::res(r)[2],                        
    min_value = terra::global(r, "min", na.rm = TRUE)[1, 1], 
    max_value = terra::global(r, "max", na.rm = TRUE)[1, 1]  
  )
}

progressr::with_progress({
  metadata_df = furrr::future_map_dfr(
    files, extract_metadata_fast,
    .options = furrr_options(seed = TRUE), 
    .progress = TRUE                       
  )
})

message("Metadata extracted from ", nrow(metadata_df), " files.")

################################################################################
#
#                         Part 3: Summarize and Save Results
#
################################################################################
message("\n--- Part 3: Summarizing and Saving All Results ---")

missing_summary = missing_days_df |>
  dplyr::filter(missing) |>
  dplyr::mutate(
    year = lubridate::year(date),
    month = lubridate::month(date, label = TRUE, abbr = TRUE)
  ) |>
  dplyr::group_by(year, month) |>
  dplyr::summarise(missing_days = n(), .groups = "drop")

p1 = ggplot(missing_summary, aes(x = month, y = missing_days, fill = month)) +
  geom_col(show.legend = FALSE) +        
  facet_wrap(~ year) +                  
  labs(                                 
    title = "Originally Missing Daily GeoTIFF Files (amsr2)",
    subtitle = "These gaps have now been filled by interpolation.",
    x = "Month",
    y = "Number of Missing Days"
  ) +
  theme_bw() +                          
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

missing_days_file = paste0("ice_access_originally_missing_days_amsr2_logical_v5.4_", current_year, ".csv")
metadata_file = paste0("ice_access_amsr2_COMPLETE_raster_metadata_v5.4_", current_year, ".csv")
plot_file = paste0("ice_access_originally_missing_days_amsr2_", current_year, ".pdf")

readr::write_csv(missing_days_df, fs::path(output_path_data, missing_days_file))
readr::write_csv(metadata_df, fs::path(output_path_data, metadata_file))

ggplot2::ggsave(
  filename = plot_file,
  plot = p1,
  path = output_path_figs,
  dpi = 300
)

message("\n=======================================================")
message("SUCCESS! All outputs have been saved.")
message("Data frames are located in:\n  ", output_path_data)
message("Plots are located in:\n  ", output_path_figs)
message("=======================================================\n")

message("Script 02 finished.")