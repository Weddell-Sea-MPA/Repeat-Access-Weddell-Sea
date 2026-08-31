################################################################################
# SCRIPT 05: CALCULATE REPEATED ACCESSIBILITY (RA)
################################################################################
#
# Created by FT
# Created on 29/08/2025
# Updated/edited by FT
# Last updated/edited on 17/Aug/2026
#
# PURPOSE OF THIS SCRIPT:
# This is the second stage of the analysis. It uses the simple
# binary "accessibility maps" created by Script 04 (step 1) to calculate the
# final RA metrics (RA Quality and RA Existence).
#
# This script is very fast. If new years of data are added, you should:
# 1. Re-run Script 04 (Step 1) to process only the new data.
# 2. Delete the '03_checkpoints_final_RA' folder.
# 3. Re-run this script to update all final RA maps.
#
# RUN THIS SCRIPT AFTER SCRIPT 04, STEP 1. 
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
################################################################################################
# Initial cleanup
# Clears all variables from the current R session's workspace to ensure a clean start.
rm(list = ls())
# Closes all open graphics windows, if any.
if(!is.null(dev.list())) dev.off()
# Clears the console screen.
cat("\014")

################################################################################################
#
#                                      USER-DEFINED PARAMETERS
#      !!! These parameters MUST match in Script 04 (step 1) and Script 05 (step 2) !!!
#
################################################################################################
message("\n--- Configuration ---")

# Anchor the paths to the current working directory set by setwd()
project_root = getwd()

# Get the current year to automatically route inputs, outputs, and the year sequence
current_year = as.character(format(Sys.time(), "%Y"))

# 1. Define the full path to the directory where ALL results are saved.
# This automatically routes to the year-specific folder created by Script 04.
storage_path = fs::path(project_root, "04_cleaned_data", current_year, "results_amsr2_FRZ") 
if (fs::dir_exists(storage_path)) {
  storage_path = fs::path_real(storage_path)
} else {
  stop("Storage directory not found. Please ensure Script 04 ran successfully for year: ", current_year)
}

# 2. Define the months to analyze (e.g., c("01", "07")). An empty c() means all available months will be used.
months_to_analyze = c()

# 3. Define the years to analyze dynamically up to the current year.
# - `seq()` generates a sequence of numbers from the start year to the current year.
years_to_analyze = seq(from=2012, to=as.numeric(current_year), by=1)

# 4. Define the ice concentration threshold (in percent) to determine accessibility. This must match Script 04.
threshold = 20 

# 5. Define the Repeated Accessibility (RA) time span in years. This is the window for the rolling calculation.
my = 2

#####################################################################################
#
#                         Install and load needed packages
#
#####################################################################################
# Check if the 'pacman' package manager is installed; if not, install it.
if (!require("pacman")) install.packages("pacman")
# Use pacman's p_load function to load all required packages. It will automatically install any that are missing.
pacman::p_load("tidyverse", "terra", "zoo", "fs", "future", "furrr", "progressr")

# Set up the progress bar handler to display progress updates in the console.
# `global = TRUE` makes this handler available for all progress updates.
progressr::handlers(global = TRUE)
# Specifically selects the text-based progress bar style.
progressr::handlers("progress")

#####################################################################################
#
#                         Part 1: Setup and File Discovery
#
#####################################################################################
# Prints a status message to the console.
message("\n--- Stage 2: Calculating Final RA from Accessibility Maps ---")

# --- Validate paths and define sub-directories using fs::path for robust path construction.
# Path to the folder containing the binary accessibility maps from Script 04.
intermediate_path = fs::path(storage_path, "01_intermediate_accessibility_maps")

# Paths for final results and checkpoints
final_results_path = fs::path(storage_path, "02_final_RA_rasters")
checkpoint_path = fs::path(storage_path, "03_checkpoints_final_RA")

# Create directories if they don't exist
fs::dir_create(final_results_path)
fs::dir_create(checkpoint_path)

# Check if the input directory from Script 04 exists. If not, stop the script with an error message.
if (!fs::dir_exists(intermediate_path)) stop("Intermediate maps directory not found. Please run Script 04 first.")

# --- Find all available intermediate accessibility maps.
# `fs::dir_ls` lists all files in a directory, filtered by a pattern (`glob = "*.tif"` finds all TIFF files).
access_maps = fs::dir_ls(intermediate_path, glob = "*.tif")
# If no TIFF files are found, stop the script.
if (length(access_maps) == 0) stop("No intermediate accessibility maps found.")

# --- Parse file info from map names into a structured table (a tibble).
file_info = tibble::tibble(filename = fs::path_file(access_maps)) |> # Create a tibble with just the filenames.
  dplyr::mutate( # Add new columns based on the filename.
    # Extract the 8-digit date (YYYYMMDD) from the filename using a regular expression.
    date_part = stringr::str_extract(filename, "\\d{8}"),
    # Extract the year, month, and day from the date part.
    year = as.numeric(substr(date_part, 1, 4)),
    month = substr(date_part, 5, 6),
    day = substr(date_part, 7, 8)
  ) |>
  dplyr::filter(!is.na(date_part)) # Remove any files that didn't have a valid date in their name.

# --- Determine years and months to process based on user settings and available files.
# Find all unique years present in the file list.
available_years = sort(unique(file_info$year))
# If the user specified years, use them; otherwise, use all available years.
years = if (length(years_to_analyze) > 0) years_to_analyze else available_years
# If the user specified months, use them; otherwise, use all available months.
month_selection = if (length(months_to_analyze) > 0) months_to_analyze else sort(unique(file_info$month))

# --- Create a grid of all unique month-day combinations that need to be processed.
processing_grid_full = file_info |>
  dplyr::filter(year %in% years, month %in% month_selection) |> # Filter for the selected years and months.
  dplyr::distinct(month_val = month, day_val = day) |> # Find unique month-day pairs.
  dplyr::arrange(month_val, day_val) |> # Sort the grid by date.
  dplyr::mutate(
    # For each date, create the full path for its corresponding checkpoint file.
    checkpoint_flag = fs::path(checkpoint_path, paste0("done_", month_val, "-", day_val, ".flag"))
  )

# --- Checkpointing: Filter out dates that have already been processed and have a checkpoint file.
processing_grid = processing_grid_full |>
  dplyr::filter(!fs::file_exists(checkpoint_flag)) # Keep only the rows where the checkpoint file does NOT exist.

# --- Report status to the user.
message(sprintf("Found %d total month-day combinations to analyze.", nrow(processing_grid_full)))
message(sprintf("%d are already complete.", nrow(processing_grid_full) - nrow(processing_grid)))
# If the processing grid is empty, it means all work is done. Stop the script.
if (nrow(processing_grid) == 0) stop("All final RA maps are already calculated. Exiting.")
message(sprintf("Processing %d new month-day combinations...", nrow(processing_grid)))

# --- Get projection from the first available map to apply it to the output rasters.
# `terra::rast()` loads a raster file; `terra::crs()` extracts its Coordinate Reference System.
new_proj = terra::crs(terra::rast(access_maps[1]))

#####################################################################################
#
#                         Part 2: Main Processing Loop
#
#####################################################################################
# Setup parallel processing to speed up the calculations.
# Use one less than the total number of available CPU cores to keep the system responsive.
workers_to_use = max(1, future::availableCores() - 1)
# Set the parallel processing plan to 'multisession', which creates separate R sessions for each worker.
future::plan(future::multisession, workers = workers_to_use)
message(paste("\nStarting parallel processing on", workers_to_use, "cores..."))

# Wrap the main processing loop with `progressr::with_progress` to enable the progress bar.
progressr::with_progress({
  # Use `furrr::future_pwalk` to iterate over the rows of `processing_grid` in parallel.
  # `.l` is the list/data frame to iterate over.
  # `.f` is the function to apply to each row. The column names become the function arguments.
  furrr::future_pwalk(
    .l = processing_grid,
    .f = function(month_val, day_val, checkpoint_flag) {
      # Use `tryCatch` to handle any errors that occur for a specific date, preventing the whole script from crashing.
      tryCatch({
        # 1. Find all accessibility map files for the current month-day combination across all selected years.
        date_files = file_info |>
          dplyr::filter(year %in% years, month == month_val, day == day_val) |>
          dplyr::pull(filename) # Pull the 'filename' column into a vector.
        
        # If no files are found for this date, skip to the next one.
        if(length(date_files) == 0) return(NULL)
        
        # 2. Stack all the rasters for this date into a single multi-layer raster object.
        r_stack = terra::rast(fs::path(intermediate_path, date_files))
        
        # 3. Convert the raster stack to a "long" data frame and calculate the rolling RA value.
        df_long = terra::as.data.frame(r_stack, xy=TRUE) |> # Convert raster to a data frame with x, y coordinates.
          dplyr::as_tibble() |> # Convert to a tibble for easier use with tidyverse.
          tidyr::pivot_longer( # Reshape the data from wide (one column per year) to long format.
            cols = -c(x,y), # Pivot all columns except for x and y.
            names_to = "filename", # The old column names go into a new 'filename' column.
            values_to = "accessible" # The cell values (0 or 1) go into a new 'accessible' column.
          ) |>
          # Extract the date from the filename again to ensure correct temporal ordering.
          dplyr::mutate(date_part = stringr::str_extract(filename, "\\d{8}")) |>
          dplyr::arrange(x, y, date_part) # CRITICAL: Sort by pixel (x, y) and then by date.
        
        # Calculate the RA value using a rolling window function.
        df_ra = df_long |>
          dplyr::group_by(x, y) |> # Perform the next calculation separately for each pixel location.
          dplyr::mutate(
            # For each pixel, calculate the rolling sum.
            ra_value = ifelse(accessible == 1, # Only calculate if the current year is accessible.
                              # `zoo::rollapplyr` applies a function over a right-aligned rolling window.
                              # `width = (my + 1)`: The window includes the current year plus 'my' previous years.
                              # `FUN = sum`: The function to apply is sum.
                              # `partial = TRUE`: Allows calculation for the first few years where the window is not full.
                              # `- 1`: Subtract 1 to count only the *preceding* accessible years, not the current one.
                              zoo::rollapplyr(accessible, width = (my + 1), FUN = sum, partial = TRUE, align = "right") - 1,
                              0) # If not accessible, the RA value is 0.
          ) |>
          dplyr::ungroup() # Remove the grouping.
        
        # 4. Summarize the results for each pixel across all years.
        df_summary = df_ra |>
          dplyr::group_by(x, y) |> # Group by pixel location again.
          dplyr::summarise(
            # RA Quality: The average number of preceding accessible years, expressed as a percentage of the time span 'my'.
            ra_quality_pct = mean(ra_value / my, na.rm = TRUE) * 100,
            
            # RA Existence: The percentage of years where RA was possible (i.e., there was at least one preceding accessible year).
            ra_existence_pct = mean(ra_value > 0, na.rm = TRUE) * 100,
            .groups = "drop" # Drop the grouping after summarizing.
          )
        
        # 5. Convert the summary data frames back to rasters and save them.
        # Create the RA Quality raster from the x, y, and value columns.
        r_quality = terra::rast(df_summary |> dplyr::select(x, y, ra_quality_pct), type = "xyz", crs = new_proj)
        # Construct a descriptive output filename.
        name1 = fs::path(final_results_path, paste0("RA_Quality_month-", month_val, "_day-", day_val, "_span-", my, "y_thresh-", threshold, ".tif"))
        # Write the raster to a compressed GeoTIFF file.
        terra::writeRaster(r_quality, filename = name1, overwrite=TRUE, gdal=c("COMPRESS=LZW"))
        
        # Repeat the process for the RA Existence raster.
        r_existence = terra::rast(df_summary |> dplyr::select(x, y, ra_existence_pct), type = "xyz", crs = new_proj)
        name2 = fs::path(final_results_path, paste0("RA_Existence_month-", month_val, "_day-", day_val, "_span-", my, "y_thresh-", threshold, ".tif"))
        terra::writeRaster(r_existence, filename = name2, overwrite=TRUE, gdal=c("COMPRESS=LZW"))
        
        # 6. Create an empty checkpoint file to mark this date as complete.
        fs::file_create(checkpoint_flag)
        
      }, error = function(e){
        # If an error occurred, print a message to the console and continue with the next date.
        message(sprintf("\nError on %s-%s: %s. Skipping.", month_val, day_val, e$message))
      })
    },
    .options = furrr_options(seed = TRUE), # Ensures reproducibility in parallel calculations.
    .progress = TRUE # Tells furrr to automatically update the progressr bar.
  )
})

# --- Final completion message.
message("\n=======================================================")
message("SUCCESS! Stage 2 Complete. All final RA maps created. ✅")
message("Final RA raster maps are located in:\n  ", final_results_path)
message("=======================================================\n")
message("Script 05 finished. You can now run the next script in your workflow.")