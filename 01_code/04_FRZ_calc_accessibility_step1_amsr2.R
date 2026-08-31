###############################################
# STEP 1: CREATE BINARY ACCESSIBILITY MAPS (with CRS check)
###############################################
#
# Created by FT
# Created on 29/08/2025
# Updated/edited by FT
# Last updated/edited on 17/Aug/2026
#
# PURPOSE OF THIS SCRIPT:
# This is the first stage of the analysis. It processes every raw ice
# concentration .tif file (excluding leap days), ensures all spatial data
# is in the correct projection (EPSG:6932), applies the user-defined
# navigability threshold, and saves a simple binary raster.
#
# RUN THIS SCRIPT BEFORE STEP 2, SCRIPT 05.
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
###############################################
# Initial cleanup
# Clears all objects from the R environment to ensure a fresh start.
rm(list = ls());
# Closes any open graphics windows.
if(!is.null(dev.list())) dev.off();
# Clears the console screen.
cat("\014")

###############################################
#
#                         USER-DEFINED PARAMETERS
#   !!! These parameters MUST match in Script 01 and Script 02 !!!
#
###############################################
message("\n--- Configuration ---")

# Anchor the paths to the current working directory set by setwd()
project_root = getwd()

# Get the current year to automatically route inputs and outputs
current_year = as.character(format(Sys.time(), "%Y"))

# 1. Define the path to the directory containing the SOURCE ice concentration .tif files.
# This automatically points to the year-specific folder created by Script 02.
ice_data_path = fs::path(project_root, "04_cleaned_data", current_year, "amsr2_WithLandMask_withMissingDates_v5.4") 
if (fs::dir_exists(ice_data_path)) {
  ice_data_path = fs::path_real(ice_data_path)
} else {
  stop("Input directory not found. Please ensure Script 02 ran successfully for year: ", current_year)
}

# 2. Define the path to the directory where ALL results will be saved.
# Dynamically routes results to the current year's folder.
storage_path = fs::path(project_root, "04_cleaned_data", current_year, "results_amsr2_FRZ")
# Create the main results directory if it doesn't already exist.
fs::dir_create(storage_path)

# 3. Set a switch to TRUE if you want to clip rasters to a specific area, otherwise FALSE.
clip_raster = TRUE

# 4. If clip_raster is TRUE, provide the path to the polygon shapefile (*.shp) for clipping.
shapefile_path = fs::path(project_root, "03_original_data", "00_basemaps", "research_blocks", "FRZ_densified_area_updated.shp") 
if (fs::file_exists(shapefile_path)) {
  shapefile_path = fs::path_real(shapefile_path)
}

# 5. Define the ice concentration threshold (from 0 to 100) for determining navigability.
# Any pixel with a value below or equal to this threshold will be considered "accessible".
threshold = 20 

###############################################
#
#                         Install and load needed packages
#
###############################################
# Check if the 'pacman' package manager is installed, and if not, install it.
if (!require("pacman")) install.packages("pacman")
# Use pacman to load all required packages, automatically installing any that are missing.
pacman::p_load(
  "tidyverse", # For data manipulation (dplyr, stringr, etc.)
  "terra",     # For high-performance raster processing
  "sf",        # For simple features (vector data like shapefiles)
  "fs",        # For modern file system operations
  "future",    # For setting up parallel processing
  "furrr",     # For applying functions in parallel
  "progressr"  # For creating progress bars
)

# Set up the progress bar handler so it works globally (for all functions).
progressr::handlers(global = TRUE)
# Specify the "progress" style, which creates a nice visual bar in terminals and RStudio.
progressr::handlers("progress")

###############################################
#
#                         Part 1: Setup and File Discovery
#
###############################################
# Print a message to the console indicating the start of Stage 1.
message("\n--- Stage 1: Creating Binary Accessibility Maps ---")

# --- Validate paths and create sub-directory for intermediate files
# Define the path for a sub-directory to store the intermediate binary maps.
intermediate_path = fs::path(storage_path, "01_intermediate_accessibility_maps")
# Create this intermediate directory if it doesn't already exist.
fs::dir_create(intermediate_path)

# --- List, filter, and checkpoint source files
# Get a list of all files ending in ".tif" from the source data directory.
source_files = fs::dir_ls(ice_data_path, glob = "*.tif") |>
  # Ensure all paths are absolute to prevent errors during parallel processing.
  fs::path_real()

# Create a data frame (a tibble) to manage the files that need to be processed.
files_to_process_df = tibble::tibble(source_path = source_files) |>
  # Exclude any files from February 29th by checking if the filename contains "0229-".
  dplyr::filter(!stringr::str_detect(fs::path_file(source_path), "0229-")) |>
  # For each source file, create a corresponding output path and filename for the binary map.
  dplyr::mutate(
    target_path = fs::path(intermediate_path, paste0("accessmap_", fs::path_file(source_path)))
  ) |>
  # Keep only the files that have NOT already been processed. This allows the script
  # to be re-run without re-doing completed work.
  dplyr::filter(!fs::file_exists(target_path))

# --- Report status
# Count the total number of source files found.
num_total = length(source_files)
# Count how many files were excluded because they were from a leap day.
num_excluded = length(source_files) - nrow(tibble(source_path = source_files) |> dplyr::filter(!stringr::str_detect(fs::path_file(source_path), "0229-")))
# Count how many files remain to be processed after all filtering.
num_remaining = nrow(files_to_process_df)
# Calculate how many files have already been processed in previous runs.
num_processed = num_total - num_excluded - num_remaining

# Print a summary of the file counts to the console using 'sprintf' for clean formatting.
message(sprintf("Found %d total source files.", num_total))
message(sprintf("Excluded %d leap day (Feb 29) files.", num_excluded))
message(sprintf("%d files have already been processed and will be skipped.", num_processed))

# If there are no new files to process, stop the script with an informative message.
if (num_remaining == 0) {
  stop("All source files have been processed. You can proceed to Script 05.")
}
# If there are new files, report how many will be processed.
message(sprintf("Processing %d new files...", num_remaining))


# --- Load shapefile and check projection if clipping
# This block only runs if the 'clip_raster' switch was set to TRUE.
if (clip_raster) {
  # Check if the specified shapefile exists; if not, stop the script with an error.
  if (!fs::file_exists(shapefile_path)) stop("Clipping is TRUE but shapefile not found.")
  # Read the shapefile into an 'sf' object for spatial operations.
  shape_sf = sf::read_sf(shapefile_path)
  
  # --- START OF MODIFICATION ---
  # Check if the shapefile's Coordinate Reference System (CRS) is EPSG:6932. If not, reproject it.
  # This ensures all spatial data uses the same projection for accurate overlays.
  if (sf::st_crs(shape_sf)$epsg != 6932) {
    message("Reprojecting shapefile to EPSG:6932...")
    # 'st_transform' reprojects the vector data to the specified CRS.
    shape_sf = sf::st_transform(shape_sf, crs = "EPSG:6932")
  }
  # --- END OF MODIFICATION ---
}

###############################################
#
#                         Part 2: Main Processing Loop
#
###############################################
# Setup parallel processing
# Determine the number of CPU cores to use (all available cores minus one, to keep the system responsive).
workers_to_use = max(1, future::availableCores() - 1)
# Set the parallel processing plan. 'multisession' starts new R sessions in the background.
future::plan(future::multisession, workers = workers_to_use)
# Inform the user how many cores are being used.
message(paste("\nStarting parallel processing on", workers_to_use, "cores..."))


# Wrap the processing loop with 'with_progress' to enable the progress bar.
progressr::with_progress({
  # Use 'furrr::future_walk2' to process files in parallel. It iterates over two lists
  # ('source_path' and 'target_path') and applies a function. 'walk' is used for functions
  # called for their side effects (like writing a file), not for returning a value.
  furrr::future_walk2(
    .x = files_to_process_df$source_path, # The first list to iterate over (input files).
    .y = files_to_process_df$target_path, # The second list to iterate over (output files).
    # The function to apply to each pair of source and target files.
    .f = function(source_file, target_file) {
      # Use 'tryCatch' to handle any errors for a single file without stopping the entire script.
      tryCatch({
        # 1. Load the raw raster file into a 'terra' SpatRaster object.
        r_raw = terra::rast(source_file)
        
        # --- START OF MODIFICATION ---
        # 2. Check and reproject raster if it is not in the target projection.
        target_crs = "EPSG:6932"
        # We compare the full PROJ strings ('proj=TRUE') to be certain they are identical.
        if (terra::crs(r_raw, proj=TRUE) != terra::crs(target_crs, proj=TRUE)) {
          # 'terra::project' reprojects the raster. 'method="near"' is nearest neighbour
          # resampling, which is appropriate for thematic data as it doesn't invent new pixel values.
          r_raw = terra::project(r_raw, target_crs, method="near")
        }
        # --- END OF MODIFICATION ---
        
        # 3. Clip the raster if the 'clip_raster' switch is TRUE.
        if (clip_raster) {
          # Convert the sf object to a terra SpatVector for processing with terra functions.
          shape_to_clip = terra::vect(shape_sf)
          
          # Apply a 5,000-meter buffer to the shapefile to avoid edge effects when masking.
          shape_buffered = terra::buffer(shape_to_clip, width = 5000)
          
          # Crop the raster to the extent of the *buffered* shapefile (a fast rectangular cut).
          r_raw = terra::crop(r_raw, shape_buffered)
          
          # Mask the raster using the *buffered* shapefile polygon (sets values outside to NA).
          r_raw = terra::mask(r_raw, shape_buffered)
        }
        
        # 4. Reclassify the raster's pixel values to create a binary (0/1) map.
        # Define the reclassification rules in a matrix: [from, to, new_value].
        m = rbind(
          c(100.0001, Inf, NA),          # Rule 1: Values > 100 (land, coastlines, etc.) become NA.
          c(threshold + 0.0001, 100, 0), # Rule 2: Values above the threshold become 0 (not accessible).
          c(-1, threshold, 1)            # Rule 3: Values from -1 up to the threshold become 1 (accessible).
        )
        
        # Apply the reclassification matrix to the raster.
        r_access = terra::classify(r_raw, rcl = m)
        
        # 5. Save the new binary raster map to a GeoTIFF file.
        # 'overwrite=TRUE' allows it to replace an existing file. 'gdal=c("COMPRESS=LZW")' uses
        # lossless compression to reduce file size.
        terra::writeRaster(r_access, filename = target_file, overwrite = TRUE, gdal = c("COMPRESS=LZW"))
        
        # This part of 'tryCatch' defines what to do if an error occurs.
      }, error = function(e) {
        # Print a message to the console with the filename and the error, then continue.
        message(sprintf("\nError processing %s: %s. Skipping.", fs::path_file(source_file), e$message))
      })
    },
    # Options for the parallel processing. 'seed = TRUE' ensures reproducibility for any random processes.
    .options = furrr_options(seed = TRUE),
    # This automatically connects to 'progressr' to create and update the progress bar.
    .progress = TRUE
  )
})

# Print a final message to the console indicating the script has finished successfully.
message("\n=======================================================")
message("SUCCESS! Stage 1 Complete. All accessibility maps created.")
message("Intermediate maps are located in:\n  ", intermediate_path)
message("=======================================================\n")
message("Script 04 finished. You can now run Script 05.")