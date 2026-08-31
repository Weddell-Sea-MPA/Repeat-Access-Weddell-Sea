###############################################
# *Process and Filter Spatial Data*
###############################################
#
# Created by FT
# Created on 01/09/2025
# Updated/edited by FT
# Last updated/edited on 17/Aug/2026
#
# PURPOSE OF THIS SCRIPT:
# To load an XLSX file, filter it into two separate research 
# blocks, select relevant columns, and convert
# the coordinate data into 'sf' spatial points objects.
#
# DISCLAIMER:
# This script was edited and commented with the assistance of Google's Gemini 
# (version 2.5 PRO). And updated with Version 3.1 PRO.
#
################################################################################
# Initial cleanup
rm(list = ls()); if(!is.null(dev.list())) dev.off(); cat("\014")

###############################################
#
#            Install and load needed packages
#
###############################################

# Use pacman to handle package installation and loading
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, sf, fs)

###############################################
#
#                 Configuration
#
###############################################
message("\n--- Configuration ---")

# Anchor the paths to the current working directory set by setwd()
project_root = getwd()

# Get the current year to automatically find inputs and route outputs
current_year = as.character(format(Sys.time(), "%Y"))

# Define the dynamic input filename
input_filename = paste0(current_year, "_longline_48_6data.xlsx")
file_path = fs::path(project_root, "03_original_data", input_filename)

# Check if the expected input file exists to prevent confusing errors later
if (!fs::file_exists(file_path)) {
  stop("Could not find the input file. Please ensure it is named '", input_filename, "' and is in the '03_original_data' folder.")
}

# Define the output directory (dynamically nested in the current year)
output_dir = fs::path(project_root, "04_cleaned_data", current_year)
fs::dir_create(output_dir)

message("Configuration loaded. Reading data from:\n  ", file_path)

###############################################
#
#                 Data Processing
#
###############################################
message("\n--- Processing Data ---")

# --- 1. Load the Data ---
# Read the excel file into a dataframe.
raw_data_df = readxl::read_excel(file_path)


# --- 2. Create Separate Spatial Objects ---
# Process data for research block "486_4"
processed_sf_486_4 = raw_data_df %>%
  filter(research_block_set_start == "486_4") %>%
  st_as_sf(
    coords = c("longitude_set_start", "latitude_set_start"),
    crs = "EPSG:4326", # Set Coordinate Reference System to WGS 84
    remove = FALSE     # Keep the original coordinate columns
  ) %>%
  select(research_block_set_start, latitude_set_start, longitude_set_start, year, geometry)

# Process data for research block "486_5"
processed_sf_486_5 = raw_data_df %>%
  filter(research_block_set_start == "486_5") %>%
  st_as_sf(
    coords = c("longitude_set_start", "latitude_set_start"),
    crs = "EPSG:4326",
    remove = FALSE
  ) %>%
  select(research_block_set_start, latitude_set_start, longitude_set_start, year, geometry)


# --- 3. View the Results ---
# Print the first few rows of each final spatial dataframe
message("\nProcessing complete. Head of the 486_4 data:")
print(head(processed_sf_486_4))

message("\nHead of the 486_5 data:")
print(head(processed_sf_486_5))


###############################################
#
#                 Save Data
#
###############################################
message("\n--- Saving Results ---")

# --- Save data for block 486_4 ---
output_name_4 = paste0("longline_486_4_", current_year, ".gpkg")
output_file_4 = fs::path(output_dir, output_name_4)
sf::st_write(processed_sf_486_4, output_file_4, driver = "GPKG", delete_layer = TRUE, quiet = TRUE)

# --- Save data for block 486_5 ---
output_name_5 = paste0("longline_486_5_", current_year, ".gpkg")
output_file_5 = fs::path(output_dir, output_name_5)
sf::st_write(processed_sf_486_5, output_file_5, driver = "GPKG", delete_layer = TRUE, quiet = TRUE)

# Final summary message
message("\n=======================================================")
message("SUCCESS! Spatial data separated and saved.")
message("Block 486_4 saved to:\n  ", output_file_4)
message("Block 486_5 saved to:\n  ", output_file_5)
message("=======================================================\n")

message("Script 03 finished.")