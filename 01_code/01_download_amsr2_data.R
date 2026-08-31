################################################################################
# DOWNLOAD AMSR2 DATA YEARLY ###################################################
#
# Created by FT
# Created on 01/Aug/2025
# Updated/edited by FT
# Last updated/edited on 17/Aug/2026
#
# PURPOSE OF THIS SCRIPT:
# To efficiently download AMSR2 sea ice concentration data from the University
# of Bremen's data portal. This script checks for existing files and only
# downloads new or missing ones, facilitating periodic updates.
#
# DISCLAIMER:
# This script was edited and commented with the assistance of Google's Gemini 
# (version 2.5 PRO). And updated with Version 3.1 PRO.
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
# Clears all variables from the R environment to ensure a clean start.
rm(list = ls());
# Closes all open graphics windows, if any.
if(!is.null(dev.list())) dev.off();
# Clears the console screen for a clean output.
cat("\014")

#####################################################################################
#
#                         Install and load needed packages
#
#####################################################################################
# This checks if the 'pacman' package manager is installed. If not, it installs it.
if (!require("pacman")) install.packages("pacman")
# 'pacman::p_load' is used to load all required packages. It will automatically
# install any missing packages before loading them.
pacman::p_load(
  "tidyverse", # A collection of packages (dplyr, purrr, etc.) for data science.
  "rvest",     # A package for web scraping to extract links from HTML pages.
  "fs",        # Provides a cross-platform, uniform interface to file system operations.
  "future",    # A framework for setting up parallel processing.
  "furrr",     # Combines the mapping tools of 'purrr' with the parallelism of 'future'.
  "progressr"  # Provides a universal API for progress bars.
)

# Configures the 'progressr' package to display progress bars automatically.
# progressr::handlers(global = TRUE)
# Specifically selects a text-based progress bar style suitable for terminals.
# progressr::handlers("progress")

#####################################################################################
#
#                                  Configuration
#
#####################################################################################
# Prints a message to the console to indicate the start of the configuration section.
message("\n--- Configuration ---")

# Anchor the paths to the current working directory set by setwd()
project_root = getwd()

# --- Define URLs and date ranges
# The base URL of the data portal where the AMSR2 data is stored.
base_url = "http://data.seaice.uni-bremen.de/amsr2/asi_daygrid_swath/s6250/"
# Gets the current year from the system time and converts it to a number.
current_year = as.numeric(format(Sys.time(), "%Y"))
# Gets the current month to prevent scanning future months in the current year.
current_month = as.numeric(format(Sys.time(), "%m"))
# Creates a sequence of years to check for data, from 2012 to the current year.
years_to_check = 2012:current_year

# --- Define download directory safely based on the working directory
download_dir = fs::path(project_root, "03_original_data", "downloads_amsr2_WithLandMask_v5.4")

# Checks if the specified download directory already exists.
if (!fs::dir_exists(download_dir)) {
  # If the directory does not exist, this line creates it.
  fs::dir_create(download_dir)
  # Informs the user that the directory has been created.
  message("Created download directory at: ", download_dir)
}

# Confirms that the configuration setup is complete.
message("Configuration loaded.")

#####################################################################################
#
#                         Part 1: Discover Files
#
#####################################################################################
# Prints a message to indicate the start of the file discovery process.
message("\n--- Part 1: Discovering Remote and Local Files ---")

# --- Setup parallel processing
# Determines the number of CPU cores to use for parallel tasks. It leaves one core
# free to ensure the computer remains responsive.
# TIP: If the console output becomes too chaotic to read while debugging, change this to 1.
workers_to_use = max(1, future::availableCores() - 1)
# Sets up the parallel processing plan. 'multisession' is a robust choice that
# works on Windows, macOS, and Linux.
future::plan(future::multisession, workers = workers_to_use)
# Informs the user how many cores are being used for the task.
message("Using ", workers_to_use, " parallel workers for discovery.")

# --- Function to scrape one month's directory for .tif file links
# Defines a function that takes a year and a month as input.
get_monthly_links = function(year, month) {
  # Constructs the full URL for the specified year and month's data directory.
  dir_url = paste0(base_url, year, "/", month, "/Antarctic/")
  
  # PRINT THE DIRECTORY URL BEING SCANNED
  message("\n[SCANNING] URL: ", dir_url)
  
  # 'tryCatch' handles potential errors, such as a URL not existing (404 error).
  tryCatch({
    # Reads the HTML content from the directory URL.
    page_links = rvest::read_html(dir_url) |>
      # Selects all hyperlink elements ('<a>' tags) from the HTML.
      rvest::html_elements("a") |>
      # Extracts the 'href' attribute (the actual link) from each hyperlink.
      rvest::html_attr("href") |>
      # Filters the list to keep only links that end with "-v5.4.tif".
      str_subset("\\-v5.4.tif$")
    
    # Checks if any .tif file links were found on the page.
    if (length(page_links) > 0) {
      # If links were found, it creates a 'tibble' (a modern data frame).
      tibble::tibble(
        # The first column contains the filenames.
        filename = page_links,
        # The second column contains the full, direct download URL for each file.
        download_url = paste0(dir_url, filename)
      )
    } else {
      # If no .tif files are found, print a message and return NULL.
      message("[WARNING] URL accessed successfully, but no matching .tif files were found at: ", dir_url)
      NULL
    }
  }, warning = function(w) {
    # If a warning occurs (e.g., HTTP 404 Not Found), print it.
    message("[WARNING] Reading ", dir_url, " triggered a warning: ", conditionMessage(w))
    NULL
  }, error = function(e) {
    # If an error occurs, print it.
    message("[ERROR] Failed to read ", dir_url, " - Error: ", conditionMessage(e))
    NULL
  })
}

# --- Create all year-month combinations to check
# We use month indices (1-12) to easily filter out future months in the current year
tasks = tidyr::expand_grid(year = years_to_check, month_idx = 1:12) |>
  # Filter out months that haven't happened yet in the current year
  dplyr::filter(!(year == current_year & month_idx > current_month)) |>
  # Convert the valid indices back to lowercase month abbreviations (jan, feb, etc.)
  dplyr::mutate(month = tolower(month.abb[month_idx])) |>
  dplyr::select(year, month)

# --- Scrape all pages in parallel to get a master list of all available files
# Informs the user that the web scraping process is starting.
message("Scanning server for available files...")
# Wraps the parallel operation in 'with_progress' to display a progress bar.
# progressr::with_progress({
# 'future_map2_dfr' applies the 'get_monthly_links' function to each year-month
# pair in the 'tasks' data frame, running the operations in parallel.
remote_files = furrr::future_map2_dfr(
  tasks$year, tasks$month, get_monthly_links,
  # Ensures reproducibility in parallel computations.
  .options = furrr_options(seed = TRUE)
  # Enables the progress bar for this operation.
  # , .progress = TRUE
)
# })

# Reports the total number of files found on the remote server.
message("\nFound ", nrow(remote_files), " total files available on the server.")

# --- Get a list of files already downloaded
# Lists all files in the local download directory that end with ".tif".
local_files = fs::dir_ls(download_dir, glob = "*.tif") |>
  # Extracts just the filename from the full path for easy comparison.
  fs::path_file()

# Reports how many files were found in the local directory.
message("Found ", length(local_files), " files in the local directory.")

# --- Determine which files are missing by comparing the two lists
# Filters the list of remote files, keeping only those whose 'filename' is NOT
# present in the 'local_files' list.
files_to_download = remote_files |>
  dplyr::filter(!filename %in% local_files)

#####################################################################################
#
#                         Part 2: Download Files
#
#####################################################################################
# Prints a message to indicate the start of the download section.
message("\n--- Part 2: Downloading Missing Files ---")

# --- Check if there's anything to download and proceed accordingly
# Checks if the data frame of files to download has zero rows.
if (nrow(files_to_download) == 0) {
  # If there are no files to download, it informs the user that everything is up-to-date.
  message("Your local data is already up-to-date. Nothing to download.")
} else {
  # If there are files to download, it reports how many.
  message("Starting download of ", nrow(files_to_download), " new files...")
  
  # --- Function to download a single file
  # Defines a function to handle the download of one file.
  download_single_file = function(url, dest) {
    
    # PRINT SOURCE AND DESTINATION PATHS
    message("\n[DOWNLOADING] Source: ", url)
    message("[SAVING TO] Dest: ", dest)
    
    # Uses 'tryCatch' to handle potential download errors/warnings without stopping the script.
    tryCatch({
      # Downloads the file from the 'url' and saves it to the destination 'dest'.
      # 'mode = "wb"' (write binary) is crucial for non-text files like rasters.
      # CHANGED: 'quiet = FALSE' so download.file outputs its connection logs.
      download.file(url, destfile = dest, mode = "wb", quiet = FALSE)
    }, warning = function(w) {
      # Catch warnings (e.g., connection timeouts, file partly downloaded)
      message("[WARNING] Problem downloading ", url, " - ", conditionMessage(w))
    }, error = function(e) {
      # If a hard error occurs, it prints a message with the error details.
      message("[ERROR] Failed to download ", url, " - ", conditionMessage(e))
    })
  }
  
  # --- Create destination paths for the files to download
  # Creates the full local file path for each new file to be downloaded.
  destination_paths = fs::path(download_dir, files_to_download$filename)
  
  # --- Download the missing files in parallel with a progress bar
  # Wraps the download operation to show a progress bar.
  # progressr::with_progress({
  # 'future_walk2' is used because we are calling a function for its side effect
  # (downloading a file), not for a value it returns. It runs in parallel.
  furrr::future_walk2(
    files_to_download$download_url, # The first input: the list of URLs.
    destination_paths,              # The second input: the list of destination paths.
    download_single_file,           # The function to apply to each pair of inputs.
    .options = furrr_options(seed = TRUE) # Ensures reproducibility.
    # , .progress = TRUE            # Enables the progress bar.
  )
  # })
  
  # Informs the user that the download process has finished.
  message("\nDownload complete!")
}

# Final message to indicate that the entire script has finished running.
message("Script finished.")