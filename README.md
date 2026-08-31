# Repeated Accessibility Analysis

This repository contains the data*, code, and manuscript files* for the Sea Ice Repeated Accessibility (RA) analysis in the Southern Ocean. The project evaluates the repeated accessibility of areas in relation to sea ice to support spatial planning for the proposed Weddell Sea MPA (WSMPA) Phase 1 Fisheries Research Zone (FRZ) and CCAMLR Research Blocks (RB4 and RB5).

*data and manuscript files are not available in this github repository, but might be made available upon request.

## Directory Structure

The working directory is structured to separate raw data, intermediate processing, and final outputs, ensuring reproducibility across different analysis years.

*   **`01_code/`**: Contains all R scripts required to run the pipeline.
    *   `A_supporting_code/`: Additional helper scripts and functions.
    *   `B_old/`: Archive of previous script versions (e.g., deprecated scripts) (not available in this GitHub repository).
*   **`02_metadata/`**: Project-level metadata and documentation (not available in this GitHub repository).
*   **`03_original_data/`**: Raw, unmodified inputs including downloaded AMSR2 sea ice data, shapefiles, basemaps (e.g., Quantarctica, IBCSO bathymetry), and research block polygons(not available in this GitHub repository).
*   **`04_cleaned_data/`**: Processed intermediate data (e.g., binary accessibility maps, cleaned longline data). Outputs are dynamically routed into subfolders by year (e.g., `/2026/`)(not available in this GitHub repository).
*   **`05_results/`**: Final analytical outputs, including generated figures (maps and line plots) and dataframes (CSVs). Routed by year (not available in this GitHub repository).
*   **`06_literature/`**: Reference papers and background literature (e.g., Hendrik et al., 2022) (not available in this GitHub repository).
*   **`07_manuscript/`**: Drafts of reports (not available in this GitHub repository).

---

## ⚙️ Analysis Pipeline (Code Execution)

**Important:** The scripts in the `01_code` folder are highly interdependent and *must* be run in sequential order. 

The pipeline is designed to dynamically adapt to the current calendar year. It relies on a "data handoff" system where earlier scripts calculate temporal peaks and pass those dates to subsequent mapping scripts to ensure exact alignment without redundant calculations.

### Phase 1: Data Preparation
*   `01_download_amsr2_data.R`: Downloads the raw AMSR2 sea ice concentration data.
*   `02_get_metadata_and_missing_dates.R`: Generates metadata and identifies any missing temporal gaps in the raw satellite data.
*   `03_get_longline_data_points.R`: Cleans and projects the longline fishing point data for spatial analysis (kindly given to us every year by collaborators).

### Phase 2: Fisheries Research Zone (FRZ) Analysis
*   `04_FRZ_calc_accessibility_step1_amsr2.R`: Processes raw AMSR2 files, applies the navigability threshold, and creates daily binary accessibility maps for the FRZ.
*   `05_FRZ_calc_repeat_accessibility_step2_amsr2.R`: Uses the binary maps to calculate rolling Repeated Accessibility (RA) over a predefined time span.
*   `06_FRZ_plot_line_figure.R`: Calculates 5-day trailing means, generates an annual trend line plot, and exports the peak RA date for the maps.
*   `07_FRZ_plot_map_figure.R`: Imports the peak RA date to dynamically generate and save spatial maps (+/- 5 day window) of the FRZ with longline data overlays.

### Phase 3: Research Block 4 (RB4) Analysis
*   `08_RB4_calc_accessibility_step1_amsr2.R`: Creates daily binary accessibility maps specifically clipped to Research Block 48.6_4.
*   `09_RB4_calc_repeat_accessibility_step2_amsr2.R`: Calculates the rolling Repeated Accessibility (RA) rasters for RB4.
*   `10_RB_plot_line_figure.R`: Calculates 5-day trailing means, generates an annual trend line plot for RB4, and exports the peak RA date.
*   `11_RB_plot_map_figure.R`: Imports the peak RA date to dynamically generate and save spatial maps (+/- 5 day window) of RB4 with longline data overlays.

### Phase 4: Overlap Quantification
*   `12_calculate_overlap_percentages_combined.R`: Unifies the FRZ and RB4 calculations. It imports the peak dates, dynamically extracts the exact RA percentage value underneath every longline fishing point during the 11-day peak window, and outputs summary CSVs and bar plots.
