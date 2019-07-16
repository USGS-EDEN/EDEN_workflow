# EDEN Workflow

EDEN Workflow is an attempt to reproduce the EDEN daily (and other) workflows, including ADAM input, EDENdb data upload, EDEN model input, EDEN surface creation, ERTP duration hydrographs, CSSS & WADEM webapp data (including scenarios), CoastalEDEN, and EDENdb rainfall and ET data workflows in an R environment.

## Description of Workflow folders

- **ADAM_input:** (runs daily at 7:15AM) Download of daily data from USGS, SFWMD, and ENP; collating; and formatting for ADAM input. Includes script for generating arbitrary length multiday files (multiday runs daily at 7:25AM, by default for 1 and 7 days).
- **coastal_EDEN:** (runs daily at 7:30AM; SC/GA data runs at 7:45AM) Download daily FL and SC/GA coastal data, calculate salinities and CSIs, populate database, generate plots, and transfer files.
- **coastal_hydrograph_get.sh:** (on stpweb1, runs daily at 8:30AM) Retrieve CoastalEDEN files to webserver. **(To do: Convert code to R and add to repo.)**
- **EDENdb_upload:** (runs daily at 12:24PM) Retrieve daily output from ADAM, process, load into EDENdb, create EDEN model input data, generate annotated daily median files, and update EDENdb daily values table.
_ **EDENv3:** (runs daily at 12:30) Run the EDEN model and create .nc and geotiff files.
- **netCDF_headers**: (runs daily at 2:10PM) Modify the EDEN model output netCDF files' headers.
- **stpweb1_realtime_surface_jpgs.sh:** (on stpweb1, runs daily at 2:30PM, runs stpweb1_realtime_surface_jpgs.r) Generate the thumbnails and contour images for the EDEN website. **(To do: Update code and get R script into repo.)**
- **CSSS_viewer**: (runs daily at 2:30PM) Generate the CSSS webapp data, statistics, images, and report.
- **ERTP_hydrographs** (runs daily at 2:35PM) Produce gage and tree island ERTP duration hydrograph plots and data, transfer files, and prepare alert email.
- **WADEM**: (runs daily at 2:45PM) Generate the WADEM webapp data, images, graphs.
_ **csss_get.sh:** (on stpweb1, runs daily at 2:59PM) Retrieve CSSS and WADEM files to webserver. **(To do: Convert code to R and add to repo.)**
_ **duration_hydrographs_get.sh:** (on stpweb1, runs daily at 4:30PM) Retrieve ERTP files to webserver. **(To do: Convert code to R and add to repo.)**
- **EDENdb_backup_restore:** Replace selcted EDENdb timestamps with backup .CSV data.
- **et_rainfall_processing:** Download monthly rainfall files and annual evapotranspiration data files, process, and populate EDENdb. Also add historic rainfall and ET data to EDENdb for newly-added EDEN gages.
- **CSSS_scenarios**: Generate the WADEM webapp data, images, graphs for CSSS hypothetical scenarios.
- **WADEM_scenarios**: Generate the WADEM webapp data, images, graphs for WADEM hypothetical scenarios.
