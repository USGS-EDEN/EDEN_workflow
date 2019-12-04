# EDEN Workflow

EDEN Workflow is a repository to reproduce the EDEN daily (and other) workflows, including ADAM input data collation, EDENdb data processing and upload, EDEN model input data generation, EDEN surface creation and NetCDF production, ERTP duration hydrograph creation, CSSS & WADEM webapp data generation (including scenarios), CoastalEDEN, and EDENdb rainfall and ET data workflows.

## Installation

EDEN_workflow will most easily run within RStudio as a "project" -- this will ensure that the correct folder structure is used, that updates can be pulled down from GitHub, etc.
- Initial installation: Within RStudio, select `File -> New Project -> Version Control -> Git`. Enter "https://github.com/USGS-EDEN/EDEN_workflow.git", and select `Create Project`.
- To pull down the latest updates from GitHub, from within the RStudio EDEN_workflow project select the `Git` tab in the upper right pane and press the `Pull` button.
- Scripts requiring reading of the EDEN database (e.g., ADAM_input.R, EDENv3.R, etc.) require credentials stored in a usr_pwd.R file. Sripts requiring writing to the EDEN database (e.g., EDEN_upload.R, coastal_EDEN.R, etc.) require credentials stored in a admin_pwd.R file. Contact bmccloskey@usgs.gov for these login credentials.

## Description of Workflow folders

- **coastal_EDEN:** (runs daily at 3:00AM; SC/GA data runs at 3:15AM) Download daily FL and SC/GA coastal data, calculate salinities and CSIs, populate database, generate plots, and transfer files. Example crontab entry:
```
00 03 * * * cd ~/EDEN_workflow/; /usr/local/bin/R --vanilla < coastal_EDEN/coastal_EDEN.R > coastal_EDEN/coastal_EDEN.log 2>&1
```
- **coastal_hydrograph_get.sh:** (on stpweb1, runs daily at 8:30AM) Retrieve CoastalEDEN files to webserver. **(To do: Convert code to R and add to repo.)**
- **ADAM_input:** (runs daily at 7:15AM) Download of daily data from USGS, SFWMD, and ENP; collating; and formatting for ADAM input. Can be modified to generate arbitrary length multiday files (by default runs 7-, 4-, and 1-day files), with either local or remote (eFTP) source data files, via script or command line parameters (e.g., "/usr/local/bin/R --no-save --no-site-file < ADAM_input/ADAM_input.R 7 4 1" where "7 4 1" is a list of n-day files to generate).
- **EDENdb_upload:** (runs daily at 12:24PM) Retrieve daily output from ADAM, process, load into EDENdb, generate annotated daily median files, and update EDENdb daily values table.
_ **EDENv3:** (runs daily at 12:30) Run the EDEN model and create .nc and geotiff files.
- **realtime_jpgs:** (runs daily at 2:20PM) Generate the surface, thumbnail, and contour images for the EDEN website.
- **realtime_jpgs_get.sh:** (on stpweb1, runs daily at 2:30PM) Retrieves realtime surface jpgs to the web server.
- **CSSS_viewer**: (runs daily at 2:30PM) Generate the CSSS webapp data, statistics, images, and report.
- **ERTP_hydrographs** (runs daily at 2:35PM) Produce gage and tree island ERTP duration hydrograph plots and data, transfer files, and prepare alert email.
- **WADEM**: (runs daily at 2:45PM) Generate the WADEM webapp data, images, graphs.
- **csss_get.sh:** (on stpweb1, runs daily at 2:59PM) Retrieve CSSS and WADEM files to webserver. **(To do: Convert code to R and add to repo.)**
_ **duration_hydrographs_get.sh:** (on stpweb1, runs daily at 4:30PM) Retrieve ERTP files to webserver. **(To do: Convert code to R and add to repo.)**
- **EDENdb_backup_restore:** Replace selcted EDENdb timestamps with backup .CSV data.
- **et_rainfall_processing:** Download monthly rainfall files and annual evapotranspiration data files, process, and populate EDENdb. Also add historic rainfall and ET data to EDENdb for newly-added EDEN gages.

### Irregular/infrequent workflows
- **CSSS_scenarios**: Generate the CSSS webapp data, images, graphs for CSSS hypothetical scenarios.
- **WADEM_scenarios**: Generate the WADEM webapp data, images, graphs for WADEM hypothetical scenarios.
