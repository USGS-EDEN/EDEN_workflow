# EDEN Workflow

EDEN Workflow is an attempt to reproduce the EDEN daily (and other) workflow, including ADAM input, EDENdb data upload, EDEN model input, EDEN surface prep, ERTP duration hydrographs, CSSS & WADEM webapp data, CoastalEDEN, and EDENdb rainfall and ET data in the R environment.

## Description of Workflow folders

- **ADAM_input:** Download of daily data from USGS, SFWMD, and ENP, collating, and formatting for ADAM input. Includes script for generating arbitrary length multiday files.
- **EDENdb_upload:** Retrieve daily output from ADAM, process, load into EDENdb, create EDEN model input data, generate annotated daily median files, and update EDENdb daily values table.
- **EDENdb_backup_restore:** Replace selcted EDENdb timestamps with backup .CSV data.
- **netCDF_headers**: Modify the EDEN model output netCDF files' headers.
- **CSSS_viewer**: Generate the CSSS webapp data, statistics, images, and report.
- **WADEM**: Generate the WADEM webapp data, images, graphs.
- **ERTP_hydrographs** Produce gage and tree island ERTP duration hydrograph plots and data, transfer files, and prepare alert email.
- **coastal_EDEN:** Download daily FL and SC/GA coastal data, calculate salinities and CSIs, populate database, generate plots, and transfer files.
- **et_rainfall_processing:** Download monthly rainfall files and annual evapotranspiration data files, process, and populate EDENdb. Also add historic rainfall and ET data to EDENdb for newly-added EDEN gages.