# EDEN Workflow

EDEN Workflow is an attempt to reproduce the EDEN daily (and other) workflow, including ADAM input, EDENdb data upload, EDEN model input, EDEN surface prep, ERTP duration hydrographs, CSSS webapp data, and CoastalEDEN in the R environment.

## Description of Workflow folders

- **ADAM_input:** Download of daily data from USGS, SFWMD, and ENP, collating, and formatting for ADAM input. Includes script for generating arbitrary length multiday files.
- **eden_database_upload:** Retrieve daily output from ADAM, process, load into EDENdb, create EDEN model input data, generate annotated daily median files, and update EDENdb daily values table.
- **eden_database_backup_restore:** Replace selcted EDENdb timestamps with backup .CSV data.
- **depth_header**: Modify the EDEN model output netCDF files' headers.
- **duration_hydrographs** Produce gage and tree island ERTP duration hydrograph plots and data, prepare email, generate CSSS webapp data and plots.
- **coastal_eden:** Download daily FL and SC/GA coastal data, calculate salinities and CSIs, populate database, generate plots, and transfer files.