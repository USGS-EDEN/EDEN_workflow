# -------------
# Bryan McCloskey
# bmccloskey@usgs.gov
# St. Petersburg Coastal and Marine Science Center
# US Geological Survey
#
# 06/11/2019
#
# based on:
# eden_v3.R by
# Saira Haider 
# shaider@usgs.gov
# US Geological Survey
#--------------

print("These libraries must be installed: RMySQL, geoR, geospt, raster, reshape2")
# Required libraries. If not present, run:
# install.packages(c("RMySQL", "geoR", "geospt", "raster", "reshape2"))
library (RMySQL)
library (geoR)
library (geospt)
library (raster)
library (reshape2)

try (setwd("./EDENv3"), silent = T)
source("./netCDF_IO_v3.1.R")
source ("../usr_pwd.R")
source("./EDENv3_functions.R")
# Connect to database, list of gages for which to acquire data
con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "eden_new", host = "stpweb1-dmz.er.usgs.gov")

## Convert subarea grids to anisotropic space outside of the function
# Import subareas' grid cell centroids
sa <- list.files("./input/subareas", full.names = T)
subareas <- lapply(sa, read.csv)
names(subareas) <- tools::file_path_sans_ext(basename(sa))
# Create new list of subareas with centroids in anisotropic space
subareas_aniso <- lapply(subareas, function(x) as.data.frame(coords.aniso(coords = x, aniso.pars = c(350 * pi / 180, 31 / 30))))
# Change column names
subareas_aniso <- lapply(subareas_aniso, setNames, c("x_aniso", "y_aniso"))

st <- as.Date("2019-04-01")
en <- Sys.Date() - 1
quarter <- seq(st, en, "days")
output_nc <- paste0("./output/2019_", tolower(quarters(en)), ".nc")
output_tif <- paste0("./output/", gsub("-", "", quarter), ".tif")
edenmaster <- edenGages(quarter) # Create edenmaster data.frame
gage_data <- gageData(edenmaster, quarter) # Create list contining daily gage data
eden <- lapply(gage_data, interpolate_gages, edenmaster) # Run interpolation for each day

eden_nc(eden, quarter, output_nc)
for (i in 1:length(eden)) eden_raster(eden[[i]], output_tif[i])
setwd("..")