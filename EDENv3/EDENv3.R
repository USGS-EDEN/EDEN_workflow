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

print("These libraries must be installed: RMySQL, geoR, geospt, raster, reshape2, RCurl, rgdal")
# Required libraries. If not present, run:
# install.packages(c("RMySQL", "geoR", "geospt", "raster", "reshape2", "RCurl", "rgdal"))
library (RMySQL)
library (geoR)
library (geospt)
library (raster)
library (reshape2)
library (RCurl)
library (rgdal)

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

yr <- strftime(Sys.Date() - 1, "%Y")
q <- (as.numeric(strftime(Sys.Date() - 1, "%m")) - 1) %/% 3 + 1
st <- as.Date(paste(yr, switch(q, "01", "04", "07", "10"), "01", sep = "-"))
en <- Sys.Date() - 1
quarter <- seq(st, en, "days")
output_nc <- paste0("./output/", yr, "_q", q, ".nc")
output_nc_d <- paste0("./output/d", yr, "_q", q, ".nc")
output_tif <- paste0("./output/s_", gsub("-", "", quarter), ".tif")
edenmaster <- edenGages(quarter) # Create edenmaster data.frame
gage_data <- gageData(edenmaster, quarter) # Create list contining daily gage data
eden <- lapply(gage_data, interpolate_gages, edenmaster) # Run interpolation for each day

# Set up DEM file
if (!file.exists("./input/eden_dem_cm_oc11.nc")) {
  err <- try (download.file("https://sofia.usgs.gov/eden/data/dem/eden_dem_cm_oc11.zip", "./input/eden_dem_cm_oc11.zip"))
  unzip("./input/eden_dem_cm_oc11.zip", "eden_dem_cm_oc11.nc", exdir = "./input")
}
dem.nc <- nc_open("./input/eden_dem_cm_oc11.nc")
dem <- ncvar_get(dem.nc, "dem")
nc_close(dem.nc)

eden_nc(eden, quarter, output_nc)
try (ftpUpload(output_nc, paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden-data/netcdf/", substring(output_nc, 10)), .opts = list(forbid.reuse = 1)))
for (i in 1:length(eden)) {
  print(output_tif[i])
  eden_raster(eden[[i]], output_tif[i])
  try (ftpUpload(output_tif[i], paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden-data/netcdf/", substring(output_tif[i], 10)), .opts = list(forbid.reuse = 1)))
}
eden_nc(eden, quarter, output_nc_d)
d.nc <- nc_open(output_nc_d, write = T)
ncvar_rename(d.nc, "stage", "depth")
ncatt_put(d.nc, "stage", "long_name", "Water Depth (cm)")
d <- ncvar_get(d.nc, "stage")
d <- sweep(d, c(1, 2), dem, "-")
d[d < 0] <- 0
ncvar_put(d.nc, "stage", d)
nc_close(d.nc)
try (ftpUpload(output_nc_d, paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden-data/netcdf/", substring(output_nc_d, 10)), .opts = list(forbid.reuse = 1)))
# Hack to transfer to a directory retrievable by the THREDDS server
file.copy(output_nc, "~/Desktop/RemoteDesktop/", overwrite = T)
file.copy(output_nc_d, paste0("~/Desktop/RemoteDesktop/", yr, "_q", q, "_depth.nc"), overwrite = T)
setwd("..")
