# -------------
# Bryan McCloskey
# bmccloskey@usgs.gov
# Saira Haider 
# shaider@usgs.gov
# US Geological Survey
#
# 08/10/2020
#--------------

print("These libraries must be installed: geospt, raster, reshape2, RCurl, EDEN")
# Required libraries. If not present, run:
# install.packages(c("geospt", "raster", "reshape2", "RCurl))
# remotes::install_git("https://code.usgs.gov/water/eden")
library (geospt)
library (raster)
library (reshape2)
library (RCurl)
library (EDEN)

try (setwd("./EDENv3"), silent = T)
source ("./netCDF_IO_v3.1.R")
source ("./EDENv3_functions.R")

yr <- strftime(Sys.Date() - 1, "%Y")
q <- (as.numeric(strftime(Sys.Date() - 1, "%m")) - 1) %/% 3 + 1
st <- as.Date(paste(yr, switch(q, "01", "04", "07", "10"), "01", sep = "-"))
# For realtime:
en <- Sys.Date() - 1
# For full quarters:
#en <- as.Date(paste(yr, switch(q, "03", "06", "09", "12"), switch(q, "31", "30", "30", "31"), sep = "-"))
quarter <- seq(st, en, "days")
output_nc <- paste0("./output/", yr, "_q", q, ".nc")
output_nc_d <- paste0("./output/d", yr, "_q", q, ".nc")
output_tif <- paste0("./output/s_", gsub("-", "", quarter), ".tif")
eden <- edenData(quarter) # Run interpolation for each day

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
report <- ''
if (!dir.exists("/Volumes/Users")) {
  #system("open 'smb://IGSAFPESAS022-dmz.er.usgs.gov/Users'")
  report <- "No mount to THREDDS server found; please run EDENv3.R manually to mount drive with AD credentials.\n"
}
err <- file.copy(output_nc, "/Volumes/Users/bmcclosk/Desktop/surfaces/", overwrite = T)
report <- if (!err) paste0(report, "WL surface file NOT transferred to THREDDS server\n") else paste0(report, "WL surface file transferred to THREDDS server\n")
err <- file.copy(output_nc_d, paste0("/Volumes/Users/bmcclosk/Desktop/depths/", yr, "_q", q, "_depth.nc"), overwrite = T)
report <- if (!err) paste0(report, "Depth surface file NOT transferred to THREDDS server\n") else paste0(report, "Depth surface file transferred to THREDDS server\n")
report <- paste0(report, "Check that internal THREDDS transfer succeeded (runs at 1pm): https://sflthredds.er.usgs.gov/thredds/catalog/eden/catalog.html")
to <- "bmccloskey@usgs.gov, hhenkel@usgs.gov"
system(paste0("echo 'Subject: THREDDS transfer ALERT
", report, "' | /usr/sbin/sendmail ", to))
setwd("..")
