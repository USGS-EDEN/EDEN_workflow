# -------------
# Bryan McCloskey
# bmccloskey@usgs.gov
# St. Petersburg Coastal and Marine Science Center
# US Geological Survey
#
# 07/13/2018
#--------------

print("These libraries must be installed: ncdf4, RCurl")
# Required libraries. If not present, run:
# install.packages("ncdf4")
# install.packages("RCurl")
library (ncdf4)
library (RCurl)

try (setwd("./netCDF_headers"), silent = T)

# Clean up old files
unlink("./surfaces/*.nc")
# Calculate quarter for yesterday
# Manually adjust subtracted days for other quarters
cur_qtr <- paste0(as.POSIXlt(Sys.Date() - 1)$year + 1900, "_", tolower(quarters(Sys.Date() - 1)))

ftp_dir <- "ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden-data/netcdf/"
wl_file <- paste0(cur_qtr, ".nc")
d_file <- paste0("d", cur_qtr, ".nc")
err <- try (download.file(paste0(ftp_dir, wl_file), paste0("./surfaces/", wl_file)))
err <- try (download.file(paste0(ftp_dir, d_file), paste0("./surfaces/", d_file)))

d.nc <- nc_open(paste0("./surfaces/", d_file), write = T)
ncvar_rename(d.nc, "stage", "depth")
ncatt_put(d.nc, "stage", "long_name", "depth")
depth <- ncvar_get(d.nc, "stage")
# Optional step to set < 0 values to 0
#depth[depth < 0] <- 0
#ncvar_put(d.nc, "stage", depth)
max <- max(depth, na.rm = T)
min <- min(depth, na.rm = T)
ncatt_put(d.nc, "transverse_mercator", "semi_major_axis", 6378137)
ncatt_put(d.nc, "transverse_mercator", "inverse_flattening", 298.257222101, "double")
ncatt_put(d.nc, "stage", "min", min, "double")
ncatt_put(d.nc, "stage", "max", max, "double")
nc_close(d.nc)

wl.nc <- nc_open(paste0("./surfaces/", wl_file), write = T)
stage <- ncvar_get(wl.nc, "stage")
max <- max(stage, na.rm = T)
min <- min(stage, na.rm = T)
ncatt_put(wl.nc, "transverse_mercator", "semi_major_axis", 6378137)
ncatt_put(wl.nc, "transverse_mercator", "inverse_flattening", 298.257222101, "double")
ncatt_put(wl.nc, "stage", "min", min, "double")
ncatt_put(wl.nc, "stage", "max", max, "double")
nc_close(wl.nc)

err <- try (ftpUpload(paste0("./surfaces/", wl_file), paste0(ftp_dir, wl_file)))
err <- try (ftpUpload(paste0("./surfaces/", d_file), paste0(ftp_dir, "d", cur_qtr, "_fixed.nc")))
setwd("..")
