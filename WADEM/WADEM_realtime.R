# -------------
# Bryan McCloskey
# bmccloskey@usgs.gov
# St. Petersburg Coastal and Marine Science Center
# US Geological Survey
#
# 08/08/2018
#--------------

print("These libraries must be installed: ncdf4, abind, RCurl")
# Required libraries. If not present, run:
# install.packages(c("ncdf4", "abind", "RCurl"))
library (ncdf4)
library (abind)
library (RCurl)

try (setwd("./WADEM"), silent = T)

# Set up DEM file
if (!file.exists("./input/eden_dem_cm_oc11.nc")) {
  err <- try (download.file("https://sofia.usgs.gov/eden/data/dem/eden_dem_cm_oc11.zip", "./input/eden_dem_cm_oc11.zip"))
  unzip("./input/eden_dem_cm_oc11.zip", "eden_dem_cm_oc11.nc", exdir = "./input")
}
dem.nc <- nc_open("./input/eden_dem_cm_oc11.nc")
x <- ncvar_get(dem.nc, "x")
y <- ncvar_get(dem.nc, "y")
dem <- ncvar_get(dem.nc, "dem")
nc_close(dem.nc)

file_surf <- tail(list.files("../surfaces", "^[0-9]{4}_q[1-4].nc$"), 2)
cold <- c("#d73027", "#fc8d59", "#fee090", "#ffffbf", "#e0f3f8", "#91bfdb", "#4575b4")
colr <- c("#762a83", "#af8dc3", "#e7d4e8", "#f7f7f7", "#d9f0d3", "#7fbf7b", "#1b7837")
brks <- c(-1000, -8.93, 2.63, 13.29, 19.88, 31.36, 43.53, 1000)

# depth bins
for (i in 1:length(file_surf)) {
  print(file_surf[i])
  surf.nc <- nc_open(paste0("../surfaces/", file_surf[i]))
  stage <- ncvar_get(surf.nc,"stage")
  t <- ncvar_get(surf.nc, "time")
  time <- as.Date(surf.nc$dim$time$units, format = "days since %Y-%m-%dT%H:%M:%S") + t
  nc_close(surf.nc)
  depth <- sweep(stage, c(1, 2), dem, "-")
  if (!dir.exists(paste0("./images/depth_", format(time[i],'%Y')))) dir.create(paste0("./images/depth_", format(time[i],'%Y')))
  
  for (j in 1:length(time)) {
    f <- paste0(sprintf("trans%04d", as.POSIXlt(time[j])$yday), ".png")
    png(paste0("./images/depth_", format(time[j], '%Y'), "/", f), width = 614, height = 862, bg = "transparent", type = "quartz")
    par(mar = c(0, 0, 0, 0))
    image(x, y, depth[, , j], col = cold, breaks = brks, axes = F, asp = 1)
    text(x[1], y[10], time[j], pos = 4)
    dev.off()
    err <- try(ftpUpload(paste0("./images/depth_", format(time[j], '%Y'), "/", f), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/wadem/depth/depth_", format(time[j], '%Y'), "/", f), .opts = list(forbid.reuse = 1)))
  }
}

# recession rates -- first quarter must be handled seperately
surf.nc <- nc_open(paste0("../surfaces/", file_surf[1]))
stage <- ncvar_get(surf.nc, "stage")
t <- ncvar_get(surf.nc, "time")
time <- as.Date(surf.nc$dim$time$units, format = "days since %Y-%m-%dT%H:%M:%S") + t + 14
nc_close(surf.nc)
surf.nc <- nc_open(paste0("../surfaces/", file_surf[2]))
stage <- abind(stage, ncvar_get(surf.nc, "stage"))
t <- ncvar_get(surf.nc, "time")
time <- as.Date(c(time, as.Date(surf.nc$dim$time$units, format = "days since %Y-%m-%dT%H:%M:%S") + t + 14, recursive = T))
nc_close(surf.nc)
depth <- sweep(stage, c(1, 2), dem, "-")
rr <- (depth[, , 15:dim(depth)[3]] - depth[, , 1:(dim(depth)[3] - 14)]) / 14
brks2 <- c(-1000, -0.41, 0, 0.2, 0.51, 0.78, 1.02, 1000)
for (j in 1:(length(time) - 14)) {
  f <- paste0(sprintf("trans%04d", as.POSIXlt(time[j])$yday), ".png")
  if (!dir.exists(paste0("./images/rr_", format(time[j],'%Y')))) dir.create(paste0("./images/rr_", format(time[j],'%Y')))
  png(paste0("./images/rr_", format(time[j], '%Y'), "/", f), width = 614, height = 862, bg = "transparent", type = "quartz")
  par(mar = c(0, 0, 0, 0))
  image(x, y, rr[, , j], col = colr, breaks = brks2, axes = F, asp = 1)
  text(x[1], y[10], time[j], pos = 4)
  dev.off()
  err <- try(ftpUpload(paste0("./images/rr_", format(time[j], '%Y'), "/", f), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/wadem/rr/rr_", format(time[j], '%Y'), "/", f), .opts = list(forbid.reuse = 1)))
}
setwd("..")
