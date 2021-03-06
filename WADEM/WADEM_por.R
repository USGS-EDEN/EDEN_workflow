# -------------
# Bryan McCloskey
# bmccloskey@usgs.gov
# St. Petersburg Coastal and Marine Science Center
# US Geological Survey
#
# 08/09/2018
#--------------

print("These libraries must be installed: ncdf4, abind, RCurl")
# Required libraries. If not present, run:
# install.packages("ncdf4")
# install.packages("abind")
# install.packages("RCurl")
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

file_surf <- list.files("../surfaces", "^[0-9]{4}_q[1-4].nc$")
cold_rwb <- c("#b2182b", "#ef8a62", "#fddbc7", "#f7f7f7", "#d1e5f0", "#67a9cf", "#2166ac")
cold_bgb <- c("#895b44", "#cdaa66", "#f5ca7b", "#7bed01", "#bdd2ff", "#73b2ff", "#0b2c7b")
colr <- c("#762a83", "#af8dc3", "#e7d4e8", "#f7f7f7", "#d9f0d3", "#7fbf7b", "#1b7837")
brks <- c(-1000, -8.93, 2.63, 13.29, 19.88, 31.36, 43.53, 1000)

# depth bins
for (i in 1:length(file_surf)) {
  print(file_surf[i])
  surf.nc <- nc_open(paste0("../surfaces/", file_surf[i]))
  stage <- ncvar_get(surf.nc, "stage")
  t <- ncvar_get(surf.nc, "time")
  time <- as.Date(surf.nc$dim$time$units, format = "days since %Y-%m-%dT%H:%M:%S") + t
  nc_close(surf.nc)
  depth <- sweep(stage, c(1, 2), dem, "-")
  if (!dir.exists(paste0("./images/depth_rwb_", format(time[1],'%Y')))) dir.create(paste0("./images/depth_rwb_", format(time[1],'%Y')))
  if (!dir.exists(paste0("./images/depth_bgb_", format(time[1],'%Y')))) dir.create(paste0("./images/depth_bgb_", format(time[1],'%Y')))
  
  for (j in 1:length(time)) {
    f <- paste0(sprintf("trans%04d", as.POSIXlt(time[j])$yday), ".png")
    png(paste0("./images/depth_rwb_", format(time[j], '%Y'), "/", f), width = 614, height = 862, bg = "transparent", type = "quartz")
    par(mar = c(0, 0, 0, 0))
    image(x, y, depth[, , j], col = cold_rwb, breaks = brks, axes = F, asp = 1)
    text(x[1], y[10], as.Date(time[j]), pos = 4)
    dev.off()
    #err <- try(ftpUpload(paste0("./images/depth_", format(time[j], '%Y'), "/", f), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/wadem/depth/depth_", format(time[j], '%Y'), "/", f)))
    png(paste0("./images/depth_bgb_", format(time[j], '%Y'), "/", f), width = 614, height = 862, bg = "transparent", type = "quartz")
    par(mar = c(0, 0, 0, 0))
    image(x, y, depth[, , j], col = cold_bgb, breaks = brks, axes = F, asp = 1)
    text(x[1], y[10], as.Date(time[j]), pos = 4)
    dev.off()
  }
}

all <- 46818 #length(which(!is.na(depth[, , 1])))
file_surf <- list.files("../surfaces", "^[0-9]{4}_q[1-2].nc$")
# middle three depth bins
for (i in 1:length(file_surf)) {
  if(i %% 2 == 1) mid3_area <- depth <- time <- NULL
  print(file_surf[i])
  surf.nc <- nc_open(paste0("../surfaces/", file_surf[i]))
  stage <- ncvar_get(surf.nc, "stage")
  t <- ncvar_get(surf.nc, "time")
  time <- as.Date(c(time, as.Date(surf.nc$dim$time$units, format = "days since %Y-%m-%dT%H:%M:%S") + t, recursive = T), origin = "1970/1/1")
  nc_close(surf.nc)
  depth <- abind(depth, sweep(stage, c(1, 2), dem, "-"))
  
  if(i %% 2 == 0) {
    for (j in 1:length(time)) {
      if(j == length(time)) m <- length(time) else m <- j + 1
      for (k in 1:length(x))
        for (l in 1:length(y))
          if(!is.na(depth[k, l, j]) & depth[k, l, j] <= 0) depth[k, l, j:m] <- 0
      mid3_area[j] <- 100 * length(which(depth[, , j] >= 2.63 & depth[, , j] < 31.36)) / all
    }
    png(paste0("./images/mid3_area/", format(time[j], '%Y'), "_area.png"), width = 600, height = 400, type = "quartz")
    plot(time, mid3_area, type = "l", lwd = 5, col = "darkblue", ylim = c(0, 100), main = paste("% area for optimal and suboptimal bins (≥ 2.63cm and < 31.36cm)"), xlab = "Date", ylab = "Percent area falling in middle three bins")
    dev.off()
  }
}

#recession rates -- first quarter must be handled seperately
surf.nc <- nc_open(paste0("../surfaces/", file_surf[1]))
stage <- ncvar_get(surf.nc, "stage")
t <- ncvar_get(surf.nc, "time")
time <- as.Date(surf.nc$dim$time$units, format = "days since %Y-%m-%dT%H:%M:%S") + t + 14
nc_close(surf.nc)
depth <- sweep(stage, c(1, 2), dem, "-")
rr <- (depth[, , 15:dim(depth)[3]] - depth[, , 1:(dim(depth)[3] - 14)]) / 14
brks2 <- c(-1000, -0.41, 0, 0.2, 0.51, 0.78, 1.02, 1000)
for (j in 1:(length(time) - 14)) {
  f <- paste0(sprintf("trans%04d", as.POSIXlt(time[j])$yday), ".png")
  if (!dir.exists(paste0("./images/rr_", format(time[1],'%Y')))) dir.create(paste0("./images/rr_", format(time[1],'%Y')))
  png(paste0("./images/rr_", format(time[j], '%Y'), "/", f), width = 614, height = 862, bg = "transparent", type = "quartz")
  par(mar = c(0, 0, 0, 0))
  image(x, y, rr[, , j], col = colr, breaks = brks2, axes = F, asp = 1)
  text(x[1], y[10], as.Date(time[j]), pos = 4)
  dev.off()
}
for (i in 113:length(file_surf)) {
  print(file_surf[i])
  surf.nc <- nc_open(paste0("../surfaces/", file_surf[i - 1]))
  stage <- ncvar_get(surf.nc, "stage")
  nc_close(surf.nc)
  depth <- sweep(stage, c(1, 2), dem, "-")
  dim1 <- dim(stage)[3]
  surf.nc <- nc_open(paste0("../surfaces/", file_surf[i]))
  stage <- ncvar_get(surf.nc, "stage")
  t <- ncvar_get(surf.nc, "time")
  time <- as.Date(surf.nc$dim$time$units, format = "days since %Y-%m-%dT%H:%M:%S") + t
  nc_close(surf.nc)
  dim2 <- dim(stage)[3]
  depth <- abind(depth, sweep(stage, c(1, 2), dem, "-"))
  rr <- (depth[, , (dim1 + 1):dim(depth)[3]] - depth[, , (dim1 - 13):(dim(depth)[3] - 14)]) / 14
  
  for (j in 1:length(time)) {
    f <- paste0(sprintf("trans%04d", as.POSIXlt(time[j])$yday), ".png")
    if (!dir.exists(paste0("./images/rr_", format(time[1], '%Y')))) dir.create(paste0("./images/rr_", format(time[1], '%Y')))
    png(paste0("./images/rr_", format(time[j], '%Y'), "/", f), width = 614, height = 862, bg = "transparent", type = "quartz")
    par(mar = c(0, 0, 0, 0))
    image(x, y, rr[, , j], col = colr, breaks = brks2, axes = F, asp = 1)
    text(x[1], y[10], as.Date(time[j]), pos = 4)
    dev.off()
  }
}

png("./images/wading_bird_legend_rwb.png", width = 33, height = 322, type = "quartz")
par(mar=c(0, 0, 0, 0))
plot(1:10, 1:10, type = "n")
rect(0, c(0, 1.95, 3.4, 4.85, 6.3, 7.75, 9.2), 10, c(1.95, 3.4, 4.85, 6.3, 7.75, 9.2, 10.65), col = cold_rwb, border = NA)
dev.off()
png("./images/wading_bird_legend_bgb.png", width = 33, height = 322, type = "quartz")
par(mar=c(0, 0, 0, 0))
plot(1:10, 1:10, type = "n")
rect(0, c(0, 1.95, 3.4, 4.85, 6.3, 7.75, 9.2), 10, c(1.95, 3.4, 4.85, 6.3, 7.75, 9.2, 10.65), col = cold_bgb, border = NA)
dev.off()
setwd("..")
