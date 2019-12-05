# -------------
# Bryan McCloskey
# bmccloskey@usgs.gov
# St. Petersburg Coastal and Marine Science Center
# US Geological Survey
#
# 07/26/2019
#--------------

print("These libraries must be installed: ncdf4, RColorBrewer, RCurl")
# Required libraries. If not present, run:
# install.packages(c("ncdf4", "RColorBrewer", "RCurl"))
library (ncdf4)
library (RColorBrewer)
library (RCurl)

try (setwd("./realtime_jpgs"), silent = T)

# Generate color palette
stage.col <- brewer.pal(11, "Spectral")
tmp <- col2rgb(stage.col)
tmp2 <- matrix(rep(0, 243), 3, 81)
for (j in 1:3) {
  for (i in 1:length(tmp[1, ])) {
    tmp2[, i * 2 - 1] <- tmp[, i]
    if (i != length(tmp[1, ])) tmp2[, i * 2] <-(tmp[, i] + tmp[, i + 1]) / 2
  }
  tmp <- tmp2[, 1:(i * 2 - 1)]
}
stage.col <- rgb(t(tmp2[, 1:81]), maxColorValue = 255)

# Set up WL & depth files
cur_qtr <- paste0(as.POSIXlt(Sys.Date() - 1)$year + 1900, "_", tolower(quarters(Sys.Date() - 1)))
pre_qtr <- paste0(as.POSIXlt(Sys.Date() - 90)$year + 1900, "_", tolower(quarters(Sys.Date() - 90)))
err <- try (download.file(paste0("https://sofia.usgs.gov/eden/data/realtime2/", cur_qtr, "_v3rt_nc.zip"), paste0("./surfaces/", cur_qtr, ".zip")))
unzip(paste0("./surfaces/", cur_qtr, ".zip"), exdir = "./surfaces")
file.rename(paste0("./surfaces/", cur_qtr, "_v3rt.nc"), paste0("./surfaces/", cur_qtr, ".nc"))
err <- try (download.file(paste0("https://sofia.usgs.gov/eden/data/realtime2/", pre_qtr, "_v3rt_nc.zip"), paste0("./surfaces/", pre_qtr, ".zip")))
unzip(paste0("./surfaces/", pre_qtr, ".zip"), exdir = "./surfaces")
file.rename(paste0("./surfaces/", pre_qtr, "_v3rt.nc"), paste0("./surfaces/", pre_qtr, ".nc"))
err <- try (download.file(paste0("https://sofia.usgs.gov/eden/data/realtime2/", cur_qtr, "_v3rt_depth_nc.zip"), paste0("./surfaces/", cur_qtr, ".zip")))
unzip(paste0("./surfaces/", cur_qtr, ".zip"), exdir = "./surfaces")
file.rename(paste0("./surfaces/", cur_qtr, "_v3rt_depth.nc"), paste0("./surfaces/", cur_qtr, "_depth.nc"))
err <- try (download.file(paste0("https://sofia.usgs.gov/eden/data/realtime2/", pre_qtr, "_v3rt_depth_nc.zip"), paste0("./surfaces/", pre_qtr, ".zip")))
unzip(paste0("./surfaces/", pre_qtr, ".zip"), exdir = "./surfaces")
file.rename(paste0("./surfaces/", pre_qtr, "_v3rt_depth.nc"), paste0("./surfaces/", pre_qtr, "_depth.nc"))
unlink("./surfaces/*.zip")

s.nc <- nc_open(paste0("./surfaces/", cur_qtr, ".nc"))
x <- ncvar_get(s.nc, "x")
y <- ncvar_get(s.nc, "y")
time <- ncvar_get(s.nc, "time")
time <- as.Date(s.nc$dim$time$units, format = "days since %Y-%m-%dT%H:%M:%S +0000") + time
stage <- ncvar_get(s.nc, "stage")
nc_close(s.nc)
d.nc <- nc_open(paste0("./surfaces/", cur_qtr, "_depth.nc"))
time2 <- ncvar_get(d.nc, "time")
time2 <- as.Date(d.nc$dim$time$units, format = "days since %Y-%m-%dT%H:%M:%S +0000") + time2
depth <- ncvar_get(d.nc, "depth")
nc_close(d.nc)
if (length(dim(stage)) == 2) dim(stage) <- dim(depth) <- c(287, 405, 1) #1st day of quarter defaults to matrix otherwise

for (i in length(time2):1) {
  jpeg(paste0("./images/EDENsurface_", format(time2[i], format = "%Y%m%d"), "_depth.jpg"), width = 800, height = 1000, quality = 100)
  filled.contour(x, y, depth[, , i], zlim = c(0, 267), levels = seq(0, 267, by = 6.5), col = stage.col[40:81], plot.title = title(paste("EDEN Real-Time Water Depth for", format(time2[i], format = "%m/%d/%Y"))), key.title = title(main = "water\ndepth,\ncm"), key.axes = axis(4, seq(0, 250, by = 50)), asp = 1, axes = F, frame.plot = T)
  dev.off()
  jpeg(paste0("./images/EDENsurface_", format(time2[i], format="%Y%m%d"), "_depth_contour.jpg"), width = 800, height = 1000, quality = 100)
  filled.contour(x, y, depth[, , i], zlim = c(0, 267), levels = seq(0, 267, by = 6.5), col = stage.col[40:81], plot.title = title(paste("EDEN Water Depth for", format(time2[i], format = "%m/%d/%Y"))), plot.axes = { contour(x, y, depth[, , i], zlim = c(0, 267), levels = seq(0, 250, by = 25), lwd = c(1.5, 1), add = T) }, key.title = title(main = "water\ndepth, cm"), key.axes = axis(4, seq(0, 250, by = 50)), asp = 1, axes = F, frame.plot = T)
  dev.off()
  jpeg(paste0("./images/EDENsurface_", format(time2[i], format = "%Y%m%d"), "_depth_small.jpg"), width = 25, height = 35, quality = 100)
  par(mar = c(0, 0, 0, 0))
  image(x, y, depth[, , i], zlim = c(0, 267), col = stage.col[40:81], xlab = "", xaxt = "n", ylab = "", yaxt = "n", frame.plot = F)
  dev.off()
  try (ftpUpload(paste0("./images/EDENsurface_", format(time2[i], format = "%Y%m%d"), "_depth.jpg"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden-data/jpg/EDENsurface_", format(time2[i], format = "%Y%m%d"), "_depth.jpg"), .opts = list(forbid.reuse = 1)))
  try (ftpUpload(paste0("./images/EDENsurface_", format(time2[i], format = "%Y%m%d"), "_depth_contour.jpg"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden-data/jpg/EDENsurface_", format(time2[i], format = "%Y%m%d"), "_depth_contour.jpg"), .opts = list(forbid.reuse = 1)))
  try (ftpUpload(paste0("./images/EDENsurface_", format(time2[i], format = "%Y%m%d"), "_depth_small.jpg"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden-data/jpg/EDENsurface_", format(time2[i], format = "%Y%m%d"), "_depth_small.jpg"), .opts = list(forbid.reuse = 1)))
}
for (i in length(time):1) {
  jpeg(paste0("./images/EDENsurface_", format(time[i], format = "%Y%m%d"), ".jpg"), width = 800, height = 1000, quality = 100)
  filled.contour(x, y, stage[, , i], zlim = c(-100, 548), levels = seq(-100, 548, by = 8), col = stage.col, plot.title = title(paste("EDEN Real-Time Water Surface for", format(time[i], format = "%m/%d/%Y"))), key.title = title(main = "water\nlevel, cm,\nNAVD88"), key.axes = axis(4, seq(-100, 550, by = 100)), asp = 1, axes = F, frame.plot = T)
  dev.off()
  jpeg(paste0("./images/EDENsurface_", format(time[i], format = "%Y%m%d"), "_contour.jpg"), width = 800, height = 1000, quality = 100)
  filled.contour(x, y, stage[, , i], zlim = c(-100, 548), levels = seq(-100, 548, by = 8), col = stage.col, plot.title = title(paste("EDEN Water Surface for", format(time[i], format = "%m/%d/%Y"))), plot.axes = { contour(x, y, stage[, , i], zlim = c(-100, 548), levels = seq(-100, 550, by = 50), lwd = c(1.5, 1), add = T) }, key.title = title(main = "water\nlevel, cm,\nNAVD88"), key.axes = axis(4, seq(-100, 550, by = 100)), asp = 1, axes = F, frame.plot = T)
  dev.off()
  jpeg(paste0("./images/EDENsurface_", format(time[i], format = "%Y%m%d"), "_small.jpg"), width = 25, height = 35, quality = 100)
  par(mar = c(0, 0, 0, 0))
  image(x, y, stage[, , i], zlim = c(-100, 548), col = stage.col, xlab = "", xaxt = "n", ylab = "", yaxt = "n", frame.plot = F)
  dev.off()
  try (ftpUpload(paste0("./images/EDENsurface_", format(time2[i], format = "%Y%m%d"), ".jpg"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden-data/jpg/EDENsurface_", format(time2[i], format = "%Y%m%d"), ".jpg"), .opts = list(forbid.reuse = 1)))
  try (ftpUpload(paste0("./images/EDENsurface_", format(time2[i], format = "%Y%m%d"), "_contour.jpg"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden-data/jpg/EDENsurface_", format(time2[i], format = "%Y%m%d"), "_contour.jpg"), .opts = list(forbid.reuse = 1)))
  try (ftpUpload(paste0("./images/EDENsurface_", format(time2[i], format = "%Y%m%d"), "_small.jpg"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden-data/jpg/EDENsurface_", format(time2[i], format = "%Y%m%d"), "_small.jpg"), .opts = list(forbid.reuse = 1)))
}
setwd("..")
