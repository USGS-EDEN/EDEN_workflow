# -------------
# Bryan McCloskey
# bmccloskey@usgs.gov
# St. Petersburg Coastal and Marine Science Center
# US Geological Survey
#
# 07/26/2019
#--------------

print("These libraries must be installed: ncdf4, RCurl")
# Required libraries. If not present, run:
# install.packages(c("ncdf4", "RCurl"))
library (ncdf4)
library (RCurl)

try (setwd("./realtime_pngs"), silent = T)

# Color palette
stage.col <- c("#9E0142", "#A40843", "#AB1045", "#B21746", "#B91F48", "#C0274A", "#C72E4B", "#CE364D", "#D53E4F", "#D8434D", "#DC494C", "#E04F4A", "#E45549", "#E85B47", "#EC6146", "#F06744", "#F46D43", "#F57546", "#F67D4A", "#F7854E", "#F88D52", "#F99555", "#FA9D59", "#FBA55D", "#FDAE61", "#FDB466", "#FDBA6B", "#FDC070", "#FDC776", "#FDCD7B", "#FDD380", "#FDD985", "#FEE08B", "#FEE391", "#FEE798", "#FEEB9E", "#FEEFA5", "#FEF3AB", "#FEF7B2", "#FEFBB8", "#FFFFBF", "#FBFDBA", "#F8FCB5", "#F5FBB0", "#F2FAAB", "#EFF8A6", "#ECF7A1", "#E9F69C", "#E6F598", "#DEF299", "#D7EF9B", "#CFEC9C", "#C8E99E", "#C1E69F", "#B9E3A1", "#B2E0A2", "#ABDDA4", "#A2D9A4", "#99D6A4", "#91D2A4", "#88CFA4", "#7FCCA4", "#77C8A4", "#6EC5A4", "#66C2A5", "#5FBAA8", "#59B3AB", "#52ACAE", "#4CA5B1", "#459DB4", "#3F96B7", "#388FBA", "#3288BD", "#3780B9", "#3D79B6", "#4272B2", "#486BAF", "#4D64AC", "#535DA8", "#5856A5", "#5E4FA2")
depth.col <- c("#9ECAE1", "#9AC8E0", "#97C6DF", "#94C4DE", "#91C3DE", "#8EC1DD", "#8ABFDC", "#87BDDC", "#84BCDB", "#81BADA", "#7EB8DA", "#7AB6D9", "#77B5D8", "#74B3D8", "#71B1D7", "#6EAFD6", "#6BAED6", "#68ACD5", "#65AAD4", "#63A8D3", "#60A7D2", "#5EA5D1", "#5BA3D0", "#59A1CF", "#56A0CE", "#539ECD", "#519CCC", "#4E9ACB", "#4C99CA", "#4997C9", "#4795C8", "#4493C7", "#4292C6", "#3F8FC4", "#3D8DC3", "#3B8BC2", "#3989C1", "#3787C0", "#3585BF", "#3383BE", "#3181BD", "#2F7FBC", "#2D7DBB", "#2B7BBA", "#2979B9", "#2777B8", "#2575B7", "#2373B6", "#2171B5", "#1F6FB3", "#1D6DB1", "#1C6BB0", "#1A69AE", "#1967AD", "#1765AB", "#1663AA", "#1461A8", "#125FA6", "#115DA5", "#0F5BA3", "#0E59A2", "#0C57A0", "#0B559F", "#09539D", "#08519C", "#084E98", "#084C95", "#084A92", "#08488F", "#08468C", "#084489", "#084286", "#084083", "#083E80", "#083C7D", "#083A7A", "#083877", "#083674", "#083471", "#08326E", "#08306B")

# Set up WL & depth files
cur_qtr <- paste0(as.POSIXlt(Sys.Date() - 1)$year + 1900, "_", tolower(quarters(Sys.Date() - 1)))
err <- try (download.file(paste0("https://sofia.usgs.gov/eden/data/realtime2/", cur_qtr, "_v3rt_nc.zip"), paste0("./surfaces/", cur_qtr, ".zip")))
unzip(paste0("./surfaces/", cur_qtr, ".zip"), exdir = "./surfaces")
file.rename(paste0("./surfaces/", cur_qtr, "_v3rt.nc"), paste0("./surfaces/", cur_qtr, ".nc"))
err <- try (download.file(paste0("https://sofia.usgs.gov/eden/data/realtime2/", cur_qtr, "_v3rt_depth_nc.zip"), paste0("./surfaces/", cur_qtr, ".zip")))
unzip(paste0("./surfaces/", cur_qtr, ".zip"), exdir = "./surfaces")
file.rename(paste0("./surfaces/", cur_qtr, "_v3rt_depth.nc"), paste0("./surfaces/", cur_qtr, "_depth.nc"))
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
r <- range(stage, na.rm = T)
r2 <- range(depth, na.rm = T)

for (i in length(time2):1) {
  png(paste0("./images/EDENsurface_", format(time2[i], format = "%Y%m%d"), "_depth.png"), width = 800, height = 1000, type = "cairo")
  par(mar = c(0.7, 2.7, 3.7, 2.7))
  filled.contour(x, y, depth[, , i], zlim = c(0, 267), levels = seq(r2[1], r2[2], length.out = 81), col = depth.col, plot.title = title(paste("EDEN Real-Time Water Depth for", format(time2[i], format = "%m/%d/%Y"))), key.title = title(main = "water\ndepth,\ncm"), key.axes = axis(4, seq(0, 250, by = 50)), asp = 1, axes = F, frame.plot = T)
  dev.off()
  png(paste0("./images/EDENsurface_", format(time2[i], format="%Y%m%d"), "_depth_contour.png"), width = 800, height = 1000, type = "cairo")
  par(mar = c(0.7, 2.7, 3.7, 2.7))
  filled.contour(x, y, depth[, , i], zlim = c(0, 267), levels = seq(r2[1], r2[2], length.out = 81), col = depth.col, plot.title = title(paste("EDEN Water Depth for", format(time2[i], format = "%m/%d/%Y"))), plot.axes = { contour(x, y, depth[, , i], zlim = c(0, 267), levels = seq(0, 250, by = 25), lwd = c(1.5, 1), add = T) }, key.title = title(main = "water\ndepth, cm"), key.axes = axis(4, seq(0, 250, by = 50)), asp = 1, axes = F, frame.plot = T)
  dev.off()
  png(paste0("./images/EDENsurface_", format(time2[i], format = "%Y%m%d"), "_depth_small.png"), width = 25, height = 35, type = "cairo")
  par(mar = c(0, 0, 0, 0))
  image(x, y, depth[, , i], zlim = c(0, 267), col = depth.col, xlab = "", xaxt = "n", ylab = "", yaxt = "n", frame.plot = F)
  dev.off()
  try (ftpUpload(paste0("./images/EDENsurface_", format(time2[i], format = "%Y%m%d"), "_depth.png"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden-data/png/EDENsurface_", format(time2[i], format = "%Y%m%d"), "_depth.png"), .opts = list(forbid.reuse = 1)))
  try (ftpUpload(paste0("./images/EDENsurface_", format(time2[i], format = "%Y%m%d"), "_depth_contour.png"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden-data/png/EDENsurface_", format(time2[i], format = "%Y%m%d"), "_depth_contour.png"), .opts = list(forbid.reuse = 1)))
  try (ftpUpload(paste0("./images/EDENsurface_", format(time2[i], format = "%Y%m%d"), "_depth_small.png"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden-data/png/EDENsurface_", format(time2[i], format = "%Y%m%d"), "_depth_small.png"), .opts = list(forbid.reuse = 1)))
}
for (i in length(time):1) {
  png(paste0("./images/EDENsurface_", format(time[i], format = "%Y%m%d"), ".png"), width = 800, height = 1000, type = "cairo")
  par(mar = c(0.7, 2.7, 3.7, 2.7))
  filled.contour(x, y, stage[, , i], zlim = c(-100, 548), levels = seq(r[1], r[2], length.out = 81), col = stage.col, plot.title = title(paste("EDEN Real-Time Water Surface for", format(time[i], format = "%m/%d/%Y"))), key.title = title(main = "water\nlevel, cm,\nNAVD88"), key.axes = axis(4, seq(-100, 550, by = 100)), asp = 1, axes = F, frame.plot = T)
  dev.off()
  png(paste0("./images/EDENsurface_", format(time[i], format = "%Y%m%d"), "_contour.png"), width = 800, height = 1000, type = "cairo")
  par(mar = c(0.7, 2.7, 3.7, 2.7))
  filled.contour(x, y, stage[, , i], zlim = c(-100, 548), levels = seq(r[1], r[2], length.out = 81), col = stage.col, plot.title = title(paste("EDEN Water Surface for", format(time[i], format = "%m/%d/%Y"))), plot.axes = { contour(x, y, stage[, , i], zlim = c(-100, 548), levels = seq(-100, 550, by = 50), lwd = c(1.5, 1), add = T) }, key.title = title(main = "water\nlevel, cm,\nNAVD88"), key.axes = axis(4, seq(-100, 550, by = 100)), asp = 1, axes = F, frame.plot = T)
  dev.off()
  png(paste0("./images/EDENsurface_", format(time[i], format = "%Y%m%d"), "_small.png"), width = 25, height = 35, type = "cairo")
  par(mar = c(0, 0, 0, 0))
  image(x, y, stage[, , i], zlim = c(-100, 548), col = stage.col, xlab = "", xaxt = "n", ylab = "", yaxt = "n", frame.plot = F)
  dev.off()
  try (ftpUpload(paste0("./images/EDENsurface_", format(time2[i], format = "%Y%m%d"), ".png"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden-data/png/EDENsurface_", format(time2[i], format = "%Y%m%d"), ".png"), .opts = list(forbid.reuse = 1)))
  try (ftpUpload(paste0("./images/EDENsurface_", format(time2[i], format = "%Y%m%d"), "_contour.png"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden-data/png/EDENsurface_", format(time2[i], format = "%Y%m%d"), "_contour.png"), .opts = list(forbid.reuse = 1)))
  try (ftpUpload(paste0("./images/EDENsurface_", format(time2[i], format = "%Y%m%d"), "_small.png"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden-data/png/EDENsurface_", format(time2[i], format = "%Y%m%d"), "_small.png"), .opts = list(forbid.reuse = 1)))
}
setwd("..")
