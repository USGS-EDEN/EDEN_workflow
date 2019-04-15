library (ncdf4)
try (setwd("./WADEM"), silent = T)

fls <- list.files("../CSSS_viewer/input/", "^depth_")
sc <- paste0("COP", substr(fls, 20, 24)[1:4])
sc[4] <- paste0(sc[4], "9R")
sc[5:6] <- substr(fls[5:6], 7, 10)
s.nc <- nc_open(paste0("../CSSS_viewer/input/", fls[1]))
x <- ncvar_get(s.nc, "x")
y <- ncvar_get(s.nc, "y")
y2 <- rev(y)
t <- ncvar_get(s.nc, "t")
time <- as.Date(s.nc$dim$t$units, format = "days since %Y-%m-%dT%H:%M:%S +0000") + t
nc_close(s.nc)

red <- c(137, 205, 245, 123, 189, 115, 11)
grn <- c(91, 170, 202, 237, 210, 178, 44)
blu <- c(68, 102, 123, 1, 255, 255, 123)
col <- rgb(red, grn, blu, maxColorValue = 255)
brks <- c(-1000, -8.93, 2.63, 13.29, 19.88, 31.36, 43.53, 1000)

# depth bins
for (i in 2:length(fls)) {
  print(fls[i])
  s.nc <- nc_open(paste0("../CSSS_viewer/input/", fls[i]))

  for (j in 1:length(time)) {
    if (as.POSIXlt(time[j])$yday == 0) print(time[j])
    if (!dir.exists(paste0("./images/depth_", format(time[j], '%Y'), "_", sc[i]))) dir.create(paste0("./images/depth_", format(time[j], '%Y'), "_", sc[i]))
    s <- ncvar_get(s.nc, "depth", c(1, 1, t[j] + 1), c(-1, -1, 1))
    s <- s[, 477:1] / 10
    f <- paste0(sprintf("trans%04d", as.POSIXlt(time[j])$yday), ".png")
    png(paste0("./images/depth_", format(time[j], '%Y'), "_", sc[i], "/", f), width = 754, height = 1015, bg = "transparent", type = "quartz")
    par(mar = c(0, 0, 0, 0))
    image(x, y2, s, col = col, breaks = brks, axes = F, asp = 1)
    text(x[58], y2[47], time[j], pos = 4)
    dev.off()
  }
  nc_close(s.nc)
}

# recession rates -- first quarter must be handled seperately
s.nc <- nc_open(paste0("../CSSS_viewer/input/", fls[1]))
t <- ncvar_get(s.nc, "t")
time <- as.Date(s.nc$dim$t$units, format = "days since %Y-%m-%dT%H:%M:%S +0000") + t + 14
nc_close(s.nc)
brks2 <- c(-1000, -0.41, 0, 0.2, 0.51, 0.78, 1.02, 1000)
for (i in 1:length(fls)) {
  print(fls[i])
  s.nc <- nc_open(paste0("../CSSS_viewer/input/", fls[i]))
  for (j in 1:(length(time) - 14)) {
    if (as.POSIXlt(time[j])$yday == 0) print(time[j])
    if (!dir.exists(paste0("./images/rr_", format(time[j], '%Y'), "_", sc[i]))) dir.create(paste0("./images/rr_", format(time[j], '%Y'), "_", sc[i]))
    s <- ncvar_get(s.nc, "depth", c(1, 1, t[j] + 1), c(-1, -1, 1))
    s <- s[, 477:1] / 10
    s2 <- ncvar_get(s.nc, "depth", c(1, 1, t[j] + 15), c(-1, -1, 1))
    s2 <- s2[, 477:1] / 10
    rr <- (s2 - s) / 14
    f <- paste0(sprintf("trans%04d", as.POSIXlt(time[j])$yday), ".png")
    png(paste0("./images/rr_", format(time[j], '%Y'), "_", sc[i], "/", f), width = 754, height = 1015, bg = "transparent", type = "quartz")
    par(mar = c(0, 0, 0, 0))
    image(x, y2, rr, col = col, breaks = brks2, axes = F, asp = 1)
    text(x[58], y2[47], as.Date(time[j]), pos = 4)
    dev.off()
  }
  nc_close(s.nc)
}
setwd("..")
