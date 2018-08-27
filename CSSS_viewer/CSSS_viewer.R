### Hi Heather!! ###
# -------------
# Bryan McCloskey
# bmccloskey@usgs.gov
# St. Petersburg Coastal and Marine Science Center
# US Geological Survey
#
# 07/13/2018
#--------------

print("These libraries must be installed: ncdf4, abind, RCurl, grid, gridExtra, lattice")
# Required libraries. If not present, run:
# install.packages("ncdf4")
# install.packages("abind")
# install.packages("RCurl")
# install.packages("grid")
# install.packages("gridExtra")
# install.packages("lattice")
library (ncdf4)
library (abind)
library (RCurl)
library (grid)
library (gridExtra)
library (lattice)

try (setwd("./CSSS_viewer"), silent = T)

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

# Set up WL files
cur_qtr <- paste0(as.POSIXlt(Sys.Date() - 1)$year + 1900, "_", tolower(quarters(Sys.Date() - 1)))
err <- try (download.file(paste0("https://sofia.usgs.gov/eden/data/realtime2/", cur_qtr, "_v2rt_nc.zip"), paste0("../surfaces/", cur_qtr, ".zip")))
unzip(paste0("../surfaces/", cur_qtr, ".zip"), exdir = "../surfaces")
file.rename(paste0("../surfaces/", cur_qtr, "_v2rt.nc"), paste0("../surfaces/", cur_qtr, ".nc"))
pre_qtr <- paste0(as.POSIXlt(Sys.Date() - 90)$year + 1900, "_", tolower(quarters(Sys.Date() - 90)))
err <- try (download.file(paste0("https://sofia.usgs.gov/eden/data/realtime2/", pre_qtr, "_v2rt_nc.zip"), paste0("../surfaces/", pre_qtr, ".zip")))
unzip(paste0("../surfaces/", pre_qtr, ".zip"), exdir = "../surfaces")
file.rename(paste0("../surfaces/", pre_qtr, "_v2rt.nc"), paste0("../surfaces/", pre_qtr, ".nc"))
unlink("../surfaces/*.zip")

# Define subareas
pixels <- read.csv("./input/CSSS_EDEN_subpop_key.csv")
pixels <- rbind(pixels, read.csv("./input/CSSS_subpop_A1,A2.csv"))
sub <- c("AX", "A", "A1", "A2", "B", "C", "D", "E", "F")
grid_sub <- array(NA, c(287, 405, length(sub))) # 287 x 405 x 8 grid of all subareas
for (i in 1:length(sub)) {
  if (sub[i] == "AX") {
    pixels_sub <- pixels[which(pixels$SubPopulat == "A" | pixels$SubPopulat == "A1" | pixels$SubPopulat == "A2"), ] 
  } else {
    pixels_sub <- pixels[which(pixels$SubPopulat == sub[i]),]
  }
  for (j in 1:dim(pixels_sub)[1]) {
    pixels_sub$grid_x[j] <- which(x == pixels_sub$ET_X[j])
    pixels_sub$grid_y[j] <- which(y == pixels_sub$ET_Y[j])
    if (!is.na(dem[pixels_sub$grid_x[j], pixels_sub$grid_y[j]]))
      grid_sub[pixels_sub$grid_x[j], pixels_sub$grid_y[j], i] <- dem[pixels_sub$grid_x[j], pixels_sub$grid_y[j]]
  }
  assign(paste0("xmin_", sub[i]), which(rowSums(abs(grid_sub[, , i]), na.rm = T) > 0)[1])
  assign(paste0("xmax_", sub[i]), tail(which(rowSums(abs(grid_sub[, , i]), na.rm = T) > 0), 1))
  assign(paste0("ymin_", sub[i]), which(colSums(abs(grid_sub[, , i]), na.rm = T) > 0)[1])
  assign(paste0("ymax_", sub[i]), tail(which(colSums(abs(grid_sub[, , i]), na.rm = T) > 0), 1))
  assign(paste0("grid_", sub[i]), grid_sub[get(paste0("xmin_", sub[i])):get(paste0("xmax_", sub[i])), get(paste0("ymin_", sub[i])):get(paste0("ymax_", sub[i])), i]) # smaller one-layer grids of each max extent
}
xmin_AX <- xmin_A <- xmin_A + max(which(rowSums(grid_A, na.rm = T) == 0))  # Remove discontinuous western piece of A
grid_A <- grid_A[(max(which(rowSums(grid_A, na.rm = T) == 0)) + 1):dim(grid_A)[1], ]
grid_AX <- grid_AX[(max(which(rowSums(grid_AX, na.rm = T) == 0)) + 1):dim(grid_AX)[1], ]

# Build depth arrays
time <- NULL
surf_files <- c(paste0("../surfaces/", pre_qtr, ".nc"), paste0("../surfaces/", cur_qtr, ".nc"))
for (i in 1:length(sub)) assign(paste0("depth_", sub[i]), NULL)
for (i in 1:length(surf_files)) {
  print(paste("Building subareas for", surf_files[i]))
  s.nc <- nc_open(surf_files[i])
  s <- ncvar_get(s.nc, "stage")
  t <- ncvar_get(s.nc, "time")
  time <- as.Date(c(time, as.Date(s.nc$dim$time$units, format = "days since %Y-%m-%dT%H:%M:%SZ") + t, recursive = T), origin = "1970/1/1")
  nc_close(s.nc)
  for (j in 1:length(sub)) {
    d_sub <- s[get(paste0("xmin_", sub[j])):get(paste0("xmax_", sub[j])), get(paste0("ymin_", sub[j])):get(paste0("ymax_", sub[j])), ]
    d_sub[is.na(get(paste0("grid_", sub[j])))] <- NA
    assign(paste0("depth_", sub[j]), abind(get(paste0("depth_", sub[j])), sweep(d_sub, c(1, 2), get(paste0("grid_", sub[j])), "-")))
  }
}

# Create dry arrays
for (i in 1:length(sub)) assign(paste0("dry_", sub[i]), ifelse(get(paste0("depth_", sub[i])) <= 0, 1, 0)) # dry == 1

# Build statistics data.frame
df <- data.frame(date = as.Date(time))
for (i in 1:length(sub)) {
  print(paste("Calculating stats for", sub[i]))
  dim <- sum(!is.na(get(paste0("grid_", sub[i])))) # number of cells in subarea
  for (j in 1:dim(df)[1]) {
    df[j, paste0("per_dry_", sub[i])] <- round(sum(get(paste0("dry_", sub[i]))[, , j], na.rm = T) / dim, 3) * 100
    df[j, paste0("per_dry17cm_", sub[i])] <- round(sum(ifelse(get(paste0("depth_", sub[i]))[, , j] <= 17, 1, 0), na.rm = T) / dim, 3) * 100
  }
  for (j in 40:dim(df)[1]) {
    dry_40d <- ifelse(rowSums(get(paste0("dry_", sub[i]))[, , (j - 39):j], dims = 2) == 40, 1, 0) # all dry (== 1) days, then 1
    df[j, paste0("per_dry40d_", sub[i])] <- round((sum(dry_40d, na.rm = T)) / dim, 3) * 100
  }
  for (j in 90:dim(df)[1]) {
    dry_90d <- ifelse(rowSums(get(paste0("dry_", sub[i]))[, , (j - 89):j], dims = 2) == 90, 1, 0) # all dry (== 1) days, then 1
    df[j, paste0("per_dry90d_", sub[i])] <- round((sum(dry_90d, na.rm = T)) / dim, 3) * 100
  }
  for (j in 1:dim(df)[1]) {
    df[j, paste0("depth_mean_", sub[i])] <- round(mean(get(paste0("depth_", sub[i]))[, , j], na.rm = T), 1)
    df[j, paste0("depth_sd_", sub[i])] <- round(sd(get(paste0("depth_", sub[i]))[, , j], na.rm = T), 1)
  }
}

# Update existing stats .js
err <- try (download.file("https://sofia.usgs.gov/eden/csss/por_stats.js", "./output/por_stats.js"))
js <- readChar("./output/por_stats.js", 1e8)
js <- substr(js, 1, regexpr(format(df$date[90], "%Y%m%d"), js) - 5)
for (i in 90:dim(df)[1])
  js <- paste0(js, ",\n'a", format(df$date[i], "%Y%m%d"), "' : ['", paste(df[i, 2:dim(df)[2]], collapse = "', '"), "']")
js <- paste0(js, "\n}")
write(js, "./output/por_stats.js")
err <- try (ftpUpload("./output/por_stats.js", "ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/csss/por_stats.js"))

err <- try (download.file("https://sofia.usgs.gov/eden/csss/CSSS_subarea_stats.csv.zip", "./output/CSSS_subarea_stats.csv.zip"))
unzip("./output/CSSS_subarea_stats.csv.zip", exdir = "./output/", junkpaths = T)
js2 <- read.csv("./output/CSSS_subarea_stats.csv", colClasses = "character")
js2 <- js2[1:(which(as.Date(js2$Date, "%m/%d/%Y") == df$date[90]) - 1), ]
df$date <- format(df$date, "%m/%d/%Y")
for (i in 90:dim(df)[1])
  js2[dim(js2)[1] + 1, ] <- df[i, ]
cols <- c("Date", rbind(paste("area", sub, "% dry"), paste("area", sub, "% WD <= 17cm"), paste("area", sub, "% dry >= 40 days"), paste("area", sub, "% dry >= 90 days"), paste("mean cm water depth area", sub), paste("water depth standard deviation cm area", sub)))
write.table(js2, "./output/CSSS_subarea_stats.csv", quote = F, sep = ",", row.names = F, col.names = cols)
zip("./output/CSSS_subarea_stats.csv.zip", "./output/CSSS_subarea_stats.csv", "-j9X")
err <- try (ftpUpload("./output/CSSS_subarea_stats.csv.zip", "ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/csss/CSSS_subarea_stats.csv.zip"))

# Generate recent depth graphs
js2$Date <- as.Date(js2$Date, format = "%m/%d/%Y")
cols <- which(names(js2) %in% paste0("mean.cm.water.depth.area.", c("A", "AX", "B", "C", "D", "E", "F")))
yr <- as.Date(format(Sys.Date(), "%Y-01-01")) - 1
yr <- c(as.Date(format(yr, "%Y-01-01")), yr)
doy <- as.POSIXlt(Sys.Date() - 1)$yday
w_yr <- if (doy < 305) format(yr[1], "%Y") else as.numeric(format(yr[1], "%Y")) + 1
d_yr <- if (doy < 121) as.numeric(format(yr[1], "%Y")) else format(yr[1], "%Y")
wet <- c(as.Date(paste0(w_yr, "-05-01")), as.Date(paste0(w_yr, "-10-31")))
dry <- c(as.Date(paste0(d_yr, "-11-01")), as.Date(paste0(as.numeric(d_yr) + 1, "-04-30")))
mon <- as.Date(format(Sys.Date(), "%Y-%m-01")) - 1
mon <- c(as.Date(format(mon, "%Y-%m-01")), mon)
wk <- seq(Sys.Date() - 13, Sys.Date() - 7, "day")
wk <- wk[weekdays(wk) == "Sunday"]
wk[2] <- wk + 6
yr_v <- js2[js2$Date >= yr[1] & js2$Date <= yr[2], cols]
yr_v <- sapply(yr_v, as.numeric)
wet_v <- js2[js2$Date >= wet[1] & js2$Date <= wet[2], cols]
wet_v <- sapply(wet_v, as.numeric)
dry_v <- js2[js2$Date >= dry[1] & js2$Date <= dry[2], cols]
dry_v <- sapply(dry_v, as.numeric)
mon_v <- js2[js2$Date >= mon[1] & js2$Date <= mon[2], cols]
mon_v <- sapply(mon_v, as.numeric)
wk_v <- js2[js2$Date >= wk[1] & js2$Date <= wk[2], cols]
wk_v <- sapply(wk_v, as.numeric)
yr_r <- range(yr_v)
wet_r <- range(wet_v)
dry_r <- range(dry_v)
mon_r <- range(mon_v)
wk_r <- range(wk_v)
leg <- c("A", "AX", "B", "C", "D", "E", "F")
png("./output/recent_year_subpop_mean_water_depth.png", width = 862, height = 614, type = "quartz")
plot(seq(yr[1], yr[2], "day"), yr_v[, 1], type = "n", ylim = yr_r, main = paste(format(yr[1], "%Y"), "mean water depth (cm) in CSSS subpopulations"), xlab = "Date", ylab = "Water depth (cm)", xaxt = "n")
abline(h = 0, col = "black", lwd = 3, lty = "dashed")
for (i in 1:dim(yr_v)[2])
  lines(seq(yr[1], yr[2], "day"), yr_v[, i], col = colors()[cols[i]], lwd = 3)
legend("topleft", leg, col = colors()[cols], lty = 1, lwd = 3)
axis.Date(1, at = seq(yr[1], yr[2], "month"), format = "%m-%d-%Y")
dev.off()
png("./output/recent_wet_seas_subpop_mean_water_depth.png", width = 862, height = 614, type = "quartz")
plot(seq(wet[1], wet[2], "day"), wet_v[, 1], type = "n", ylim = wet_r, main = paste(format(wet[1], "%Y"), "wet season mean water depth (cm) in CSSS subpopulations"), xlab = "Date", ylab = "Water depth (cm)", xaxt = "n")
abline(h = 0, col = "black", lwd = 3, lty = "dashed")
for (i in 1:dim(wet_v)[2])
  lines(seq(wet[1], wet[2], "day"), wet_v[, i], col = colors()[cols[i]], lwd = 3)
legend("topleft", leg, col = colors()[cols], lty = 1, lwd = 3)
axis.Date(1, at = seq(wet[1], wet[2], "7 days"), format = "%m-%d-%Y")
dev.off()
png("./output/recent_dry_seas_subpop_mean_water_depth.png", width = 862, height = 614, type = "quartz")
plot(seq(dry[1], dry[2], "day"), dry_v[, 1], type = "n", ylim = dry_r, main = paste(format(dry[1], "%Y"), "-", format(dry[2], "%Y"), "dry season mean water depth (cm) in CSSS subpopulations"), xlab = "Date", ylab = "Water depth (cm)", xaxt = "n")
abline(h = 0, col = "black", lwd = 3, lty = "dashed")
for (i in 1:dim(dry_v)[2])
  lines(seq(dry[1], dry[2], "day"), dry_v[, i], col = colors()[cols[i]], lwd = 3)
legend("topleft", leg, col = colors()[cols], lty = 1, lwd = 3)
axis.Date(1, at = seq(dry[1], dry[2], "7 days"), format = "%m-%d-%Y")
dev.off()
png("./output/recent_month_subpop_mean_water_depth.png", width = 862, height = 614, type = "quartz")
plot(seq(mon[1], mon[2], "day"), mon_v[, 1], type = "n", ylim = mon_r, main = paste(format(mon[1], "%B, %Y"), "mean water depth (cm) in CSSS subpopulations"), xlab = "Date", ylab = "Water depth (cm)", xaxt = "n")
abline(h = 0, col = "black", lwd = 3, lty = "dashed")
for (i in 1:dim(mon_v)[2])
  lines(seq(mon[1], mon[2], "day"), mon_v[, i], col = colors()[cols[i]], lwd = 3)
legend("topleft", leg, col = colors()[cols], lty = 1, lwd = 3)
axis.Date(1, at = seq(mon[1], mon[2], "7 days"), format="%m-%d-%Y")
dev.off()
png("./output/recent_week_subpop_mean_water_depth.png", width = 862, height = 614, type = "quartz")
plot(seq(wk[1], wk[2], "day"), wk_v[, 1], type = "n", ylim = wk_r, main = paste(wk[1], "-", wk[2], "mean water depth (cm) in CSSS subpopulations"), xlab = "Date", ylab = "Water depth (cm)", xaxt = "n")
abline(h = 0, col = "black", lwd = 3, lty = "dashed")
for (i in 1:dim(wk_v)[2])
  lines(seq(wk[1], wk[2], "day"), wk_v[, i], col = colors()[cols[i]], lwd = 3)
legend("topleft", leg, col = colors()[cols], lty = 1, lwd = 3)
axis.Date(1, at = seq(wk[1], wk[2], "day"), format = "%m-%d-%Y")
dev.off()
err <- try (ftpUpload("./output/recent_year_subpop_mean_water_depth.png", "ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/csss/recent_year_subpop_mean_water_depth.png"))
err <- try (ftpUpload("./output/recent_wet_seas_subpop_mean_water_depth.png", "ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/csss/recent_wet_seas_subpop_mean_water_depth.png"))
err <- try (ftpUpload("./output/recent_dry_seas_subpop_mean_water_depth.png", "ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/csss/recent_dry_seas_subpop_mean_water_depth.png"))
err <- try (ftpUpload("./output/recent_month_subpop_mean_water_depth.png", "ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/csss/recent_month_subpop_mean_water_depth.png"))
err <- try (ftpUpload("./output/recent_week_subpop_mean_water_depth.png", "ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/csss/recent_week_subpop_mean_water_depth.png"))

# Generate depth surface images
col <- c("deepskyblue", "steelblue", "blue3", "blue4")
s.nc <- nc_open(surf_files[2])
s <- ncvar_get(s.nc, "stage")
time <- ncvar_get(s.nc, "time")
time <- as.Date(s.nc$dim$time$units, format = "days since %Y-%m-%dT%H:%M:%SZ") + time
nc_close(s.nc)
d <- sweep(s, c(1, 2), dem, "-")
for (i in 1:length(time)) {
  if (!dir.exists(paste0("./images/", format(time[i],'%Y')))) dir.create(paste0("./images/", format(time[i],'%Y')))
  if (!dir.exists(paste0("./images/", format(time[i],'%Y'), "_nest/"))) dir.create(paste0("./images/", format(time[i],'%Y'), "_nest/"))
  f <- paste0(sprintf("trans%04d", as.POSIXlt(time[i])$yday), ".png")
  png(paste0("./images/", format(time[i], '%Y'), "/", f), width = 614, height = 862, bg = "transparent", type = "quartz")
  par(mar = c(0, 0, 0, 0))
  image(x, y, d[, , i], col = col, breaks = c(0, 17, 30, 46, 600), axes = F, asp = 1)
  text(x[1], y[10], time[i], pos = 4)
  #points(x[c(1, 1, 287, 287)], y[c(1, 405, 1, 405)], pch = 16) #positioning markers for Leaflet map
  dev.off()
  err <- try (ftpUpload(paste0("./images/", format(time[i], '%Y'), "/", f), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/csss/images/", format(time[i], '%Y'), "/", f)))
  if (as.POSIXlt(time[i])$yday >= 59 & as.POSIXlt(time[i])$yday <= 195) { #leapyear tweak undone
    f2 <- paste0(sprintf("trans%04d", as.POSIXlt(time[i])$yday - 59), ".png")
    file.copy(paste0("./images/", format(time[i], '%Y'), "/", f), paste0("./images/", format(time[i],'%Y'), "_nest/", f2), overwrite = T)
    err <- try (ftpUpload(paste0("./images/", format(time[i], '%Y'), "_nest/", f2), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/csss/images/", format(time[i], '%Y'), "_nest/", f)))
  }
}

# Generate PDF report
if (!as.POSIXlt(Sys.Date() - 1)$year %% 4 & as.POSIXlt(Sys.Date() - 1)$yday >= 59) {
  doy <- as.POSIXlt(Sys.Date() - 1)$yday
} else doy <- as.POSIXlt(Sys.Date() - 1)$yday

if (doy >= 0 & doy <= 89) {
  qtr <- 1; z <- doy
} else if (doy >= 90 & doy <= 180) {
  qtr <- 2; z <- doy - 89
} else if (doy >= 181 & doy <= 272) {
  qtr <- 3; z <- doy - 180
} else {
  qtr <- 4; z <- doy - 272
}

# Set up WL files
yr <- as.POSIXlt(Sys.Date() - 1)$year + 1900
for (i in 1991:yr)
  if (!file.exists(paste0("../surfaces/", i, "_q", qtr, ".nc")))
    err <- try (download.file(paste0("https://sflthredds.er.usgs.gov/thredds/fileServer/eden/surfaces/", i, "_q", qtr, ".nc"), paste0("../surfaces/", i, "_q", qtr, ".nc")))
file_surf <- list.files("../surfaces", paste0("^[0-9]{4}_q", qtr, ".nc$"))
time <- NULL
for (i in 1:length(sub)) assign(paste0("depth_", sub[i]), NULL)
for (i in 1:length(file_surf)) {
  print(file_surf[i])
  surf.nc <- nc_open(paste0("../surfaces/", file_surf[i]))
  stage <- try(ncvar_get(surf.nc, "stage", c(1, 1, z), c(-1, -1, 1)), T)
  if (!inherits(stage,"try-error")) {
    t <- ncvar_get(surf.nc, "time", z, 1)
    time <- as.Date(c(time, as.Date(surf.nc$dim$time$units, format = "days since %Y-%m-%dT%H:%M:%SZ") + t, recursive = T), origin = "1970/1/1")
    nc_close(surf.nc)
    for (j in 1:length(sub)) {
      stage_sub <- stage[get(paste0("xmin_", sub[j])):get(paste0("xmax_", sub[j])), get(paste0("ymin_", sub[j])):get(paste0("ymax_", sub[j]))]
      stage_sub[is.na(get(paste0("grid_", sub[j])))] <- NA
      assign(paste0("depth_", sub[j]), abind(get(paste0("depth_", sub[j])), stage_sub - get(paste0("grid_", sub[j])), along = 3))
    }
  }
}

for (i in 1:length(sub)) assign(paste0("dry_", sub[i]), ifelse(get(paste0("depth_", sub[i])) <= 0, 1, 0)) #dry == 1
dt <- data.frame(date = as.Date(time))
for (i in 1:length(sub))
  for (j in 1:dim(dt)[1])
    dt[j, paste0("dry_", sub[i])] <- sum(get(paste0("dry_", sub[i]))[, , j], na.rm = T)
dt$tot <- rowSums(dt[, c(2, 6:(length(sub) + 1))])
dt[, 2:(length(sub) + 2)] <- dt[, 2:(length(sub) + 2)] * 0.4 * 0.4

pdf(paste0("./output/csss_yr_cmp_report_", format(Sys.Date() - 1, "%Y%m%d"), ".pdf"), width = 8.5, height = 11)
grid.text(paste0("Cape Sable Seaside Sparrow Habitat Conditions (Sparrow Viewer Tool)\nAmount of Dry\u00B9 Habitat Area (km\u00B2) by Subpopulation\n", format(Sys.Date() - 1, "%m/%d/%Y"), " Compared to ", format(Sys.Date() - 1, "%m/%d"), " on Previous Years of Record"), y = 0.95, gp = gpar(fontsize = 16, fontface = "bold"))
grid.table(dt, rows = NULL, cols = c("Date", "AX", "A", "A1", "A2", "B", "C", "D", "E", "F", "Total"))
grid.text("Total Area (km\u00B2)\nWithin Each\nSubpopulation:", x = 0.1, y = 0.11, gp = gpar(fontface = "bold"))
grid.text("332.48    260.96    84    48.32    171.04    37.76    39.36    102.08    24.16     706.88", x = 0.56, y = 0.12, gp = gpar(fontface = "bold"))
grid.text("1 Dry Habitat is defined as areas where the water table is below ground level.", y = 0.05)
bc1 <- barchart(dt$tot ~ dt$date, horizontal = F, ylab = "km\u00B2", ylim = c(0, max(dt$tot) + 50), main = paste0("Cape Sable Seaside Sparrows (CSSS)\nAmount of dry habitat (km\u00B2) on ", format(Sys.Date() - 1, "%m/%d/%Y"), " compared to\n", format(Sys.Date() - 1, "%m/%d"), " in previous years of record, all CSSS subpopulations"), col = "darkolivegreen4", scales = list(x = list(labels = c(91:99, "00", "01", "02", "03", "04", "05", "06", "07", "08", "09", 10:17))))
bc2 <- barchart(dt$dry_AX ~ dt$date, horizontal = F, ylab = "km\u00B2", ylim = c(0, max(dt$dry_AX) + 25), main = paste0("Cape Sable Seaside Sparrows (CSSS)\nAmount of dry habitat (km\u00B2) on ", format(Sys.Date() - 1, "%m/%d/%Y"), " compared to\n", format(Sys.Date() - 1, "%m/%d"), " in previous years of record, CSSS-subpopulation AX"), col = "firebrick", scales = list(x = list(labels = c(91:99, "00", "01", "02", "03", "04", "05", "06", "07", "08", "09", 10:17))))
print(bc1, position = c(0, 0.5, 1, 1), more = T)
print(bc2, position = c(0, 0, 1, 0.5))
dev.off()

ftpUpload(paste0("./output/csss_yr_cmp_report_", format(Sys.Date() - 1, "%Y%m%d"), ".pdf"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/csss/csss_yr_cmp_report_", format(Sys.Date() - 1, "%Y%m%d"), ".pdf"))
ftpUpload(paste0("./output/csss_yr_cmp_report_", format(Sys.Date() - 1, "%Y%m%d"), ".pdf"), "ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/csss/csss_yr_cmp_report.pdf")
setwd("..")
