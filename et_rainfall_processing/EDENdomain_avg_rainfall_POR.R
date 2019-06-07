# -------------
# Bryan McCloskey
# bmccloskey@usgs.gov
# St. Petersburg Coastal and Marine Science Center
# US Geological Survey
#
# 02/11/2019
#--------------

# POR extraction of average rainfall for all pixels overlapping the EDEN domain

print("These libraries must be installed: ncdf4, PBSmapping, RCurl")
# Required libraries. If not present, run:
# install.packages("ncdf4")
# install.packages("PBSmapping")
# install.packages("RCurl")
library (ncdf4)
library (PBSmapping)
library (RCurl)

try (setwd("./et_rainfall_processing"), silent = T)

pixel <- read.table("./pixels-all.txt", header = T, sep = "\t", colClasses = c("integer", "numeric", "numeric"))
names(pixel)[2:3] <- c("Y", "X")
attr(pixel, "projection") <- "LL"
pixel_utm <- convUL(pixel, km = F)
pixel_utm$keep <- NA

file_surf <- rev(list.files("../surfaces", "^[0-9]{4}_q[1-4].nc$", full.names = T))[1]
surf.nc <- nc_open(file_surf)
x <- ncvar_get(surf.nc, "x")
y <- ncvar_get(surf.nc, "y")
stage <- ncvar_get(surf.nc, "stage")
for (i in 1:length(x))
  for (j in 1:length(y))
    if (!is.na(stage[i, j, 1])) {
      print(paste("x =", i, "; y = ", j))
      # Closest 2km pixel to EDEN pixel center
      p <- which.min(((pixel_utm$X - x[i]) ^ 2 + (pixel_utm$Y - y[j]) ^ 2) ^ 0.5)
      pixel_utm$keep[p] <- 1
    }

pix2 <- pixel_utm[which(pixel_utm$keep == 1), ]

# assumes data delivery in month after data coverage
year <- format(Sys.Date(), "%Y")
month <- format(Sys.Date(), "%m")
if (month == "01") { month <- "12"; year <- as.numeric(year) - 1 } else month <- as.numeric(month) - 1
fst <- as.Date(paste(year, month, "01", sep = "-"))
lst <- (seq(fst, length = 2, by = "month") - 1)[2]
dt <- seq(as.Date("2002-01-01"), lst, by = "day")
rain_old <- list.files("./rainfall/rainfall_archive/", "rainfall_[0-9]{4}.txt$", full.names = T)
rain_new <- list.files("./rainfall/rainfall_archive/", "rainfall_[0-9]{6}.txt$", full.names = T)

write("date,avg", "./daily_average_rainfall-all_EDEN_pixels.csv")
for (i in 1:length(rain_old)) {
  print(2001 + i)
  yr <- seq(as.Date(paste0(2001 + i, "-01-01")), as.Date(paste0(2001 + i, "-12-31")), by = "day")
  rf <- read.table(rain_old[i], header = F, colClasses = c("character", "NULL", "NULL", "integer", "numeric"))
  rf$V1 <- as.Date(rf$V1, format = "%m/%d/%Y")
  for (j in 1:length(yr)) {
    if (!(yr[j] %in% rf$V1) | !length(rf$V5[which(rf$V1 == yr[j] & rf$V4 %in% pix2$pixel)])) {
      write(paste(yr[j], 0, sep = ","), "./daily_average_rainfall-all_EDEN_pixels.csv", append = T)
    } else {
      avg <- sum(rf$V5[which(rf$V1 == yr[j] & rf$V4 %in% pix2$pixel)]) / 2529
      write(paste(yr[j], avg, sep = ","), "./daily_average_rainfall-all_EDEN_pixels.csv", append = T)
    }
  }
}

for (i in 1:length(rain_new)) {
  print(rain_new[i])
  yr <- as.numeric(substr(rain_new[i], 39, 42))
  mn <- as.numeric(substr(rain_new[i], 43, 44))
  if (mn == 12) { yr2 <- yr + 1; mn2 <- 1
  } else { yr2 <- yr; mn2 <- mn + 1 }
  m <- seq(as.Date(paste0(yr, "-", mn, "-01")), as.Date(paste0(yr2, "-", mn2, "-01")) - 1, by = "day")
  rf <- read.table(rain_new[i], header = F, colClasses = c("character", "integer", "numeric"))
  rf$V1 <- as.Date(rf$V1, format = "%m/%d/%Y")
  rf$V2 <- rf$V2 - 10000000
  for (j in 1:length(m)) {
    if (!(m[j] %in% rf$V1) | !length(rf$V3[which(rf$V1 == m[j] & rf$V2 %in% pix2$pixel)])) {
      write(paste(m[j], 0, sep = ","), "./daily_average_rainfall-all_EDEN_pixels.csv", append = T)
    } else {
      avg <- sum(rf$V3[which(rf$V1 == m[j] & rf$V2 %in% pix2$pixel)]) / 2529
      write(paste(m[j], avg, sep = ","), "./daily_average_rainfall-all_EDEN_pixels.csv", append = T)
    }
  }
}
ftpUpload("./daily_average_rainfall-all_EDEN_pixels.csv", "ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/bmccloskey/daily_average_rainfall-all_EDEN_pixels.csv")
setwd("..")
