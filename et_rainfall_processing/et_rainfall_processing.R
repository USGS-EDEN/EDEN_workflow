# -------------
# Bryan McCloskey
# bmccloskey@usgs.gov
# St. Petersburg Coastal and Marine Science Center
# US Geological Survey
#
# 07/23/2018
#--------------

print("These libraries must be installed: RMySQL")
# Required libraries. If not present, run:
# install.packages("RMySQL")
library (RMySQL)

try (setwd("./et_rainfall_processing"), silent = T)
source ("../admin_pwd.R")
# Connect to database and download data file
con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "eden", host = "igsafpesgsz03.er.usgs.gov")
pixel <- read.table("./pixels-all.txt", header = T, sep = "\t", colClasses = c("integer", "numeric", "numeric"))

# Assign closest pixel center to gage
db <- dbGetQuery(con, "select station_id, station_name_web, station_name, latitude, longitude from station group by station_name")
lat <- sapply(db$latitude, function (x) strsplit(x, ","))
long <- sapply(db$longitude, function (x) strsplit(x, ","))
lat_d <- as.numeric(lapply(lat, "[[", 1))
lat_m <- as.numeric(lapply(lat, "[[", 2))
lat_s <- as.numeric(lapply(lat, "[[", 3))
long_d <- as.numeric(lapply(long, "[[", 1))
long_m <- as.numeric(lapply(long, "[[", 2))
long_s <- as.numeric(lapply(long, "[[", 3))
db$lat <- lat_d + lat_m / 60 + lat_s / 3600
db$long <- -long_d - long_m / 60 - long_s / 3600
db$pixel <- pixel$pixel[apply(db[, c("lat", "long")], 1, function (x) which.min(((x["lat"] - pixel$lat) ^ 2 + (x["long"] - pixel$long) ^ 2)))]

# Rainfall -- assumes data delivery in month after data coverage
year <- format(Sys.Date(), "%Y")
month <- format(Sys.Date(), "%m")
if (month == "01") { month <- "12"; year <- as.numeric(year) - 1 } else month <- as.numeric(month) - 1
dt <- as.Date(paste(year, month, "01", sep = "-"))
new <- strftime(seq(dt, (seq(dt, length = 2, by = "month") - 1)[2], 1), "%m/%d/%Y") # rainfall may be missing some days
download.file(paste0("ftp://ftpint.usgs.gov/from_pub/er/eden/NEXRAD_", strftime(dt, "%m%Y"), ".dat"), paste0("./rainfall/rainfall_", strftime(dt, "%Y%m"), ".txt"))
rain <- read.table(paste0("./rainfall/rainfall_", strftime(dt, "%Y%m"), ".txt"), colClasses = c("character", "integer", "numeric"), col.names = c("date", "pixel", "rain"))
rain$pixel <- rain$pixel - 10000000
for (i in 1:length(db$station_name))
  new <- merge(new, rain[which(rain$pixel == db$pixel[i]), c("date", "rain")], by = 1, all = T)
new[is.na(new)] <- 0
names(new) <- c("date", db$station_name_web)
new$date <- as.POSIXct(new$date, tz = "EST", format = "%m/%d/%Y")

for (i in 1:dim(new)[1]) {
  query <- paste0("replace into rainfall set date = '", new$date[i], "'") # replace erases non-specified columns!!
  for (j in 2:dim(new)[2])
    query <- paste0(query, ", `rainfall_", names(new)[j], "` = ", new[i,j])
  dbSendQuery(con, query)
}

# ET -- assumes data delivery in calendar year after data coverage
dt <- as.POSIXlt(Sys.Date())
dt$mday <- 1
dt$mon <- 0
dt$year <- dt$year - 1
dt <- as.Date(dt)
pixel[, 2:3] <- round(pixel[, 2:3], 4)
#new <- strftime(seq(dt, (seq(dt, length = 2, by = "year") - 1)[2], 1), "%m/%d/%Y")
new <- data.frame(date = strftime(seq(dt, (seq(dt, length = 2, by = "year") - 1)[2], 1), "%m/%d/%Y"))
#et <- read.table(paste0("./et/et_", strftime(dt, "%Y"), ".txt"), colClasses = c("character", "NULL", "NULL", "integer", "numeric"), col.names = c("date", "NULL", "NULL", "pixel", "et"))
et.nc <- nc_open("~/Downloads/fl.et.2019.v.1.0.nc")
x <- ncvar_get(et.nc, "lon")
x <- round(x, 4)
y <- ncvar_get(et.nc, "lat")
y <- round(y, 4)
et <- ncvar_get(et.nc, "PET")
nc_close(dem.nc)
for (i in 1:length(db$station_name)) {
  #  new <- merge(new, et[which(et$pixel == db$pixel[i]), c("date", "et")], by = 1, all = T)
  x_pix <- which(x == pixel$longitude[db$pixel[i]])
  y_pix <- which(y == pixel$latitude[db$pixel[i]])
  new <- cbind(new, et[x_pix, y_pix, ])
}
names(new) <- c("date", db$station_name_web)
new$date <- as.POSIXct(new$date, tz = "EST", format = "%m/%d/%Y")

for (i in 1:dim(new)[1]) {
  query <- paste0("replace into et set date = '", new$date[i], "'") # replace erases non-specified columns!!
  for (j in 2:dim(new)[2])
    query <- paste0(query, ", `et_", names(new)[j], "` = ", new[i,j])
  dbSendQuery(con, query)
}

# add historical data for newly-added stations
gage <- "USSO_T"
dt <- seq(as.Date("2002-01-01"), as.Date("2017-03-31"), by = "day")
rain_old <- list.files("./rainfall/", "^rainfall_200[2-8].txt$")
rain_new <- list.files("./rainfall/", "^rainfall_20[0-9]{4}.txt$")
et <- list.files("./et/", "^et_[0-9]{4}.txt$")
db<-dbGetQuery(con, paste0("select station_id, latitude, longitude from station where station_name_web = '", gage, "'"))
lat <- sapply(db$latitude, function (x) strsplit(x, ","))
long <- sapply(db$longitude, function (x) strsplit(x, ","))
lat_d <- as.numeric(lapply(lat, "[[", 1))
lat_m <- as.numeric(lapply(lat, "[[", 2))
lat_s <- as.numeric(lapply(lat, "[[", 3))
long_d <- as.numeric(lapply(long, "[[", 1))
long_m <- as.numeric(lapply(long, "[[", 2))
long_s <- as.numeric(lapply(long, "[[", 3))
db$lat <- lat_d + lat_m / 60 + lat_s / 3600
db$long <- -long_d - long_m / 60 - long_s / 3600
db$pixel <- pixel$pixel[apply(db[, c("lat", "long")], 1, function (x) which.min(((x["lat"] - pixel$lat) ^ 2 + (x["long"] - pixel$long) ^ 2)))]

dt2 <- dt3 <- NULL
for(i in 1:length(rain_old)) {
  rain <- read.table(paste0("./rainfall/", rain_old[i]), colClasses = c("character", "NULL", "NULL", "integer", "numeric"), col.names = c("date", "NULL", "NULL", "pixel", "rain"))
  dt2 <- rbind(dt2, rain[which(rain$pixel == db$pixel), c("date", "rain")])
}
for(i in 1:length(rain_new)) {
  rain <- read.table(paste0("./rainfall/", rain_new[i]), colClasses = c("character", "integer", "numeric"), col.names = c("date", "pixel", "rain"))
  dt2 <- rbind(dt2, rain[which(rain$pixel == db$pixel + 10000000), c("date", "rain")])
}
dt2$date <- as.Date(dt2$date, format = "%m/%d/%Y")
dt2 <- merge(dt, dt2, by = 1, all.x = T)
dt2[is.na(dt2)] <- 0
names(dt2)[1] <- "date"
for (i in 1:dim(dt2)[1]) {
  query <- paste0("update rainfall set `rainfall_", gage, "` = ", dt2$rain[i], " where date = '", dt2$date[i], "'")
  dbSendQuery(con, query)
}

for(i in 1:length(et)) {
  et2 <- read.table(paste0("./et/", et[i]), colClasses = c("character", "NULL", "NULL", "integer", "numeric"), col.names = c("date", "NULL", "NULL", "pixel", "et"))
  dt3 <- rbind(dt3, et2[which(et2$pixel == db$pixel), c("date", "et")])
}
dt3$date <- as.Date(dt3$date, format = "%m/%d/%Y")
for (i in 1:dim(dt3)[1]) {
  query <- paste0("update et set `et_", gage, "` = ", dt3$et[i], " where date = '", dt3$date[i], "'")
  dbSendQuery(con, query)
}
setwd("..")
