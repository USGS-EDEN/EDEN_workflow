# -------------
# Bryan McCloskey
# bmccloskey@usgs.gov
# St. Petersburg Coastal and Marine Science Center
# US Geological Survey
#
# 07/12/2018
#--------------

print("These libraries must be installed: RMySQL")
# Required libraries. If not present, run:
# install.packages("RMySQL")
library (RMySQL)

try (setwd("./EDENdb_upload"), silent = T)
source ("../admin_pwd.R")
# Connect to database and download data file
con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "eden", host = "igsafpesgsz03.er.usgs.gov")
# This will work for local non-ADAM-format files
data_file <- "input.csv"
file_columns <- c("character", "character", "numeric", "character")
err <- try (z <- read.csv(data_file, colClasses = file_columns))
print(head(z))
names(z) <- c("date_tm", "site", "value", "flag")

# Format timestamps
z$date_tm <- as.POSIXct(z$date_tm, tz = "EST", format = "%m/%d/%Y %H:%M")
# Timestamp range present in file:
first <- sort(unique(z$date_tm))[1]
last <- rev(sort(unique(z$date_tm)))[1]
range <- seq.POSIXt(first, last, "hour")
# Remove non-hourly
z <- z[z$date_tm %in% range, ]
z$flag[z$flag == "NULL"] <- NA
print(head(z))
print(tail(z))
print(range(range))
print(unique(z$site))
print(unique(z$flag))
for (j in 1:(length(unique(z$site)) %/% 5 + 1)) {
  url <- paste0("https://sofia.usgs.gov/eden/eve/index.php?day_hour=hourly&timeseries_start=", as.Date(first) - 1, "&timeseries_end=", as.Date(last) + 2)
  for (i in 1:5) if (!is.na(unique(z$site)[(j - 1) * 5 + i])) url <- paste0(url, "&site_list%5B%5D=", unique(z$site)[(j - 1) * 5 + i])
  print(url)
}

# Upload matrix to EDENdb
for (i in 1:dim(z)[1]) {
  if (i %% 1000 == 0) print(i)
  query <- paste0("update stage set `stage_", z$site[i], "` = ", z$value[i], ", `flag_", z$site[i], "` = ")
  if (is.na(z$flag[i])) tmp <- "NULL" else tmp <- paste0("'", z$flag[i], "'")
  query <- paste0(query, tmp, " where datetime = '", z$date_tm[i], "'")
  # Upload timestamp row to database
  err <- try (dbSendQuery(con, query))
}

## Optional: Update daily stage values table with medians of haurly values
gages <- dbGetQuery(con, "select station_name_web, dry_elevation from station where display = 1 and station_name_web != 'Alligator_Creek' and station_name_web != 'East_Side_Creek' and station_name_web != 'G-3777' and station_name_web != 'Manatee_Bay_Creek' and station_name_web != 'Raulerson_Brothers_Canal' and station_name_web != 'Barron_River'")
gages$dry_elevation[which(is.na(gages$dry_elevation))] <- -9999
# Determine whether flags are estimated or hindcast
for (j in 1:length(gages$station_name_web))
  gages$hindcast[j] <- dbGetQuery(con, paste0("select min(datetime) from stage where `stage_", gages$station_name_web[j], "` is not null and `flag_", gages$station_name_web[j], "` is null"))
# Calculate range of date coverage
start <- as.Date(range[1], tz = "EST")
end <- as.Date(rev(range)[1], tz = "EST")
range <- seq.Date(start, end, by = "day")
for (i in length(range):1) {
  dbDisconnect(con)
  con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "eden", host = "igsafpesgsz03.er.usgs.gov")
  # Select hourly values
  query2 <- "select datetime"
  for (j in 1:length(gages$station_name_web))
    query2 <- paste0(query2, ", `stage_", gages$station_name_web[j], "`, `flag_", gages$station_name_web[j], "`")
  query2 <- paste0(query2, " from stage where datetime >= ", format(range[i], "%Y%m%d000000"), " and datetime < ", format(range[i] + 1, "%Y%m%d000000"), " order by datetime")
  err <- try (db2 <- dbGetQuery(con, query2), silent = T)
  if (inherits(err, "try-error")) next # RMySQL library has bug occasionally causing this query to fail
  query <- paste0("update stage_daily set date = '", range[i], "'")
  for (j in seq(2, dim(db2)[2], by = 2)) {
    # Calculate flags
    flag <- ifelse(length(which(db2[, j + 1] == "M")) == length(db2[, j + 1]) | length(which(is.na(db2[, j]))) == length(db2[, j]), "M", ifelse(median(db2[, j], na.rm=T) < gages$dry_elevation[j / 2], "D", ifelse(length(which(is.na(db2[, j + 1]))) > 0, "O" ,ifelse(db2$datetime < gages$hindcast[j / 2], "H", "E"))))
    if (is.na(median(db2[, j], na.rm=T))) val <- "NULL" else val <- median(db2[, j], na.rm = T)
    query <- paste0(query, ", `", names(db2)[j], "` = ", val, ", `", names(db2)[j + 1] ,"` = '", flag, "'")
  }
  query <- paste0(query, " where date = '", range[i], "'")
  dbSendQuery(con, query)
}
setwd("..")
