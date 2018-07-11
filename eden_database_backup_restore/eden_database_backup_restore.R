# Required library. If not present, run:
# install.packages("RMySQL")
library(RMySQL)

# Connect to database and load data file
source("./admin_pwd.R")
con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "eden_new", host = "stpweb1-dmz.er.usgs.gov")
data_file <- "EDENdb2018q1backup.csv"
file_columns <- c("character", rep(c("numeric", "character"), 309))
err <- try(z <- read.csv(data_file, colClasses = file_columns, na.strings = "NULL", check.names = F))

# Convert to time object
z$datetime <- as.POSIXct(z$datetime, tz="EST", format="%Y-%m-%d %H:%M:%S")

# Upload matrix to EDENdb
for (i in 1:dim(z)[1]) { # Loop through rows
  query <- "update stage set "
  for (j in 2:dim(z)[2]) { # Loop through columns
    # Quote both flags and numbers, not null
    if (is.na(z[i, j])) tmp <- "NULL" else tmp <- paste0("'", z[i, j], "'")
    query <- paste0(query, "`", names(z)[j], "` = ", tmp, ", ")
  }
  # Remove trailing comma
  query <- paste0(substr(query, 1, nchar(query) - 2), " where datetime = '", z$datetime[i], "'")
  # Upload timestamp row to database
  err <- try(dbSendQuery(con, query), T)
  if (inherits(err, "try-error")) print(paste0(report, "EDENdb upload error for ", z$datetime[i], ": ", err))
}

## Optional: Update daily stage values table with medians of haurly values
# Manual run often fails due to to R bug in MySQL connection library; daily data will be automatically overwritten by next day's auto process
gages <- dbGetQuery(con, "select station_name_web, dry_elevation from station where display = 1 and station_name_web != 'Alligator_Creek' and station_name_web != 'East_Side_Creek' and station_name_web != 'G-3777' and station_name_web != 'Manatee_Bay_Creek' and station_name_web != 'Raulerson_Brothers_Canal' and station_name_web != 'Barron_River'")
gages$dry_elevation[which(is.na(gages$dry_elevation))] <- -9999
# Calculate range of date coverage
start <- as.Date(sort(unique(z$datetime))[1], tz = "EST")
end <- as.Date(rev(sort(unique(z$datetime)))[1], tz = "EST")
range <- seq.Date(start, end, by = "day")
for (i in length(range):1) {
  dbDisconnect(con)
  con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "eden_new", host = "stpweb1-dmz.er.usgs.gov")
  # Select hourly values
  query2 <- "select datetime"
  for (j in 1:length(gages$station_name_web))
    query2 <- paste0(query2, ", `stage_", gages$station_name_web[j], "`, `flag_", gages$station_name_web[j], "`")
  query2 <- paste0(query2, " from stage where datetime >= ", format(range[i], "%Y%m%d000000"), " and datetime < ", format(range[i] + 1, "%Y%m%d000000"), " order by datetime")
  db2 <- dbGetQuery(con, query2)
  query <- paste0("update stage_daily set date = '", range[i], "'")
  for (j in seq(2, dim(db2)[2], by = 2)) {
    # Calculate flags
    flag <- ifelse(length(which(db2[, j + 1] == "M")) == length(db2[, j + 1]) | length(which(is.na(db2[, j]))) == length(db2[, j]), "M", ifelse(median(db2[, j], na.rm=T) < gages$dry_elevation[j / 2], "D", ifelse(length(which(is.na(db2[, j + 1]))) > 0, "O" , "E")))
    if (is.na(median(db2[, j], na.rm=T))) val <- "NULL" else val <- median(db2[, j], na.rm=T)
    query <- paste0(query, ", `", names(db2)[j], "` = ", val, ", `", names(db2)[j + 1] ,"` = '", flag, "'")
  }
  query <- paste0(query, " where date = '", range[i], "'")
  dbSendQuery(con, query)
}
