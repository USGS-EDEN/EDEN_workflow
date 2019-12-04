# -------------
# Bryan McCloskey
# bmccloskey@usgs.gov
# St. Petersburg Coastal and Marine Science Center
# US Geological Survey
#
# 07/12/2018
#--------------

print("These libraries must be installed: RMySQL, RCurl")
# Required libraries. If not present, run:
# install.packages("RMySQL")
# install.packages("RCurl")
library (RMySQL)
library (RCurl)

# Command line arguments for number of days to load
days <- as.numeric(commandArgs(trailingOnly = T))
if (length(days) != 1)
  days <- 4

try (setwd("./EDENdb_upload"), silent = T)
source ("../admin_pwd.R")
# Connect to database and download data file
con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "eden_new", host = "stpweb1-dmz.er.usgs.gov")
# This will work for local or FTP ADAM-format files, as needed
data_file <- "ftp://ftpint.usgs.gov/from_pub/er/ISOutput_Run.txt"
file_columns <- c(rep("NULL", 3), "numeric", "character", "numeric", rep("NULL", 2), "numeric", rep("NULL", 9), "character", "character", "numeric", rep("NULL", 20), "character", rep("NULL", 4))
err <- try (z <- read.csv(data_file, colClasses = file_columns))

# If successful, process data; 'report' contains text of alert email
if (inherits(err, "try-error")) { report <- "ISOutput_Run.txt input file not downloaded.\n"
} else { # Process and upload
  # Remove unwanted gage
  z <- z[which(z$station_name_web != "Shark_River_Below_Gunboat_Island_Acoustic"), ]
  # Format timestamps
  z$date_tm <- as.POSIXct(z$date_tm, tz = "EST", format = "%m/%d/%Y %H:%M:%S")
  # Timestamp range present in file:
  first <- sort(unique(z$date_tm))[1]
  last <- rev(sort(unique(z$date_tm)))[1]
  range <- seq.POSIXt(first, last, "hour")
  # Remove non-hourly
  z <- z[z$date_tm %in% range, ]
  # Retain all timestamps from within last four days
  z <- z[which(as.Date(z$date_tm, "EST") >= as.Date(Sys.Date() - days)), ]
  # Timestamps present in file
  timestamp <- sort(unique(z$date_tm))

  # Check exptected date of first timestamp
  report <- if (as.Date(timestamp[1]) == Sys.Date() - days) {
    paste("Expected first timestamp date of", Sys.Date() - days, "confirmed\n\n")
  } else paste0("***WARNING*** First timestamp of file is not expected date of ", Sys.Date() - days, "\n\n")
  # Report first and last timestamp, number of reviewed values
  report <- paste0(report, "ISOutput_Run.txt input file beginning timestamp: ", timestamp[1], "\n")
  report <- paste0(report, "ISOutput_Run.txt input file ending timestamp: ", timestamp[length(timestamp)], "\n")
  rev_values <- z$RevValue[!is.na(z$RevValue)]
  report <- paste0(report, "Number of reviewed values: ", length(rev_values), "\n\n")
  report <- paste0(report, "Gages with unexpected number of timestamps or usable data values, or > +/- 0.15ft initial jump in ISOutput_Run.txt input file:\n\n")
  # Initialize vector of column names
  names <- "datetime"

  # Check data length of gages present in file
  stations <- unique(z$station_name_web)
  for (i in 1:length(stations)) {
    # Number of timestamps prior to today -- expect 96 hourly timestamps
  	tmp <- dim(z[which(z$station_name_web == stations[i] & as.Date(z$date_tm, tz = "EST") < Sys.Date()), ])[1]
  	# Number of timestamps with no Reviewed or Preliminary values
  	tmp2 <- dim(z[which(z$station_name_web == stations[i] & is.na(z$RevValue) & is.na(z$PrelimValue) & as.Date(z$date_tm, tz = "EST") < Sys.Date()), ])[1]
  	if (tmp != days * 24) report <- paste0(report, stations[i], ": ", tmp, " timestamps present before ", Sys.Date(), "\n")
  	if (tmp2 != 0) report <- paste0(report, stations[i], ": ", tmp2, " missing data values before ", Sys.Date()," (no reviewed, actual, or preliminary values)\n")
  	# Build vector of stage and flag columns
  	names[i * 2] <- paste0("stage_", stations[i])
  	names[i * 2 + 1] <- paste0("flag_", stations[i])
  }

  # List of expected upload gages from EDENdb
  gages <- dbGetQuery(con, "select station_name, station_name_web, agency_acronym as agency, utm_easting, utm_northing, dry_elevation, convert_to_navd88_feet as conv from station, agency, station_datum where station.database_agency_id = agency.agency_id and station.station_id = station_datum.station_id and edenmaster_new = 1 group by station_name order by agency, station_name_web")
  # Check presence of surfacing gages in data file
  for (i in 1:length(gages$station_name_web))
  	if (dim(z[z$station_name_web == gages$station_name_web[i], ])[1] == 0) report <- paste0(report, "Expected gage ", gages$station_name_web[i], " missing from data file\n")

  # Initialize matrix of values to upload
  v <- data.frame(matrix(NA, length(timestamp), length(stations) * 2))
  v <- cbind(timestamp, v)
  names(v) <- names
  # Loop through gages to construct matrix
  for (i in 1:length(stations)) {
    # Remove duplicate records
  	tmp <- unique(z[z$station_name_web == stations[i], ])
  	# Order rows by timestamp
  	tmp <- tmp[order(tmp$date_tm), ]
  	# If Reviewed value is blank, use Preliminary value
    v[, 2 * i] <- ifelse(is.na(tmp$RevValue), tmp$PrelimValue, tmp$RevValue)
    ### Non-datum-conversion adjustments ###
    if (unique(z$station_name_web)[i] == "SITE_8C") v[, 2 * i] <- v[, 2 * i] - 0.16
  	if (unique(z$station_name_web)[i] == "3ANE" | unique(z$station_name_web)[i] == "3ANE_GW") v[, 2 * i] <- v[, 2 * i] + 0.4
    # Calculate flag value
    v[, 2 * i + 1] <- ifelse(grepl("Actual Value", tmp$RevNote), NA, ifelse(grepl("X$", tmp$RevNote), ifelse(is.na(tmp$ActualValue), "G", "E"), ifelse(grepl("No Prelim", tmp$PrelimNote), "M", ifelse(grepl("Pred", tmp$PrelimNote), ifelse(is.na(tmp$ActualValue), "G", "E"), NA))))
    # Check value of previous timestamp in database for difference > 0.15 ft
    query <- paste0("select `stage_", stations[i], "` from stage where datetime = '", timestamp[1] - 3600, "'")
  	strt_chk <- unlist(dbGetQuery(con, query))
  	if (!is.null(strt_chk) & length(strt_chk)) if (!is.na(v[1, 2*i]) & !is.na(strt_chk)) if (abs(v[1, 2*i] - strt_chk) > 0.15) report <- paste0(report, "Gage ", stations[i], " value for ", timestamp[1] - 3600, ": ", strt_chk, "; midnight value for ", timestamp[1], ": ", v[1, 2 * i], "\n")
  }

  # Upload matrix to EDENdb
  for (i in 1:dim(v)[1]) {
    ### Caution! 'replace' erases non-specified columns!! Use 'update' if modifying subsets of columns
  	query <- paste0("replace into stage set datetime='", v$datetime[i], "', ")
  	for (j in 2:dim(v)[2]) {
  	  # Quote both flags and numbers, not null
  		if (is.na(v[i, j])) tmp <- "NULL" else tmp <- paste0("'", v[i, j], "'")
  		query <- paste0(query, "`", names(v)[j], "`=", tmp, ", ")
  	}
  	# Remove trailing comma
  	query <- substr(query, 1, nchar(query) - 2)
  	# Upload timestamp row to database
  	err <- try (dbSendQuery(con, query))
  	if (inherits(err, "try-error")) report <- paste0(report, "\nEDENdb upload error for ", v$datetime[i], ": ", err)
  }
} # End process and upload

## Plot data for review
#for (i in seq(2, dim(v)[2], 2)) {
#  g <- dbGetQuery(con, paste0("select datetime, `", names[i], "` as st from stage where datetime >= '", timestamp[1] - 3600, "' and datetime <= '", rev(timestamp)[1] + 7200, "' order by datetime"))
#  g$datetime <- as.POSIXct(g$datetime, tz = "EST", format = "%Y-%m-%d %H:%M")
#  pts <- c(1, 2, length(g$datetime) - 2, length(g$datetime) - 1, length(g$datetime))
#  g1 <- data.frame(names[i], g$datetime[pts], g$st[pts])
#  write.table(g1, "./pdf/marryup.txt", sep="\t", quote=F, row.names=F, col.names=F, append=T)
#  pdf(paste0("./pdf/", names[i], ".pdf"), 14, 7)
#  plot(g$datetime, g$st, type="l", main = names[i], ylab="ft.")
#  points(g$datetime[pts], g$st[pts], pch=16, col="blue")
#  dev.off()
#}

## Generate daily median input files & annotated daily median file
# Change ENP agency name
gages$agency[which(gages$agency == "ENP")] <- "NPS"
# Date range to generate files for
dt <- seq(Sys.Date() - 4, Sys.Date() - 1, "days")
# Default dry values for gages missing them
gages$dry_elevation[which(is.na(gages$dry_elevation))] <- -9999
# Loop by dates to generate annotated daily median flag files
for (j in 1:length(dt)) {
  # Counter of failed gages
  fail <- 0
  text <- "Agency	Station	X	Y	Daily Median Water Level (cm, NAVD88)	Date	Data Type"
  write.table(text, paste0("./flag/", format(dt[j],"%Y%m%d"), "_median_flag.txt"), quote = F, row.names = F, col.names = F, eol = "\r\n")
  for (i in 1:length(gages$station_name_web)) {
    # Select gage data
	  query <- paste0("select datetime, `stage_", gages$station_name_web[i], "`+", gages$conv[i], " as stage, `flag_", gages$station_name_web[i], "` as flag from stage where datetime >= ", format(dt[j], "%Y%m%d010000"), " and datetime < ", format(dt[j] + 1, "%Y%m%d000001"), " order by datetime")
	  db <- dbGetQuery(con, query)
	  # Calculate daily flags
	  flag <- ifelse(length(which(db$flag == "M")) == 24, "M", ifelse(median(db$stage, na.rm = T) < gages$dry_elevation[i], "D", ifelse(length(which(is.na(db$flag))) > 0, "O", "E")))
	  # Calculate medians, generate output text
	  text <- c(gages$agency[i], gages$station_name_web[i], round(gages$utm_easting[i], 1), round(gages$utm_northing[i], 1), round(median(db$stage, na.rm=T) * 12 * 2.54), format(dt[j], "%Y%m%d"), flag)
	  err <- try (write.table(t(text), paste0("./flag/", format(dt[j], "%Y%m%d"), "_median_flag.txt"), sep = "\t", quote = F, row.names = F, col.names = F, append = T, eol = "\r\n"))
	  if (inherits(err, "try-error")) { report <- paste0(report, "\nAnnotated daily median file NOT generated for ", gages$station_name_web[i], " for ", dt[j]); fail <- fail + 1 }
  }
  if (fail == 0) report <- paste0(report, "\n\nAnnotated daily median file generated for ", dt[j])
  # Transfer file to eFTP
  err <- try(ftpUpload(paste0("./flag/", format(dt[j],"%Y%m%d"), "_median_flag.txt"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden-data/netcdf/", format(dt[j],"%Y%m%d"), "_median_flag.txt"), .opts = list(forbid.reuse = 1)))
  report <- if (inherits(err, "try-error")) paste0(report, "\nAnnotated daily median file NOT transferred for ", dt[j]) else paste0(report, "\nAnnotated daily median file transferred for ", dt[j])
}
err <- try (write(report, paste0("./reports/report_", format(Sys.Date(), "%Y%m%d"), ".txt")))
# Email report of data upload
### System level commands may not work if local environment does not have sendmail installed!!
to <- "bmccloskey@usgs.gov, mdpetkew@usgs.gov, jmclark@usgs.gov, matthews@usgs.gov, dantolin@usgs.gov, bhuffman@usgs.gov, hhenkel@usgs.gov"
system(paste0("echo 'Subject: EDENdb upload report
", report, "' | /usr/sbin/sendmail ", to))

# Update daily stage values table with medians of hourly values
gages <- dbGetQuery(con, "select station_name_web, dry_elevation from station where display = 1 and station_name_web != 'Alligator_Creek' and station_name_web != 'East_Side_Creek' and station_name_web != 'G-3777' and station_name_web != 'Manatee_Bay_Creek' and station_name_web != 'Raulerson_Brothers_Canal' and station_name_web != 'Barron_River'")
gages$dry_elevation[which(is.na(gages$dry_elevation))] <- -9999
# Determine whether flags are estimated or hindcast
for (j in 1:length(gages$station_name_web))
  gages$hindcast[j] <- dbGetQuery(con, paste0("select min(datetime) from stage where `stage_", gages$station_name_web[j], "` is not null and `flag_", gages$station_name_web[j], "` is null"))
# Calculate range of date coverage
start <- dbGetQuery(con, "select date(min(datetime)) as date from stage")
end <- dbGetQuery(con, "select date(max(datetime)) as date from stage")
range <- seq.Date(as.Date(as.character(start), "%Y-%m-%d"), as.Date(as.character(end), "%Y-%m-%d"), "day")
for (i in length(range):1) {
  if (as.POSIXlt(range[i])$yday == 0) print(as.POSIXlt(range[i])$year + 1900)
  dbDisconnect(con)
  con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "eden_new", host = "stpweb1-dmz.er.usgs.gov")
  # Select hourly values
  query2 <- "select datetime"
  for (j in 1:length(gages$station_name_web))
    query2 <- paste0(query2, ", `stage_", gages$station_name_web[j], "`, `flag_", gages$station_name_web[j], "`")
  query2 <- paste0(query2, " from stage where datetime >= ", format(range[i], "%Y%m%d000000"), " and datetime < ", format(range[i] + 1, "%Y%m%d000000"), " order by datetime")
  err <- try (db2 <- dbGetQuery(con, query2), silent = T)
  if (inherits(err, "try-error")) next # RMySQL library has bug occasionally causing this query to fail
  # Determin whether to insert (new rows) or update (existing rows)
  date_check <- dbGetQuery(con, paste0("select count(date) as ct from stage_daily where date = '", range[i], "'"))
  in_up <- if (date_check$ct == 1) "update" else "insert into"
  query <- paste0(in_up, " stage_daily set date = '", range[i], "'")
  if (dim(db2)[2] != 0) # missed days
    for (j in seq(2, dim(db2)[2], by = 2)) {
      # Calculate flags
      flag <- ifelse(length(which(db2[, j + 1] == "M")) == length(db2[, j + 1]) | length(which(is.na(db2[, j]))) == length(db2[, j]), "M", ifelse(median(db2[, j], na.rm=T) < gages$dry_elevation[j / 2], "D", ifelse(length(which(is.na(db2[, j + 1]))) > 0, "O" ,ifelse(db2$datetime < gages$hindcast[j / 2], "H", "E"))))
      if (is.na(median(db2[, j], na.rm = T))) val <- "NULL" else val <- median(db2[, j], na.rm = T)
      query <- paste0(query, ", `", names(db2)[j], "` = ", val, ", `", names(db2)[j + 1] ,"` = '", flag, "'")
    }
  if (date_check$ct == 1)
    query <- paste0(query, " where date = '", range[i], "'")
  dbSendQuery(con, query)
}

# update test databases
range <- dbGetQuery(con, paste0("select datetime from stage where datetime > 20190801000000 order by datetime"))
range <- as.POSIXct(range$datetime, tz = "EST")
for (i in 1:length(range)) {
  date_check <- dbGetQuery(con, paste0("select count(datetime) as ct from stage_test where datetime = '", range[i], "'"))
  if (date_check$ct == 0)
    dbSendQuery(con, paste0("insert into stage_test (datetime) values ('", range[i], "')"))
  date_check <- dbGetQuery(con, paste0("select count(datetime) as ct from supp_stage where datetime = '", range[i], "'"))
  if (date_check$ct == 0)
    dbSendQuery(con, paste0("insert into supp_stage (datetime) values ('", range[i], "')"))
}
range <- dbGetQuery(con, paste0("select date from stage_daily where date > 20190801 order by date"))
range <- as.Date(range$date)
for (i in 1:length(range)) {
  date_check <- dbGetQuery(con, paste0("select count(date) as ct from stage_daily_test where date = '", range[i], "'"))
  if (date_check$ct == 0)
    dbSendQuery(con, paste0("insert into stage_daily_test (date) values ('", range[i], "')"))
  date_check <- dbGetQuery(con, paste0("select count(date) as ct from supp_stage_daily where date = '", range[i], "'"))
  if (date_check$ct == 0)
    dbSendQuery(con, paste0("insert into supp_stage_daily (date) values ('", range[i], "')"))
}
setwd("..")
