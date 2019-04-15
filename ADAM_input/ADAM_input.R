# -------------
# Bryan McCloskey
# bmccloskey@usgs.gov
# St. Petersburg Coastal and Marine Science Center
# US Geological Survey
#
# 07/11/2018
#--------------

print("These libraries must be installed: RMySQL, RCurl, stringr")
# Required libraries. If not present, run:
# install.packages("RMySQL")
# install.packages("RCurl")
# install.packages("stringr")
library (RMySQL)
library (RCurl)
library (stringr)

try (setwd("./ADAM_input"), silent = T)
source ("../usr_pwd.R")
# Connect to database, list of gages for which to acquire data
con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "eden_new", host = "stpweb1-dmz.er.usgs.gov")
db <- dbGetQuery(con, "select station_name, operating_agency_id, usgs_nwis_id, dd, param, case when vertical_datum_id = 2 then vertical_conversion else 0 end as conv from station, station_datum where station.station_id = station_datum.station_id and realtime = 1 group by station_name order by operating_agency_id, station_name")

# Dummy NWIS IDs, other legacy params, for new gages
db$usgs_nwis_id[db$station_name == "SPARO"]    <- "999999999999998"
db$usgs_nwis_id[db$station_name == "USSO+"]    <- "999999999999997"
db$usgs_nwis_id[db$station_name == "L28GAPG1"] <- "999999999999996"
db$usgs_nwis_id[db$station_name == "USSO-T"]   <- "999999999999995"
db$usgs_nwis_id[db$station_name == "2A159+"]   <- "999999999999994"
db$param[db$param == ""] <- "99999"
db$dd[db$dd == 0] <- "9"

# Initialize email report
report <- ""
# Table of daily hourly values by agency
tbl <- matrix(0, 3, 4)
header <- "agency_cd\tsite_no\tdd_nu\tparameter_cd\tUVTYPE\tdate_tm\tTZCD\tVALUE\tPRECISION\tREMARK\tFLAGS\tQA"
write(header, "./output/data_uv_AQ.txt")
days <- c(Sys.Date() - 4, Sys.Date())
# 15-minute and 6-minute timestamps
start <- strptime(paste(days[1], "00:00:00"), "%Y-%m-%d %H:%M:%S", "EST")
end <- strptime(paste(days[2], "07:00:00"), "%Y-%m-%d %H:%M:%S", "EST")
range <- seq.POSIXt(start, end, "15 min")
range <- sort(unique(c(range, seq.POSIXt(start, end, "6 min"))))
range2 <- seq.POSIXt(start, end, "1 hour")
# Yesterday's noon timestamp
noon <- which(as.POSIXlt(range)$yday == as.POSIXlt(Sys.Date())$yday - 1 & as.POSIXlt(range)$hour == 12 & as.POSIXlt(range)$min == 0)
day1 <- as.Date(Sys.Date()) - 4
day2 <- as.Date(Sys.Date()) - 3
day3 <- as.Date(Sys.Date()) - 2
day4 <- as.Date(Sys.Date()) - 1

# Initialize number of hourly values count variable
usgs_cnt <- enp_cnt <- sfwmd_cnt <- rep(0, length(range2))
int <- NULL
# Initialize noon-values counter variable
cnt <- 0
usgs_gages <- dbGetQuery(con, "select station_name_web, usgs_nwis_id, dd, param, ts_id from station where operating_agency_id = 4 and (realtime = 1) and station_name_web != 'S150_T' order by station_name_web")
usgs_gages <- rbind(usgs_gages, c("Shark_River_Acoustic", "252230081021300", 9, "00065", "171031"))

for (i in 1:length(usgs_gages$station_name_web)) {
  # Generate AQUARIUS URLs; add 1 for DST end timestamps
  url <- paste0("https://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=", usgs_gages$usgs_nwis_id[i], "&startDT=", days[1], "&endDT=", days[2], "&parameterCd=", usgs_gages$param[i], "&access=3")
  tmp <- try (read.table(url, header = T, sep = "\t", colClasses = "character"))
  if (inherits(tmp, "try-error")) {
    report <- paste0(report, "USGS data _NOT_ downloaded for ", usgs_gages$station_name_web[i], "\n")
  } else {
    # Remove data descriptor header row
    tmp <- tmp[2:dim(tmp)[1], ]
    # Find the column that contains the data
    dd_col <- if (!is.na(usgs_gages$ts_id[i])) which(grepl(usgs_gages$ts_id[i], names(tmp)))[1] else which(grepl(usgs_gages$param[i], names(tmp)))[1]
    # Check for presence of correct TS ID
    if (is.na(dd_col)) { 
      report <- paste0(report, "Gage ", usgs_gages$station_name_web[i], " missing TS ID\n")
    } else {
      # Convert timestamps; shift DST
      tmp$datetime <- as.POSIXct(tmp$datetime, "EST", format = "%Y-%m-%d %H:%M")
      tmp$datetime <- as.POSIXct(ifelse(tmp$tz_cd == "EDT", tmp$datetime - 3600, tmp$datetime), "EST", origin = "1970-01-01")
      tmp <- tmp[tmp$datetime %in% range, ]
      # Remove rows with "_Eqp" or "_Dry" flags
      if (any(grepl("_Eqp|_Dry|_Fld", tmp[, dd_col])))
        tmp <- tmp[-which(grepl("_Eqp|_Dry|_Fld", tmp[, dd_col])), ]
      # URLs with no returned data
      if (dim(tmp)[1] == 0 || tmp[, 1] == "") {
        report <- paste0(report, "Gage ", usgs_gages$station_name_web[i], " missing\n")
      } else {
        # Check presence of yesterday's noon timestamp; add to count
        if (length(which(tmp$datetime == range[noon]))) if(tmp[which(tmp$datetime == range[noon]), dd_col] != "") cnt <- cnt + 1
        # Build data.frame of USGS data
        usgs <- data.frame("USGS", tmp$site_no, usgs_gages$dd[i], usgs_gages$param[i], "da", datetime = format(tmp$datetime, "%m/%d/%Y %H:%M:%S"), "EST", tmp[, dd_col], 9, 9, 9, 9)
        # Counts of gages with values each day for report
        if (length(which(as.Date(usgs$datetime, format="%m/%d/%Y %H:%M:%S") == day1))) tbl[1, 1] <- tbl[1, 1] + 1
        if (length(which(as.Date(usgs$datetime, format="%m/%d/%Y %H:%M:%S") == day2))) tbl[1, 2] <- tbl[1, 2] + 1
        if (length(which(as.Date(usgs$datetime, format="%m/%d/%Y %H:%M:%S") == day3))) tbl[1, 3] <- tbl[1, 3] + 1
        if (length(which(as.Date(usgs$datetime, format="%m/%d/%Y %H:%M:%S") == day4))) tbl[1, 4] <- tbl[1, 4] + 1
        for (j in 1:length(usgs_cnt)) if (length(which(tmp$datetime == range2[j])) == 1) usgs_cnt[j] <- usgs_cnt[j] + 1
        # Write USGS data out to file
        write.table(usgs, "./output/data_uv_AQ.txt", sep = "\t", quote = F, row.names = F, col.names = F, append = T)
      }
    }
  }
}
# Report USGS gage count
report <- paste0(report, "USGS gages with ", range[noon], " values: ", cnt, ".\n")

# Initialize ENP data.frame
enp <- NULL
# Loop through days
for (i in 4:0) {
  enp_file <- NULL
  ## For local files, download manually to ./enp and uncomment to following:
  # enp_file <- list.files("./enp", paste0("enp_", format(days[2] - i, "%Y%m%d"), "_0[456]")); err <- err2 <- 1

  ## By default, download remote files (comment out next two lines to use local files)
  # Find today's ENP file name
  err2 <- try (enp_file <- tail(strsplit(grep(paste0("enp_", format(days[2] - i, "%Y%m%d"), "_0[456]"), strsplit(getURL("ftp://ftpint.usgs.gov/from_pub/er/enp/"), "\r*\n")[[1]], value = T), " ")[[1]], n = 1))
  # Download today's ENP file
  err <- try (download.file(paste0("ftp://ftpint.usgs.gov/from_pub/er/enp/", enp_file), paste0("./enp/", enp_file)))

  if (inherits(err, "try-error") | inherits(err2, "try-error") | !file.exists(paste0("./enp/", enp_file)) | !file.info(paste0("./enp/", enp_file))$size) {
    report <- paste0(report, "ENP input file _NOT_ downloaded for ", format(days[2] - i, "%m/%d/%Y"), ".\n")
  } else {
    report <- paste0(report, "ENP file downloaded for ", format(days[2] - i, "%m/%d/%Y"), ".\n")
    # Read in ENP data
    enp_tmp <- read.table(paste0("./enp/", enp_file), colClasses = c("NULL", rep("character", 3)))
    # Create timestamp
    enp_tmp$V3 <- paste0(substr(enp_tmp$V3, 5, 6), "/", substr(enp_tmp$V3, 7, 8), "/", substr(enp_tmp$V3, 1, 4), " ", substr(enp_tmp$V4, 3, 4), ":", substr(enp_tmp$V4, 5, 6), ":00")
    # Extract data values
    enp_tmp$V5 <- sapply(strsplit(enp_tmp$V4, "/"), "[", 4)
    # Extract data type
    enp_tmp$V4 <- sapply(strsplit(enp_tmp$V4, "/"), "[", 2)
    # Filter wanted data types
    enp_tmp <- enp_tmp[enp_tmp$V4 %in% c("HH", "DWM", "STAGE", "STM"), ]
    # Filter unwanted gages
    enp_tmp <- enp_tmp[enp_tmp$V2 %in% db$station_name, ]
    # Filter NA values
    enp_tmp <- enp_tmp[!is.na(enp_tmp$V5), ]
    # Format and filter timestamps
    enp_tmp$date_tm <- as.POSIXct(enp_tmp$V3, tz="EST", format="%m/%d/%Y %H:%M:%S")
    enp_tmp <- enp_tmp[enp_tmp$date_tm %in% range, ]
    # Merge dates together; keep most recent values
    if (is.null(enp)) enp <- enp_tmp else {
      enp <- merge(enp, enp_tmp, by = c("V2", "V3", "V4", "date_tm"), all = T)
      enp$V5.x[is.na(enp$V5.x) | (!is.na(enp$V5.y) & enp$V5.x != enp$V5.y)] <- enp$V5.y[is.na(enp$V5.x) | (!is.na(enp$V5.y) & enp$V5.x != enp$V5.y)]
      enp <- enp[, 1:5]
      names(enp)[5] <- "V5"
    }
  }
}
# Report yesterday's ENP noon timestamp count
report <- paste0(report, "ENP gages with ", range[noon], " values: ", length(unique(enp$V2[which(enp$date_tm == range[noon])])), ".\n")
# Counts of gages with values each day for report
tbl[2, 1] <- length(which(as.POSIXlt(enp$date_tm)$min == 0 & as.POSIXlt(enp$date_tm)$hour == 0 & as.Date(enp$date_tm) == day1))
tbl[2, 2] <- length(which(as.POSIXlt(enp$date_tm)$min == 0 & as.POSIXlt(enp$date_tm)$hour == 0 & as.Date(enp$date_tm) == day2))
tbl[2, 3] <- length(which(as.POSIXlt(enp$date_tm)$min == 0 & as.POSIXlt(enp$date_tm)$hour == 0 & as.Date(enp$date_tm) == day3))
tbl[2, 4] <- length(which(as.POSIXlt(enp$date_tm)$min == 0 & as.POSIXlt(enp$date_tm)$hour == 0 & as.Date(enp$date_tm) == day4))
for (j in 1:length(enp_cnt)) enp_cnt[j] <- length(which(enp$date_tm == range2[j]))
# Loop through expected ENP gages
for (i in which(db$operating_agency_id == 1))
  # Write ENP data to file
  if (length(enp$V3[enp$V2 == db$station_name[i]])) {
    df <- data.frame("USNPS", db$usgs_nwis_id[i], str_pad(db$dd[i], 4), db$param[i], "da", enp$V3[enp$V2 == db$station_name[i]], "EST", as.numeric(enp$V5[enp$V2 == db$station_name[i]]) + db$conv[i], "9", "", "", "9")
    write.table(df, "./output/data_uv_AQ.txt", sep = "\t", quote = F, row.names = F, col.names = F, append = T)
  }

# Initialize SFWMD data.frame
sfwmd <- NULL
# Loop through days
for (i in 4:0) {
  sfwmd_file <- NULL
  ## For local files, download manually to ./sfwmd and uncomment to following:
  # sfwmd_file <- list.files("./sfwmd", paste0("sfwmd_", format(days[2] - i, "%Y%m%d"), "_0[56]")); err <- err2 <- 1
  
  ## By default, download remote files (comment out next two lines to use local files)
  # Find today's SFWMD file name
  err2 <- try (sfwmd_file <- tail(strsplit(grep(paste0("sfwmd_", format(days[2] - i, "%Y%m%d"), "_0[56]"), strsplit(getURL("ftp://ftpint.usgs.gov/from_pub/er/eden/"), "\r*\n")[[1]], value=T), " ")[[1]], n = 1))
  # Download today's SFWMD file
  err <- try (download.file(paste0("ftp://ftpint.usgs.gov/from_pub/er/eden/", sfwmd_file), paste0("./sfwmd/", sfwmd_file)))
  if (inherits(err, "try-error") | inherits(err2, "try-error") | is.null(sfwmd_file) | !file.exists(paste0("./sfwmd/", sfwmd_file)) | !file.info(paste0("./sfwmd/", sfwmd_file))$size) {
    report <- paste0(report, "SFWMD input file _NOT_ downloaded for ", format(days[2] - i, "%m/%d/%Y"), ".\n")
  } else {
    report <- paste0(report, "SFWMD file downloaded for ", format(days[2] - i, "%m/%d/%Y"), ".\n")
    # Read in SFWMD data
    sfwmd_tmp <- read.fwf(paste0("./sfwmd/", sfwmd_file), widths = c(11, 17, 13, 13, 10, 2), strip.white = T, colClasses = c(rep("character", 4), "numeric", "character"))
    # Create timestamp
    sfwmd_tmp$V3 <- paste0(substr(sfwmd_tmp$V3, 5, 6), "/", substr(sfwmd_tmp$V3, 7, 8), "/", substr(sfwmd_tmp$V3, 1, 4), " ", substr(sfwmd_tmp$V3, 9, 10), ":", substr(sfwmd_tmp$V3, 11, 12), ":00")
    # Filter unwanted gages
    sfwmd_tmp <- sfwmd_tmp[sfwmd_tmp$V1 %in% db$station_name, ]
    # Format and filter timestamps
    sfwmd_tmp$date_tm <- as.POSIXct(sfwmd_tmp$V3, tz = "EST", format = "%m/%d/%Y %H:%M:%S")
    sfwmd_tmp <- sfwmd_tmp[sfwmd_tmp$date_tm %in% range, ]
    # Filter flagged timestamps
    sfwmd_tmp <- sfwmd_tmp[which(sfwmd_tmp$V6 != "<" & sfwmd_tmp$V6 != "?"), ]
    # Filter NA values
    sfwmd_tmp <- sfwmd_tmp[!is.na(sfwmd_tmp$V5), ]
    # Merge dates together; keep most recent values
    if (is.null(sfwmd)) sfwmd <- sfwmd_tmp else {
      sfwmd <- merge(sfwmd, sfwmd_tmp, by = c("V1", "V2", "V3", "V4", "V6", "date_tm"), all = T)
      sfwmd$V5.x[is.na(sfwmd$V5.x) | (!is.na(sfwmd$V5.y) & sfwmd$V5.x != sfwmd$V5.y)] <- sfwmd$V5.y[is.na(sfwmd$V5.x) | (!is.na(sfwmd$V5.y) & sfwmd$V5.x != sfwmd$V5.y)]
      sfwmd <- sfwmd[, 1:7]
      names(sfwmd)[7] <- "V5"
    }
  }
}
# Report yesterday's SFWMD noon timestamp count
report <- paste0(report, "SFWMD gages with ", range[noon], " values: ", length(unique(sfwmd$V1[which(sfwmd$date_tm == range[noon])])), ".\n")
# Counts of gages with values each day for report
tbl[3, 1] <- length(which(as.POSIXlt(sfwmd$date_tm)$min == 0 & as.POSIXlt(sfwmd$date_tm)$hour == 0 & as.Date(sfwmd$date_tm) == day1))
tbl[3, 2] <- length(which(as.POSIXlt(sfwmd$date_tm)$min == 0 & as.POSIXlt(sfwmd$date_tm)$hour == 0 & as.Date(sfwmd$date_tm) == day2))
tbl[3, 3] <- length(which(as.POSIXlt(sfwmd$date_tm)$min == 0 & as.POSIXlt(sfwmd$date_tm)$hour == 0 & as.Date(sfwmd$date_tm) == day3))
tbl[3, 4] <- length(which(as.POSIXlt(sfwmd$date_tm)$min == 0 & as.POSIXlt(sfwmd$date_tm)$hour == 0 & as.Date(sfwmd$date_tm) == day4))
for (j in 1:length(sfwmd_cnt)) sfwmd_cnt[j] <- length(which(sfwmd$date_tm == range2[j]))
# Loop through expected SFWMD gages
for (i in which(db$operating_agency_id == 2 | db$operating_agency_id == 3))
  # Write SFWMD data to file
  if (length(sfwmd$V3[sfwmd$V1 == db$station_name[i]])) {
    df <- data.frame("FL005", db$usgs_nwis_id[i], paste0("   ", db$dd[i]), db$param[i], "da", sfwmd$V3[sfwmd$V1 == db$station_name[i]], "EST", as.numeric(sfwmd$V5[sfwmd$V1 == db$station_name[i]]) + db$conv[i], "9", "", "", "9")
    write.table(df, "./output/data_uv_AQ.txt", sep = "\t", quote = F, row.names = F, col.names = F, append = T)
  }

# Add table of daily gage counts to report
tot <- sum(usgs_cnt[1:96], enp_cnt[1:96], sfwmd_cnt[1:96])
report <- paste0(report, "\nApproximate gages by agency, each day:\nAgency\tDay 1\tDay 2\tDay 3\tDay 4\nUSGS\t", paste0(tbl[1, ], collapse = "\t"), "\nENP\t", paste0(tbl[2, ], collapse = "\t"), "\nSFWMD\t", paste0(tbl[3, ], collapse = "\t"), "\n\nTotal number of hourly values in data file: ", tot, "\n")
write(tot, "./reports/hour_values_count.txt")

png("./reports/gage_count.png", 862, 614, type = "quartz")
plot(range2, usgs_cnt, type = "l", lwd = 3, ylim = range(usgs_cnt, enp_cnt, sfwmd_cnt, 0, na.rm = T), main = paste(as.Date(range2[length(range2)]), "ADAM input gage hourly values count"), xlab = "Date", ylab = "Gage values count")
points(range2, usgs_cnt, pch = 16)
lines(range2, enp_cnt, lwd = 3, col = "blue")
points(range2, enp_cnt, pch = 16, col = "blue")
lines(range2, sfwmd_cnt, lwd = 3, col = "red")
points(range2, sfwmd_cnt, pch = 16, col = "red")
abline(v = range2[which(as.POSIXlt(range2)$hour == 0)], lty = "dashed")
legend("bottomleft", c("USGS", "ENP", "SFWMD"), col = c("black", "blue", "red"), lwd = 3, pch = 16)
dev.off()

err <- try (ftpUpload("./output/data_uv_AQ.txt", "ftp://ftpint.usgs.gov/private/nonvisible/er/data_uv_AQ.txt"))
report <- if (inherits(err, "try-error")) paste0(report, "\ndata_uv_AQ.txt file NOT transferred for ", days[2]) else paste0(report, "\ndata_uv_AQ.txt file transferred for ", days[2])
zip("./output/data_uv_AQ.txt.zip", "./output/data_uv_AQ.txt", "-j9X")
err <- try (ftpUpload("./reports/hour_values_count.txt", "ftp://ftpint.usgs.gov/private/nonvisible/er/hour_values_count.txt"))
err <- try (write(report, paste0("./reports/report_", format(Sys.Date(), "%Y%m%d"), ".txt")))

### System level commands may not work if local environment does not have sendmail installed!!
to <- "bmccloskey@usgs.gov, mdpetkew@usgs.gov, matthews@usgs.gov, jmclark@usgs.gov, wbguimar@usgs.gov, dantolin@usgs.gov, bhuffman@usgs.gov, kjconlon@usgs.gov"
system(paste0("(echo 'Subject: data_uv_AQ.txt upload report
", report, "';uuencode ", "./output/data_uv_AQ.txt.zip data_uv_AQ.txt.zip;uuencode ", "./reports/gage_count.png gage_count.png) | /usr/sbin/sendmail ", to))
setwd("..")
