# -------------
# Bryan McCloskey
# bmccloskey@usgs.gov
# St. Petersburg Coastal and Marine Science Center
# US Geological Survey
#
# 07/17/2019
# v.2.0
#--------------

print("These libraries must be installed: RMySQL, RCurl, stringr")
# Required libraries. If not present, run:
# install.packages("RMySQL", "RCurl", "stringr")
library (RMySQL)
library (RCurl)
library (stringr)

try (setwd("./ADAM_input"), silent = T)
source ("../usr_pwd.R")
# Connect to database, list of gages for which to acquire data
con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "eden_new", host = "stpweb1-dmz.er.usgs.gov")

gages <- dbGetQuery(con, "select station_name_web, station_name, operating_agency_id, usgs_nwis_id, dd, param, ts_id, case when vertical_datum_id = 2 then vertical_conversion else 0 end as conv from station, station_datum where station.station_id = station_datum.station_id and realtime = 1 group by station_name order by operating_agency_id, station_name")
# Dummy NWIS IDs, other legacy params, for new gages
gages$usgs_nwis_id[gages$station_name == "SPARO"]    <- "999999999999998"
gages$usgs_nwis_id[gages$station_name == "USSO+"]    <- "999999999999997"
gages$usgs_nwis_id[gages$station_name == "L28GAPG1"] <- "999999999999996"
gages$usgs_nwis_id[gages$station_name == "USSO-T"]   <- "999999999999995"
gages$usgs_nwis_id[gages$station_name == "2A159+"]   <- "999999999999994"
gages$param[gages$param == ""] <- "99999"
gages$dd[gages$dd == 0] <- "9"

# Command line arguments for number of days to load
lday <- commandArgs(trailingOnly = T)
if (length(lday) == 0)
  lday <- c(7, 4, 1)

header <- "agency_cd\tsite_no\tdd_nu\tparameter_cd\tUVTYPE\tdate_tm\tTZCD\tVALUE\tPRECISION\tREMARK\tFLAGS\tQA"
for (d in lday) {
  # Initialize email report
  report <- ""
  
  write(header, paste0("./output/data_uv_AQ", d, ".txt"))
  days <- c(Sys.Date() - d, Sys.Date())
  # 15-minute and 6-minute timestamps
  start <- strptime(paste(days[1], "00:00:00"), "%Y-%m-%d %H:%M:%S", "EST")
  end <- strptime(paste(days[2], "23:54:00"), "%Y-%m-%d %H:%M:%S", "EST")
  range <- seq.POSIXt(start, end, "15 min")
  range <- sort(unique(c(range, seq.POSIXt(start, end, "6 min"))))
  range2 <- seq.POSIXt(start, end, "1 hour")

  # Initialize number of hourly values count variable
  usgs_cnt <- enp_cnt <- sfwmd_cnt <- rep(0, length(range2))

  usgs_gages <- gages[gages$operating_agency_id == 4, c("station_name_web", "usgs_nwis_id", "dd", "param", "ts_id")]
  usgs_gages <- rbind(usgs_gages, c("Shark_River_Acoustic", "252230081021300", 9, "00065", "171031"))
  cnt <- dim(usgs_gages)[1]
  for (i in 1:length(usgs_gages$station_name_web)) {
    # Generate AQUARIUS URLs; add 1 for DST end timestamps
    url <- paste0("https://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=", usgs_gages$usgs_nwis_id[i], "&startDT=", days[1], "&endDT=", days[2], "&parameterCd=", usgs_gages$param[i], "&access=3")
    usgs <- try (read.table(url, header = T, sep = "\t", colClasses = "character"))
    if (inherits(usgs, "try-error")) {
      report <- paste0(report, "USGS data _NOT_ downloaded for ", usgs_gages$station_name_web[i], "\n")
    } else {
      # Remove data descriptor header row
      usgs <- usgs[2:dim(usgs)[1], ]
      # Find the column that contains the data
      dd_col <- if (!is.na(usgs_gages$ts_id[i])) which(grepl(usgs_gages$ts_id[i], names(usgs)))[1] else which(grepl(usgs_gages$param[i], names(usgs)))[1]
      # Check for presence of correct TS ID
      if (is.na(dd_col)) { 
        report <- paste0(report, "USGS Gage ", usgs_gages$station_name_web[i], " missing TS ID\n")
      } else {
        # Convert timestamps; shift DST
        usgs$datetime <- as.POSIXct(usgs$datetime, "EST", format = "%Y-%m-%d %H:%M")
        usgs$datetime <- as.POSIXct(ifelse(usgs$tz_cd == "EDT", usgs$datetime - 3600, usgs$datetime), "EST", origin = "1970-01-01")
        usgs <- usgs[usgs$datetime %in% range, ]
        # Remove rows with "_Eqp", "_Dry", or "_Fld" flags
        if (any(grepl("_Eqp|_Dry|_Fld", usgs[, dd_col])))
          usgs <- usgs[-which(grepl("_Eqp|_Dry|_Fld", usgs[, dd_col])), ]
        # URLs with no returned data
        if (dim(usgs)[1] == 0 || usgs[, 1] == "") {
          report <- paste0(report, "USGS Gage ", usgs_gages$station_name_web[i], " missing\n")
          cnt <- cnt - 1
        } else {
          # Counts of gages with values each hour
          for (j in 1:length(usgs_cnt)) if (length(which(usgs$datetime == range2[j])) == 1) usgs_cnt[j] <- usgs_cnt[j] + 1
          # Build data.frame of USGS data
          usgs <- data.frame("USGS", usgs$site_no, usgs_gages$dd[i], usgs_gages$param[i], "da", datetime = format(usgs$datetime, "%m/%d/%Y %H:%M:%S"), "EST", usgs[, dd_col], 9, 9, 9, 9)
          # Write USGS data out to file
          write.table(usgs, paste0("./output/data_uv_AQ", d, ".txt"), sep = "\t", quote = F, row.names = F, col.names = F, append = T)
        }
      }
    }
  }
  # Report USGS gage count
  report <- paste0(report, "USGS gages downloaded: ", cnt, "\n\n")

  # Initialize ENP data.frame
  enp <- NULL
  # Loop through days
  for (i in d:0) {
    ## Look for local file, otherwise look for remote file
    enp_file <- list.files("./enp/", format(days[2] - i, "%Y%m%d"))
    if (!length(enp_file)) {
      # Find day's ENP file name
      err2 <- try (enp_file <- tail(strsplit(grep(paste0("enp_", format(days[2] - i, "%Y%m%d"), "_0[456]"), strsplit(getURL("ftp://ftpint.usgs.gov/from_pub/er/enp/"), "\r*\n")[[1]], value = T), " ")[[1]], n = 1))
      # Download day's ENP file
      err <- try (download.file(paste0("ftp://ftpint.usgs.gov/from_pub/er/enp/", enp_file), paste0("./enp/", enp_file)))
      
      if (inherits(err, "try-error") | inherits(err2, "try-error"))
        report <- paste0(report, "ENP input file _NOT_ downloaded for ", format(days[2] - i, "%m/%d/%Y"), ".\n")
    }
    if (!length(enp_file) | !file.exists(paste0("./enp/", enp_file)) | !file.info(paste0("./enp/", enp_file))$size) {
      report <- paste0(report, "ENP local input file _NOT_ found for ", format(days[2] - i, "%m/%d/%Y"), ".\n")
    } else {
      report <- paste0(report, "ENP file found for ", format(days[2] - i, "%m/%d/%Y"), ".\n")
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
      enp_tmp <- enp_tmp[enp_tmp$V2 %in% gages$station_name, ]
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
  # Report ENP gage count
  report <- paste0(report, "ENP gages present: ", length(unique(enp$V2)), ".\n\n")

  # Loop through expected ENP gages
  for (i in which(gages$operating_agency_id == 1))
    # Write ENP data to file
    if (length(enp$V3[enp$V2 == gages$station_name[i]])) {
      df <- data.frame("USNPS", gages$usgs_nwis_id[i], str_pad(gages$dd[i], 4), gages$param[i], "da", enp$V3[enp$V2 == gages$station_name[i]], "EST", as.numeric(enp$V5[enp$V2 == gages$station_name[i]]) + gages$conv[i], "9", "", "", "9")
      write.table(df, paste0("./output/data_uv_AQ", d, ".txt"), sep = "\t", quote = F, row.names = F, col.names = F, append = T)
    }

  # Initialize SFWMD data.frame
  sfwmd <- NULL
  # Loop through days
  for (i in d:0) {
    ## Look for local file, otherwise look for remote file
    sfwmd_file <- list.files("./sfwmd/", format(days[2] - i, "%Y%m%d"))
    if (!length(sfwmd_file)) {
      # Find day's SFWMD file name
      err2 <- try (sfwmd_file <- tail(strsplit(grep(paste0("sfwmd_", format(days[2] - i, "%Y%m%d"), "_0[56]"), strsplit(getURL("ftp://ftpint.usgs.gov/from_pub/er/eden/"), "\r*\n")[[1]], value=T), " ")[[1]], n = 1))
      # Download today's SFWMD file
      err <- try (download.file(paste0("ftp://ftpint.usgs.gov/from_pub/er/eden/", sfwmd_file), paste0("./sfwmd/", sfwmd_file)))
      if (inherits(err, "try-error") | inherits(err2, "try-error"))
        report <- paste0(report, "SFWMD input file _NOT_ downloaded for ", format(days[2] - i, "%m/%d/%Y"), ".\n")
    }
    if (!length(sfwmd_file) | !file.exists(paste0("./sfwmd/", sfwmd_file)) | !file.info(paste0("./sfwmd/", sfwmd_file))$size) {
      report <- paste0(report, "SFWMD local input file _NOT_ found for ", format(days[2] - i, "%m/%d/%Y"), ".\n")
    } else {
      report <- paste0(report, "SFWMD file found for ", format(days[2] - i, "%m/%d/%Y"), ".\n")
      # Read in SFWMD data
      sfwmd_tmp <- read.fwf(paste0("./sfwmd/", sfwmd_file), widths = c(11, 17, 13, 13, 10, 2), strip.white = T, colClasses = c(rep("character", 4), "numeric", "character"))
      # Create timestamp
      sfwmd_tmp$V3 <- paste0(substr(sfwmd_tmp$V3, 5, 6), "/", substr(sfwmd_tmp$V3, 7, 8), "/", substr(sfwmd_tmp$V3, 1, 4), " ", substr(sfwmd_tmp$V3, 9, 10), ":", substr(sfwmd_tmp$V3, 11, 12), ":00")
      # Filter unwanted gages
      sfwmd_tmp <- sfwmd_tmp[sfwmd_tmp$V1 %in% gages$station_name, ]
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
  # Report SFWMD gage count
  report <- paste0(report, "SFWMD gages present: ", length(unique(sfwmd$V1)), ".\n\n")

  # Loop through expected SFWMD gages
  for (i in which(gages$operating_agency_id == 2 | gages$operating_agency_id == 3))
    # Write SFWMD data to file
    if (length(sfwmd$V3[sfwmd$V1 == gages$station_name[i]])) {
      df <- data.frame("FL005", gages$usgs_nwis_id[i], paste0("   ", gages$dd[i]), gages$param[i], "da", sfwmd$V3[sfwmd$V1 == gages$station_name[i]], "EST", as.numeric(sfwmd$V5[sfwmd$V1 == gages$station_name[i]]) + gages$conv[i], "9", "", "", "9")
      write.table(df, paste0("./output/data_uv_AQ", d, ".txt"), sep = "\t", quote = F, row.names = F, col.names = F, append = T)
    }
  
  # Add table of daily gage counts to report
  for (j in 1:length(sfwmd_cnt)) {
    enp_cnt[j] <- length(which(enp$date_tm == range2[j]))
    sfwmd_cnt[j] <- length(which(sfwmd$date_tm == range2[j]))
  }
  tot <- sum(usgs_cnt, enp_cnt, sfwmd_cnt)
  report <- paste0(report, "Total number of hourly values in data file: ", tot, "\n")

  png(paste0("./reports/gage_count", d, ".png"), 862, 614, type = "quartz")
  plot(range2, usgs_cnt, type = "l", lwd = 3, ylim = range(usgs_cnt, enp_cnt, sfwmd_cnt, 0, na.rm = T), main = paste(as.Date(range2[length(range2)]), "ADAM input gage hourly values count"), xlab = "Date", ylab = "Gage values count")
  points(range2, usgs_cnt, pch = 16)
  lines(range2, enp_cnt, lwd = 3, col = "blue")
  points(range2, enp_cnt, pch = 16, col = "blue")
  lines(range2, sfwmd_cnt, lwd = 3, col = "red")
  points(range2, sfwmd_cnt, pch = 16, col = "red")
  abline(v = range2[which(as.POSIXlt(range2)$hour == 0)], lty = "dashed")
  legend("bottomleft", c("USGS", "ENP", "SFWMD"), col = c("black", "blue", "red"), lwd = 3, pch = 16)
  dev.off()
  
  err <- try (ftpUpload(paste0("./output/data_uv_AQ", d, ".txt"), paste0("ftp://ftpint.usgs.gov/private/nonvisible/er/data_uv_AQ", d, ".txt")))
  report <- if (inherits(err, "try-error")) paste0(report, "\ndata_uv_AQ", d, ".txt file NOT transferred for ", days[2]) else paste0(report, "\ndata_uv_AQ.txt file transferred for ", days[2])
  zip(paste0("./output/data_uv_AQ", d, ".txt.zip"), paste0("./output/data_uv_AQ", d, ".txt"), "-j9X")
  err <- try (write(report, paste0("./reports/report_", d, "_", format(Sys.Date(), "%Y%m%d"), ".txt")))
  
  ### System level commands may not work if local environment does not have sendmail installed!!
  to <- "bmccloskey@usgs.gov, mdpetkew@usgs.gov, matthews@usgs.gov, jmclark@usgs.gov, wbguimar@usgs.gov, dantolin@usgs.gov, bhuffman@usgs.gov, kjconlon@usgs.gov"
  system(paste0("(echo 'Subject: data_uv_AQ", d, ".txt ", d, "-day upload report
  ", report, "';uuencode ", "./output/data_uv_AQ", d, ".txt.zip data_uv_AQ", d, ".txt.zip;uuencode ", "./reports/gage_count", d, ".png gage_count", d, ".png) | /usr/sbin/sendmail ", to))
}
setwd("..")
