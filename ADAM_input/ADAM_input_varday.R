# -------------
# Bryan McCloskey
# bmccloskey@usgs.gov
# St. Petersburg Coastal and Marine Science Center
# US Geological Survey
#
# 07/11/2018
#--------------

### ALERT: This script depends on previously downloaded local ENP and SFWMD files! Does not download fresh copies!!

print("These libraries must be installed: RMySQL, RCurl, stringr")
# Required libraries. If not present, run:
# install.packages("RMySQL")
# install.packages("RCurl")
# install.packages("stringr")
library(RMySQL)
library(RCurl)
library(stringr)

try(setwd("./ADAM_input"), silent = T)
source("../usr_pwd.R")
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

# Number of days to pull data for -- default one-day and seven-day
num_days <- c(1, 7)

for (d in num_days) {
  # Initialize email report
  report <- ""
  header <- "agency_cd\tsite_no\tdd_nu\tparameter_cd\tUVTYPE\tdate_tm\tTZCD\tVALUE\tPRECISION\tREMARK\tFLAGS\tQA"
  write(header, paste0("./output/data_uv_AQ", d, ".txt"))
  days <- seq(Sys.Date() - d, Sys.Date(), "days")
  # 15-minute and 6-minute timestamps
  start <- strptime(paste(days[1], "00:00:00"), "%Y-%m-%d %H:%M:%S", "EST")
  end <- strptime(paste(days[length(days)], "23:54:00"), "%Y-%m-%d %H:%M:%S", "EST")
  range <- seq.POSIXt(start, end, "15 min")
  range <-sort(unique(c(range, seq.POSIXt(start, end, "6 min"))))
  # Yesterday's noon timestamp
  noon <- which(as.POSIXlt(range)$yday == as.POSIXlt(Sys.Date())$yday - 1 & as.POSIXlt(range)$hour == 12 & as.POSIXlt(range)$min == 0)

  ## Depends on local files! Does not download fresh copies!!
  enp_files <- sfwmd_files <- NULL
  for (i in 1:length(days)) {
    enp_files <- c(enp_files, list.files("./enp/", format(days[i], "%Y%m%d")))
    sfwmd_files <- c(sfwmd_files, list.files("./sfwmd/", format(days[i], "%Y%m%d")))
  }
  
  cnt <- 0
  usgs_gages <- dbGetQuery(con, "select station_name_web, usgs_nwis_id, dd, param, ts_id from station where operating_agency_id = 4 and (realtime = 1) and station_name_web != 'S150_T' order by station_name_web")
  usgs_gages <- rbind(usgs_gages, c("Shark_River_Acoustic", "252230081021300", 9, "00065", "171031"))
  for (i in 1:length(usgs_gages$station_name_web)) {
    # Generate AQUARIUS URLs; add 1 for DST end timestamps
    url <- paste0("https://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=", usgs_gages$usgs_nwis_id[i], "&startDT=", days[1], "&endDT=", days[length(days)], "&parameterCd=", usgs_gages$param[i], "&access=3")
    tmp <- try(read.table(url, header = T, sep = "\t", colClasses = "character"))
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
        if (any(apply(tmp, 1, function(x) any(grepl("_Eqp|_Dry|_Fld", x)))))
          tmp <- tmp[-which(apply(tmp, 1, function(x) any(grepl("_Eqp|_Dry|_Fld", x)))), ]
        # URLs with no returned data
        if (dim(tmp)[1] == 0 || tmp[, 1] == "") {
          report <- paste(report, "Gage", usgs_gages$station_name_web[i], "missing\n") }
        else {
          # Check presence of yesterday's noon timestamp; add to count
          if (length(which(tmp$datetime == range[noon]))) if (tmp[which(tmp$datetime == range[noon]), dd_col] != "") cnt <- cnt + 1
          report <- paste(report, "Gage", usgs_gages$station_name_web[i], "values:", length(tmp[, dd_col]), "\n")
          # Build data.frame of USGS data
          usgs <- data.frame("USGS", tmp$site_no, usgs_gages$dd[i], usgs_gages$param[i], "da", datetime = format(tmp$datetime, "%m/%d/%Y %H:%M:%S"), "EST", tmp[, dd_col], 9, 9, 9, 9)
          # Write USGS data out to file
          write.table(usgs[, 1:12], paste0("./output/data_uv_AQ", d, ".txt"), sep = "\t", quote = F, row.names = F, col.names = F, append = T)
        }
      }
    }
  }
  # Report USGS gage count
  report <- paste0(report, "USGS gages with ", range[noon], " values: ", cnt, ".\n")
  
  report <- paste0(report, "ENP input files used:\n", paste(enp_files, collapse = ", \n"), "\n")
  # Initialize ENP data.frame
  enp <- NULL
  # Loop through days
  for (i in 1:length(enp_files)) {
    # Read in ENP data
    enp_tmp <- read.table(paste0("./enp/", enp_files[i]), colClasses = c("NULL", "character", "character", "character"))
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
    enp_tmp$date_tm <- as.POSIXct(enp_tmp$V3, tz = "EST", format = "%m/%d/%Y %H:%M:%S")
    enp_tmp <- enp_tmp[enp_tmp$date_tm %in% range, ]
    # Merge dates together; keep most recent values
    if (is.null(enp)) enp <- enp_tmp else {
      enp <- merge(enp, enp_tmp, by = c("V2", "V3", "V4", "date_tm"), all = T)
      enp$V5.x[is.na(enp$V5.x) | (!is.na(enp$V5.y) & enp$V5.x != enp$V5.y)] <- enp$V5.y[is.na(enp$V5.x) | (!is.na(enp$V5.y) & enp$V5.x != enp$V5.y)]
      enp <- enp[,1:5]
      names(enp)[5] <- "V5"
    }
  }
  # Report yesterday's ENP noon timestamp count
  report <- paste0(report, "ENP gages with ", range[noon], " values: ", length(unique(enp$V2[which(enp$date_tm == range[noon])])), ".\n")
  # Loop through expected ENP gages
  for (i in which(db$operating_agency_id == 1))
    # Write ENP data to file
    if (length(enp$V3[enp$V2 == db$station_name[i]])) {
      report <- paste(report, "Gage", db$station_name[i], "values:", length(enp$V5[enp$V2 == db$station_name[i]]), "\n")
      df <- data.frame("USNPS", db$usgs_nwis_id[i], str_pad(db$dd[i], 4), db$param[i], "da", enp$V3[enp$V2 == db$station_name[i]], "EST", as.numeric(enp$V5[enp$V2 == db$station_name[i]]) + db$conv[i], "9", "", "", "9")
      write.table(df, paste0("./output/data_uv_AQ", d, ".txt"), sep = "\t", quote = F, row.names = F, col.names = F, append = T)
    }
  
  report <- paste0(report, "SFWMD input files used:\n", paste(sfwmd_files, collapse = ", \n"), "\n")
  # Initialize SFWMD data.frame
  sfwmd <- NULL
  # Loop through days
  for (i in 1:length(sfwmd_files)) {
    # Read in SFWMD data
    sfwmd_tmp <- read.fwf(paste0("./sfwmd/", sfwmd_files[i]), widths = c(11, 17, 13, 13, 10, 2), strip.white = T, colClasses = c("character", "character", "character", "character", "numeric", "character"))
    # Create timestamp
    sfwmd_tmp$V3 <- paste0(substr(sfwmd_tmp$V3, 5, 6), "/", substr(sfwmd_tmp$V3, 7, 8), "/", substr(sfwmd_tmp$V3, 1, 4), " ", substr(sfwmd_tmp$V3, 9, 10), ":", substr(sfwmd_tmp$V3, 11, 12), ":00")
    # Filter unwanted gages
    sfwmd_tmp <- sfwmd_tmp[sfwmd_tmp$V1 %in% db$station_name, ]
    # Format and filter timestamps
    sfwmd_tmp$date_tm <- as.POSIXct(sfwmd_tmp$V3, "EST", format = "%m/%d/%Y %H:%M:%S")
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
  # Report yesterday's SFWMD noon timestamp count
  report <- paste0(report, "SFWMD gages with ", range[noon], " values: ", length(unique(sfwmd$V1[which(sfwmd$date_tm == range[noon])])), ".\n")
  # Loop through expected SFWMD gages
  for (i in which(db$operating_agency_id == 2 | db$operating_agency_id == 3))
    # Write SFWMD data to file
    if (length(sfwmd$V3[sfwmd$V1 == db$station_name[i]])) {
      report <- paste(report, "Gage", db$station_name[i], "values:", length(sfwmd$V5[sfwmd$V1 == db$station_name[i]]), "\n")
      df <- data.frame("FL005", db$usgs_nwis_id[i], paste0("   ", db$dd[i]), db$param[i], "da", sfwmd$V3[sfwmd$V1 == db$station_name[i]], "EST", as.numeric(sfwmd$V5[sfwmd$V1 == db$station_name[i]]) + db$conv[i], "9", "", "", "9")
      write.table(df, paste0("./output/data_uv_AQ", d, ".txt"), sep = "\t", quote = F, row.names = F, col.names = F, append = T)
    }

  err <- try(ftpUpload(paste0("./output/data_uv_AQ", d, ".txt"), paste0("ftp://ftpint.usgs.gov/private/nonvisible/er/data_uv_AQ", d, ".txt")))
  if (inherits(err,"try-error")) report <- paste0(report, "\ndata_uv_AQ", d, ".txt file NOT transferred to /private/nonvisible/er") else report <- paste0(report, "\ndata_uv_AQ", d, ".txt file transferred to /private/nonvisible/er")

  err <- try(ftpUpload(paste0("./output/data_uv_AQ", d, ".txt"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/bmccloskey/data_uv_AQ", d, ".txt")))
  if (inherits(err,"try-error")) report <- paste0(report, "\ndata_uv_AQ", d, ".txt file NOT transferred to /pub/er/fl/st.petersburg/bmccloskey") else report <- paste0(report, "\ndata_uv_AQ", d, ".txt file transferred to /pub/er/fl/st.petersburg/bmccloskey")
  zip(paste0("./output/data_uv_AQ", d, ".txt.zip"), paste0("./output/data_uv_AQ", d, ".txt"))
  err <- try(write(report, paste0("./reports/report_", format(Sys.Date(), "%Y%m%d"), "_", d, "days.txt")))
  
  ### System level commands may not work if local environment does not have sendmail installed!!
  to <- "bmccloskey@usgs.gov, mdpetkew@usgs.gov, matthews@usgs.gov, jmclark@usgs.gov, wbguimar@usgs.gov, dantolin@usgs.gov, bhuffman@usgs.gov"
  system(paste0("echo 'Subject: data_uv-AQ-extended-", d, "day upload report\n
  ", report, "' | /usr/sbin/sendmail ", to))
}
