# -------------
# Bryan McCloskey
# bmccloskey@usgs.gov
# St. Petersburg Coastal and Marine Science Center
# US Geological Survey
#
# 08/17/2018
#--------------

print("These libraries must be installed: RMySQL, RCurl, stringr")
# Required libraries. If not present, run:
# install.packages(c("RMySQL", "RCurl", "stringr"))
library (RMySQL)
library (RCurl)
library (stringr)

try (setwd("./ADAM_input"), silent = T)
source ("../usr_pwd.R")
# Connect to database, list of gages for which to acquire data
con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "eden", host = "igsafpesgsz03.er.usgs.gov")
# Quarterly and annual: add non-realtime gages
db <- dbGetQuery(con, "select station_name, operating_agency_id, usgs_nwis_id, dd, param, case when vertical_datum_id = 2 then vertical_conversion else 0 end as conv from station, station_datum where station.station_id = station_datum.station_id and (realtime = 1 or station_name = '3B-SE+' or station_name = 'S141@H' or station_name = 'S141@T') group by station_name order by operating_agency_id, station_name")

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
# Table of gages by agency
tbl <- matrix(0, 3, 1)
header <- "agency_cd\tsite_no\tdd_nu\tparameter_cd\tUVTYPE\tdate_tm\tTZCD\tVALUE\tPRECISION\tREMARK\tFLAGS\tQA"
qtr <- quarters(Sys.Date() - 90)
qtr <- switch(qtr, "Q4" = "Q1", "Q1" = "Q2", "Q2" = "Q3", "Q3" = "Q4")
yr <- format(Sys.Date() - 90, "%Y")
if (qtr == "Q1") yr <- as.numeric(yr) + 1
outfile <- paste0("./output/data_uv_WY", yr, qtr, ".txt")
write(header, outfile)
# Manually calculate first and last day of quarter
days <- c(as.Date("2020-07-01"), as.Date("2020-09-30"))
# range: include all of final day for quarterly/annual
start <- strptime(paste(days[1], "00:00:00"), "%Y-%m-%d %H:%M:%S", tz = "EST")
end <- strptime(paste(days[2], "23:54:00"), "%Y-%m-%d %H:%M:%S", tz = "EST")
range <- seq.POSIXt(start, end, by = "1 hour")

# Initialize counter variables
cnt <- 0
# add realtime = 2 for quarterly/annual
usgs_gages <- dbGetQuery(con, "select station_name_web, usgs_nwis_id, dd, param, ts_id from station where operating_agency_id = 4 and (realtime = 1 or realtime = 2) and station_name_web != 'S150_T' order by station_name_web")
usgs_gages <- rbind(usgs_gages, c("Shark_River_Acoustic", "252230081021300", 9, "00065", "171031"))
for (i in 1:length(usgs_gages$station_name_web)) {
  # Generate AQUARIUS URLs; add 1 for DST end timestamps
  url <- paste0("https://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=", usgs_gages$usgs_nwis_id[i], "&startDT=", days[1], "&endDT=", days[2], "&parameterCd=", usgs_gages$param[i], "&access=3")
  tmp <- try(read.table(url, header = T, sep = "\t", colClasses = "character"), silent = T)
  if (inherits(tmp, "try-error")) {
    report <- paste0(report, "USGS data _NOT_ downloaded for ", usgs_gages$station_name_web[i], "\n")
  } else {
    # Remove data descriptor header row
    tmp <- tmp[2:dim(tmp)[1], ]
    # Remove rows with "_Eqp" or "_Dry" flags
    if (any(apply(tmp, 1, function(x) any(grepl("_Eqp|_Dry|_Fld", x)))))
      tmp <- tmp[-which(apply(tmp, 1, function(x) any(grepl("_Eqp|_Dry|_Fld", x)))), ]
    # URLs with no returned data
    if (dim(tmp)[1] == 0 || tmp[, 1] == "") {
      report <- paste0(report, " Gage ", usgs_gages$station_name_web[i], " missing\n")
    } else {
      # Convert timestamps; shift DST
      tmp$datetime <- as.POSIXct(tmp$datetime, tz = "EST", format = "%Y-%m-%d %H:%M")
      tmp$datetime <- as.POSIXct(ifelse(tmp$tz_cd == "EDT", tmp$datetime - 3600, tmp$datetime), tz = "EST", origin = "1970-01-01")
      tmp <- tmp[tmp$datetime %in% range, ]
      # Find the column that contains the data
      dd_col <- if (!is.na(usgs_gages$ts_id[i])) which(grepl(usgs_gages$ts_id[i], names(tmp)))[1] else which(grepl(usgs_gages$param[i], names(tmp)))[1]
      report <- paste(report, "Gage", usgs_gages$station_name_web[i], "values:", length(tmp[, dd_col]), "\n")
      # Build data.frame of USGS data
      usgs <- data.frame("USGS", tmp$site_no, usgs_gages$dd[i], usgs_gages$param[i], "da", datetime = format(tmp$datetime, "%m/%d/%Y %H:%M:%S"), "EST", tmp[, dd_col], 9, 9, 9, 9)
      # Counts of gages with values on first day of range for report
      if (length(which(as.Date(usgs$datetime, format="%m/%d/%Y %H:%M:%S") == days[1]))) cnt <- cnt + 1
      # Write USGS data out to file
      write.table(usgs, outfile, sep="\t", quote=F, row.names=F, col.names=F, append=T)
    }
  }
}
# Report USGS gage count
report <- paste0(report, "USGS gages with ", days[1], " values: ", cnt, ".\n")

# Enter filenames to download quarterly/annual file to local working directory
enp_file <- "enp_20201117_1141"
#err <- try(download.file(paste0("ftp://ftpint.usgs.gov/from_pub/er/enp/", enp_file), paste0("./enp/", enp_file)), silent = T)
if (inherits(err, "try-error") | !file.exists(paste0("./enp/", enp_file)) | !file.info(paste0("./enp/", enp_file))$size) {
  report <- paste0(report, "ENP quarterly input file _NOT_ downloaded.\n")
} else {
  report <- paste0(report, "ENP quarterly file downloaded.\n")
  # Read in ENP data
  enp <- read.table(paste0("./enp/", enp_file), colClasses=c("NULL", rep("character", 3)))
  # Create timestamp
  enp$V3 <- paste0(substr(enp$V3, 5, 6), "/", substr(enp$V3, 7, 8), "/", substr(enp$V3, 1, 4), " ", substr(enp$V4, 3, 4), ":", substr(enp$V4, 5, 6), ":00")
  # Extract data values
  enp$V5 <- sapply(strsplit(enp$V4, "/"), "[", 4)
  # Extract data type
  enp$V4 <- sapply(strsplit(enp$V4, "/"), "[", 2)
  # Filter wanted data types
  enp <- enp[enp$V4 %in% c("HH", "DWM", "STAGE", "STM"), ]
  # Filter unwanted gages
  enp <- enp[enp$V2 %in% db$station_name, ]
  # Filter NA values
  enp <- enp[!is.na(enp$V5), ]
  # Format and filter timestamps
  enp$date_tm <- as.POSIXct(enp$V3, tz="EST", format="%m/%d/%Y %H:%M:%S")
  enp <- enp[enp$date_tm %in% range, ]
}
# Loop through expected ENP gages
for (i in which(db$operating_agency_id == 1))
  # Write ENP data to file
  if (length(enp$V3[enp$V2 == db$station_name[i]])) {
    report <- paste(report, "Gage", db$station_name[i], "values:", length(enp$V5[enp$V2 == db$station_name[i]]), "\n")
    write.table(data.frame("USNPS", db$usgs_nwis_id[i], str_pad(db$dd[i], 4), db$param[i], "da", format(enp$date_tm[enp$V2 == db$station_name[i]], "%m/%d/%Y %H:%M:%S"), "EST", as.numeric(enp$V5[enp$V2 == db$station_name[i]]) + db$conv[i], "9", "", "", "9"), outfile, sep="\t", quote=F, row.names=F, col.names=F, append=T)
  } else report <- paste0(report, " Gage ", db$station_name[i], " missing\n")
# Report first day of quarter's ENP gage count
report <- paste0(report, "ENP gages with ", days[1], " values: ", length(which(as.POSIXlt(enp$date_tm)$min == 0 & as.POSIXlt(enp$date_tm)$hour == 0 & as.Date(enp$date_tm) == days[1])), ".\n")

sfwmd_file <- "sfwmd_qtr_20201115_0627"
#err <- try(download.file(paste0("ftp://ftpint.usgs.gov/from_pub/er/eden/", sfwmd_file), paste0("./sfwmd/", sfwmd_file)), silent = T)
if (inherits(err, "try-error") | !file.exists(paste0("./sfwmd/", sfwmd_file)) | !file.info(paste0("./sfwmd/", sfwmd_file))$size) {
  report <- paste0(report, "SFWMD quarterly input file _NOT_ downloaded.\n")
} else {
  report <- paste0(report, "SFWMD quarterly file downloaded.\n")
  # Read in SFWMD data
  sfwmd <- read.fwf(paste0("./sfwmd/", sfwmd_file), widths=c(11, 17, 13, 13, 10, 2), strip.white=T, colClasses=c(rep("character", 4), "numeric", "character"))
  # Create timestamp
  sfwmd$V3 <- paste0(substr(sfwmd$V3, 5, 6), "/", substr(sfwmd$V3, 7, 8), "/", substr(sfwmd$V3, 1, 4), " ", substr(sfwmd$V3, 9, 10), ":", substr(sfwmd$V3, 11, 12), ":00")
  # Filter unwanted gages
  sfwmd <- sfwmd[sfwmd$V1 %in% db$station_name, ]
  # Format and filter timestamps
  sfwmd$date_tm <- as.POSIXct(sfwmd$V3, tz = "EST", format = "%m/%d/%Y %H:%M:%S")
  sfwmd <- sfwmd[sfwmd$date_tm %in% range, ]
  # Filter flagged timestamps
  sfwmd <- sfwmd[which(sfwmd$V6 != "<" & sfwmd$V6 != "?"), ]
  # Filter NA values
  sfwmd <- sfwmd[!is.na(sfwmd$V5), ]
}
# Loop through expected SFWMD gages
for (i in which(db$operating_agency_id == 2 | db$operating_agency_id == 3))
  # Write SFWMD data to file
  if(length(sfwmd$V3[sfwmd$V1 == db$station_name[i]])) {
    # quarterly/annual, add 1st hour to data
    if (as.Date(sfwmd$date_tm[sfwmd$V1 == db$station_name[i]][1]) == days[1] & as.POSIXlt(sfwmd$date_tm[sfwmd$V1 == db$station_name[i]][1])$hour == 1) {
      write.table(data.frame("FL005", db$usgs_nwis_id[i], paste0("   ", db$dd[i]), db$param[i], "da", paste0(substring(sfwmd$V3[sfwmd$V1 == db$station_name[i]][1], 1, 11), "00:00:00"), "EST", as.numeric(sfwmd$V5[sfwmd$V1 == db$station_name[i]][1]) + db$conv[i], "9", "", "", "9"), outfile, sep="\t", quote=F, row.names=F, col.names=F, append=T)
      report <- paste0(report, "Initial midnight values added to SWFMD gage ", db$station_name[i], "\n")
    }
    report <- paste(report, "Gage", db$station_name[i], "values:", length(sfwmd$V5[sfwmd$V1 == db$station_name[i]]), "\n")
    write.table(data.frame("FL005", db$usgs_nwis_id[i], paste0("   ", db$dd[i]), db$param[i], "da", sfwmd$V3[sfwmd$V1 == db$station_name[i]], "EST", as.numeric(sfwmd$V5[sfwmd$V1 == db$station_name[i]]) + db$conv[i], "9", "", "", "9"), outfile, sep="\t", quote=F, row.names=F, col.names=F, append=T)
  } else report <- paste0(report, " Gage ", db$station_name[i], " missing\n")
# Report first day of quarter's SFWMD gage count
report <- paste0(report, "SFWMD gages with ", days[1], " values: ", length(which(as.POSIXlt(sfwmd$date_tm)$min == 0 & as.POSIXlt(sfwmd$date_tm)$hour == 1 & as.Date(sfwmd$date_tm) == days[1])), ".\n")

s141ht <- dbGetQuery(con, "select station_name_web, operating_agency_id, usgs_nwis_id, dd, param, case when vertical_datum_id = 2 then vertical_conversion else 0 end as conv from station, station_datum where station.station_id = station_datum.station_id and (station_name = 'S141@H' or station_name = 'S141@T') group by station_name order by operating_agency_id, station_name")
s141ht_dat <- read.csv("./sfwmd/S141s_WY2020Q1.csv", colClasses=c("character", "character", "numeric", "NULL"))
s141ht_dat$Date <- as.POSIXct(s141ht_dat$Date, tz = "EST", format = "%m/%d/%Y %H:%M")
s141ht_dat <- s141ht_dat[s141ht_dat$Date %in% range, ]
for (i in 1:length(s141ht$station_name)) {
  report <- paste(report, "Gage", s141ht$station_name[i], "values:", length(s141ht_dat$Stage[s141ht_dat$Site == s141ht$station_name[i]]), "\n")
  write.table(data.frame("FL005", s141ht$usgs_nwis_id[i], paste0("   ", s141ht$dd[i]), s141ht$param[i], "da", format(s141ht_dat$Date[s141ht_dat$Site == s141ht$station_name[i]], "%m/%d/%Y %H:%M:%S"), "EST", as.numeric(s141ht_dat$Stage[s141ht_dat$Site == s141ht$station_name[i]]) + s141ht$conv[i], "9", "", "", "9"), outfile, sep="\t", quote=F, row.names=F, col.names=F, append=T)
}

### System level commands may not work if local environment does not have curl, zip, or sendmail installed
### May need to transfer/perform manually instead!!
err <- try(system(paste0("curl -T ", outfile, " ftp://ftpint.usgs.gov/private/nonvisible/er/data_uv_", qtr, yr, ".txt")), silent = T)
report <- if (inherits(err, "try-error")) paste0(report, "\n", outfile, " file NOT transferred") else paste0(report, "\n", outfile, " file transferred")
zip(paste0(outfile, ".zip"), outfile)

to <- "bmccloskey@usgs.gov, mdpetkew@usgs.gov, matthews@usgs.gov, jmclark@usgs.gov, wbguimar@usgs.gov, dantolin@usgs.gov, bhuffman@usgs.gov, kjconlon@usgs.gov"
system(paste0("(echo 'Subject: ", outfile, " upload report\n
", report, "';uuencode ", outfile, ".zip ", outfile, ".zip;uuencode ./reports/gage_count.png gage_count.png) | /usr/sbin/sendmail ", to))

gages <- dbGetQuery(con, "select station_name_web, station_name, operating_agency_id, usgs_nwis_id, param, ts_id, vertical_datum_id from station where station_name_web != 'Alligator_Creek' and station_name_web != 'Barron_River' and station_name_web != 'East_Side_Creek' and station_name_web != 'G-3777' and station_name_web != 'Manatee_Bay_Creek' and station_name_web != 'Raulerson_Brothers_Canal' group by station_name order by station_name_web")
write("", "./output/marryup.txt", sep="\t")
usgs_gages <- gages[gages$operating_agency_id == 4, ]
for (i in 1:length(usgs_gages$station_name_web)) {
  url_s <- paste0("https://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=", usgs_gages$usgs_nwis_id[i], "&startDT=", days[1], "&endDT=", days[1], "&parameterCd=", usgs_gages$param[i], "&access=3")
  url_e <- paste0("https://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=", usgs_gages$usgs_nwis_id[i], "&startDT=", days[2], "&endDT=", days[2] + 1, "&parameterCd=", usgs_gages$param[i], "&access=3")
  tmp <- try(read.table(url_s, header = T, sep = "\t", colClasses = "character"), silent = T)
  if (!inherits(tmp, "try-error")) {
    tmp <- tmp[2:dim(tmp)[1], ]
    if (any(apply(tmp, 1, function(x) any(grepl("_Eqp|_Dry|_Fld", x)))))
      tmp <- tmp[-which(apply(tmp, 1, function(x) any(grepl("_Eqp|_Dry|_Fld", x)))), ]
    if (dim(tmp)[1] != 0 && tmp[, 1] != "") {
      tmp$datetime <- as.POSIXct(tmp$datetime, tz = "EST", format = "%Y-%m-%d %H:%M")
      tmp$datetime <- as.POSIXct(ifelse(tmp$tz_cd == "EDT", tmp$datetime - 3600, tmp$datetime), tz = "EST", origin = "1970-01-01")
      tmp <- tmp[which(tmp$datetime == range[1]), ]
      if (dim(tmp)[1] != 0 && tmp[, 1] != "") {
        dd_col <- if (!is.na(usgs_gages$ts_id[i])) which(grepl(usgs_gages$ts_id[i], names(tmp)))[1] else which(grepl(usgs_gages$param[i], names(tmp)))[1]
        usgs_pre <- dbGetQuery(con, paste0("select datetime, `stage_", usgs_gages$station_name_web[i], "` as st from stage where datetime = '", range[1] - 3600, "'"))
        usgs_pre <- data.frame("USGS", usgs_gages$station_name_web[i], range[1] - 3600, usgs_pre$st)
        usgs_s <- data.frame("USGS", usgs_gages$station_name_web[i], range[1], tmp[, dd_col])
        write.table(usgs_pre, "./output/marryup.txt", sep="\t", quote=F, row.names=F, col.names=F, append=T)
        write.table(usgs_s, "./output/marryup.txt", sep="\t", quote=F, row.names=F, col.names=F, append=T)
      }
    }
  }
  tmp <- try(read.table(url_e, header = T, sep = "\t", colClasses = "character"), silent = T)
  if (!inherits(tmp, "try-error")) {
    tmp <- tmp[2:dim(tmp)[1], ]
    if (any(apply(tmp, 1, function(x) any(grepl("_Eqp|_Dry|_Fld", x)))))
      tmp <- tmp[-which(apply(tmp, 1, function(x) any(grepl("_Eqp|_Dry|_Fld", x)))), ]
    if (dim(tmp)[1] != 0 && tmp[, 1] != "") {
      tmp$datetime <- as.POSIXct(tmp$datetime, tz = "EST", format = "%Y-%m-%d %H:%M")
      tmp$datetime <- as.POSIXct(ifelse(tmp$tz_cd == "EDT", tmp$datetime - 3600, tmp$datetime), tz = "EST", origin = "1970-01-01")
      tmp <- tmp[which(tmp$datetime == range[length(range)]), ]
      if (dim(tmp)[1] != 0 && tmp[, 1] != "") {
        dd_col <- if (!is.na(usgs_gages$ts_id[i])) which(grepl(usgs_gages$ts_id[i], names(tmp)))[1] else which(grepl(usgs_gages$param[i], names(tmp)))[1]
        usgs_post <- dbGetQuery(con, paste0("select datetime, `stage_", usgs_gages$station_name_web[i], "` as st from stage where datetime = '", range[length(range)] + 3600, "'"))
        usgs_post <- data.frame("USGS", usgs_gages$station_name_web[i], range[length(range)] + 3600, usgs_post$st)
        usgs_e <- data.frame("USGS", usgs_gages$station_name_web[i], range[length(range)], tmp[, dd_col])
        write.table(usgs_e, "./output/marryup.txt", sep="\t", quote=F, row.names=F, col.names=F, append=T)
        write.table(usgs_post, "./output/marryup.txt", sep="\t", quote=F, row.names=F, col.names=F, append=T)
      }
    }
  }
}
enp_gages <- gages[gages$operating_agency_id == 1, ]
for (i in 1:length(enp_gages$station_name_web)) {
  conv <- dbGetQuery(con, paste0("select vertical_conversion as conv from station, station_datum where station.station_id = station_datum.station_id and station_name_web = '", enp_gages$station_name_web[i], "'"))
  if (enp_gages$vertical_datum_id[i] == 3) conv$conv <- 0
  enp_pre <- dbGetQuery(con, paste0("select datetime, `stage_", enp_gages$station_name_web[i], "` as st from stage where datetime = '", range[1] - 3600, "'"))
  enp_pre <- data.frame("ENP", enp_gages$station_name_web[i], range[1] - 3600, enp_pre$st)
  write.table(enp_pre, "./output/marryup.txt", sep="\t", quote=F, row.names=F, col.names=F, append=T)
  if (length(enp$V5[which(enp$V2 == enp_gages$station_name_web[i] & enp$date_tm == range[1])])) {
    enp_s <- data.frame("ENP", enp_gages$station_name_web[i], range[1], as.numeric(enp$V5[which(enp$V2 == enp_gages$station_name_web[i] & enp$date_tm == range[1])]) + conv$conv)
    write.table(enp_s, "./output/marryup.txt", sep="\t", quote=F, row.names=F, col.names=F, append=T)
  }
  enp_post <- dbGetQuery(con, paste0("select datetime, `stage_", enp_gages$station_name_web[i], "` as st from stage where datetime = '", range[length(range)] + 3600, "'"))
  enp_post <- data.frame("ENP", enp_gages$station_name_web[i], range[length(range)] + 3600, enp_post$st)
  if (length(enp$V5[which(enp$V2 == enp_gages$station_name_web[i] & enp$date_tm == range[length(range)])])) {
    enp_e <- data.frame("ENP", enp_gages$station_name_web[i], range[length(range)], as.numeric(enp$V5[which(enp$V2 == enp_gages$station_name_web[i] & enp$date_tm == range[length(range)])]) + conv$conv)
    write.table(enp_e, "./output/marryup.txt", sep="\t", quote=F, row.names=F, col.names=F, append=T)
  }
  write.table(enp_post, "./output/marryup.txt", sep="\t", quote=F, row.names=F, col.names=F, append=T)
}
sfwmd_gages <- gages[gages$operating_agency_id == 2 | gages$operating_agency_id == 3, ]
for (i in 1:length(sfwmd_gages$station_name)) {
  conv <- dbGetQuery(con, paste0("select vertical_conversion as conv from station, station_datum where station.station_id = station_datum.station_id and station_name = '", sfwmd_gages$station_name[i], "'"))
  if (sfwmd_gages$vertical_datum_id[i] == 3) conv$conv <- 0
  sfwmd_pre <- dbGetQuery(con, paste0("select datetime, `stage_", sfwmd_gages$station_name_web[i], "` as st from stage where datetime = '", range[1] - 3600, "'"))
  sfwmd_pre <- data.frame("SFWMD", sfwmd_gages$station_name_web[i], range[1] - 3600, sfwmd_pre$st)
  write.table(sfwmd_pre, "./output/marryup.txt", sep="\t", quote=F, row.names=F, col.names=F, append=T)
  if (length(sfwmd$V5[which(sfwmd$V1 == sfwmd_gages$station_name[i] & sfwmd$date_tm == range[2])])) {
    sfwmd_s <- data.frame("SFWMD", sfwmd_gages$station_name_web[i], range[2], as.numeric(sfwmd$V5[which(sfwmd$V1 == sfwmd_gages$station_name[i] & sfwmd$date_tm == range[2])]) + conv$conv)
    write.table(sfwmd_s, "./output/marryup.txt", sep="\t", quote=F, row.names=F, col.names=F, append=T)
  }
  sfwmd_post <- dbGetQuery(con, paste0("select datetime, `stage_", sfwmd_gages$station_name_web[i], "` as st from stage where datetime = '", range[length(range)] + 3600, "'"))
  sfwmd_post <- data.frame("SFWMD", sfwmd_gages$station_name_web[i], range[length(range)] + 3600, sfwmd_post$st)
  if (length(sfwmd$V5[which(sfwmd$V1 == sfwmd_gages$station_name[i] & sfwmd$date_tm == range[length(range)])])) {
    sfwmd_e <- data.frame("SFWMD", sfwmd_gages$station_name_web[i], range[length(range)], as.numeric(sfwmd$V5[which(sfwmd$V1 == sfwmd_gages$station_name[i] & sfwmd$date_tm == range[length(range)])]) + conv$conv)
    write.table(sfwmd_e, "./output/marryup.txt", sep="\t", quote=F, row.names=F, col.names=F, append=T)
  }
  write.table(sfwmd_post, "./output/marryup.txt", sep="\t", quote=F, row.names=F, col.names=F, append=T)
}

setwd("..")
