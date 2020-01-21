# -------------
# Bryan McCloskey
# bmccloskey@usgs.gov
# St. Petersburg Coastal and Marine Science Center
# US Geological Survey
#
# 07/16/2018
#--------------

print("These libraries must be installed: RMySQL, RCurl, CSI, zoo")
# Required libraries. If not present, run:
# install.packages(c("RMySQL", "RCurl", "zoo"))
# devtools::install_github("USGS-R/CSI")
library (RMySQL)
library (RCurl)
library (CSI)
library (zoo)

try (setwd("./coastal_EDEN"), silent = T)
source ("../admin_pwd.R")
# Connect to database, list of gages for which to acquire data
con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "eden_new", host = "stpweb1-dmz.er.usgs.gov")
db <- dbGetQuery(con, "SELECT station_name_web, coastal, usgs_nwis_id FROM station WHERE coastal IS NOT NULL ORDER BY station_name_web")
params <- data.frame(matrix(ncol = 3, nrow = 0))
for (i in 1:length(db$station_name_web)) {
  tmp <- unlist(strsplit(db$coastal[i], ';'))
  for (j in 1:length(tmp)) {
    tmp2 <- substr(tmp[j], 1, 2)
    column <- paste(db$station_name_web[i], switch (tmp2, st = 'stage', te = 'temperature', sa = 'salinity'), sep = '_')
    code <- switch (tmp2, st = '00065', te = '00010', sa = '00480')
    params[dim(params)[1] + 1, ] <- c(column, db$usgs_nwis_id[i], code)
  }
}
names(params) <- c('column', 'id', 'param')
days <- c(Sys.Date() - 14, Sys.Date())
# 15-minute timestamps
start <- strptime(paste(days[1], "00:00:00"), "%Y-%m-%d %H:%M:%S", tz = "EST")
end <- strptime(paste(days[2], "07:00:00"), "%Y-%m-%d %H:%M:%S", tz = "EST")
range <- seq.POSIXt(start, end, by = "15 min")
v <- data.frame(datetime = range)

# Retrieve data and build input data.frame
report <- ""
options(stringsAsFactors = F) # needed to keep strings from becoming factors on merge
for (i in 1:length(params$column)) {
  # Generate AQUARIUS URLs; add 1 for DST end timestamps
  url <- paste0("https://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=", params$id[i], "&startDT=", days[1], "&endDT=", days[2], "&parameterCd=", params$param[i], "&access=3")
  tmp <- try (read.table(url, header = T, sep = "\t", colClasses = "character"))
  if (inherits(tmp, "try-error")) {
    report <- paste0(report, "Data _NOT_ downloaded for ", params$column[i], "\n")
  } else {
    # Remove data descriptor header row
    tmp <- tmp[2:dim(tmp)[1], ]
    # URLs with no returned data
    if (tmp[, 1] == "") {
      report <- paste0(report, params$column[i], " missing\n")
    } else {
      # Remove rows with "_Eqp" flag
      if (length(grep("_Eqp", tmp[, 5])) != 0)
        tmp <- tmp[-grep("_Eqp", tmp[, 5]), ]
      # Convert timestamps; shift DST
      tmp$datetime <- as.POSIXct(tmp$datetime, tz = "EST", format = "%Y-%m-%d %H:%M")
      tmp$datetime <- as.POSIXct(ifelse(tmp$tz_cd == "EDT", tmp$datetime - 3600, tmp$datetime), tz = "EST", origin = "1970-01-01")
      tmp <- tmp[tmp$datetime %in% range, ]
      # Find the column that contains the data
      dd_col <- 5
      v <- cbind(v, merge(range, tmp, by.x = 1, by.y = "datetime", all.x = T)[, dd_col])
      names(v)[dim(v)[2]] <- params$column[i]
      max <- dbGetQuery(con, paste0("select max(`", params$column[i], "`) from coastal"))
      min <- dbGetQuery(con, paste0("select min(`", params$column[i], "`) from coastal"))
      if (any(as.numeric(v[, params$column[i]]) > max, na.rm = T)) report <- paste(report, "Values found for", params$column[i], "greater than historical maximum of", max, "\n")
      if (any(as.numeric(v[, params$column[i]]) < min, na.rm = T)) report <- paste(report, "Values found for", params$column[i], "less than historical minimum of", min, "\n")
      daily_mean <- dbGetQuery(con, paste0("select avg(`", params$column[i], "`) from coastal group by date(datetime)"))[, 1]
      max_rise <- max(daily_mean[2:length(daily_mean)] - daily_mean[1:(length(daily_mean) - 1)], na.rm = T)
      max_fall <- max(daily_mean[1:(length(daily_mean) - 1)] - daily_mean[2:length(daily_mean)], na.rm = T)
      z <- read.zoo(v[, c("datetime", params$column[i])], tz = "EST")
      z <- aggregate(z, as.Date, function (x) mean(as.numeric(x), na.rm = T))
      if (any(as.numeric(z[2:15]) - as.numeric(z[1:14]) > max_rise, na.rm = T)) report <- paste(report, "Values found for", params$column[i], "greater than historical maximum rise of", max_rise, "per day\n")
      if (any(as.numeric(z[1:14]) - as.numeric(z[2:15]) > max_fall, na.rm = T)) report <- paste(report, "Values found for", params$column[i], "greater than historical maximum fall of", max_fall, "per day\n")
    }
  }
}

# Upload data.frame to CoastalEDENdb
for (i in 1:dim(v)[1]) {
  ### Caution! 'replace' erases non-specified columns!! Use 'update' if modifying subsets of columns
  query <- paste0("replace into coastal set datetime = '", v$datetime[i], "'")
  for (j in 2:dim(v)[2]) {
    # Quote both flags and numbers, not null
    if (is.na(v[i, j])) tmp <- "NULL" else tmp <- paste0("'", v[i, j], "'")
    query <- paste0(query, ", `", names(v)[j], "` = ", tmp)
  }
  # Upload timestamp row to database
  err <- try(dbSendQuery(con, query), T)
  if (inherits(err, "try-error")) report <- paste0(report, "\nCoastalEDENdb upload error for ", v$datetime[i], ": ", err)
} # End process and upload

# Create year+ plots of parameters, and thumbnails
for (i in 1:dim(params)[1]) {
  tmp <- unlist(strsplit(params$column[i], '_'))
  lab <- switch (tmp[length(tmp)], stage = 'Water-level elevation, feet NAVD88', temperature = 'Water column temperature, degrees Celsius', salinity = 'Salinity, PPT')

  db2 <- dbGetQuery(con, paste0("select date(datetime) as date, avg(`", params$column[i], "`) as avg from coastal where datetime >= ", as.numeric(format(Sys.Date(), "%Y")) - 3, format(Sys.Date(), "%m"), "01000000 group by date(datetime)"))
  db2$date <- as.POSIXct(db2$date, tz = "EST", format = "%Y-%m-%d")
  last_meas <- rev(which(!is.na(db2$avg)))[1]
  db3 <- data.frame(date = db2$date, avg = NA)
  for (j in 7:last_meas) db3$avg[j] <- mean(db2$avg[(j - 6):j], na.rm = T)
  jpeg(paste0("./images/", params$column[i], "_thumb.jpg"), width = 360, height = 150, pointsize = 2, quality = 100, type = "quartz")
  par(mar = c(0, 0, 0, 0) + .1)
  plot(db2$date, db2$avg, type = "n", xlab = "", ylab = "", ylim = range(db2$avg, na.rm = T), xaxt = "n", xaxt = "n", main = "")
  grid(nx = NA, ny = NULL, col = "black", lty = "dashed")
  abline(v = db2$date[which(format(db2$date, "%m") == "01" & format(db2$date, "%d") == "01")], lty = "dashed", col = "black")
  lines(db2$date, db2$avg, lwd = 2)
  points(db2$date[last_meas], db2$avg[last_meas], pch = 20)
  lines(db3$date, db3$avg, lwd = 2, col = "green")
  points(db3$date[last_meas], db3$avg[last_meas], pch = 20, col = "green")
  dev.off()
  jpeg(paste0("./images/", params$column[i], "_full.jpg"), width = 2400, height = 800, quality = 100, type = "quartz")
  plot(db2$date, db2$avg, type = "n", xlab = "Month of year", ylab = lab, ylim = range(db2$avg, na.rm = T), xaxt = "n", main = paste(c("Gage", tmp[-length(tmp)]), collapse = " "))
  axis(1, at = db2$date[which(format(db2$date, "%m") == "01" & format(db2$date, "%d") == "01")], labels = as.numeric(format(Sys.Date(), "%Y")) - 3:0, hadj = -1.5)
  grid(nx = NA, ny = NULL, col = "black", lty = "dashed")
  abline(v = db2$date[which(format(db2$date, "%m") == "01" & format(db2$date, "%d") == "01")], lty = "dashed", col = "black")
  lines(db2$date, db2$avg, lwd = 3)
  points(db2$date[last_meas], db2$avg[last_meas], pch = 20, cex = 3)
  lines(db3$date, db3$avg, lwd = 3, col = "green")
  points(db3$date[last_meas], db3$avg[last_meas], pch = 20, cex = 3, col = "green")
  text(db2$date[last_meas], db2$avg[last_meas], paste(round(db2$avg[last_meas], 2), "\n", format(db2$date[last_meas], "%m/%d/%Y"), sep = ""), pos = 3, cex = 1.25, font = 2, col = "gray35", offset = 2)
  text(db3$date[last_meas], db3$avg[last_meas], round(db3$avg[last_meas], 2), pos = 4, cex = 1.25, font = 2, col = "green3", offset = 1)
  legend("topleft", c(paste(as.numeric(format(Sys.Date(), "%Y")) - 3, "/", format(Sys.Date(), "%Y"), tmp[length(tmp)]), "Rolling seven-day average"), col = c("black", "green"), lty = 1, lwd = 3)
  dev.off()
  err <- try (ftpUpload(paste0("./images/", params$column[i], "_thumb.jpg"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/coastal_eden/thumbnails/", params$column[i], "_thumb.jpg"), .opts = list(forbid.reuse = 1)))
  if (inherits(err, "try-error")) report <- paste0(report, "\n", params$column[i], " thumbnail NOT transferred") else report <- paste0(report, "\n", params$column[i], " thumbnail transferred")
  err <- try (ftpUpload(paste0("./images/", params$column[i], "_full.jpg"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/coastal_eden/full/", params$column[i], "_full.jpg"), .opts = list(forbid.reuse = 1)))
  if (inherits(err, "try-error")) report <- paste0(report, "\n", params$column[i], " full-size NOT transferred") else report <- paste0(report, "\n", params$column[i], " full-sized transferred")
}

db <- dbGetQuery(con, "select station_name_web from station where coastal like '%sa%' order by station_name_web")
query <- "select date_format(datetime, '%Y') as Year, date_format(datetime, '%m') as Month"
for (j in 1:dim(db)[1])
  query <- paste0(query, ", avg(`", db$station_name_web[j], "_salinity`) as `", db$station_name_web[j], "`")
query <- paste(query, "from coastal group by Year, Month")
sal <- dbGetQuery(con, query)
#sal <- CSIinterp(sal)
csi <- CSIcalc(sal)
CSIstack(csi, "./csi/", T, F, F, leg = "bottom")
CSIwrite(csi, "./csi")
for (j in 1:dim(db)[1]) {
  err <- try (ftpUpload(paste0("./csi/", db$station_name_web[j], "_stacked_thumb.png"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/coastal_eden/csi_stacked/", db$station_name_web[j], "_stacked_thumb.png"), .opts = list(forbid.reuse = 1)))
  if (inherits(err, "try-error")) report <- paste0(report, "\n", db$station_name_web[j], " CSI thumbnail NOT transferred") else report <- paste0(report, "\n", db$station_name_web[j], " CSI thumbnail transferred")
  err <- try (ftpUpload(paste0("./csi/", db$station_name_web[j], "_stacked.png"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/coastal_eden/csi_stacked/", db$station_name_web[j], "_stacked.png"), .opts = list(forbid.reuse = 1)))
  if (inherits(err, "try-error")) report <- paste0(report, "\n", db$station_name_web[j], " CSI full-size NOT transferred") else report <- paste0(report, "\n", db$station_name_web[j], " CSI full-sized transferred")
  err <- try (ftpUpload(paste0("./csi/", db$station_name_web[j], ".csv"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/coastal_eden/csi_values/", db$station_name_web[j], ".csv"), .opts = list(forbid.reuse = 1)))
  if (inherits(err, "try-error")) report <- paste0(report, "\n", db$station_name_web[j], " CSI values NOT transferred") else report <- paste0(report, "\n", db$station_name_web[j], " CSI values transferred")
}

col <- c("tan4", "tan2", "darkolivegreen4", "lightblue", "skyblue3", "darkgreen")
for (j in 1:length(db$station_name_web)) {
  por <- dbGetQuery(con,paste0("select date(datetime) as date, avg(`", db$station_name_web[j], "_salinity`) as sal from coastal group by date"))
  por$date <- as.POSIXct(por$date, tz = "EST", format = "%Y-%m-%d")
  rng <- dbGetQuery(con, paste0("select min(date(datetime)) as start, max(date(datetime)) as end from coastal where `", db$station_name_web[j], "_salinity` is not null"))
  yr3 <- dbGetQuery(con, paste0("select date(datetime) as date, avg(`", db$station_name_web[j], "_salinity`) as sal, avg(`", db$station_name_web[j], "_temperature`) as temp from coastal where datetime >= ", as.numeric(format(Sys.Date(), "%Y")) - 3, format(Sys.Date(), "%m"), "01000000 group by date"))
  yr3$date <- as.Date(yr3$date, format = "%Y-%m-%d")
  win30 <- rep(NA, length(yr3$date))
  for (i in 30:length(win30))
    win30[i] <- mean(yr3$sal[(i-29):i], na.rm = T)
  qyear <- matrix(NA, length(yr3$date), 6); median <- matrix(NA, length(yr3$date), 1)
  for (i in 1:length(qyear[, 1])) {
    qyear[i, ] <- quantile(por$sal[as.POSIXlt(por$date)$mon == as.numeric(format(yr3$date[i], "%m")) - 1], c(0, .1, .25, .75, .9, 1), na.rm = T)
    median[i, ] <- quantile(por$sal[as.POSIXlt(por$date)$mon == as.numeric(format(yr3$date[i], "%m")) - 1], 0.5, na.rm = T)
  }
  median[as.POSIXlt(yr3$date)$mday == 1] <- NA
  jpeg(paste0("./duration_hydrographs/", db$station_name_web[j], "_salinity_thumb.jpg"), width = 360, height = 150, pointsize = 2, quality = 100, type = "quartz")
  par(mar = c(0, 0, 0, 0) + .1)
  plot(qyear[, 1], type = "n", xlab = "", ylab = "", ylim = range(qyear, na.rm = T), xaxt = "n", yaxt = "n", main = "", axes = F, frame.plot = T)
  for (i in 1:5) polygon(c(1:length(yr3$date), length(yr3$date):1), c(qyear[, i], rev(qyear[, i + 1])), col = col[i])
  lines(median, lwd = 2, col = "yellow")
  lines(yr3$sal, lwd = 3, col = "grey")
  lines(win30, lwd = 4, col = "black")
  par(new = T)
  plot(yr3$temp, type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd = 3, col = "red")
  dev.off()
  jpeg(paste0("./duration_hydrographs/", db$station_name_web[j], "_salinity.jpg"), width = 2400, height = 800, quality = 100, type = "quartz")
  par(mar = c(5, 4, 4, 5) + .1)
  plot(qyear[, 1], type = "n", xlab = "Month of year", ylab = "Salinity (PPT)", ylim = range(qyear, na.rm = T), xaxt = "n", main = paste(db$station_name_web[j], "three year salinity, 30 day moving window"))
  axis(1, at = which(as.POSIXlt(yr3$date)$mday == 1), labels = format(yr3$date[which(as.POSIXlt(yr3$date)$mday == 1)], "%b"), hadj = -1)
  axis(1, at = which(as.POSIXlt(yr3$date)$yday == 0), labels = format(yr3$date[which(as.POSIXlt(yr3$date)$yday == 0)], "%Y"), padj = 1)
  for (i in 1:5) polygon(c(1:length(yr3$date), length(yr3$date):1), c(qyear[, i], rev(qyear[, i + 1])), col = col[i])
  lines(median, lwd = 3, col = "yellow")
  lines(yr3$sal, lwd = 4, col = "grey")
  lines(win30, lwd = 5, col = "black")
  par(new = T)
  plot(yr3$temp, type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd = 4, col = "red")
  axis(4)
  mtext("Temperature (°C)", side = 4, line = 3)
  legend("topright", c("90% to Max.", "75% to 90%", "25% to 75%", "10% to 25%", "Min. to 10%"), fill = col[5:1], cex = 1.5, bty = "n", title = "Salinity bins")
  legend("topright", c("Salinity 30 day moving window", "Daily salinity values", "Daily temperature values", "Historic monthly mean salinity"), lwd = c(5, 4, 4, 4), col = c("black", "grey", "red", "yellow"), inset = c(.075, 0), cex = 1.5, bty = "n")
  legend("topleft", paste("Period of record:", rng$start, "to", rng$end), cex = 1.5, bty = "n")
  dev.off()
  ftpUpload(paste0("./duration_hydrographs/", db$station_name_web[j], "_salinity_thumb.jpg"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/coastal_eden/salinity_duration_hydrographs/", db$station_name_web[j], "_salinity_thumb.jpg"), .opts = list(forbid.reuse = 1))
  ftpUpload(paste0("./duration_hydrographs/", db$station_name_web[j], "_salinity.jpg"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/coastal_eden/salinity_duration_hydrographs/", db$station_name_web[j], "_salinity.jpg"), .opts = list(forbid.reuse = 1))
}

### System level commands may not work if local environment does not have sendmail installed!!
to <- "bmccloskey@usgs.gov, mdpetkew@usgs.gov"
system(paste0("echo 'Subject: CoastalEDENdb upload report
", report, "' | /usr/sbin/sendmail ", to))
setwd("..")
