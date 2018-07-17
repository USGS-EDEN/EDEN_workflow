# Required libraries. If not present, run:
# install.packages("RMySQL")
# devtools::install_github("bmccloskey/CSI")
library (RMySQL)
library (CSI)

### Set to containing directory of script!
setwd("~/Desktop/coastal_eden_git")
dir <- getwd()

# Connect to database, list of gages for which to acquire data
usr = "eve_write"
pword = "!eV3_r0ck5!"
con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "eden_new", host = "stpweb1-dmz.er.usgs.gov")

gages <- read.csv(paste0(dir, "/SC_GA_salinity_gages.csv"), colClasses = c("character", "character", "character", "numeric", "numeric"))
days <- c(Sys.Date() - 14, Sys.Date() - 1)
range <- seq.Date(days[1], days[2], "day")
v <- data.frame(datetime = range)

# Retrieve data and build input data.frame
report <- ""
for (i in 1:length(gages$station_name_web)) {
  print(gages$station_name_web[i])
  # Generate AQUARIUS URLs; add 1 for DST end timestamps
  url <- paste0("https://waterservices.usgs.gov/nwis/dv/?format=rdb&sites=", gages$NWIS_ID[i], "&startDT=", days[1], "&endDT=", days[2], "&parameterCd=00095&statCd=00003&access=3")
  tmp <- try (read.table(url, header = T, sep = "\t", colClasses = "character"), silent = T)
  if (inherits(tmp, "try-error")) {
    report <- paste0(report, "Data _NOT_ downloaded for ", gages$station_name_web[i], "\n")
  } else {
    # Remove data descriptor header row
    tmp <- tmp[2:dim(tmp)[1], ]
    # Remove rows with "_Eqp" flag
    if (any(apply(tmp, 1, function (x) any(grepl("_Eqp", x)))))
      tmp <- tmp[-which(apply(tmp, 1, function (x) any(grepl("_Eqp", x)))), ]
    # URLs with no returned data
    if (tmp[, 1] == "") {
      report <- paste0(report, gages$station_name_web[i], " missing\n")
    } else {
      # Convert timestamps; shift DST
      tmp$datetime <- as.Date(tmp$datetime, format = "%Y-%m-%d")
      tmp <- tmp[tmp$datetime %in% range, ]
      # Find the column that contains the data
      dd_col <- ifelse(dim(tmp)[2] > 5, 6, 4)
      tmp <- tmp[tmp[, dd_col] != "", ]
      v <- cbind(v, merge(range, tmp, by.x = 1, by.y = "datetime", all.x = T)[, dd_col])
      names(v)[dim(v)[2]] <- gages$station_name_web[i]
    }
  }
}
for (i in 2:dim(v)[2]) v[, i] <- as.numeric(as.character(v[, i]))
k1 <- 0.0120    # Wagner et al., 2006
k2 <- -0.2174
k3 <- 25.3283
k4 <- 13.7714
k5 <- -6.4788
k6 <- 2.5842
r <- v[, 2:dim(v)[2]] / 53087
v[, 2:dim(v)[2]] <- k1 + k2 * r ^ 0.5 + k3 * r + k4 * r ^ 1.5 + k5 * r ^ 2 + k6 * r ^ 2.5

# Upload data.frame to CoastalEDENdb
for (i in 1:dim(v)[1]) {
  ### Caution! 'replace' erases non-specified columns!! Use 'update' if modifying subsets of columns
  query <- paste0("replace into coastal_sc_ga set date='", v$datetime[i], "', ")
  for (j in 2:dim(v)[2]) {
    if (is.na(v[i, j])) tmp <- "NULL" else tmp <- v[i, j]
    query <- paste0(query, names(v)[j], "_salinity = ", tmp, ", ")
  }
  # Remove trailing comma
  query <- substr(query, 1, nchar(query) - 2)
  # Upload timestamp row to database
  err <- try(dbSendQuery(con, query), T)
  if (inherits(err, "try-error")) report <- paste0(report, "\nCoastalEDENdb upload error for ", v$datetime[i], ": ", err)
} # End process and upload

# Create year+ plots of parameters, and thumbnails
for (i in 1:dim(gages)[1]) {
  db2 <- dbGetQuery(con, paste0("select date, ", gages$station_name_web[i], "_salinity as sal from coastal_sc_ga where date >= ", as.numeric(format(Sys.Date(), "%Y")) - 3, format(Sys.Date(), "%m"), "01 order by date"))
  db2$date <- as.Date(db2$date, format = "%Y-%m-%d")
  last_meas <- rev(which(!is.na(db2$sal)))[1]
  db3 <- data.frame(date = db2$date, sal = NA)
  for (j in 7:last_meas) db3$sal[j] <- mean(db2$sal[(j - 6):j], na.rm = T)
  jpeg(filename = paste0(dir, "/images_sc_ga/", gages$station_name_web[i], "_thumb.jpg"), width = 360, height = 150, units = "px", pointsize = 2, quality = 100, bg = "white", type = "quartz")
  par(mar = c(0, 0, 0, 0) + .1)
  plot(db2$date, db2$sal, type = "n", xlab = "", ylab = "", ylim = range(db2$sal, na.rm = T), xaxt = "n", xaxt = "n", main = "")
  grid(nx = NA, ny = NULL, col = "black", lty = "dashed")
  abline(v = which(format(db2$date, "%m") == "01" & format(db2$date, "%d") == "01"), lty = "dashed", col = "black")
  lines(db2$date, db2$sal, lwd = 2)
  points(db2$date[last_meas], db2$sal[last_meas], pch = 20)
  lines(db3$date, db3$sal, lwd = 2, col = "green")
  points(db3$date[last_meas], db3$sal[last_meas - 6], pch = 20, col = "green")
  dev.off()
  jpeg(filename = paste0(dir, "/images_sc_ga/", gages$station_name_web[i], "_full.jpg"), width = 2400, height = 800, units = "px", pointsize = 12, quality = 100, bg = "white", type = "quartz")
  plot(db2$date, db2$sal, type = "n", xlab = "Year", ylab = "Salinity (PPT)", ylim = range(db2$sal, na.rm = T), xaxt = "n", main = paste(gages$station_name_web[i], "salinity"))
  axis(1, at = db2$date[which(format(db2$date, "%m") == "01" & format(db2$date, "%d") == "01")], labels = as.numeric(format(Sys.Date(), "%Y")) - 2:0, hadj = -1, padj = 1.7)
  grid(nx = NA, ny = NULL, col = "black", lty = "dashed")
  abline(v = db2$date[which(format(db2$date, "%m") == "01" & format(db2$date, "%d") == "01")], lty = "dashed", col = "black")
  lines(db2$date, db2$sal, lwd = 3)
  points(db2$date[last_meas], db2$sal[last_meas], pch = 20, cex = 3)
  lines(db2$date, db3$sal, lwd = 3, col = "green")
  points(db3$date[last_meas], db3$sal[last_meas], pch = 20, cex = 3, col = "green")
  text(db3$date[last_meas], db2$sal[last_meas], paste(round(db2$sal[last_meas], 2), "\n", format(db2$date[last_meas], "%m/%d/%Y"), sep = ""), pos = 3, cex = 1.25, font = 2, col = "gray35", offset = 2)
  text(db3$date[last_meas], db3$sal[last_meas], round(db3$sal[last_meas], 2), pos = 4, cex = 1.25, font = 2, col = "green3", offset = 1)
  legend("topleft", c(paste(as.numeric(format(Sys.Date(), "%Y")) - 3, "-", format(Sys.Date(), "%Y"), gages$station_name_web[i], "salinity"), "Rolling seven-day average salinity"), col = c("black", "green"), lty = 1, lwd = 3)
  dev.off()
  err <- try (system(paste0("curl -T ", dir, "/images_sc_ga/", gages$station_name_web[i], "_thumb.jpg ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden-data/coastal_sc_ga/")))
  if (inherits(err, "try-error")) report <- paste0(report, "\n", gages$station_name_web[i], " thumbnail NOT transferred") else report <- paste0(report, "\n", gages$station_name_web[i], " thumbnail transferred")
  err <- try (system(paste0("curl -T ", dir, "/images_sc_ga/", gages$station_name_web[i], "_full.jpg ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden-data/coastal_sc_ga/")))
  if (inherits(err, "try-error")) report <- paste0(report, "\n", gages$station_name_web[i], " full-size NOT transferred") else report <- paste0(report, "\n", gages$station_name_web[i], " full-sized transferred")
}

query <- "select date_format(date, '%Y') as Year, date_format(date, '%m') as Month"
for(j in 1:dim(gages)[1])
  query <- paste0(query, ", avg(", gages$station_name_web[j], "_salinity) as ", gages$station_name_web[j])
query <- paste(query, "from coastal_sc_ga group by Year, Month")
sal <- dbGetQuery(con, query)
sal <- CSIinterp(sal)
csi <- CSIcalc(sal)
CSIstack(csi, paste0(dir, "/csi_stacked_sc_ga/"), T, F)
for(j in 1:dim(gages)[1]) {
  err <- try (system(paste0("curl -T ", dir, "/csi_stacked_sc_ga/", gages$station_name_web[j], "_stacked_thumb.png ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden-data/coastal_sc_ga/")))
  if (inherits(err, "try-error")) report <- paste0(report, "\n", gages$station_name_web[j], " CSI thumbnail NOT transferred") else report <- paste0(report, "\n", gages$station_name_web[j], " CSI thumbnail transferred")
  err <- try (system(paste0("curl -T ", dir, "/csi_stacked_sc_ga/", gages$station_name_web[j], "_stacked.png ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden-data/coastal_sc_ga/")))
  if (inherits(err, "try-error")) report <- paste0(report, "\n", gages$station_name_web[j], " CSI full-size NOT transferred") else report <- paste0(report, "\n", gages$station_name_web[j], " CSI full-sized transferred")
}

col <- c("tan4", "tan2", "darkolivegreen4", "lightblue", "skyblue3", "darkgreen")
for (j in 1:length(gages$station_name_web)) {
  por <- dbGetQuery(con, paste0("select date, ", gages$station_name_web[j], "_salinity as sal from coastal_sc_ga order by date"))
  por$date <- as.Date(por$date, format = "%Y-%m-%d")
  rng <- dbGetQuery(con, paste0("select min(date) as start, max(date) as end from coastal_sc_ga where ", gages$station_name_web[j], "_salinity is not null"))
  yr3 <- dbGetQuery(con, paste0("select date, ", gages$station_name_web[j], "_salinity as sal from coastal_sc_ga where date >= ", as.numeric(format(Sys.Date(), "%Y")) - 3, format(Sys.Date(), "%m"), "01 order by date"))
  yr3$date <- as.Date(yr3$date, format = "%Y-%m-%d")
  win30 <- rep(NA, length(yr3$date))
  for (i in 30:length(win30))
    win30[i] <- mean(yr3$sal[(i-29):i], na.rm = T)
  qyear <- matrix(NA, length(yr3$date), 6); median <- matrix(NA, length(yr3$date), 1)
  for (i in 1:length(qyear[, 1])) {
    qyear[i, ] <- quantile(por$sal[as.POSIXlt(por$date)$mon == as.numeric(format(yr3$date[i], "%m")) - 1], c(0, .1, .25, .75, .9, 1), na.rm = T)
    median[i] <- quantile(por$sal[as.POSIXlt(por$date)$mon == as.numeric(format(yr3$date[i], "%m")) - 1], 0.5, na.rm = T)
  }
  median[as.POSIXlt(yr3$date)$mday == 1] <- NA
  jpeg(filename = paste0(dir, "/salinity_duration_hydrographs_sc_ga/", gages$station_name_web[j], "_salinity_thumb.jpg"), width = 360, height = 150, units = "px", pointsize = 2, quality = 100, bg = "white", type = "quartz")
  par(mar = c(0, 0, 0, 0) + .1)
  plot(qyear[, 1], type = "n", xlab = "", ylab = "", ylim = range(qyear, na.rm = T), xaxt = "n", yaxt = "n", main = "", axes = F, frame.plot = T)
  for (i in 1:5) polygon(c(1:length(yr3$date), length(yr3$date):1), c(qyear[, i], rev(qyear[, i + 1])), col = col[i])
  lines(median, lwd = 2, col = "yellow")
  lines(yr3$sal, lwd = 3, col = "grey")
  lines(win30, lwd = 4, col = "black")
  dev.off()
  jpeg(filename = paste0(dir, "/salinity_duration_hydrographs_sc_ga/", gages$station_name_web[j], "_salinity.jpg"), width = 2400, height = 800, units = "px", pointsize = 12, quality = 100, bg = "white", type = "quartz")
  par(mar = c(5, 4, 4, 5) + .1)
  plot(qyear[, 1], type = "n", xlab = "Month of year", ylab = "Salinity (PPT)", ylim = range(qyear, na.rm = T), xaxt = "n", main = paste(gages$station_name_web[j], "three year salinity, 30 day moving window"))
  axis(1, at = which(as.POSIXlt(yr3$date)$mday == 1), labels = format(yr3$date[which(as.POSIXlt(yr3$date)$mday == 1)], "%b"), hadj = -1)
  axis(1, at = which(as.POSIXlt(yr3$date)$yday == 0), labels = format(yr3$date[which(as.POSIXlt(yr3$date)$yday == 0)], "%Y"), padj = 1)
  for (i in 1:5) polygon(c(1:length(yr3$date), length(yr3$date):1), c(qyear[, i], rev(qyear[, i + 1])), col = col[i])
  lines(median, lwd = 3, col = "yellow")
  lines(yr3$sal, lwd = 4, col = "grey")
  lines(win30, lwd = 5, col = "black")
  legend("topright", c("90% to Max.", "75% to 90%", "25% to 75%", "10% to 25%", "Min. to 10%"), fill = col[5:1], cex = 1.5, bty = "n", title = "Salinity bins")
  legend("topright", c("Salinity 30 day moving window", "Daily salinity values", "Historic monthly mean salinity"), lwd = c(5, 4, 4), col = c("black", "grey", "yellow"), inset = c(.075, 0), cex = 1.5, bty = "n")
  legend("topleft", paste("Period of record:", rng$start, "to", rng$end), cex = 1.5, bty = "n")
  dev.off()
  system(paste0("curl -T ", dir, "/salinity_duration_hydrographs_sc_ga/", gages$station_name_web[j], "_salinity_thumb.jpg ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden-data/coastal_sc_ga/"))
  system(paste0("curl -T ", dir, "/salinity_duration_hydrographs_sc_ga/", gages$station_name_web[j], "_salinity.jpg ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden-data/coastal_sc_ga/"))
}

system(paste0("echo 'Subject: CoastalEDENdb_sc_ga upload report
", report, "' | /usr/sbin/sendmail bmccloskey@usgs.gov"))
