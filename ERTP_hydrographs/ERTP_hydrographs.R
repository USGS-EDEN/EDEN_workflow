# -------------
# Bryan McCloskey
# bmccloskey@usgs.gov
# St. Petersburg Coastal and Marine Science Center
# US Geological Survey
#
# 07/25/2018
#--------------

print("These libraries must be installed: ncdf4, RCurl, RMySQL")
# Required libraries. If not present, run:
# install.packages("ncdf4")
# install.packages("RCurl")
# install.packages("RMySQL")
library (ncdf4)
library (RCurl)
library (RMySQL)

try (setwd("./ERTP_hydrographs"), silent = T)
source ("../usr_pwd.R")
# Connect to database, list of gages for which to acquire data
con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "eden_new", host = "stpweb1-dmz.er.usgs.gov")
gages <- dbGetQuery(con, "select station_name_web, convert_to_navd88_feet as conv, vertical_conversion, average_elevation from station, station_datum, station_vegetation where station.station_id = station_datum.station_id and station.station_id = station_vegetation.station_id and station_vegetation.community_level_id = 1 and ertp_ge_flag is not null and edenmaster_end = 'curren' group by station_name_web")
# add gages with no average_elevation
gages2 <- dbGetQuery(con, "select station_name_web, convert_to_navd88_feet as conv, vertical_conversion from station, station_datum where station.station_id = station_datum.station_id and (station_name_web = 'EPSW' or station_name_web = 'NCL' or station_name_web = 'NMP' or station_name_web = 'SPARO' or station_name_web like 'S12%' or station_name_web = 'G-1502' or station_name_web like 'S332%' or station_name_web like 'S175%' or station_name_web = 'S18C_T')")
gages2$average_elevation <- NA
gages <- rbind(gages, gages2)

xx <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 365)
col <- c("black", "tan4", "tan2", "darkolivegreen4", "lightblue", "skyblue3", "darkgreen")
file_HW <- "./input/mail_head-HW.txt"
file_LW <- "./input/mail_head-LW.txt"
dt <- Sys.Date() - 1
mdy <- format(dt, "%m/%d/%Y")
ymd <- as.numeric(format(dt, "%Y%m%d"))
hw <- paste0(readChar(file_HW, file.info(file_HW)$size), mdy, ",</p>\n<p>Water levels at the following water-level gages equal or exceed the 90th percentile for the month:</p>\n\n<p>")
lw <- paste0(readChar(file_LW, file.info(file_LW)$size), mdy, ",</p>\n<p>Water levels at the following water-level gages equal or are below the 25th percentile for the month:</p>\n\n<p>")
date_check <- dbGetQuery(con, paste0("select date(datetime) as date from stage where datetime >= ", ymd, " and datetime < ", ymd + 1, " group by date"))

if (length(date_check) == 0) {
  hw <- paste0(hw, "<strong>Unavailable gage data from one or more cooperator agency for ", mdy, "; conditions at gages are not available.</strong>")
  lw <- paste0(lw, "<strong>Unavailable gage data from one or more cooperator agency for ", mdy, "; conditions at gages are not available.</strong>") 
} else { 
  high_counter <- low_counter <- 0
  lab <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", NA)
  for (j in 1:length(gages$station_name_web)) {
    st <- if (gages$station_name_web[j] == "3ANE_GW" | gages$station_name_web[j] == "3ANW_GW") substr(gages$station_name_web[j], 1, 4) else gages$station_name_web[j]
    query <- paste0("select date(datetime) as date, avg(`stage_", st, "`) + ", gages$conv[j], " as stage from stage where datetime >= 20020701000000 and datetime < 20121019000000 group by date")
    iop <- dbGetQuery(con, query)
    iop$date <- as.Date(iop$date)
    yr <- dbGetQuery(con, paste0("select date(datetime) as date, avg(`stage_", gages$station_name_web[j], "`) + ", gages$conv[j], " as stage from stage where datetime >= ", substr(ymd, 1, 4), "0101000000 group by date"))
    yr$date <- as.Date(yr$date)

    qyear <- matrix(NA, 365, 6); median <- matrix(NA, 365, 1)
    for (i in 1:length(qyear[, 1])) {
      qyear[i, ] <- quantile(iop$stage[as.POSIXlt(iop$date)$mon == as.numeric(format(strptime(paste("2010", i), "%Y %j"), "%m")) - 1] ,c(0, .1, .25, .75, .9, 1), na.rm = T)
      median[i, ] <- quantile(iop$stage[as.POSIXlt(iop$date)$mon == as.numeric(format(strptime(paste("2010", i), "%Y %j"), "%m")) - 1], 0.5, na.rm = T)
    }
    median[xx] <- NA
    if (!is.na(gages$average_elevation[j]) & !is.na(rev(yr$stage)[1]) & rev(yr$stage)[1] >= rev(qyear[, 5])[1]) {
      hw <- paste0(hw, "<a href='http://sofia.usgs.gov/eden/water_level_percentiles.php?name=", gages$station_name_web[j], "&type=gage'>", gages$station_name_web[j], "</a><br />\n")
      high_counter <- high_counter + 1
    }
    if (!is.na(gages$average_elevation[j]) & !is.na(rev(yr$stage)[1]) & rev(yr$stage)[1] <= rev(qyear[, 3])[1]) {
      lw <- paste0(lw, "<a href='http://sofia.usgs.gov/eden/water_level_percentiles.php?name=", gages$station_name_web[j], "&type=gage'>", gages$station_name_web[j], "</a><br />\n")
      low_counter <- low_counter + 1
    }

    jpeg(paste0("./images/", gages$station_name_web[j], "_monthly.jpg"), width = 1200, height = 800, quality = 100, type = "quartz")
    par(mar = c(5, 4, 4, 5) + .1)
    plot(qyear[, 1], type = "n", xlab = "Month of year", ylab = "Water-level elevation, feet NAVD88", ylim = range(qyear, yr$stage, na.rm = T), xaxt = "n", main = paste("Gage", gages$station_name_web[j]))
    axis(1, at = xx, labels = lab, hadj = -1.5)
    for (i in 1:5) polygon(c(1:365, 365:1), c(qyear[, i], rev(qyear[, i + 1])), col = col[i + 1])
    lines(median, lwd = 3, col = "yellow")
    grid(nx = NA, ny = NULL, col = "black", lty = "dashed")
    abline(v = xx, lty = "dashed", col = "black")
    if (!is.na(gages$average_elevation[j])) abline(h = gages$average_elevation[j], lwd = 3, lty = 2, col = "darkgreen")
    lines(yr$stage, lwd = 3)
    points(length(yr$date), rev(yr$stage)[1], pch = 20, cex = 3)
    text(length(yr$date), rev(yr$stage)[1], paste0(round(rev(yr$stage)[1], 2), " ft.\n", format(rev(yr$date)[1], "%m/%d/%Y")), pos = 4, cex = 1.25, font = 2)
    par(new = T)
    plot(qyear[, 1], ylim = range(qyear, na.rm = T) - gages$vertical_conversion[j], type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    axis(4)
    mtext("Water-level elevation, feet NGVD29", side = 4, line = 3)
    dev.off()
    err <- try (ftpUpload(paste0("./images/", gages$station_name_web[j], "_monthly.jpg"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/ertp_hydrographs/hydrographs/", gages$station_name_web[j], "_monthly.jpg")))
    
    jpeg(paste0("./images/", gages$station_name_web[j], "_monthly_med.jpg"), width = 800, height = 533, quality = 100, type = "quartz")
    par(mar = c(5, 4, 4, 5) + .1)
    plot(qyear[, 1], type = "n", xlab = "Month of year", ylab = "Water-level elevation, feet NAVD88", ylim = range(qyear, yr$stage, na.rm = T), xaxt = "n", main = paste0("Gage ", gages$station_name_web[j]))
    axis(1, at = xx, labels = lab, hadj = -0.7)
    for (i in 1:5) polygon(c(1:365, 365:1), c(qyear[, i], rev(qyear[, i + 1])), col = col[i + 1])
    lines(median, lwd = 3, col = "yellow")
    grid(nx = NA, ny = NULL, col = "black", lty = "dashed")
    abline(v = xx, lty = "dashed", col = "black")
    if (!is.na(gages$average_elevation[j])) abline(h = gages$average_elevation[j], lwd = 3, lty = 2, col = "darkgreen")
    lines(yr$stage, lwd = 3)
    points(length(yr$date), rev(yr$stage)[1], pch = 20, cex = 3)
    text(length(yr$date), rev(yr$stage)[1], paste0(round(rev(yr$stage)[1], 2), " ft.\n", format(rev(yr$date)[1], "%m/%d/%Y")), pos = 4, cex = 1.25, font = 2)
    par(new = T)
    plot(qyear[, 1], ylim = range(qyear, na.rm = T) - gages$vertical_conversion[j], type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    axis(4)
    mtext("Water-level elevation, feet NGVD29", side = 4, line = 3)
    dev.off()
    err <- try (ftpUpload(paste0("./images/", gages$station_name_web[j], "_monthly_med.jpg"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/ertp_hydrographs/hydrographs/", gages$station_name_web[j], "_monthly_med.jpg")))
    
    jpeg(paste0("./images/", gages$station_name_web[j], "_monthly_thumb.jpg"), width = 240, height = 160, pointsize = 2, quality = 100, type = "quartz")
    par(mar = c(0, 0, 0, 0) + .1)
    plot(qyear[, 1], type = "n", xlab = "", ylab = "", ylim = range(qyear, yr$stage, na.rm = T), xaxt = "n", xaxt = "n", main = "")
    for (i in 1:5) polygon(c(1:365, 365:1), c(qyear[, i], rev(qyear[, i + 1])), col = col[i + 1])
    lines(median, lwd = 1.5, col = "yellow")
    grid(nx = NA, ny = NULL, col = "black", lty = "dashed")
    abline(v = xx, lty = "dashed", col = "black")
    if (!is.na(gages$average_elevation[j])) abline(h = gages$average_elevation[j], lwd = 2, lty = 2, col = "darkgreen")
    lines(yr$stage, lwd = 2)
    points(length(yr$date), rev(yr$stage)[1], pch = 20)
    dev.off()
    err <- try (ftpUpload(paste0("./images/", gages$station_name_web[j], "_monthly.jpg"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/ertp_hydrographs/thumbnails/", gages$station_name_web[j], "_monthly_thumb.jpg")))
    
    write(c(mdy, rev(yr$stage)[1]), paste0("./table/", gages$station_name_web[j], ".txt"), sep = "\t", ncolumns = 2)
    write(t(cbind(qyear[xx, 1:3], median[xx], qyear[xx, 4:6])), paste0("./table/", gages$station_name_web[j], ".txt"), sep = "\t", ncolumns = 7, append = T)
    err <- try (ftpUpload(paste0("./table/", gages$station_name_web[j], ".txt"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/ertp_hydrographs/table/", gages$station_name_web[j], ".txt")))
  }
}
if(high_counter == 0) hw <- paste0(hw, "<strong>No gages equal or exceed the 90th percentile for the month on ", mdy, ".</strong>")
if(low_counter == 0) lw <- paste0(lw, "<strong>No gages equal or are below the 25th percentile for the month on ", mdy, ".</strong>")

# Set up WL files
cur_qtr <- paste0(as.POSIXlt(Sys.Date() - 1)$year + 1900, "_", tolower(quarters(Sys.Date() - 1)))
err <- try (download.file(paste0("https://sofia.usgs.gov/eden/data/realtime2/", cur_qtr, "_v2rt_nc.zip"), paste0("../surfaces/", cur_qtr, ".zip")))
unzip(paste0("../surfaces/", cur_qtr, ".zip"), exdir = "../surfaces")
file.rename(paste0("../surfaces/", cur_qtr, "_v2rt.nc"), paste0("../surfaces/", cur_qtr, ".nc"))
unlink("../surfaces/*.zip")

yr <- as.POSIXlt(Sys.Date() - 1)$year + 1900
qtr <- substr(cur_qtr, 7, 7)
for (i in 2002:yr)
  for (j in 1:4)
    if (!file.exists(paste0("../surfaces/", i, "_q", j, ".nc")) & !(i == yr & j > qtr))
      err <- try (download.file(paste0("https://sflthredds.er.usgs.gov/thredds/fileServer/eden/surfaces/", i, "_q", j, ".nc"), paste0("../surfaces/", i, "_q", j, ".nc")))
file_surf <- list.files("../surfaces", "^2(002_q[3-4]|00[3-9]_q[1-4]|01[0-9]_q[1-4]).nc$")

tree <- dbGetQuery(con, "select * from tree_islands order by `order`")
hw <- paste0(hw, "</p>\n<p>Water levels at the following tree islands equal or exceed the maximum tree island ground elevation, or the 90th percentile water level for the month:</p>\n<p>")
lw <- paste0(lw, "</p>\n<p>Water levels at the following tree islands equal or are below the 25th percentile water level for the month:</p>\n<p>")

high_counter <- 0
low_counter <- 0
err <- try (surf.nc <- nc_open(paste0("../surfaces/", rev(file_surf)[1])))
fst_day <- as.Date(substr(surf.nc$dim$time$units, 12, 21))
if (inherits(err, "try-error") | fst_day + rev(surf.nc$dim$time$vals)[1] != Sys.Date() - 1) {
  hw <- paste0(hw, "<strong>Unavailable gage data from one or more cooperator agency for ", mdy, "; real-time EDEN surfaces could not be generated, therefore conditions at tree islands are not available.</strong>")
  lw <- paste0(lw, "<strong>Unavailable gage data from one or more cooperator agency for ", mdy, "; real-time EDEN surfaces could not be generated, therefore conditions at tree islands are not available.</strong>")
} else {
  x <- ncvar_get(surf.nc, "x")
  y <- ncvar_get(surf.nc, "y")
  dt <- NULL
  nc_close(surf.nc)
  for (i in 1:length(file_surf)) {
    surf.nc <- nc_open(paste0("../surfaces/", file_surf[i]))
    time <- ncvar_get(surf.nc, "time")
    dt <- c(dt, as.Date(substr(surf.nc$dim$time$units, 12, 21)) + time) # allows skipped days
    nc_close(surf.nc)
  }
  dt <- as.Date(dt, origin = "1970-01-01")
  for (j in 1:length(tree$island)) {
    tree_x <- which.min(abs(tree$utm_easting[j] - x))
    tree_y <- which.min(abs(tree$utm_northing[j] - y))
    stage <- NULL
    for (i in 1:length(file_surf)) {
      surf.nc <- nc_open(paste0("../surfaces/", file_surf[i]))
      stage <- c(stage, ncvar_get(surf.nc, "stage", c(tree_x, tree_y, 1), c(1, 1, -1)))
      nc_close(surf.nc)
    }
    stage <- stage / 12 / 2.54

    qyear <- matrix(NA, 365, 6); median <- matrix(NA, 365, 1)
    for (i in 1:length(qyear[, 1])) {
      qyear[i, ] <- quantile(stage[as.POSIXlt(dt)$mon == as.numeric(format(as.Date(i - 1, "2018-01-01"), "%m")) - 1 & dt < as.Date("2012/10/19")], c(0, .1, .25, .75, .9, 1), na.rm = T)
      median[i, ] <- quantile(stage[as.POSIXlt(dt)$mon == as.numeric(format(as.Date(i - 1, "2018-01-01"), "%m")) - 1 & dt < as.Date("2012/10/19")], 0.5, na.rm = T)
    }
    median[xx] <- NA
    if (rev(stage)[1] >= tree$elevation[j]) {
      hw <- paste0(hw, "<a href='http://sofia.usgs.gov/eden/water_level_percentiles.php?name=", tree$island[j], "&type=treeisland'>", tree$island[j], "</a> (overtopped)<br />\n")
      high_counter <- high_counter + 1
    }
    if (rev(stage)[1] >= qyear[as.POSIXlt(rev(dt)[1])$yday + 1, 5]) {
      hw <- paste0(hw, "<a href='http://sofia.usgs.gov/eden/water_level_percentiles.php?name=", tree$island[j], "&type=treeisland'>", tree$island[j], "</a> (water levels over 90th percentile)<br />\n")
      high_counter <- high_counter + 1
    }
    if (rev(stage)[1] <= qyear[as.POSIXlt(rev(dt)[1])$yday + 1, 3]) {
      lw <- paste0(lw, "<a href='http://sofia.usgs.gov/eden/water_level_percentiles.php?name=", tree$island[j], "&type=treeisland'>", tree$island[j], "</a><br />\n")
      low_counter <- low_counter + 1
    }

    jpeg(paste0("./images/", tree$island[j], "_monthly.jpg"), width = 1200, height = 800, quality = 100, type = "quartz")
    par(mar = c(5, 4, 4, 5) + .1)
    plot(qyear[, 1], type = "n", xlab = "Month of year", ylab = "Water-level elevation, feet NAVD88", ylim = range(qyear, stage, na.rm = T), xaxt = "n", main = paste0("Tree Island ", tree$island[j]))
    axis(1, at = xx, labels = lab, hadj = -1.5)
    for (i in 1:5) polygon(c(1:365, 365:1), c(qyear[, i], rev(qyear[, i + 1])), col = col[i + 1])
    lines(median, lwd = 3, col = "yellow")
    grid(nx = NA, ny = NULL, col = "black", lty = "dashed")
    abline(v = xx, lty = "dashed", col = "black")
    abline(h = tree$elevation[j], lwd = 3, lty = 2, col = "darkgreen")
    lines(stage[as.POSIXlt(dt)$year + 1900 == yr], lwd = 3)
    points(length(stage[as.POSIXlt(dt)$year + 1900 == yr]), rev(stage)[1], pch = 20, cex = 3)
    text(length(stage[as.POSIXlt(dt)$year + 1900 == yr]), rev(stage)[1], paste0(round(rev(stage[as.POSIXlt(dt)$year + 1900 == yr])[1], 2), " ft.\n", mdy), pos = 4, cex = 1.25, font = 2)
    par(new = T)
    plot(qyear[, 1], ylim = range(qyear, stage, na.rm = T) - tree$conversion[j], type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    axis(4)
    mtext("Water-level elevation, feet NGVD29", side = 4, line = 3)
    dev.off()
    err <- try (ftpUpload(paste0("./images/", tree$island[j], "_monthly.jpg"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/ertp_hydrographs/hydrographs/", tree$island[j], "_monthly.jpg")))
    
    jpeg(paste0("./images/", tree$island[j], "_monthly_med.jpg"), width = 800, height = 533, quality = 100, type = "quartz")
    par(mar = c(5, 4, 4, 5) + .1)
    plot(qyear[, 1], type = "n", xlab = "Month of year", ylab = "Water-level elevation, feet NAVD88", ylim = range(qyear, stage, na.rm = T), xaxt = "n", main = paste0("Tree Island ", tree$island[j]))
    axis(1, xx, labels = lab, hadj = -0.7)
    for (i in 1:5) polygon(c(1:365, 365:1), c(qyear[, i], rev(qyear[, i + 1])), col = col[i + 1])
    lines(median, lwd = 3, col = "yellow")
    grid(nx = NA, ny = NULL, col = "black", lty = "dashed")
    abline(v = xx, lty = "dashed", col = "black")
    abline(h = tree$elevation[j], lwd = 3, lty = 2, col = "darkgreen")
    lines(stage[as.POSIXlt(dt)$year + 1900 == yr], lwd = 3)
    points(length(stage[as.POSIXlt(dt)$year + 1900 == yr]), rev(stage)[1], pch = 20, cex = 3)
    text(length(stage[as.POSIXlt(dt)$year + 1900 == yr]), rev(stage)[1], paste0(round(rev(stage)[1], 2), " ft.\n", mdy), pos = 4, cex = 1.25, font = 2)
    par(new = T)
    plot(qyear[, 1], ylim = range(qyear, stage, na.rm = T) - tree$conversion[j], type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    axis(4)
    mtext("Water-level elevation, feet NGVD29", side = 4, line = 3)
    dev.off()
    err <- try (ftpUpload(paste0("./images/", tree$island[j], "_monthly_med.jpg"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/ertp_hydrographs/hydrographs/", tree$island[j], "_monthly_med.jpg")))
    
    jpeg(paste0("./images/", tree$island[j], "_monthly_thumb.jpg"), width = 240, height = 160, pointsize = 2, quality = 100, type = "quartz")
    par(mar = c(0, 0, 0, 0) + .1)
    plot(qyear[, 1], type = "n", xlab = "", ylab = "", ylim = range(qyear, stage, na.rm = T), xaxt = "n", xaxt = "n", main = "")
    for (i in 1:5) polygon(c(1:365, 365:1), c(qyear[, i], rev(qyear[, i + 1])), col = col[i + 1])
    lines(median, lwd = 1.5, col = "yellow")
    grid(nx = NA, ny = NULL, col = "black", lty = "dashed")
    abline(v = xx, lty = "dashed", col = "black")
    abline(h = tree$elevation[j], lwd = 2, lty = 2, col = "darkgreen")
    lines(stage[as.POSIXlt(dt)$year + 1900 == yr], lwd = 2)
    points(length(stage[as.POSIXlt(dt)$year + 1900 == yr]), rev(stage)[1], pch = 20)
    dev.off()
    err <- try (ftpUpload(paste0("./images/", tree$island[j], "_monthly_thumb.jpg"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/ertp_hydrographs/thumbnails/", tree$island[j], "_monthly_thumb.jpg")))
    
    write(c(mdy, rev(stage)[1]), paste0("./table/", tree$island[j], ".txt"), sep = "\t", ncolumns = 2)
    write(t(cbind(qyear[xx, 1:3], median[xx], qyear[xx, 4:6])), paste0("./table/", tree$island[j], ".txt"), sep = "\t", ncolumns = 7, append = T)
    err <- try (ftpUpload(paste0("./table/", tree$island[j], ".txt"), paste0("ftp://ftpint.usgs.gov/pub/er/fl/st.petersburg/eden/ertp_hydrographs/table/", tree$island[j], ".txt")))
  }
}
if (high_counter == 0) hw <- paste0(hw, "<strong>No tree islands equal or exceed the maximum tree island ground elevation, or the 90th percentile water level for the month on ", mdy, ".</strong>")
if (low_counter == 0) lw <- paste0(lw, "<strong>No tree islands equal or are below the 25th percentile for the month on ", mdy, ".</strong>")
fileName <- "./input/mail_tail.txt"
hw <- paste0(hw, "</p>\n", readChar(fileName, file.info(fileName)$size))
lw <- paste0(lw, "</p>\n", readChar(fileName, file.info(fileName)$size))
write(hw, "./output/mail_HW.txt")
write(lw, "./output/mail_LW.txt")

### System level commands may not work if local environment does not have sendmail installed!!
to <- "bmccloskey@usgs.gov, hhenkel@usgs.gov, Daniel.B.Hughes@usace.army.mil, barry.n.baxter@noaa.gov, Grady.H.Caulk@usace.army.mil, DavidW@miccosukeetribe.com, ricksanda@gmail.com, Meredith.A.Moreno@usace.army.mil"
system(paste0("/usr/sbin/sendmail -f 'EDEN Water Level Alert <bmccloskey@usgs.gov>' ", to, " < ./output/mail_HW.txt"))
to <- "bmccloskey@usgs.gov, barry.n.baxter@noaa.gov, ricksanda@gmail.com, Meredith.A.Moreno@usace.army.mil"
system(paste0("/usr/sbin/sendmail -f 'EDEN Water Level Alert <bmccloskey@usgs.gov>' ", to, " < ./output/mail_LW.txt"))
setwd("..")
