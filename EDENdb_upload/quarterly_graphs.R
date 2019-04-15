library (RMySQL)
library (RCurl)

try (setwd("./EDENdb_upload"), silent = T)
source ("../usr_pwd.R")
# Connect to database and download data file
con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "eden_new", host = "stpweb1-dmz.er.usgs.gov")

gages <- dbGetQuery(con, "select station_name_web, station_name, station.station_id, vertical_conversion as conv from station, station_datum where station.station_id = station_datum.station_id and station_name_web != 'Alligator_Creek' and station_name_web != 'East_Side_Creek' and station_name_web != 'G-3777' and station_name_web != 'Manatee_Bay_Creek' and station_name_web != 'Raulerson_Brothers_Canal' and station_name_web != 'Barron_River'")
for (i in 1:dim(gages)[1]) {
  pred <- dbGetQuery(con, paste0("select station_name_web, predictor_rank from station_gapfill, station where station_gapfill.predictor_station_id = station.station_id and station_gapfill.station_id = ", gages$station_id[i], " order by predictor_rank"))
  query <- paste0("select datetime, `stage_", gages$station_name_web[i], "` as st")
  if (dim(pred)[1] > 0) query <- paste0(query, ", `stage_", pred$station_name_web[1], "` as p1")
  if (dim(pred)[1] > 1) query <- paste0(query, ", `stage_", pred$station_name_web[2], "` as p2")
  if (dim(pred)[1] > 2) query <- paste0(query, ", `stage_", pred$station_name_web[3], "` as p3")
  query <- paste0(query, " from stage where datetime >= 20160927000000 and datetime < 20171006000000 order by datetime")
  ts <- dbGetQuery(con, query)
  ts$datetime <- as.POSIXct(ts$datetime, tz = "EST", format = "%Y-%m-%d %H:%M:%S")
  if (any(!is.na(ts$st))) {
    yran <- range(ts[, 2:dim(ts)[2]], na.rm = T)
    png(paste0("./images/", gages$station_name_web[i], ".png"), width = 1200, height = 800, type = "quartz")
    plot(ts$datetime, ts$st, type = "n", lwd = 2, ylim = yran, main = paste("2018q2 for", gages$station_name_web[i]), xlab = "Date", ylab = "Stage (ft.)")
    abline(v = ts$datetime[ts$datetime == "2016-09-30 23:00:00"], lwd = 3, col = "darkred")
    abline(v = ts$datetime[ts$datetime == "2017-10-01 01:00:00"], lwd = 3, col = "darkred")
    lines(ts$datetime, ts$st, lwd = 2)
    lines(ts$datetime, ts$p1, lwd = 2, lty = 2, col = "blue")
    lines(ts$datetime, ts$p2, lwd = 2, lty = 3, col = "red")
    lines(ts$datetime, ts$p3, lwd = 2, lty = 4, col = "darkgreen")
    if (any(enp$V2 == gages$station_name[i])) points(enp$date_tm[which(enp$V2 == gages$station_name[i])], as.numeric(enp$V5[which(enp$V2 == gages$station_name[i])]) + gages$conv[i], pch = 16)
    if (any(sfwmd$V1 == gages$station_name[i])) points(sfwmd$date_tm[which(sfwmd$V1 == gages$station_name[i])], sfwmd$V5[which(sfwmd$V1 == gages$station_name[i])] + gages$conv[i], pch = 16)
    legend("topleft", c(gages$station_name_web[i], paste("P1:", pred$station_name_web[1]), paste("P2:", pred$station_name_web[2]), paste("P3:", pred$station_name_web[3])), lty = 1:4, lwd = 2, col = c("black", "blue", "red", "darkgreen"))
    dev.off()
  }
}
