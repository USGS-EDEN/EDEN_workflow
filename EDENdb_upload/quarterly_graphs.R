library (RMySQL)
library (RCurl)

try (setwd("./EDENdb_upload"), silent = T)
source ("../usr_pwd.R")
# Connect to database and download data file
con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "eden", host = "igsafpesgsz03.er.usgs.gov")

gages <- dbGetQuery(con, "select station_name_web, station_name, station.station_id, vertical_conversion as conv from station, station_datum where station.station_id = station_datum.station_id and station_name_web != 'Alligator_Creek' and station_name_web != 'East_Side_Creek' and station_name_web != 'G-3777' and station_name_web != 'Manatee_Bay_Creek' and station_name_web != 'Raulerson_Brothers_Canal' and station_name_web != 'Barron_River'")
for (i in 1:dim(gages)[1]) {
  pred <- dbGetQuery(con, paste0("select station_name_web, predictor_rank from station_gapfill, station where station_gapfill.predictor_station_id = station.station_id and station_gapfill.station_id = ", gages$station_id[i], " order by predictor_rank"))
  query <- paste0("select datetime, `flag_", gages$station_name_web[i], "` as fl, `stage_", gages$station_name_web[i], "` as st")
  if (dim(pred)[1] > 0) query <- paste0(query, ", `stage_", pred$station_name_web[1], "` as p1")
  if (dim(pred)[1] > 1) query <- paste0(query, ", `stage_", pred$station_name_web[2], "` as p2")
  if (dim(pred)[1] > 2) query <- paste0(query, ", `stage_", pred$station_name_web[3], "` as p3")
  query <- paste0(query, " from stage where datetime >= 20200928000000 and datetime < 20210102000000 order by datetime")
  ts <- dbGetQuery(con, query)
  ts$datetime <- as.POSIXct(ts$datetime, tz = "EST", format = "%Y-%m-%d %H:%M:%S")
  if (any(!is.na(ts$st))) {
    yran <- range(ts[, 3:dim(ts)[2]], na.rm = T)
    png(paste0("./images/", gages$station_name_web[i], ".png"), width = 1200, height = 800, type = "quartz")
    layout(rbind(1, 2), heights = c(14, 1))
    plot(ts$datetime, ts$st, type = "n", ylim = yran, main = paste("WY2021Q1 for", gages$station_name_web[i]), xlab = "Date", ylab = "Stage (ft.)")
    abline(v = ts$datetime[ts$datetime == "2020-09-30 23:00:00"], lwd = 3, col = "darkred")
    abline(v = ts$datetime[ts$datetime == "2021-01-01 01:00:00"], lwd = 3, col = "darkred")
    points(ts$datetime, ts$p1, pch = 16, col = "blue", cex = 0.5)
    points(ts$datetime, ts$p2, pch = 16, col = "red", cex = 0.5)
    points(ts$datetime, ts$p3, pch = 16, col = "darkgreen", cex = 0.5)
    points(ts$datetime, ts$st, pch = 16)
    points(ts$datetime[ts$fl %in% c("G", "E")], ts$st[ts$fl %in% c("G", "E")], pch = 16, col = "orange")
    par(mar = c(0, 0, 0, 0))
    plot.new()
    legend("center", ncol = 5, c(paste(gages$station_name_web[i], "measured"), paste(gages$station_name_web[i], "estimated"), paste("P1:", pred$station_name_web[1]), paste("P2:", pred$station_name_web[2]), paste("P3:", pred$station_name_web[3])), pch = 16, col = c("black", "orange", "blue", "red", "darkgreen"))
    dev.off()
  }
}
setwd("..")
