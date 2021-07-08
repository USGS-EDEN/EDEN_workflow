library (RMySQL)
library (RCurl)
library(RColorBrewer)

try (setwd("./EDENdb_upload"), silent = T)
source ("../usr_pwd.R")
# Connect to database and download data file
con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "eden", host = "igsafpesgsz03.er.usgs.gov")

gages <- dbGetQuery(con, "select station_name_web, station_name, station.station_id, vertical_conversion as conv, operating_agency_id, usgs_nwis_id, param, ts_id from station, station_datum where station.station_id = station_datum.station_id and station_name_web != 'Alligator_Creek' and station_name_web != 'East_Side_Creek' and station_name_web != 'G-3777' and station_name_web != 'Manatee_Bay_Creek' and station_name_web != 'Raulerson_Brothers_Canal' and station_name_web != 'Barron_River' group by station_name_web")
cols <- brewer.pal(8, "Accent")
for (i in 1:dim(gages)[1]) {
  pred <- dbGetQuery(con, paste0("select station_name_web, predictor_rank from station_gapfill, station where station_gapfill.predictor_station_id = station.station_id and station_gapfill.station_id = ", gages$station_id[i], " order by predictor_rank"))
  query <- paste0("select datetime, `flag_", gages$station_name_web[i], "` as fl, `stage_", gages$station_name_web[i], "` as st")
  if (dim(pred)[1] > 0) query <- paste0(query, ", `stage_", pred$station_name_web[1], "` as p1")
  if (dim(pred)[1] > 1) query <- paste0(query, ", `stage_", pred$station_name_web[2], "` as p2")
  if (dim(pred)[1] > 2) query <- paste0(query, ", `stage_", pred$station_name_web[3], "` as p3")
  query <- paste0(query, " from stage where datetime >= 20201229000000 and datetime < 20210402000000 order by datetime")
  ts <- dbGetQuery(con, query)
  ts$datetime <- as.POSIXct(ts$datetime, tz = "EST", format = "%Y-%m-%d %H:%M:%S")
  if (any(!is.na(ts$st))) {
    yran <- range(ts[, 3:dim(ts)[2]], na.rm = T)
    png(paste0("./images/", gages$station_name_web[i], ".png"), width = 1200, height = 800, type = "quartz")
    layout(rbind(1, 2), heights = c(14, 1))
    plot(ts$datetime, ts$st, type = "n", ylim = yran, main = paste("WY2021Q2 for", gages$station_name_web[i]), xlab = "Date", ylab = "Stage (ft.)")
    abline(v = ts$datetime[ts$datetime == "2020-12-31 23:00:00"], lwd = 3, col = "darkred")
    abline(v = ts$datetime[ts$datetime == "2021-04-01 01:00:00"], lwd = 3, col = "darkred")
    points(ts$datetime, ts$p1, pch = 16, col = cols[1], cex = 0.5)
    points(ts$datetime, ts$p2, pch = 16, col = cols[2], cex = 0.5)
    points(ts$datetime, ts$p3, pch = 16, col = cols[3], cex = 0.5)
    if (gages$operating_agency_id[i] == 4 & !is.na(gages$usgs_nwis_id[i])) {
      print(gages$station_name_web[i])
      url <- paste0("https://waterservices.usgs.gov/nwis/iv/?format=rdb&sites=", gages$usgs_nwis_id[i], "&startDT=2020-12-29&endDT=2021-04-02&parameterCd=", gages$param[i], "&access=3")
      u <- try (read.table(url, header = T, sep = "\t", colClasses = "character"))
      u <- u[2:dim(u)[1], ]
      if (dim(u)[1] > 2) {
        dd_col <- if (!is.na(gages$ts_id[i])) which(grepl(gages$ts_id[i], names(u)))[1] else which(grepl(gages$param[i], names(u)))[1]
        u$datetime <- as.POSIXct(u$datetime, "EST", format = "%Y-%m-%d %H:%M")
        u$datetime <- as.POSIXct(ifelse(u$tz_cd == "EDT", u$datetime - 3600, u$datetime), "EST", origin = "1970-01-01")
        if (any(grepl("_Eqp|_Dry|_Fld", u[, dd_col])))
          u <- u[-which(grepl("_Eqp|_Dry|_Fld", u[, dd_col]))]
        points(u$datetime, u[, dd_col], pch = 16, col = cols[4], cex = 2)
      } else print(paste(gages$station_name_web[i], ": no NWIS data"))
    }
    points(ts$datetime, ts$st, pch = 16, col = cols[8])
    points(ts$datetime[ts$fl %in% c("G", "E")], ts$st[ts$fl %in% c("G", "E")], pch = 16, col = cols[7])
    par(mar = c(0, 0, 0, 0))
    plot.new()
    legend("center", ncol = 6, c(paste(gages$station_name_web[i], "measured"), paste(gages$station_name_web[i], "estimated"), "NWIS", paste("P1:", pred$station_name_web[1]), paste("P2:", pred$station_name_web[2]), paste("P3:", pred$station_name_web[3])), pch = 16, col = cols[c(8, 7, 4, 1, 2, 3)])
    dev.off()
  }
}
setwd("..")
