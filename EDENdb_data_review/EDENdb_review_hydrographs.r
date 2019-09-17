# -------------
# Bryan McCloskey
# bmccloskey@usgs.gov
# St. Petersburg Coastal and Marine Science Center
# US Geological Survey
#
# 07/18/2019
#--------------

print("These libraries must be installed: RMySQL")
# Required libraries. If not present, run:
# install.packages("RMySQL")
library (RMySQL)

try (setwd("./EDENdb_data_review"), silent = T)
source ("../usr_pwd.R")
# Connect to database, list of gages for which to acquire data
con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "eden_new", host = "stpweb1-dmz.er.usgs.gov")

gages <- dbGetQuery(con, "describe stage")
gages <- substring(gages[seq(2, dim(gages)[1], 2), "Field"], 7)
days <- c(Sys.Date() - 122, Sys.Date())
# 15-minute and 6-minute timestamps
start <- strptime(paste(days[1], "00:00:00"), "%Y-%m-%d %H:%M:%S", "EST")
end <- strptime(paste(days[2], "23:00:00"), "%Y-%m-%d %H:%M:%S", "EST")
range <- seq.POSIXt(start, end, "1 hour")

nearest <- dbGetQuery(con, "select station_id, station_name_web, utm_easting as x, utm_northing as y from station")

# Plot of EDENdb values w/ predictor stations
pdf("./images/EDENdb_review_data_w_predictors.pdf", 14, 12)
for (i in 1:length(gages)) {
  t <- dbGetQuery(con, paste0("select datetime, `stage_", gages[i], "` as stage, `flag_", gages[i], "` as flag from stage where datetime >= '", start, "' and datetime <= '", end, "' order by datetime"))
  t$datetime <- as.POSIXct(t$datetime, tz = "EST", format = "%Y-%m-%d %H:%M:%S")
  xy <- dbGetQuery(con, paste0("select utm_easting as x, utm_northing as y from station where station_name_web = '", gages[i], "'"))
  n <- sort( (xy$x - nearest$x) ^ 2 + (xy$y - nearest$y) ^ 2 )[2:4] # Three nearest
  n <- nearest[which(((xy$x - nearest$x) ^ 2 + (xy$y - nearest$y) ^ 2) %in% n), ]
  
  title <- paste0("Unit Values for previous four months (", days[1], " - ", days[2], ") for Station ", gages[i], " with three nearest gages")
  plot(t$datetime, t$stage, type = "p", pch = 20, col = "black", main = title, xlab = "Date", ylab = "Feet above NAVD88/NVGD29")
  legend <- pch <- col <- ""
  legend[1] <- gages[i]
  pch[1] <- 20
  col[1] <- "black"
  for (j in 1:3) {
    u <- dbGetQuery(con, paste0("select datetime, `stage_", n$station_name_web[j], "` as stage, `flag_", n$station_name_web[j], "` as flag from stage where datetime >= '", start, "' and datetime <= '", end, "' order by datetime"))
    u$datetime <- as.POSIXct(u$datetime, tz = "EST", format = "%Y-%m-%d %H:%M:%S")
    lines(u$datetime, u$stage, col = palette()[j + 1] ,pch = ".", type = "p")
    legend[j + 1] <- n$station_name_web[j]
    pch[j + 1] <- 20
    col[j + 1] <- palette()[j + 1]
  }
  legend(x = "bottomleft", legend = legend, pch = as.numeric(pch), col = col)
}
dev.off()

