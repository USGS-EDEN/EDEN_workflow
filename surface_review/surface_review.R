# Required libraries. If not present, run:
# install.packages("ncdf4")
# install.packages("RMySQL")
# install.packages("abind")
library (ncdf4)
library (RMySQL)
library (abind)

# Connect to database, list of gages for which to acquire data
try (setwd("./surface_review"), silent = T)
source ("../usr_pwd.R")
con <- dbConnect(MySQL(), user = usr, password = pword, dbname = "eden_new", host = "stpweb1-dmz.er.usgs.gov")

# Vectors of plotting colors
stage.col <- c("#9E0142", "#A40843", "#AB1045", "#B21746", "#B91F48", "#C0274A", "#C72E4B", "#CE364D", "#D53E4F", "#D8434D", "#DC494C", "#E04F4A", "#E45549", "#E85B47", "#EC6146", "#F06744", "#F46D43", "#F57546", "#F67D4A", "#F7854E", "#F88D52", "#F99555", "#FA9D59", "#FBA55D", "#FDAE61", "#FDB466", "#FDBA6B", "#FDC070", "#FDC776", "#FDCD7B", "#FDD380", "#FDD985", "#FEE08B", "#FEE391", "#FEE798", "#FEEB9E", "#FEEFA5", "#FEF3AB", "#FEF7B2", "#FEFBB8", "#FFFFBF", "#FBFDBA", "#F8FCB5", "#F5FBB0", "#F2FAAB", "#EFF8A6", "#ECF7A1", "#E9F69C", "#E6F598", "#DEF299", "#D7EF9B", "#CFEC9C", "#C8E99E", "#C1E69F", "#B9E3A1", "#B2E0A2", "#ABDDA4", "#A2D9A4", "#99D6A4", "#91D2A4", "#88CFA4", "#7FCCA4", "#77C8A4", "#6EC5A4", "#66C2A5", "#5FBAA8", "#59B3AB", "#52ACAE", "#4CA5B1", "#459DB4", "#3F96B7", "#388FBA", "#3288BD", "#3780B9", "#3D79B6", "#4272B2", "#486BAF", "#4D64AC", "#535DA8", "#5856A5", "#5E4FA2")
depth.col <- c("#9ECAE1", "#9AC8E0", "#97C6DF", "#94C4DE", "#91C3DE", "#8EC1DD", "#8ABFDC", "#87BDDC", "#84BCDB", "#81BADA", "#7EB8DA", "#7AB6D9", "#77B5D8", "#74B3D8", "#71B1D7", "#6EAFD6", "#6BAED6", "#68ACD5", "#65AAD4", "#63A8D3", "#60A7D2", "#5EA5D1", "#5BA3D0", "#59A1CF", "#56A0CE", "#539ECD", "#519CCC", "#4E9ACB", "#4C99CA", "#4997C9", "#4795C8", "#4493C7", "#4292C6", "#3F8FC4", "#3D8DC3", "#3B8BC2", "#3989C1", "#3787C0", "#3585BF", "#3383BE", "#3181BD", "#2F7FBC", "#2D7DBB", "#2B7BBA", "#2979B9", "#2777B8", "#2575B7", "#2373B6", "#2171B5", "#1F6FB3", "#1D6DB1", "#1C6BB0", "#1A69AE", "#1967AD", "#1765AB", "#1663AA", "#1461A8", "#125FA6", "#115DA5", "#0F5BA3", "#0E59A2", "#0C57A0", "#0B559F", "#09539D", "#08519C", "#084E98", "#084C95", "#084A92", "#08488F", "#08468C", "#084489", "#084286", "#084083", "#083E80", "#083C7D", "#083A7A", "#083877", "#083674", "#083471", "#08326E", "#08306B")

# Initialize vars
time <- stage <- depth <- NULL

# Place WL and depth .nc files to be reviewed in working directory
file_surf <- list.files("./surfaces", "^[0-9]{4}_q[1-4].nc$")
depth_surf <- list.files("./surfaces", "^d[0-9]{4}_q[1-4].nc$")

# Loop through potentially multiple surfaces, create data arrays
for(i in 1:length(file_surf)) {
  # WL .nc's
  surf.nc <- nc_open(paste0("./surfaces", "/", file_surf[i]))

  # Set up X and Y vectors
  if(i==1) { x <- ncvar_get(surf.nc, "x"); y <- ncvar_get(surf.nc, "y") }
  
  # Stage array
  stage <- abind(stage, ncvar_get(surf.nc, "stage"))
  
  # Time vector
  t <- ncvar_get(surf.nc, "time")
  time <- as.Date(c(time, as.Date(surf.nc$dim$time$units, format="days since %Y-%m-%dT%H:%M:%SZ") + t, recursive = T), origin = "1970/1/1")
  nc_close(surf.nc)

  # Depth .nc's
  surf.nc <- nc_open(paste0("./surfaces", "/", depth_surf[i]))
  
  # Depth array
  depth <- abind(depth, ncvar_get(surf.nc, "depth"))
  nc_close(surf.nc)
}

## Create gage hydrographs comparing EDENdb to .nc pixel values
gages <- dbGetQuery(con,"select station_name_web, utm_easting, utm_northing, convert_to_navd88_feet as conv, vertical_conversion, edenmaster_new from station, station_datum where station.station_id = station_datum.station_id and station_name_web != 'Alligator_Creek' and station_name_web != 'Barron_River' and station_name_web != 'East_Side_Creek' and station_name_web != 'G-3777' and station_name_web != 'Manatee_Bay_Creek' and station_name_web != 'Raulerson_Brothers_Canal' group by station_name_web")

# Loop through gages
for (j in 1:dim(gages)[1]) {
  # Closest .nc pixel to gage UTM
  xval <- which.min(abs(x - gages$utm_easting[j]))
  yval <- which.min(abs(y - gages$utm_northing[j]))
  
  # Data values of closest pixel
  wl <- stage[xval, yval, ]
  d <- depth[xval, yval, ]
  
  # EDENdb data values
  db <- dbGetQuery(con, paste0("select datetime, `stage_", gages$station_name_web[j], "` + ", gages$conv[j], " as stage from stage where datetime >= ", format(time[1] - 1,"%Y%m%d"), "000000 and datetime < ", format(time[length(time)] + 2, "%Y%m%d"), "000000 order by datetime"))
  # Convert timestamps
  db$datetime <- as.POSIXct(db$datetime, tz = "EST", format = "%Y-%m-%d %H:%M:%S")
  # Convert to centimeters
  db$stage <- db$stage * 12 * 2.54
  # Initialize WL and depth vectors
  db$wl <- db$d <- NA
  # .nc's are daily, EDENdb are hourly; assign surface values to noon timestamps
  db$wl[seq(37, dim(db)[1] - 24, 24)] <- wl
  db$d[seq(37, dim(db)[1] - 24, 24)] <- d
  # Get y-axis data range
  range <- range(db$stage, db$wl, na.rm = T)
  if (is.infinite(range[1]) | is.infinite(range[2])) range <- c(-1, 1)
  # Main title
  main <- ifelse(gages$edenmaster_new[j] == 1, paste(gages$station_name_web[j], "(surfacing)"), paste(gages$station_name_web[j], "(non-surfacing)"))
  jpeg(paste0("./surf_edendb_graphs/", gages$station_name_web[j], ".jpg"), width = 1200, height = 800, units = "px", pointsize = 12, quality = 100, bg = "white", type = "quartz")
  par(mar = c(5, 4, 4, 5) + .1)
  plot(db$datetime, db$stage, type = "l", ylim = range, main = main, xlab = "Date", ylab = "Water level (cm NAVD88)")
  points(db$datetime, db$wl, col = "blue")
  # Plot depth data, if present
  if (!is.na(db$d[37])) {
    par(new = T)
    plot(db$datetime, db$d, col = "darkblue", pch = 16, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    axis(4)
    mtext("Depth (cm)", side = 4, line = 3)
  }
  legend("topleft", c("EDENdb hourly values", "EDEN WL surface", "EDEN depth surface"), col = c("black", "blue", "darkblue"), lty = c(1, 0, 0), pch = c(NA, 1, 16))
  dev.off()
  jpeg(paste0("./surf_edendb_thumbs/", gages$station_name_web[j], ".jpg"), width = 240, height = 160, units = "px", pointsize = 12, quality = 100, bg = "white", type = "quartz")
  par(mar = c(0, 0, 0, 0))
  plot(db$datetime, db$stage, type = "l", ylim = range)
  points(db$datetime, db$wl, col = "blue")
  if (!is.na(db$d[37])) {
    par(new = T)
    plot(db$datetime, db$d, col = "darkblue", pch = 16)
  }
  dev.off()
}

# Establish plotting ranges
r1 <- range(stage, na.rm = T)
r1[1] <- floor(r1[1])
r1[2] <- ceiling(r1[2])
r2 <- range(depth, na.rm = T)
r2[1] <- floor(r2[1])
r2[2] <- ceiling(r2[2])
print(r1)
print(r2)

# Plot WL and depth PNGs with 3cm contours
for (i in 1:length(time)) {
  png(paste0("./wl_contours/day", sprintf("%04d", i - 1), ".png"), width = 1228, height = 1724, units = "px", pointsize = 12, bg = "transparent", type = "quartz")
  par(mar = c(0, 0, 0, 0))
  image(x, y, stage[, , i], col = stage.col, breaks = seq(r1[1], r1[2], length.out = 82), axes = F, asp = 1, xlab = "", ylab = "")
  text(x[1], y[10], as.Date(time[i]), pos = 4)
  # 3cm contours "by = 3"
  contour(x, y, stage[, , i], levels = seq(r1[1], r1[2], by = 3), lwd = c(1, 0.5), add = T)
  dev.off()

  png(paste0("./depth_contours/day", sprintf("%04d", i - 1), ".png"), width = 1228, height = 1724, units = "px", pointsize = 12, bg = "transparent", type = "quartz")
  par(mar = c(0, 0, 0, 0))
  image(x, y, depth[, , i], col = depth.col, breaks = seq(r2[1], r2[2], length.out = 82), axes = F, asp = 1, xlab = "", ylab = "")
  text(x[1], y[10], as.Date(time[i]), pos = 4)
  contour(x, y, depth[, , i], levels = seq(r2[1], r2[2], by = 3), lwd = c(1, 0.5), add = T)
  dev.off()
}
