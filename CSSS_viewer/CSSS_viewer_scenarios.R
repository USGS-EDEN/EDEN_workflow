library (ncdf4)
try (setwd("./CSSS_viewer"), silent = T)

fls <- list.files("./input/", "^depth_")[c(2, 3, 8)]
sc <- c("ALTN2", "ALTO", "ECB19RR")
s.nc <- nc_open(paste0("./input/", fls[1]))
x <- ncvar_get(s.nc, "x")
y <- ncvar_get(s.nc, "y")
y2 <- rev(y)
t <- ncvar_get(s.nc, "t")
time <- as.Date(s.nc$dim$t$units, format = "days since %Y-%m-%dT%H:%M:%S +0000") + t
s <- ncvar_get(s.nc, "depth", c(1, 1, 1), c(-1, -1, 1))
nc_close(s.nc)

# Define subareas
pixels <- read.csv("./input/CSSS_EDEN_subpop_key.csv")
pixels <- rbind(pixels, read.csv("./input/CSSS_subpop_A1,A2.csv"))
sub <- c("AX", "A", "A1", "A2", "B", "C", "D", "E", "F")
grid_sub <- array(NA, c(length(x), length(y), length(sub))) # 352 x 477 x 8 grid of all subareas
for (i in 1:length(sub)) {
  if (sub[i] == "AX") {
    pixels_sub <- pixels[which(pixels$SubPopulat == "A" | pixels$SubPopulat == "A1" | pixels$SubPopulat == "A2"), ] 
  } else {
    pixels_sub <- pixels[which(pixels$SubPopulat == sub[i]),]
  }
  for (j in 1:dim(pixels_sub)[1]) {
    pixels_sub$grid_x[j] <- which(x == pixels_sub$ET_X[j])
    pixels_sub$grid_y[j] <- which(y == pixels_sub$ET_Y[j])
    if (!is.na(s[pixels_sub$grid_x[j], pixels_sub$grid_y[j]]))
      grid_sub[pixels_sub$grid_x[j], pixels_sub$grid_y[j], i] <- s[pixels_sub$grid_x[j], pixels_sub$grid_y[j]]
  }
  assign(paste0("xmin_", sub[i]), which(rowSums(abs(grid_sub[, , i]), na.rm = T) > 0)[1])
  assign(paste0("xmax_", sub[i]), tail(which(rowSums(abs(grid_sub[, , i]), na.rm = T) > 0), 1))
  assign(paste0("ymin_", sub[i]), which(colSums(abs(grid_sub[, , i]), na.rm = T) > 0)[1])
  assign(paste0("ymax_", sub[i]), tail(which(colSums(abs(grid_sub[, , i]), na.rm = T) > 0), 1))
  assign(paste0("grid_", sub[i]), grid_sub[get(paste0("xmin_", sub[i])):get(paste0("xmax_", sub[i])), get(paste0("ymin_", sub[i])):get(paste0("ymax_", sub[i])), i]) # smaller one-layer grids of each max extent
}
xmin_AX <- xmin_A <- xmin_A + max(which(rowSums(grid_A, na.rm = T) == 0))  # Remove discontinuous western piece of A
grid_A <- grid_A[(max(which(rowSums(grid_A, na.rm = T) == 0)) + 1):dim(grid_A)[1], ]
grid_AX <- grid_AX[(max(which(rowSums(grid_AX, na.rm = T) == 0)) + 1):dim(grid_AX)[1], ]

for (i in 1:length(fls)) {
  # Build depth, dry arrays
  print(paste("Building subareas for", fls[i]))
  s.nc <- nc_open(paste0("./input/", fls[i]))
  for (j in 1:length(sub)) {
    cntx <- get(paste0("xmax_", sub[j])) - get(paste0("xmin_", sub[j])) + 1
    cnty <- get(paste0("ymax_", sub[j])) - get(paste0("ymin_", sub[j])) + 1
    s <- ncvar_get(s.nc, "depth", c(get(paste0("xmin_", sub[j])), get(paste0("ymin_", sub[j])), 1), c(cntx, cnty, -1))
    s <- s / 10
    s[is.na(get(paste0("grid_", sub[j])))] <- NA
    assign(paste0("depth_", sub[j], "_", sc[i]), s)
    assign(paste0("dry_", sub[j], "_", sc[i]), ifelse(get(paste0("depth_", sub[j], "_", sc[i])) <= 0, 1, 0)) # dry == 1
  }
  nc_close(s.nc)
  
  # Build statistics data.frame
  df <- data.frame(date = as.Date(time))
  for (j in 1:length(sub)) {
    print(paste("Calculating stats for", sub[j], sc[i]))
    dim <- sum(!is.na(get(paste0("grid_", sub[j])))) # number of cells in subarea
    for (k in 1:dim(df)[1]) {
      df[k, paste0("per_dry_", sub[j], "_", sc[i])] <- round(sum(get(paste0("dry_", sub[j], "_", sc[i]))[, , k], na.rm = T) / dim, 3) * 100
      df[k, paste0("per_dry17cm_", sub[j], "_", sc[i])] <- round(sum(ifelse(get(paste0("depth_", sub[j], "_", sc[i]))[, , k] <= 17, 1, 0), na.rm = T) / dim, 3) * 100
    }
    for (k in 40:dim(df)[1]) {
      dry_40d <- ifelse(rowSums(get(paste0("dry_", sub[j], "_", sc[i]))[, , (k - 39):k], dims = 2) == 40, 1, 0) # all dry (== 1) days, then 1
      df[k, paste0("per_dry40d_", sub[j], "_", sc[i])] <- round((sum(dry_40d, na.rm = T)) / dim, 3) * 100
    }
    for (k in 90:dim(df)[1]) {
      dry_90d <- ifelse(rowSums(get(paste0("dry_", sub[j], "_", sc[i]))[, , (k - 89):k], dims = 2) == 90, 1, 0) # all dry (== 1) days, then 1
      df[k, paste0("per_dry90d_", sub[j], "_", sc[i])] <- round((sum(dry_90d, na.rm = T)) / dim, 3) * 100
    }
    for (k in 1:dim(df)[1]) {
      df[k, paste0("depth_mean_", sub[j], "_", sc[i])] <- round(mean(get(paste0("depth_", sub[j], "_", sc[i]))[, , k], na.rm = T), 1)
      df[k, paste0("depth_sd_", sub[j], "_", sc[i])] <- round(sd(get(paste0("depth_", sub[j], "_", sc[i]))[, , k], na.rm = T), 1)
    }
  }
  
  # Stats por .js
  write(paste0("var stats_", sc[i], " = {"), paste0("./output/por_stats_", sc[i], ".js"))
  for (j in 1:length(df$date)) {
    line <- paste0(format(df$date[j], "'a%Y%m%d' : ['"), paste(df[j, 2:dim(df)[2]], collapse = "', '"), "']")
    if (j != length(df$date)) # no trailing comma
      line <- paste0(line, ",")
    write(line, paste0("./output/por_stats_", sc[i], ".js"), append = T)
  }
  write("}", paste0("./output/por_stats_", sc[i], ".js"), append = T)
  
  # Stats por .csv
  tmp <- rbind(paste("area", sub, "% dry"), paste("area", sub, "% WD <= 17cm"), paste("area", sub, "% dry >= 40 days"), paste("area", sub, "% dry >= 90 days"), paste("mean cm water depth area", sub), paste("water depth standard deviation cm area", sub))
  cols <- c("Date", tmp)
  write.table(df, paste0("./output/CSSS_subarea_stats_", sc[i], ".csv"), quote = F, sep = ",", row.names = F, col.names = cols)
  zip(paste0("./output/CSSS_subarea_stats_", sc[i], ".csv.zip"), paste0("./output/CSSS_subarea_stats_", sc[i], ".csv"), "-j9X")
  
  # Generate depth surface images
  col <- c("deepskyblue", "steelblue", "blue3", "blue4")
  s.nc <- nc_open(paste0("./input/", fls[i]))
  for (j in 1:length(time)) {
    if (!dir.exists(paste0("./images/", format(time[j], '%Y'), "_", sc[i]))) dir.create(paste0("./images/", format(time[j], '%Y'), "_", sc[i]))
    s <- ncvar_get(s.nc, "depth", c(1, 1, t[j] + 1), c(-1, -1, 1))
    s <- s[, 477:1] / 10
    f <- paste0(sprintf("trans%04d", as.POSIXlt(time[j])$yday), ".png")
    png(paste0("./images/", format(time[j], '%Y'), "_", sc[i], "/", f), width = 754, height = 1015, bg = "transparent", type = "quartz")
    par(mar = c(0, 0, 0, 0))
    image(x, y2, s, col = col, breaks = c(0, 17, 30, 46, 600), axes = F, asp = 1)
    text(x[58], y2[47], time[j], pos = 4)
    # points(x[c(58, 58, 344, 344)], y2[c(38, 442, 38, 442)], pch = 16) #positioning markers for Leaflet map
    dev.off()
  }
  nc_close(s.nc)
}

# Calc subarea annual stats
year <- data.frame(year = 1965:2005)
start_nest <- which(as.POSIXlt(time)$mon == 2 & as.POSIXlt(time)$mday == 1)
end_nest<-which(as.POSIXlt(time)$mon == 6 & as.POSIXlt(time)$mday == 15)
start_year <- which(as.POSIXlt(time)$yday == 0)
end_year <- which(as.POSIXlt(time)$mon == 11 & as.POSIXlt(time)$mday == 31)
for (m in 1:length(fls)) {
  for (i in 1:length(sub)) {
    print(sub[i])
    su <- paste0(sub[i], "_", sc[m])
    dim <- sum(!is.na(get(paste0("grid_", sub[i]))))
    n40 <- n90 <- matrix(NA, dim(get(paste0("dry_", su)))[1], dim(get(paste0("dry_", su)))[2])
    for (j in 1:length(year$year)) {
      for (k in 1:dim(get(paste0("dry_", su)))[1])
        for (l in 1:dim(get(paste0("dry_", su)))[2]) {
          r <- rle(get(paste0("dry_", su))[k, l, start_nest[j]:end_nest[j]])
          n40[k, l] <- ifelse(any(r$lengths[r$values == 1] >= 40), 1, 0)
          n90[k, l] <- ifelse(any(r$lengths[r$values == 1] >= 90), 1, 0)
        }
      year[j, paste0("nest40_", su)] <- round((sum(n40, na.rm = T)) / dim, 3) * 100
      year[j, paste0("nest90_", su)] <- round((sum(n90, na.rm = T)) / dim, 3) * 100
    }
    assign(paste0("wet_", su), ifelse(get(paste0("dry_", su)) == 0, 1, 0)) #invert; wet == 1
    for (j in 1:length(year$year)) {
      rs <- rowSums(get(paste0("wet_", su))[, , start_year[j]:end_year[j]], dims = 2)
      year[j, paste0("year0_to_89_", su)] <- round((sum(ifelse(rs <= 89, 1, 0), na.rm = T)) / dim, 3) * 100
      year[j, paste0("year90_to_210_", su)] <- round((sum(ifelse(rs >= 90 & rs <= 210, 1, 0), na.rm = T)) / dim, 3) * 100
      year[j, paste0("year211_", su)] <- round((sum(ifelse(rs >= 211, 1, 0), na.rm = T)) / dim, 3) * 100
    }
    for (j in 4:length(year$year)) {
      fy <- rowSums(get(paste0("wet_", su))[, , start_year[j - 3]:end_year[j]], dims = 2) / 4
      year[j, paste0("four_hyd_mean_", su)] <- round(mean(fy, na.rm = T), 1)
      fy2 <- fy[fy >= 90 & fy <= 210]
      year[j, paste0("four_hyd_per_", su)] <- round(length(fy2[!is.na(fy2)]) * 100 / dim, 1)
    }
    year[, paste0("four_hyd_sd_", su)] <- NA # Initialize SD column
  }
  
  h4 <- array(NA, c(dim(grid_sub)[1], dim(grid_sub)[2], length(year$year) - 3))
  for (i in (length(year$year) - 3):1) {
    print(year$year[i] + 4)
    yr1 <- which(as.POSIXlt(time)$year + 1900 == year$year[i] & as.POSIXlt(time)$mon == 0 & as.POSIXlt(time)$mday == 1)
    subt <- time[yr1:(yr1 + 1460)]
    h1 <- array(NA, c(dim(grid_sub)[1], dim(grid_sub)[2], 4))
    sd <- qual <- matrix(NA, dim(grid_sub)[1], dim(grid_sub)[2])
    s.nc <- nc_open(paste0("./input/", fls[m]))
    s <- ncvar_get(s.nc, "depth", c(1, 1, yr1), c(-1, -1, 1461))
    nc_close(s.nc)
    wet <- ifelse(s < 0, 0, 1)
    h4[, , i] <- rowSums(wet, dims = 2) / 4
    st <- c(which(as.POSIXlt(subt)$yday == 0))
    en <- c(which(as.POSIXlt(subt)$mday == 31 & as.POSIXlt(subt)$mon == 11))
    for (j in 1:4)
      h1[, , j] <- rowSums(wet[, , st[j]:en[j]], dims = 2)
    for (k in 1:dim(h1)[1])
      for (l in 1:dim(h1)[2])
        sd[k, l] <- sd(h1[k, l, ])
    for (k in 1:dim(h1)[1])
      for (l in 1:dim(h1)[2])
        qual[k, l] <- sum(h1[k, l, ] >= 90 & h1[k, l, ] >= 210)
    
    for (j in 1:length(sub)) {
      print(sub[j])
      sub_sd <- sd[get(paste0("xmin_", sub[j])):get(paste0("xmax_", sub[j])), get(paste0("ymin_", sub[j])):get(paste0("ymax_", sub[j]))]
      sub_sd[is.na(get(paste0("grid_", sub[j])))] <- NA
      year[i + 3, paste0("four_hyd_sd_", sub[j], "_", sc[m])] <- round(mean(sub_sd, na.rm = T), 1)
    }
    
    # Create four-year hydroperiod maps
    col <- rgb(c(230, 245, 18, 0, 24), c(76, 217, 194, 255, 116), c(0, 1, 73, 255, 205), maxColorValue = 255)
    if (!dir.exists(paste0("./images/hydrop_", sc[m]))) dir.create(paste0("./images/hydrop_", sc[m]))
    png(paste0("./images/hydrop_", sc[m], "/four_year_hydroperiod_", year$year[i] + 4, "_", sc[m], ".png"), width = 754, height = 1015, bg = "transparent", type = "quartz")
    par(mar = c(0, 0, 0, 0))
    image(x, y2, h4[, 477:1, i], col = col, breaks = c(0, 138.2, 163, 200.3, 220.3, 400), axes = F, asp = 1)
    text(x[58], y2[47], year$year[i] + 4, pos = 4)
    dev.off()
    png(paste0("./images/hydrop_", sc[m], "/four_year_hydroperiod_", year$year[i] + 4, "_mask65_", sc[m], ".png"), width = 754, height = 1015, bg = "transparent", type = "quartz")
    par(mar = c(0, 0, 0, 0))
    image(x, y2, h4[, 477:1, i], col = col, breaks = c(0, 138.2, 163, 200.3, 220.3, 400), axes = F, asp = 1)
    mask <- ifelse(sd >= 56.8, 1, NA)
    image(x, y2, mask, col = rgb(0, 0, 0, 0.65), add = T)
    text(x[58], y2[47], year$year[i] + 4, pos = 4)
    dev.off()
    png(paste0("./images/hydrop_", sc[m], "/four_year_hydroperiod_", year$year[i] + 4, "_sd_", sc[m], ".png"), width = 754, height = 1015, bg = "transparent", type = "quartz")
    par(mar = c(0, 0, 0, 0))
    image(x, y2, sd[, 477:1], col = rev(col), breaks = c(-1, 26.7, 33.7, 46.7, 56.8, 400), axes = F, asp = 1)
    text(x[58], y2[47], year$year[i] + 4, pos = 4)
    dev.off()
  }
}

for (a in 1:length(fls)) {
  su <- sc[a]
  if (!dir.exists(paste0("./images/graphs_", su))) dir.create(paste0("./images/graphs_", su))
  for (i in 1:length(sub)) {
    j <- grep(paste0("nest40_", sub[i], "_", su), names(year))
    png(paste0("./images/graphs_", su, "/nest40_", sub[i], "_", su, ".png"), width = 800, height = 400, type = "quartz")
    mp <- barplot(year[, j], col = "burlywood4", ylim = c(0, 100), main = paste0(su, ": ", sub[i], " % area with 40 consecutive dry days"), xaxt = "n")
    text(x = mp[seq(1, length(mp), by = 2)] - .5, y = -5, year$year[seq(1, length(mp), by = 2)], srt = 45, xpd = T)
    rmean <- (year[1:(length(year$year) - 3), j] + year[2:(length(year$year) - 2), j] + year[3:(length(year$year) - 1), j] + year[4:(length(year$year)), j]) / 4
    lines(mp[4:length(mp)], rmean, lwd = 3)
    legend(8, -11, c("annual %", "4yr moving avg."), col = c("burlywood4", "black"), lty = c(0, 1), lwd = c(NA, 3), pch = c(15, NA), pt.cex = 2, xpd = T, bty = "n")
    dev.off()
    k <- grep(paste0("nest90_", sub[i], "_", su), names(year))
    png(paste0("./images/graphs_", su, "/nest90_", sub[i], "_", su, ".png"), width = 800, height = 400, type = "quartz")
    mp <- barplot(year[, k], col = "burlywood4", ylim = c(0, 100), main = paste0(su, ": ", sub[i], " % area with 90 consecutive dry days"), xaxt = "n")
    text(x = mp[seq(1, length(mp), by = 2)] - .5, y = -5, year$year[seq(1, length(mp), by = 2)], srt = 45, xpd = T)
    rmean <- (year[1:(length(year$year) - 3), k] + year[2:(length(year$year) - 2), k] + year[3:(length(year$year) - 1), k] + year[4:(length(year$year)), k]) / 4
    lines(mp[4:length(mp)], rmean, lwd = 3)
    legend(8, -11, c("annual %", "4yr moving avg."), col = c("burlywood4", "black"), lty = c(0, 1), lwd = c(NA, 3), pch = c(15, NA), pt.cex = 2, xpd = T, bty = "n")
    dev.off()
    l <- grep(paste0("year0_to_89_", sub[i], "_", su), names(year))
    png(paste0("./images/graphs_", su, "/year0_to_89_", sub[i], "_", su, ".png"), width = 800, height = 400, type = "quartz")
    mp <- barplot(year[, l], col = "lightblue", ylim = c(0, 100), main = paste0(su, ": ", sub[i], " % area with 0 to 89 non-consecutive hydroperiod days"), xaxt = "n")
    text(x = mp[seq(1, length(mp), by = 2)] - .5, y = -5, year$year[seq(1, length(mp), by = 2)], srt = 45, xpd = T)
    rmean <- (year[1:(length(year$year) - 3), l] + year[2:(length(year$year) - 2), l] + year[3:(length(year$year) - 1), l] + year[4:(length(year$year)), l]) / 4
    lines(mp[4:length(mp)], rmean, lwd = 3)
    legend(8, -11, c("annual %", "4yr moving avg."), col = c("lightblue", "black"), lty = c(0, 1), lwd = c(NA, 3), pch = c(15, NA), pt.cex = 2, xpd = T, bty = "n")
    dev.off()
    m <- grep(paste0("year90_to_210_", sub[i], "_", su), names(year))
    png(paste0("./images/graphs_", su, "/year90_to_210_", sub[i], "_", su, ".png"), width = 800, height = 400, type = "quartz")
    mp <- barplot(year[, m], col = "lightblue", ylim = c(0, 100), main = paste0(su, ": ", sub[i], " % area with 90 to 210 non-consecutive hydroperiod days"), xaxt = "n")
    text(x = mp[seq(1, length(mp), by = 2)] - .5, y = -5, year$year[seq(1, length(mp), by = 2)], srt = 45, xpd = T)
    rmean <- (year[1:(length(year$year) - 3), m] + year[2:(length(year$year) - 2), m] + year[3:(length(year$year) - 1), m] + year[4:(length(year$year)), m]) / 4
    lines(mp[4:length(mp)], rmean, lwd = 3)
    legend(8, -11, c("annual %", "4yr moving avg."), col = c("lightblue", "black"), lty = c(0, 1), lwd = c(NA, 3), pch = c(15, NA), pt.cex = 2, xpd = T, bty = "n")
    dev.off()
    n <- grep(paste0("year211_", sub[i], "_", su), names(year))
    png(paste0("./images/graphs_", su, "/year211_", sub[i], "_", su, ".png"), width = 800, height = 400, type = "quartz")
    mp <- barplot(year[, n], col = "lightblue", ylim = c(0, 100), main = paste0(su, ": ", sub[i], " % area with ≥ 211 non-consecutive hydroperiod days"), xaxt = "n")
    text(x = mp[seq(1, length(mp), by = 2)] - .5, y = -5, year$year[seq(1, length(mp), by = 2)], srt = 45, xpd = T)
    rmean <- (year[1:(length(year$year) - 3), n] + year[2:(length(year$year) - 2), n] + year[3:(length(year$year) - 1), n] + year[4:(length(year$year)), n]) / 4
    lines(mp[4:length(mp)], rmean, lwd = 3)
    legend(8, -11, c("annual %", "4yr moving avg."), col = c("lightblue", "black"), lty = c(0, 1), lwd = c(NA, 3), pch = c(15, NA), pt.cex = 2, xpd = T, bty = "n")
    dev.off()
    o <- grep(paste0("four_hyd_mean_", sub[i], "_", su), names(year))
    png(paste0("./images/graphs_", su, "/four_hyd_mean_", sub[i], "_", su, ".png"), width = 800, height = 400, type = "quartz")
    mp <- barplot(year[4:length(year$year), o], col = "darkblue", ylim = c(0, 365), main = paste0(su, ": ", sub[i], " mean four-year hydroperiod days"), xaxt = "n")
    text(x = mp[seq(1, length(mp), by = 2)] - .5, y = -20, year$year[seq(4, length(mp) + 3, by = 2)] + 1, srt = 45, xpd = T)
    rmean <- (year[4:(length(year$year) - 3), o] + year[5:(length(year$year) - 2), o] + year[6:(length(year$year) - 1), o] + year[7:(length(year$year)), o]) / 4
    lines(mp[4:length(mp)], rmean, lwd = 3)
    legend(8, -40, c("annual avg. # of days", "4yr moving avg."), col = c("darkblue", "black"), lty = c(0, 1), lwd = c(NA, 3), pch = c(15, NA), pt.cex = 2, xpd = T, bty = "n")
    dev.off()
    q <- grep(paste0("four_hyd_per_", sub[i], "_", su), names(year))
    png(paste0("./images/graphs_", su, "/four_hyd_per_", sub[i], "_", su, ".png"), width = 800, height = 400, type = "quartz")
    mp <- barplot(year[4:length(year$year), q], col = "darkblue", ylim = c(0, 100), main = paste0(su, ": ", sub[i], " mean four-year hydroperiod days, percent area between 90 and 210"), xaxt = "n")
    text(x = mp[seq(1, length(mp), by = 2)] - .5, y = -5, year$year[seq(4, length(mp) + 3, by = 2)] + 1, srt = 45, xpd = T)
    rmean <- (year[4:(length(year$year) - 3), q] + year[5:(length(year$year) - 2), q] + year[6:(length(year$year) - 1), q] + year[7:(length(year$year)), q]) / 4
    lines(mp[4:length(mp)], rmean, lwd = 3)
    legend(8, -11, c("annual avg. # of days, % area", "4yr moving avg."), col = c("darkblue", "black"), lty = c(0, 1), lwd = c(NA, 3), pch = c(15, NA), pt.cex = 2, xpd = T, bty = "n")
    dev.off()
    p <- grep(paste0("four_hyd_sd_", sub[i], "_", su), names(year))
    png(paste0("./images/graphs_", su, "/four_hyd_sd_", sub[i], "_", su, ".png"), width = 800, height = 400, type = "quartz")
    mp <- barplot(year[4:length(year$year), p], col = "darkblue", ylim = c(0, 135), main = paste0(su, ": ", sub[i], " mean four-year hydroperiod days, standard deviation"), xaxt = "n")
    text(x = mp[seq(1, length(mp), by = 2)] - .5, y = -5, year$year[seq(4, length(mp) + 3, by = 2)] + 1, srt = 45, xpd = T)
    rmean <- (year[4:(length(year$year) - 3), p] + year[5:(length(year$year) - 2), p] + year[6:(length(year$year) - 1), p] + year[7:(length(year$year)), p]) / 4
    lines(mp[4:length(mp)], rmean, lwd = 3)
    legend(8, -11, c("stnd. dev. of annual avg. # of days", "4yr moving avg."), col = c("darkblue", "black"), lty = c(0, 1), lwd = c(NA, 3), pch = c(15, NA), pt.cex = 2, xpd = T, bty = "n")
    dev.off()
  }
}

for (a in 1:length(fls))
  write.table(year[, c(1, (2:73) + (a - 1) * 72)], paste0("./output/CSSS_subarea_summary_stats_", sc[a], ".csv"), quote = F, sep = ",", row.names = F, col.names = c("Year", rbind(paste("area", sub, "% dry >= 40 continuous breeding season days"), paste("area", sub, "% dry >= 90 continuous breeding season days"), paste("area", sub, "% 0 to 89 day discontinuous annual hydroperiod"), paste("area", sub, "% 90 to 210 day discontinuous annual hydroperiod"), paste("area", sub, "% >= 211 day discontinuous annual hydroperiod"), paste("mean four-year hydroperiod days area", sub), paste("mean four-year hydroperiod days percent area", sub), paste("mean four-year hydroperiod standard deviation days area", sub))))

for (a in 1:length(fls)) {
  fl <- paste0("./output/perdry_sum_", sc[a], "_html.txt")
  yr <- year[, c(1, (2:73) + (a - 1) * 72)]
  yr <- yr[, c(1, 10:17, 2:9, 18:73)]
  write("", fl)
  for (i in 1:length(yr$year))
    write(paste0("<tr><td bgcolor='#a0c1e7'>", yr$year[i], "</td><td bgcolor='white'>", paste(yr[i, grep("nest", names(yr))], collapse = "%</td><td bgcolor='white'>"), "%</td></tr>"), fl, append = T)
  write("\n", fl, append = T)
  for (i in 1:length(yr$year))
    write(paste0("<tr><td bgcolor='#a0c1e7'>", yr$year[i], "</td><td bgcolor='white'>", paste(yr[i, grep("year[029]", names(yr))], collapse = "%</td><td bgcolor='white'>"), "%</td></tr>"), fl, append = T)
  write("\n", fl, append = T)
  yr$year <- yr$year + 1
  # swap % for days where appropriate
  for (i in 4:length(yr$year))
    write(paste0("<tr><td bgcolor='#a0c1e7'>", yr$year[i], "</td><td bgcolor='white'>", paste(round(yr[i, grep("four_hyd", names(yr))]), collapse = " days</td><td bgcolor='white'>"), " days</td></tr>"), fl, append = T)
  fl2 <- paste0("./output/perdry_sum_", sc[a], "_html_avgs.txt")
  avg <- round(colMeans(yr, na.rm = T), 1)
  write(paste0("<tr><td bgcolor='#a0c1e7'><strong>Avg.</strong></td><td bgcolor='white'><strong>", paste(avg[grep("nest", names(avg))], collapse = "%</strong></td><td bgcolor='white'><strong>"), "%</strong></td></tr>"), fl2)
  write("\n", fl2, append = T)
  write(paste0("<tr><td bgcolor='#a0c1e7'><strong>Avg.</strong></td><td bgcolor='white'><strong>", paste(avg[grep("year[029]", names(avg))], collapse = "%</strong></td><td bgcolor='white'><strong>"), "%</strong></td></tr>"), fl2, append = T)
  write("\n", fl2, append = T)
  write(paste0("<tr><td bgcolor='#a0c1e7'><strong>Avg.</strong></td><td bgcolor='white'><strong>", paste(round(avg[grep("four_hyd", names(avg))]), collapse = " days</strong></td><td bgcolor='white'><strong>"), " days</strong></td></tr>"), fl2, append = T)
}
setwd("..")