# Setup from CSSS_viewer.R

# Create depth arrays (<2min)
file_surf <- list.files("../surfaces")
time <- NULL
for (i in 1:length(sub)) assign(paste0("depth_", sub[i]), NULL)
for (i in 1:length(file_surf)) {
  print(file_surf[i])
  surf.nc <- nc_open(paste0("../surfaces/", file_surf[i]))
  stage <- ncvar_get(surf.nc, "stage")
  t <- ncvar_get(surf.nc, "time")
  time <- as.Date(c(time, as.Date(surf.nc$dim$time$units, format = "days since %Y-%m-%dT%H:%M:%S") + t, recursive = T), origin = "1970/1/1")
  nc_close(surf.nc)
  for (j in 1:length(sub)) {
    stage_sub <- stage[get(paste0("xmin_", sub[j])):get(paste0("xmax_", sub[j])), get(paste0("ymin_", sub[j])):get(paste0("ymax_", sub[j])), ]
    stage_sub[is.na(get(paste0("grid_", sub[j])))] <- NA
    assign(paste0("depth_", sub[j]), abind(get(paste0("depth_", sub[j])), sweep(stage_sub, c(1, 2), get(paste0("grid_", sub[j])), "-")))
  }
}

for (i in 1:length(sub)) assign(paste0("dry_", sub[i]), ifelse(get(paste0("depth_", sub[i])) <= 0, 1, 0)) #dry == 1
# Calc subarea daily stats (~30min -- mostly subarea AX & A, dry days)
dt <- data.frame(date = time)
for (i in 1:length(sub)) {
  print(sub[i])
  dim <- sum(!is.na(get(paste0("grid_", sub[i]))))
  for (j in 1:dim(dt)[1]) {
    dt[j, paste0("per_dry_", sub[i])] <- round(sum(get(paste0("dry_", sub[i]))[, , j], na.rm = T) / dim, 3) * 100
    dt[j, paste0("per_dry17cm_", sub[i])] <- round(sum(ifelse(get(paste0("depth_", sub[i]))[, , j] <= 17, 1, 0), na.rm = T) / dim, 3) * 100
  }
  for (j in 40:dim(dt)[1]) {
    dry_40d <- ifelse(rowSums(get(paste0("dry_", sub[i]))[, , (j - 39):j], dims = 2) == 40, 1, 0) #all dry (== 1) days, then 1
    dt[j, paste0("per_dry40d_", sub[i])] <- round((sum(dry_40d, na.rm = T)) / dim, 3) * 100
  }
  for (j in 90:dim(dt)[1]) {
    dry_90d <- ifelse(rowSums(get(paste0("dry_", sub[i]))[, , (j-89):j], dims = 2) == 90, 1, 0) #all dry (== 1) days, then 1
    dt[j, paste0("per_dry90d_", sub[i])] <- round((sum(dry_90d, na.rm = T)) / dim, 3) * 100
  }
  for (j in 1:dim(dt)[1]) {
    dt[j, paste0("depth_mean_", sub[i])] <- round(mean(get(paste0("depth_", sub[i]))[, , j], na.rm = T), 1)
    dt[j, paste0("depth_sd_", sub[i])] <- round(sd(get(paste0("depth_", sub[i]))[, , j], na.rm = T), 1)
  }
}

write("var stats = {", "./output/por_stats.js")
for (i in 1:length(dt$date))
  if (i == length(dt$date)) # remove last comma
    write(paste0(format(dt$date[i], "'a%Y%m%d' : ['"), paste(dt[i, 2:dim(dt)[2]], collapse = "', '"), "']"), "./output/por_stats.js", append = T) else
    write(paste0(format(dt$date[i], "'a%Y%m%d' : ['"), paste(dt[i, 2:dim(dt)[2]], collapse = "', '"), "'],"), "./output/por_stats.js", append = T)
write("}", "./output/por_stats.js", append = T)

# Calc subarea annual stats (~3min)
year <- data.frame(year=1991:2020)
start_nest <- which(as.POSIXlt(time)$mon == 2 & as.POSIXlt(time)$mday == 1)
end_nest <- which(as.POSIXlt(time)$mon == 6 & as.POSIXlt(time)$mday == 15)
start_year <- which(as.POSIXlt(time)$yday == 0)
end_year <- which(as.POSIXlt(time)$mon == 11 & as.POSIXlt(time)$mday == 31)
for (i in 1:length(sub)) {
  print(sub[i])
  dim <- sum(!is.na(get(paste0("grid_", sub[i]))))
  n40 <- n90 <- matrix(NA, dim(get(paste0("dry_", sub[i])))[1], dim(get(paste0("dry_", sub[i])))[2])
  for (j in 1:length(year$year)) {
    for (k in 1:dim(get(paste0("dry_", sub[i])))[1])
      for (l in 1:dim(get(paste0("dry_", sub[i])))[2]) {
        r <- rle(get(paste0("dry_", sub[i]))[k, l, start_nest[j]:end_nest[j]])
        n40[k, l] <- ifelse(any(r$lengths[r$values == 1] >= 40), 1, 0)
        n90[k, l] <- ifelse(any(r$lengths[r$values == 1] >= 90), 1, 0)
      }
    year[j, paste0("nest40_", sub[i])] <- round((sum(n40, na.rm = T)) / dim, 3) * 100
    year[j, paste0("nest90_", sub[i])] <- round((sum(n90, na.rm = T)) / dim, 3) * 100
  }
  assign(paste0("wet_", sub[i]), ifelse(get(paste0("dry_", sub[i])) == 0, 1, 0)) #invert; wet == 1
  for (j in 1:length(year$year)) {
    year[j, paste0("year0_to_89_", sub[i])] <- round((sum(ifelse(rowSums(get(paste0("wet_", sub[i]))[, , start_year[j]:end_year[j]], dims = 2) <= 89, 1, 0), na.rm = T)) / dim, 3) * 100
    year[j, paste0("year90_to_210_", sub[i])] <- round((sum(ifelse(rowSums(get(paste0("wet_", sub[i]))[, , start_year[j]:end_year[j]], dims = 2) >= 90 & rowSums(get(paste0("wet_", sub[i]))[, , start_year[j]:end_year[j]], dims = 2) <= 210, 1, 0), na.rm = T)) / dim, 3) * 100
    year[j, paste0("year211_", sub[i])] <- round((sum(ifelse(rowSums(get(paste0("wet_", sub[i]))[, , start_year[j]:end_year[j]], dims = 2) >= 211, 1, 0), na.rm = T)) / dim, 3) * 100
  }
  for (j in 4:length(year$year)) {
    tmp <- rowSums(get(paste0("wet_", sub[i]))[, , start_year[j - 3]:end_year[j]], dims = 2) / 4
    year[j, paste0("four_hyd_mean_", sub[i])] <- round(mean(tmp, na.rm = T), 1)
    tmp2 <- tmp[tmp >= 90 & tmp <= 210]
    year[j, paste0("four_hyd_per_", sub[i])] <- round(length(tmp2[!is.na(tmp2)]) * 100 / dim, 1)
  }
  year[, paste0("four_hyd_sd_", sub[i])] <- NA # Initialize SD column
}

h4 <- array(NA, c(287, 405, length(year$year) - 3))
for (i in (length(year$year) - 3):1) {
  print(year$year[i] + 4)
  h1 <- array(NA, c(287, 405, 4))
  sd <- matrix(NA, 287, 405)
  #qual <- matrix(NA, 287, 405)
  wet <- time <- NULL
  for (j in 1:16) {
    surf.nc <- nc_open(paste0("../surfaces/", file_surf[j + (i - 1) * 4]))
    stage <- ncvar_get(surf.nc, "stage")
    t <- ncvar_get(surf.nc, "time")
    time <- as.Date(c(time, as.Date(surf.nc$dim$time$units, format = "days since %Y-%m-%dT%H:%M:%S") + t, recursive = T), origin = "1970/1/1")
    nc_close(surf.nc)
    depth <- sweep(stage, c(1, 2), dem, "-")
    wet <- abind(wet, ifelse(depth < 0, 0, 1))
  }
  wet[, 149:405, ] <- NA
  h4[, , i] <- rowSums(wet, dims = 2) / 4
  st <- c(which(as.POSIXlt(time)$yday == 0))
  en <- c(which(as.POSIXlt(time)$mday == 31 & as.POSIXlt(time)$mon == 11))
  for (j in 1:4)
    h1[, , j] <- rowSums(wet[, , st[j]:en[j]], dims = 2)
  for (k in 1:dim(h1)[1])
    for (l in 1:dim(h1)[2])
      sd[k, l] <- sd(h1[k, l, ])
  #for (k in 1:dim(h1)[1])
    #for (l in 1:dim(h1)[2])
      #qual[k, l] <- sum(h1[k, l, ] >= 90 & h1[k, l, ] >= 210)

  for (j in 1:length(sub)) {
    print(sub[j])
    sub_sd <- sd[get(paste0("xmin_", sub[j])):get(paste0("xmax_", sub[j])), get(paste0("ymin_", sub[j])):get(paste0("ymax_", sub[j]))]
    sub_sd[is.na(get(paste0("grid_", sub[j])))] <- NA
    year[i + 3, paste0("four_hyd_sd_", sub[j])] <- round(mean(sub_sd, na.rm = T), 1)
  }

# Create four-year hydroperiod maps
  col<-rgb(c(230, 245, 18, 0, 24), c(76, 217, 194, 255, 116), c(0, 1, 73, 255, 205), maxColorValue = 255)
  png(paste0("./images/hydrop4/four_year_hydroperiod_", year$year[i] + 4,"_altRange2.png"), width = 614, height = 862, units = "px", pointsize = 12, bg = "transparent", type = "quartz")
  par(mar = c(0, 0, 0, 0))
  image(x, y, h4[, , i], col = col, breaks = c(0, 138.2, 163, 200.3, 220.3, 400), axes = F, asp = 1)
  text(x[1], y[10], year$year[i] + 4, pos = 4)
  dev.off()
  png(paste0("./images/hydrop4/four_year_hydroperiod_", year$year[i] + 4, "_mask65_altRange2.png"), width = 614, height = 862, units = "px", pointsize = 12, bg = "transparent", type = "quartz")
  par(mar = c(0, 0, 0, 0))
#  image(x,y,h4[,,i],col=col,breaks=c(0,24.5,89.5,210.5,400),axes=F,asp=1)
  image(x, y, h4[, , i], col = col, breaks = c(0, 138.2, 163, 200.3, 220.3, 400), axes = F, asp = 1)
  mask <- ifelse(sd >= 56.8, 1, NA)
  image(x, y, mask, col = rgb(0, 0, 0, 0.65), add = T)
  text(x[1], y[10], year$year[i] + 4, pos = 4)
  dev.off()
#  col2<-rgb(c(0,0,122,245,165),c(169,255,237,217,3),c(230,255,0,73,0),maxColorValue=255)
  png(paste0("./images/hydrop4/four_year_hydroperiod_", year$year[i] + 4, "_sd_altRange2.png"), width = 614, height = 862, units = "px", pointsize = 12, bg = "transparent", type = "quartz")
  par(mar = c(0, 0, 0, 0))
  image(x, y, sd, col = rev(col), breaks = c(-1, 26.7, 33.7, 46.7, 56.8, 400), axes = F, asp = 1)
  text(x[1], y[10], year$year[i] + 4, pos = 4)
  dev.off()
#  png(filename=paste0("~/Desktop/scripts/eden/csss_dev/hydrop4/four_year_hydroperiod_",year$year[i]+4,"_count.png"),width=614,height=862,units="px",pointsize=12,bg="transparent",type="quartz")
#  par(mar=c(0,0,0,0))
#  image(x,y,qual,col=col,breaks=c(-1,1.9,2.9,3.9,5),axes=F,asp=1)
#  text(x[1],y[10],year$year[i]+4,pos=4)
#  dev.off()
}

for(i in 1:length(sub)) {
  j <- grep("nest40", names(year))[i]
  png(paste0("./images/graphs/nest40_", sub[i], ".png"), width = 600, height = 400, units = "px", pointsize = 12, bg = "white", type = "quartz")
  mp <- barplot(year[, j], col = "burlywood4", ylim = c(0, 100), main = paste(sub[i], "% area with 40 consecutive dry days"), xaxt = "n")
  text(x = mp[seq(1, length(mp), by = 2)] - .5, y = - 5, year$year[seq(1, length(mp), by = 2)], srt = 45, xpd = T)
  rmean <- (year[1:(length(year$year) - 3), j] + year[2:(length(year$year) - 2), j] + year[3:(length(year$year) - 1), j] + year[4:(length(year$year)), j]) / 4
  lines(mp[4:length(mp)], rmean, lwd = 3)
  legend(8, -11, c("annual %", "4yr moving avg."), col = c("burlywood4", "black"), lty = c(0, 1), lwd = c(NA, 3), pch = c(15, NA), pt.cex = 2, xpd = T, bty = "n")
  dev.off()
  k <- grep("nest90", names(year))[i]
  png(paste0("./images/graphs/nest90_", sub[i], ".png"), width = 600, height = 400, units = "px", pointsize = 12, bg = "white", type = "quartz")
  mp <- barplot(year[, k], col = "burlywood4", ylim = c(0, 100), main = paste(sub[i], "% area with 90 consecutive dry days"), xaxt = "n")
  text(x = mp[seq(1, length(mp), by = 2)] - .5, y = - 5, year$year[seq(1, length(mp), by = 2)], srt = 45, xpd = T)
  rmean <- (year[1:(length(year$year) - 3), k] + year[2:(length(year$year) - 2), k] + year[3:(length(year$year) - 1), k] + year[4:(length(year$year)), k]) / 4
  lines(mp[4:length(mp)], rmean, lwd = 3)
  legend(8, -11, c("annual %", "4yr moving avg."), col = c("burlywood4", "black"), lty = c(0, 1), lwd = c(NA, 3), pch = c(15, NA), pt.cex = 2, xpd = T, bty = "n")
  dev.off()
  l <- grep("year0", names(year))[i]
  png(paste0("./images/graphs/year0_to_89_", sub[i], ".png"), width = 600, height = 400, units = "px", pointsize = 12, bg = "white", type = "quartz")
  mp <- barplot(year[, l], col = "lightblue", ylim = c(0, 100), main = paste(sub[i], "% area with 0 to 89 non-consecutive hydroperiod days"), xaxt = "n")
  text(x = mp[seq(1, length(mp), by = 2)] - .5, y = -5, year$year[seq(1, length(mp), by = 2)], srt = 45, xpd = T)
  rmean <- (year[1:(length(year$year) - 3), l] + year[2:(length(year$year) - 2), l] + year[3:(length(year$year) - 1), l] + year[4:(length(year$year)), l]) / 4
  lines(mp[4:length(mp)], rmean, lwd = 3)
  legend(8, -11, c("annual %", "4yr moving avg."), col = c("lightblue", "black"), lty = c(0, 1), lwd = c(NA, 3), pch = c(15, NA), pt.cex = 2, xpd = T, bty = "n")
  dev.off()
  m <- grep("year90", names(year))[i]
  png(paste0("./images/graphs/year90_to_210_", sub[i], ".png"), width = 600, height = 400, units = "px", pointsize = 12, bg = "white", type = "quartz")
  mp <- barplot(year[, m], col = "lightblue", ylim = c(0, 100), main = paste(sub[i], "% area with 90 to 210 non-consecutive hydroperiod days"), xaxt = "n")
  text(x = mp[seq(1, length(mp), by = 2)] - .5, y = -5, year$year[seq(1, length(mp), by = 2)], srt = 45, xpd = T)
  rmean <- (year[1:(length(year$year) - 3), m] + year[2:(length(year$year) - 2), m] + year[3:(length(year$year) - 1), m] + year[4:(length(year$year)), m]) / 4
  lines(mp[4:length(mp)], rmean, lwd = 3)
  legend(8, -11, c("annual %", "4yr moving avg."), col = c("lightblue", "black"), lty = c(0, 1), lwd = c(NA, 3), pch = c(15, NA), pt.cex = 2, xpd = T, bty = "n")
  dev.off()
  n <- grep("year211", names(year))[i]
  png(paste0("./images/graphs/year211_", sub[i], ".png"), width = 600, height = 400, units = "px", pointsize = 12, bg = "white", type = "quartz")
  mp <- barplot(year[, n], col = "lightblue", ylim = c(0, 100), main = paste(sub[i], "% area with ≥ 211 non-consecutive hydroperiod days"), xaxt = "n")
  text(x = mp[seq(1, length(mp), by = 2)] - .5, y = -5, year$year[seq(1, length(mp), by = 2)], srt = 45, xpd = T)
  rmean <- (year[1:(length(year$year) - 3), n] + year[2:(length(year$year) - 2), n] + year[3:(length(year$year) - 1), n] + year[4:(length(year$year)), n]) / 4
  lines(mp[4:length(mp)], rmean, lwd = 3)
  legend(8, -11, c("annual %", "4yr moving avg."), col = c("lightblue", "black"), lty = c(0, 1), lwd = c(NA, 3), pch = c(15, NA), pt.cex = 2, xpd = T, bty = "n")
  dev.off()
  o <- grep("four_hyd_mean", names(year))[i]
  png(paste0("./images/graphs/four_hyd_mean_", sub[i], ".png"), width = 600, height = 400, units = "px", pointsize = 12, bg = "white", type = "quartz")
  mp <- barplot(year[4:length(year$year), o], col = "darkblue", ylim = c(0, 365), main = paste(sub[i], "mean four-year hydroperiod days"), xaxt = "n")
  text(x = mp[seq(1, length(mp), by = 2)] - .5, y = -20, year$year[seq(4, length(mp) + 3, by = 2)], srt = 45, xpd = T)
  rmean <- (year[4:(length(year$year) - 3), o] + year[5:(length(year$year) - 2), o] + year[6:(length(year$year) - 1), o] + year[7:(length(year$year)), o]) / 4
  lines(mp[4:length(mp)], rmean, lwd = 3)
  legend(8, -40, c("annual avg. # of days", "4yr moving avg."), col = c("darkblue", "black"), lty = c(0, 1), lwd = c(NA, 3), pch = c(15, NA), pt.cex = 2, xpd = T, bty = "n")
  dev.off()
  q <- grep("four_hyd_per", names(year))[i]
  png(paste0("./images/graphs/four_hyd_per_", sub[i], ".png"), width = 600, height = 400, units = "px", pointsize = 12, bg = "white", type = "quartz")
  mp <- barplot(year[4:length(year$year), q], col = "darkblue", ylim = c(0, 100), main = paste(sub[i], "mean four-year hydroperiod days, percent area between 90 and 210"), xaxt = "n")
  text(x = mp[seq(1, length(mp), by = 2)] - .5, y = -5, year$year[seq(4, length(mp) + 3, by = 2)], srt = 45, xpd = T)
  rmean <- (year[4:(length(year$year) - 3), q] + year[5:(length(year$year) - 2), q] + year[6:(length(year$year) - 1), q] + year[7:(length(year$year)), q]) / 4
  lines(mp[4:length(mp)], rmean, lwd = 3)
  legend(8, -11, c("annual avg. # of days, % area", "4yr moving avg."), col = c("darkblue", "black"), lty = c(0, 1), lwd = c(NA, 3), pch = c(15, NA), pt.cex = 2, xpd = T, bty = "n")
  dev.off()
  p <- grep("four_hyd_sd", names(year))[i]
  png(paste0("./images/graphs/four_hyd_sd_", sub[i], ".png"), width = 600, height = 400, units = "px", pointsize = 12, bg = "white", type = "quartz")
  mp <- barplot(year[4:length(year$year), p], col = "darkblue", ylim = c(0, 100), main = paste(sub[i], "mean four-year hydroperiod days, standard deviation"), xaxt = "n")
  text(x = mp[seq(1, length(mp), by = 2)] - .5, y = -5, year$year[seq(4, length(mp) + 3, by = 2)], srt = 45, xpd = T)
  rmean <- (year[4:(length(year$year) - 3), p] + year[5:(length(year$year) - 2), p] + year[6:(length(year$year) - 1), p] + year[7:(length(year$year)), p]) / 4
  lines(mp[4:length(mp)], rmean, lwd = 3)
  legend(8, -11, c("stnd. dev. of annual avg. # of days", "4yr moving avg."), col = c("darkblue", "black"), lty = c(0, 1), lwd = c(NA, 3), pch = c(15, NA), pt.cex = 2, xpd = T, bty = "n")
  dev.off()
}

png("./images/legend_4yhp2.png", width = 33, height = 161, units = "px", pointsize = 12, type = "quartz")
par(mar = c(0, 0, 0, 0))
plot(1:10, 1:10, type = "n")
rect(0, c(0, 2.5, 4.5, 6.5, 8.5), 10, c(2.5, 4.5, 6.5, 8.5, 10.5), col = col, border = NA)
dev.off()
png("./images/legend_4yhpsd2.png", width = 33, height = 161, units = "px", pointsize = 12, type = "quartz")
par(mar = c(0, 0, 0, 0))
plot(1:10, 1:10, type = "n")
rect(0, c(0, 2.5, 4.5, 6.5, 8.5), 10, c(2.5, 4.5, 6.5, 8.5, 10.5), col = rev(col), border = NA)
dev.off()

write.table(year, "./output/CSSS_subarea_summary_stats.csv", quote = F, sep = ",", row.names = F, col.names = c("Year", rbind(paste("area", sub, "% dry >= 40 continuous breeding season days"), paste("area", sub, "% dry >= 90 continuous breeding season days"), paste("area", sub, "% 0 to 89 day discontinuous annual hydroperiod"), paste("area", sub, "% 90 to 210 day discontinuous annual hydroperiod"), paste("area", sub, "% >= 211 day discontinuous annual hydroperiod"), paste("mean four-year hydroperiod days area", sub), paste("mean four-year hydroperiod days percent area", sub), paste("mean four-year hydroperiod standard deviation days area", sub))))

year <- year[, c(1, 10:17, 2:9, 18:73)]
write("", "./output/perdry_sum_html.txt")
for (i in 1:length(year$year))
  write(paste0("<tr><td bgcolor='#a0c1e7'>", year$year[i], "</td><td bgcolor='white'>", paste(year[i, grep("nest", names(year))], collapse = "%</td><td bgcolor='white'>"), "%</td></tr>"),"./output/perdry_sum_html.txt", append = T)
for (i in 1:length(year$year))
  write(paste0("<tr><td bgcolor='#a0c1e7'>", year$year[i], "</td><td bgcolor='white'>", paste(year[i, grep("year[029]", names(year))], collapse = "%</td><td bgcolor='white'>"), "%</td></tr>"), "./output/perdry_sum_html.txt", append = T)
# swap % for days where appropriate
for (i in 4:length(year$year))
  write(paste0("<tr><td bgcolor='#a0c1e7'>", year$year[i + 1], "</td><td bgcolor='white'>", paste(round(year[i, grep("four_hyd", names(year))]), collapse = " days</td><td bgcolor='white'>"), " days</td></tr>"), "./output/perdry_sum_html.txt", append = T)

# Create POR depth maps
col<-c("deepskyblue","steelblue","blue3","blue4")
for (k in 1:length(file_surf)) {
  print(file_surf[k])
  surf.nc<-nc_open(paste0("../surfaces/",file_surf[k]))
  stage<-ncvar_get(surf.nc,"stage")
  time<-ncvar_get(surf.nc, "time")
  time <- as.Date(surf.nc$dim$time$units, format = "days since %Y-%m-%dT%H:%M:%S") + time
  nc_close(surf.nc)
  depth<-sweep(stage,c(1,2),dem,"-")
  for (i in 1:length(time)) {
    png(filename=paste0("images/",format(time[i],'%Y'),sprintf("/trans%04d",as.POSIXlt(time[i])$yday),".png"),width=614,height=862,units="px",pointsize=12,bg="transparent",type="quartz")
    par(mar=c(0,0,0,0))
    image(x,y,depth[,,i],col=col,breaks=c(0,17,30,46,600),axes=F,asp=1)
    text(x[1],y[10],as.Date(time[i]),pos=4)
    #points(x[c(1,1,287,287)],y[c(1,405,1,405)],pch=16) #positioning markers for Leaflet map
    dev.off()
    if(as.POSIXlt(time[i])$year%%4!=0)
      if(as.POSIXlt(time[i])$yday>=59 & as.POSIXlt(time[i])$yday<=195)
        file.copy(paste0("images/",format(time[i],'%Y'),sprintf("/trans%04d",as.POSIXlt(time[i])$yday),".png"),paste0("images/",format(time[i],'%Y'),sprintf("_nest/trans%04d",as.POSIXlt(time[i])$yday-59),".png"),overwrite=T)
    if(as.POSIXlt(time[i])$year%%4==0)
      if(as.POSIXlt(time[i])$yday>=60 & as.POSIXlt(time[i])$yday<=196)
        file.copy(paste0("images/",format(time[i],'%Y'),sprintf("/trans%04d",as.POSIXlt(time[i])$yday),".png"),paste0("images/",format(time[i],'%Y'),sprintf("_nest/trans%04d",as.POSIXlt(time[i])$yday-60),".png"),overwrite=T)
  }
}

# POR L31N transect plots
trans_pix<-read.table("~/Desktop/R_old/six_tenths_transect/transect_pixel_utm.txt",header=T)
trans_pix2<-read.table("~/Desktop/R_old/six_tenths_transect/transect2_pixel_utm.txt",header=T)
for (i in 1:length(trans_pix$X)) {
  trans_pix$xcoord[i]<-which.min(abs(x-trans_pix$X[i]))
  trans_pix$ycoord[i]<-which.min(abs(y-trans_pix$Y[i]))
  trans_pix2$xcoord[i]<-which.min(abs(x-trans_pix2$X[i]))
  trans_pix2$ycoord[i]<-which.min(abs(y-trans_pix2$Y[i]))
  trans_pix$dem[i]<-dem[trans_pix$xcoord[i],trans_pix$ycoord[i]]
  trans_pix2$dem[i]<-dem[trans_pix2$xcoord[i],trans_pix2$ycoord[i]]
}
# ~30 min
for (i in 1:length(file_surf)) {
  print(file_surf[i])
  surf.nc<-nc_open(paste0("../surfaces/",file_surf[i]))
  stage<-ncvar_get(surf.nc,"stage")
  t<-ncvar_get(surf.nc,"time")
  time <- as.Date(surf.nc$dim$time$units, format = "days since %Y-%m-%dT%H:%M:%S") + t
  nc_close(surf.nc)
  for (j in 1:length(time)) {
    for (k in 1:length(trans_pix$X)) {
      trans_pix$wl[k]<-stage[trans_pix$xcoord[k],trans_pix$ycoord[k],j]
      trans_pix2$wl[k]<-stage[trans_pix2$xcoord[k],trans_pix2$ycoord[k],j]
    }
    rn<-range(trans_pix$wl,trans_pix2$wl,trans_pix$dem,trans_pix2$dem,na.rm=T)
    png(filename=paste0("./images/transect/",format(time[j],'%Y%m%d'),".png"),width=1200,height=800,units="px",pointsize=12,bg="white",type="quartz")
    par(mar=c(5,4,4,5)+.1)
    plot(trans_pix$wl,type="l",col="blue",lwd=3,ylim=rn,main=paste("0.6 mile (east) transect for",format(time[j],'%m/%d/%Y')),xlab="North-to-south transect pixels",ylab="Water level (cm NAVD88)")
    lines(trans_pix$dem,lwd=3)
    legend("topleft",c("EDEN WL surface","EDEN DEM"),col=c("blue","black"),lty=1,lwd=3)
    dev.off()
    jpeg(filename=paste0("./images/transect_thumb/",format(time[j],'%Y%m%d'),".png"),width=240,height=160,units="px",pointsize=12,quality=100,bg="white",type="quartz")
    par(mar=c(0,0,0,0))
    plot(trans_pix$wl,type="l",col="blue",lwd=2,ylim=rn)
    lines(trans_pix$dem,lwd=2)
    dev.off()
    png(filename=paste0("./images/transect2/",format(time[j],'%Y%m%d'),".png"),width=1200,height=800,units="px",pointsize=12,bg="white",type="quartz")
    par(mar=c(5,4,4,5)+.1)
    plot(trans_pix2$wl,type="l",col="blue",lwd=3,ylim=rn,main=paste("2.1 mile (west) transect for",format(time[j],'%m/%d/%Y')),xlab="North-to-south transect pixels",ylab="Water level (cm NAVD88)")
    lines(trans_pix2$dem,lwd=3)
    legend("topleft",c("EDEN WL surface","EDEN DEM"),col=c("blue","black"),lty=1,lwd=3)
    dev.off()
    jpeg(filename=paste0("./images/transect2_thumb/",format(time[j],'%Y%m%d'),".png"),width=240,height=160,units="px",pointsize=12,quality=100,bg="white",type="quartz")
    par(mar=c(0,0,0,0))
    plot(trans_pix2$wl,type="l",col="blue",lwd=2,ylim=rn)
    lines(trans_pix2$dem,lwd=2)
    dev.off()
  }
}
