# -------------
# Bryan McCloskey
# bmccloskey@usgs.gov
# St. Petersburg Coastal and Marine Science Center
# US Geological Survey
#
# 08/21/2018
#--------------

print("These libraries must be installed: ncdf4, RColorBrewer")
# Required libraries. If not present, run:
# install.packages("ncdf4")
# install.packages("RColorBrewer")
library(ncdf4)
library(RColorBrewer)

try (setwd("./realtime_jpgs"), silent = T)

# Generate color palette
stage.col <- brewer.pal(11, "Spectral")
tmp <- col2rgb(stage.col)
tmp2 <- matrix(rep(0, 243), 3, 81)
for (j in 1:3) {
  for (i in 1:length(tmp[1, ])) {
    tmp2[, i * 2 - 1] <- tmp[, i]
    if (i != length(tmp[1, ])) tmp2[, i * 2] <-(tmp[, i] + tmp[, i + 1]) / 2
  }
  tmp <- tmp2[, 1:(i * 2 - 1)]
}
stage.col <- rgb(t(tmp2[, 1:81]), maxColorValue = 255)

# Set up WL & depth files
cur_qtr <- paste0(as.POSIXlt(Sys.Date() - 1)$year + 1900, "_", tolower(quarters(Sys.Date() - 1)))
pre_qtr <- paste0(as.POSIXlt(Sys.Date() - 90)$year + 1900, "_", tolower(quarters(Sys.Date() - 90)))
err <- try (download.file(paste0("https://sofia.usgs.gov/eden/data/realtime2/", cur_qtr, "_v2rt_nc.zip"), paste0("./surfaces/", cur_qtr, ".zip")))
unzip(paste0("./surfaces/", cur_qtr, ".zip"), exdir = "./surfaces")
file.rename(paste0("./surfaces/", cur_qtr, "_v2rt.nc"), paste0("./surfaces/", cur_qtr, ".nc"))
err <- try (download.file(paste0("https://sofia.usgs.gov/eden/data/realtime2/", pre_qtr, "_v2rt_nc.zip"), paste0("./surfaces/", pre_qtr, ".zip")))
unzip(paste0("./surfaces/", pre_qtr, ".zip"), exdir = "./surfaces")
file.rename(paste0("./surfaces/", pre_qtr, "_v2rt.nc"), paste0("./surfaces/", pre_qtr, ".nc"))
err <- try (download.file(paste0("https://sofia.usgs.gov/eden/data/realtime2/", cur_qtr, "_v2rt_depth_nc.zip"), paste0("./surfaces/", cur_qtr, ".zip")))
unzip(paste0("./surfaces/", cur_qtr, ".zip"), exdir = "./surfaces")
file.rename(paste0("./surfaces/", cur_qtr, "_v2rt_depth.nc"), paste0("./surfaces/", cur_qtr, "_depth.nc"))
err <- try (download.file(paste0("https://sofia.usgs.gov/eden/data/realtime2/", pre_qtr, "_v2rt_depth_nc.zip"), paste0("./surfaces/", pre_qtr, ".zip")))
unzip(paste0("./surfaces/", pre_qtr, ".zip"), exdir = "./surfaces")
file.rename(paste0("./surfaces/", pre_qtr, "_v2rt_depth.nc"), paste0("./surfaces/", pre_qtr, "_depth.nc"))
unlink("./surfaces/*.zip")

surf.nc <- open.ncdf("/export1/htdocs/eden/programs/surface.nc")
x<-get.var.ncdf(surf.nc, "x")
y<-get.var.ncdf(surf.nc, "y")
time<-get.var.ncdf(surf.nc, "time")
time<-as.POSIXct(surf.nc$dim$time$units,format="days since %Y-%m-%dT%H:%M:%SZ")+86400*time
stage<-get.var.ncdf(surf.nc,"stage")
close.ncdf(surf.nc)
surf.nc<-open.ncdf("/export1/htdocs/eden/programs/surface_depth.nc")
time2<-get.var.ncdf(surf.nc, "time")
time2<-as.POSIXct(surf.nc$dim$time$units,format="days since %Y-%m-%dT%H:%M:%SZ")+86400*time2
stage2<-get.var.ncdf(surf.nc,"depth")
close.ncdf(surf.nc)
if (length(dim(stage))==2) dim(stage)<-dim(stage2)<-c(287,405,1) #1st day of quarter defaults to matrix otherwise

for (i in length(time2):1) {
  bitmap_100per_jpeg(file=paste("/export1/htdocs/eden/data/jpgs/EDENsurface_",format(time2[i],format="%Y%m%d"),"_depth.jpg",sep=""),type="jpeg",width=800,height=1000,units="px",pointsize=12,taa=4)
  filled.contour.nolines(x,y,stage2[,,i],zlim=c(0,267),levels=seq(0,267,by=6.5),col=stage.col[40:81],plot.title=title(paste("EDEN Real-Time Water Depth for ",format(time2[i],format="%m/%d/%Y"),sep="")),key.title=title(main="water\ndepth,\ncm"),key.axes=axis(4,seq(0,250,by=50)),asp=1,axes=F,frame.plot=T)
  dev.off()
  bitmap_100per_jpeg(file=paste("/export1/htdocs/eden/data/jpgs/EDENsurface_",format(time2[i],format="%Y%m%d"),"_depth_contour.jpg",sep=""),type="jpeg",width=800,height=1000,units="px",pointsize=12,taa=4)
  filled.contour.nolines(x,y,stage2[,,i],zlim=c(0,267),levels=seq(0,267,by=6.5),col=stage.col[40:81],plot.title=title(paste("EDEN Water Depth for ",format(time2[i],format="%m/%d/%Y"),sep="")),plot.axes={contour(x,y,stage2[,,i],zlim=c(0,267),levels=seq(0,250,by=25),lwd=c(1.5,1),add=T);points(db[,3:2],pch=4)},key.title=title(main="water\ndepth, cm"),key.axes=axis(4,seq(0,250,by=50)),asp=1,axes=F,frame.plot=T)
  dev.off()
  bitmap_100per_jpeg(file=paste("/export1/htdocs/eden/data/jpgs/EDENsurface_",format(time2[i],format="%Y%m%d"),"_depth_small.jpg",sep=""),type="jpeg",width=25,height=35,units="px",pointsize=12,taa=4)
  par(mar=c(0,0,0,0))
  image(x,y,stage2[,,i],zlim=c(0,267),col=stage.col[40:81],xlab="",xaxt="n",ylab="",yaxt="n",frame.plot=F)
  dev.off()
}
for (i in length(time):1) {
bitmap_100per_jpeg(file=paste("/export1/htdocs/eden/data/jpgs/EDENsurface_",format(time[i],format="%Y%m%d"),".jpg",sep=""),type="jpeg",width=800,height=1000,units="px",pointsize=12,taa=4)
filled.contour.nolines(x,y,stage[,,i],zlim=c(-100,548),levels=seq(-100,548,by=8),col=stage.col,plot.title=title(paste("EDEN Real-Time Water Surface for ",format(time[i],format="%m/%d/%Y"),sep="")),key.title=title(main="water\nlevel, cm,\nNAVD88"),key.axes=axis(4,seq(-100,550,by=100)),asp=1,axes=F,frame.plot=T)
dev.off()
bitmap_100per_jpeg(file=paste("/export1/htdocs/eden/data/jpgs/EDENsurface_",format(time[i],format="%Y%m%d"),"_contour.jpg",sep=""),type="jpeg",width=800,height=1000,units="px",pointsize=12,taa=4)
filled.contour.nolines(x,y,stage[,,i],zlim=c(-100,548),levels=seq(-100,548,by=8),col=stage.col,plot.title=title(paste("EDEN Water Surface for ",format(time[i],format="%m/%d/%Y"),sep="")),plot.axes={contour(x,y,stage[,,i],zlim=c(-100,548),levels=seq(-100,550,by=50),lwd=c(1.5,1),add=T);points(db[,3:2],pch=4)},key.title=title(main="water\nlevel, cm,\nNAVD88"),key.axes=axis(4,seq(-100,550,by=100)),asp=1,axes=F,frame.plot=T)
dev.off()
bitmap_100per_jpeg(file=paste("/export1/htdocs/eden/data/jpgs/EDENsurface_",format(time[i],format="%Y%m%d"),"_small.jpg",sep=""),type="jpeg",width=25,height=35,units="px",pointsize=12,taa=4)
par(mar=c(0,0,0,0))
image(x,y,stage[,,i],zlim=c(-100,548),col=stage.col,xlab="",xaxt="n",ylab="",yaxt="n",frame.plot=F)
dev.off()
}
setwd("..")
