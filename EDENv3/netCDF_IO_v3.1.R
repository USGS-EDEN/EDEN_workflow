################################################
###  netCDF_IO_v3.1_varTYX.r
###  routines for reading & writing netCDF
###  Projection is hard-coded for UTM, Zone 17, NAD83
###       to change, modify the ERSI string in createNetCDFfile function 
###
###  Leonard Pearlstine 
###  South Florida Natural Resources Center
###  Everglades National Park
###     May 11 2016  
###  revised Sept 21, 2016  
###  revised July 24, 2017
###  added t.stepVec for arbitary time steps in createNetCDFfile July 28, 2017



## Modified by Mark McKelvy to rearrange netcdf header order
## Order now:  time, y, x
## Includes time stamp of "T12:00:00 +0000" for the date
################################################

if(require(ncdf4)) {
  message('ncdf4 loaded correctly')
} else {
  install.packages('ncdf4')
}
library(ncdf4)

###
### Open 
openNetCDF <- function(inputfile){
  nc_open( inputfile )
}

###
### READ netCDF file attributes
getNetCDFattr <- function(nc.ncdf4){
   # print file information
   #print(nc.ncdf4)
   # get x & y UTM vectors
   x <- ncvar_get(nc.ncdf4, "x", verbose = F)
   y <- ncvar_get(nc.ncdf4, "y", verbose = F)
   # get size of x & y
   x.size <- dim(x)
   y.size <- dim(y)
   cell.size <- x[2] - x[1]
   #    #  initial x coordinate is x[1] and last x coordinate is x[x.size]
   UTMx.left <- x[1]
   UTMx.right <-  x[x.size]   
   # In some raster GIS files, Y index = 0 is at the bottom = lowest UTM value
   # in other files, it is at the top: Y = 0 = highest UTM value
   if (y[1] > y[y.size]){
      UTMy.top <- y[1]
      UTMy.bottom <- y[y.size]
      top.to.bottom <-TRUE
   } else {
      UTMy.top <- y[y.size]
      UTMy.bottom <- y[1]
      top.to.bottom <-FALSE
   }
   extent <- c(UTMx.left,UTMx.right,UTMy.top,UTMy.bottom)
   
      # get time variable
     t = tryCatch({
        ncvar_get(nc.ncdf4, "time") 
     }, error = function(e) {
        tryCatch({
           ncvar_get(nc.ncdf4, "t") # if error, check if dimension is called "t"
        }, error = function(e) {
          t.units <- NA
        })
     })
     
     #  tunits$value is the units string (e.g., "years since 1965" or "days since 1900-01-01 00:00:00.0 -0:00")
     tunits = tryCatch({
        ncatt_get(nc.ncdf4,"time","units") 
     }, error = function(e) {
        tryCatch({
           ncatt_get(nc.ncdf4,"t","units") 
        }, error = function(e) {
          t.size <- NA
        })
     })
     # get the size of t
     t.size <- dim(t)

   # Get the names of the data layers
   layers = names(nc.ncdf4[['var']])

   output<-list(utm.x=x, utm.y=y, t=t, x.size=x.size, y.size=y.size,
                cell.size=cell.size, t.size=t.size, t.units=tunits, extent=extent,
                top.to.bottom=top.to.bottom, layers=layers)
   return(output)   
}
###
### READ netCDF layer data at subset X, Y, & time step; default is single x,y,step
### 1st time step = 1 (not 0)
getNetCDFlayerXYTimeStep <- function(nc.ncdf4,layer.name,x,y,step, x.length=1, y.length=1, step.length=1){
   nc.array <- ncvar_get(nc.ncdf4, layer.name, start=c(x, y, step), count=c(x.length, y.length, step.length))
   return(nc.array)
}
###
### READ netCDF full x,y data at subset time step
### 1st time step = 1 (not 0)
getNetCDFlayerTimeStep <- function(nc.ncdf4, layer.name, step, step.length=1){
  #print(paste0(step,"  ",step.length)) # for debugging only
  nc.array  <- ncvar_get(nc.ncdf4, layer.name, start=c(1, 1, step), count=c(-1, -1, step.length))
  return(nc.array)
}
###
### READ netCDF full x,y data at all time steps
getNetCDFlayer <- function(nc.ncdf4,layer.name){
   nc.array <- ncvar_get(nc.ncdf4, layer.name)
   return(nc.array)
}

###
### Create a new netCDF file for writing
### if start.dataStr or t.size is omitted, static (no time series) layers are created
### if t.stepVec is included, the supplied vector of index positions for time is used.
###                          otherwise, time is incremented uniformly by day (daily=T) or year (daily=F).
createNetCDFfile <- function(out.name,            # name of created netCDF file
                             layer.name,          # name of data layer(s); may be a vector of names 
                             units,               # units for each layer
                             prec,                # data type of each layer = 'short' 'integer' 'float' 'double' 'char' 'byte'
                             long.name,           # expanded name of each layer
                             daily=TRUE,          # 'TRUE' for daily or arbitary time step;  'FALSE' for annual time step
                             extent,              # c(UTMx.left, UTMx.right, UTMy.top, UTMy.bottom)
                             cell.size,           # grid cell size
                             fill.value,          # background numeric fill or missing value
                             source.name,         # software that created the file: name, version number, & configuration info
                             institution,         # institution or organization that created the file
                             qaqc,                # quality control/quality assurance status
                             comments,            # ~ 1 to 3 sentence explanation of what this file contains.
                             start.dateStr=NULL,  # start date of file as string - format: 'yyyy-mm-dd'
                             t.size=NULL,         # number of weekly summaries to output
                             t.stepVec=NULL,      # vector of time index positions
                             layer.min,           # minimum value of data layer(s); may be a vector of minimums
                             layer.max)           # maximum value of data layer(s); may be a vector of minimums
{   
  
  timeDim = TRUE
  if (is.null(start.dateStr) | is.null(t.size)){
    timeDim = FALSE
  }
  
  if(timeDim){
     #------------------------------------------------------------------
     # create vector of time since start.date 
     #------------------------------------------------------------------ 
     #
     tCntVec <- vector(mode="integer", length=t.size)
     this.year <- as.numeric(substr(start.dateStr, 1, 4))
     if(is.null(t.stepVec)){
       count.since.start <- 0
       tCntVec[1] <- 0
       if (t.size > 1){
          for (tpos in 2:t.size){
            tCntVec[tpos] <- count.since.start
             if(daily){        # daily time step
                count.since.start <- count.since.start+1
             }else{            # annual time step
                this.year <- this.year+1
                start <- strptime(start.dateStr, format="%Y-%m-%d") # e.g., "1995-01-01"
                current <- paste0(this.year,'-01-01')
                end <- strptime(current, format="%Y-%m-%d") 
                dif <- end-start 
                count.since.start <- ceiling(as.numeric(dif[1])) 
             }
            tCntVec[tpos] <- count.since.start
          }
       }
     } else {
       tCntVec <- t.stepVec
     }
     timeStr <- paste0("days since ",start.dateStr,"T12:00:00 +0000")
  } 
   #--------------------------------
   # dimensions
   #--------------------------------
   #extent = c(UTMx.left, UTMx.right, UTMy.top, UTMy.bottom)
   #nx <- (extent[2] - extent[1])/cell.size
   #ny <- (extent[3] - extent[4])/cell.size
   #nt <- t.size 
   print("X Dim")
   dimX <- ncdim_def( "x", "", 1:length(seq(extent[1], extent[2], by = cell.size)), create_dimvar=FALSE, longname="x coordinate of projection")
   print("Y Dim")
   dimY <- ncdim_def( "y", "", 1:length(seq(extent[4], extent[3], by = cell.size)), create_dimvar=FALSE, longname="y coordinate of projection")   
   varTime <- NULL
   if (timeDim){
     print(paste("T Dim", 1:length(tCntVec)))
     dimT <- ncdim_def( "time", "", 1:length(tCntVec), unlim=FALSE, create_dimvar=FALSE, longname="time step" )
     print("T Var")
     varTime <- ncvar_def("time", timeStr, dimT, fill.value, prec="integer", longname="time step")
   }
   print("Y Var")
   varY <- ncvar_def("y", "meters", dimY, fill.value, prec="double", longname="y coordinate of projection")
   print("X Var")
   varX <- ncvar_def("x", "meters", dimX, fill.value, prec="double", longname="x coordinate of projection")
   #--------------------------------
   # varables 
   #--------------------------------
   mv <-  fill.value     # missing value to use 
   # prec argument Valid options: 'short' 'integer' 'float' 'double' 'char' 'byte'. 
   var_list <- vector("list", 1)
   i <- 1
   if (timeDim){
     var_list[[i]] <- varTime
     i <- i + 1
   }
   var_list[[i]] <- varY
   i <- i + 1
   var_list[[i]] <- varX
   i <- i + 1
   
   projec <- ncvar_def( "transverse_mercator", "meters", list(), prec="integer" )
   var_list[[i]] <- projec
   i <- i + 1
   
   j <- 1
   while(j <= length(layer.name)){
     if (timeDim){
       var_list[[i]] <- ncvar_def(layer.name[j], units[j], list(dimX,dimY,dimT), mv, prec=prec[j], longname=long.name[j] )
     } else {
       var_list[[i]] <- ncvar_def(layer.name[j], units[j], list(dimX,dimY), mv, prec=prec[j], longname=long.name[j] )
     }
     i <- i + 1
     j <- j + 1
   }   
     
   #---------------------
   # Create the nc file
   #---------------------
   print("Create")
   nc <- nc_create( out.name, var_list, force_v4=FALSE, verbose=FALSE )
   
   print("Write X")
   ncvar_put( nc, varX, seq(extent[1], extent[2], by = cell.size)) 
   print("Write Y")
   ncvar_put( nc, varY, seq(extent[4], extent[3], by = cell.size)) 
   if (timeDim){
     print("Write Time")
     ncvar_put( nc, varTime, tCntVec) 
   }
   
   #----------------------------------------------
   # Add additional attributes to the variables
   #----------------------------------------------
   ncatt_put( nc, "x", "_CoordinateAxisType","GeoX", prec="text")
   ncatt_put( nc, "x", "standard_name", "projection_x_coordinate", prec="text")
   ncatt_put( nc, "y", "_CoordinateAxisType","GeoY", prec="text")
   ncatt_put( nc, "y", "standard_name", "projection_y_coordinate", prec="text")
   if (timeDim){
     ncatt_put( nc, "time", "_CoordinateAxisType","Time", prec="text")
     ncatt_put( nc, "time", "standard_name", "time", prec="text")
   }
   ESRIstr <- 'PROJCS[\"NAD_1983_UTM_Zone_17N\",GEOGCS[\"GCS_North_American_1983\",DATUM[\"D_North_American_1983\",SPHEROID[\"GRS_1980\",6378137.0,298.257222101]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]],PROJECTION[\"Transverse_Mercator\"],PARAMETER[\"False_Easting\",500000.0],PARAMETER[\"False_Northing\",0.0],PARAMETER[\"Central_Meridian\",-81.0],PARAMETER[\"Scale_Factor\",0.9996],PARAMETER[\"Latitude_Of_Origin\",0.0],UNIT[\"Meter\",1.0]]'
   ncatt_put( nc, "transverse_mercator", "grid_mapping_name", "transverse_mercator", prec="text")
   ncatt_put( nc, "transverse_mercator", "longitude_of_central_meridian", -81.0, prec="double")
   ncatt_put( nc, "transverse_mercator", "latitude_of_projection_origin", 0.0, prec="double")
   ncatt_put( nc, "transverse_mercator", "scale_factor_at_central_meridian", 0.9996, prec="double")
   ncatt_put( nc, "transverse_mercator", "false_easting", 500000.0, prec="double")
   ncatt_put( nc, "transverse_mercator", "false_northing", 0.0, prec="double")
   ncatt_put( nc, "transverse_mercator", "semi_major_axis", 6378137.0, prec="double")
   ncatt_put( nc, "transverse_mercator", "semi_minor_axis", 6356752.314140356, prec="double")
   ncatt_put( nc, "transverse_mercator", "_CoordinateAxisTypes", "GeoY GeoX", prec="text")
   for (i in 1:length(layer.name)){
     ncatt_put( nc, layer.name[i], "esri_pe_string",  ESRIstr, prec="text")
     ncatt_put( nc, layer.name[i], "grid_mapping" ,"transverse_mercator", prec="text")
     ncatt_put( nc, layer.name[i], "min", layer.min[i]) 
     ncatt_put( nc, layer.name[i], "max", layer.max[i]) 
   }
   #-----------------------------------------------------------------
   # Add Global Attribute metadata (idvar==0) to file & close
   #-----------------------------------------------------------------
   ncatt_put( nc, 0, "institution", institution)
   ncatt_put( nc, 0, "source", source.name)
   ncatt_put( nc, 0, "qaqc", qaqc)
   ncatt_put( nc, 0, "comment", comments)
   ncatt_put( nc, 0, "Conventions", "CF-1.4")
   ncatt_put( nc, 0, "cerp_version", "CERP-1.2")
   
   return(nc)

}

###
### Write matrix to data layer
vec2nc <- function(nc.file, mat, layer, time_step=NULL ){
  if (!is.null(time_step)){
    ncvar_put( nc.file, layer, mat, start=c(1,1,time_step), count=c(-1,-1,1) ) 
  } else {
    ncvar_put( nc.file, layer, mat, start=c(1,1), count=c(-1,-1) ) 
  }
}

###
### close
closeNetCDF <- function(nc){
   nc_close(nc)
}
