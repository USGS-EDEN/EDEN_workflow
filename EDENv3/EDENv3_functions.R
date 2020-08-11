eden_nc <- function (interp_list, date_range, output_file) {
  # Find min and max for netCDF attributes
  depth_min <- min(unlist(lapply(interp_list, function (df) min(df$stage, na.rm = T))))
  depth_max <- max(unlist(lapply(interp_list, function (df) max(df$stage, na.rm = T))))

  ## -------------------------------------------------------------------------
  # Set up netCDF header info 
  xDim            <- 287
  yDim            <- 405
  cell_size       <- 400
  extent          <- c(463400, 577800, 2951800, 2790200)
  out_layer       <- "stage"
  long_layer_name <- "Water Stage (cm)"
  out_units       <- "cm"
  out_prec        <- "float"
  background      <- NaN                         
  source_name     <- "EDENv3.R"
  institution     <- "USGS"
  qaqc            <- "under review"
  comments        <- "Product derived from RBF interpolation of gages over the EDEN extent"

  nc_out <- createNetCDFfile(out.name = output_file,
                             layer.name = out_layer,
                             units = out_units,
                             prec = out_prec,
                             long.name = long_layer_name,
                             daily = T,
                             extent = extent,
                             cell.size = cell_size,
                             fill.value = background,
                             source.name = source_name,
                             institution = institution,
                             qaqc = qaqc,
                             comments = comments,
                             start.dateStr = min(date_range), 
                             t.size = length(date_range),
                             layer.min = depth_min,
                             layer.max = depth_max
  )
  
  # Convert df of interp to a matrix & export as netcdf
  for(j in 1:length(date_range)){ # j <- 1
    print(paste("Day ", j, " of ", length(date_range)))
    
    dt <- interp_list[[j]]
    # create matrix for netcdf
    out_mat <- acast(dt, X_COORD ~ Y_COORD, value.var = out_layer)

    vec2nc(nc_out, out_mat, out_layer, j)
  }
  
  closeNetCDF(nc_out) 
}
eden_nc_50m <- function (interp_list, date_range, output_file) {
  # Find min and max for netCDF attributes
  depth_min <- min(unlist(lapply(interp_list, function (df) min(df$stage, na.rm = T))))
  depth_max <- max(unlist(lapply(interp_list, function (df) max(df$stage, na.rm = T))))
  
  ## -------------------------------------------------------------------------
  # Set up netCDF header info 
  xDim            <- 287*8
  yDim            <- 405*8
  cell_size       <- 50
  extent          <- c(463225, 577975, 2951975, 2790025)
  out_layer       <- "stage"
  long_layer_name <- "Water Stage (cm)"
  out_units       <- "cm"
  out_prec        <- "float"
  background      <- NaN                         
  source_name     <- "EDENv3.R"
  institution     <- "USGS"
  qaqc            <- "under review"
  comments        <- "Product derived from RBF interpolation of gages over the EDEN extent"
  
  nc_out <- createNetCDFfile(out.name = output_file,
                             layer.name = out_layer,
                             units = out_units,
                             prec = out_prec,
                             long.name = long_layer_name,
                             daily = T,
                             extent = extent,
                             cell.size = cell_size,
                             fill.value = background,
                             source.name = source_name,
                             institution = institution,
                             qaqc = qaqc,
                             comments = comments,
                             start.dateStr = min(date_range), 
                             t.size = length(date_range),
                             layer.min = depth_min,
                             layer.max = depth_max
  )
  
  # Convert df of interp to a matrix & export as netcdf
  for(j in 1:length(date_range)){ # j <- 1
    print(paste("Day ", j, " of ", length(date_range)))
    
    dt <- interp_list[[j]]
    # create matrix for netcdf
    out_mat <- acast(dt, X_COORD ~ Y_COORD, value.var = out_layer)
    
    vec2nc(nc_out, out_mat, out_layer, j)
  }
  
  closeNetCDF(nc_out) 
}
eden_raster <- function (eden_layer, output_tif) {
  coordinates(eden_layer) <- ~X_COORD + Y_COORD
  proj4string(eden_layer) <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  gridded(eden_layer) <- T
  eden_layer <- raster(eden_layer)
  writeRaster(eden_layer, output_tif, "GTiff", overwrite = T, NAflag = -9999)
}
