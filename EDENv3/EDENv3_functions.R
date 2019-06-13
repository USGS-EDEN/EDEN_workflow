# Function that takes gages from a subarea and runs the RBF function
run_eden_rbf <- function(subarea_df, n_neigh, subarea_name){
  subarea_index <- grep(subarea_name, names(subareas_aniso))
  
  # Convert df to SpatialPoints df
  coordinates(subarea_df) <- ~x_aniso + y_aniso
  proj4string(subarea_df) <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  
  # Run RBF
  subarea_rbf <- rbf(stage_cm ~ x_aniso + y_aniso, data = subarea_df, func = "M", eta = 0, rho = 0, n.neigh = n_neigh, newdata = subareas_aniso[[subarea_index]])
  
  # Final clean up
  subarea_rbf <- cbind(subareas[[subarea_index]], subarea_rbf$var1.pred)
  colnames(subarea_rbf)[3] <- "stage"
  
  return(subarea_rbf)
}

eden_nc <- function(date_range, output_file){
  # Current quarter
  cur_qtr <- paste0(as.POSIXlt(Sys.Date())$year + 1900, quarters(Sys.Date()))
  # Surface creation quarter
  surf_qtr <- paste0(as.POSIXlt(date_range[1])$year + 1900, quarters(date_range[1]))
  
  gage_query <- "select station_name_web, agency_acronym as agency, utm_easting, utm_northing, dry_elevation, convert_to_navd88_feet as conv, area from station, agency, station_datum where station.database_agency_id = agency.agency_id and station.station_id = station_datum.station_id and"
  if (cur_qtr == surf_qtr) {
    # List of expected upload gages from EDENdb, realtime
    gage_query = paste(gage_query, "edenmaster_new = 1")
  } else {
    # List of expected upload gages from EDENdb, historic
    surf_qtr_strt <- as.POSIXlt(date_range[1])$year + 1900 + as.numeric(substr(quarters(date_range[1]), 2, 2)) / 4
    surf_qtr_end <- as.POSIXlt(date_range[1])$year + 1900 + as.numeric(substr(quarters(rev(date_range)[1]), 2, 2)) / 4
    gage_query <- paste(gage_query, "station.edenmaster_start_num <=", surf_qtr_strt, "and station.edenmaster_end_num >=", surf_qtr_end)
  }
  gage_query <- paste(gage_query, "group by station_name_web order by agency, station_name_web")
  gages <- dbGetQuery(con, gage_query)
  gages$agency[which(gages$agency == "ENP")] <- "NPS"
  # Default dry values for gages missing them
  gages$dry_elevation[which(is.na(gages$dry_elevation))] <- -9999
    
  # Create edenmaster
  master <- read.csv("./input/pseudogage_subareaID.csv", stringsAsFactors = FALSE)
  for (i in 1:dim(gages)[1]) {
    bits <- rev(unlist(strsplit(substr(paste(as.integer(intToBits(gages$area[i])), collapse = ""), 1, 8), "")))
    row <- c(gages$station_name_web[i], round(gages$utm_easting[i], digits = 1), round(gages$utm_northing[i], digits = 1), bits)
    master <- rbind(master, row)
  }
  print(paste("edenmaster generated for", surf_qtr))
    
  # Import gage data
  gage_list <- lapply(file_list, read.table, sep = "\t", header = TRUE)
  
  # Find first day of the simulations
  day1 <- min(date_range)
  
  # Time steps
  tDim <- length(date_range)
  
  ## --------------------------------------------------------------------------
  # Run interpolations & prepare netCDF
  
  # Run interpolation for each day
  interp_list <- lapply(gage_list, interpolate_gages, edenmaster = "edenmaster.csv")
  
  # Find min and max for netCDF attributes
  depth_min <- min(unlist(lapply(interp_list, function(df) min(df$stage))))
  depth_max <- max(unlist(lapply(interp_list, function(df) max(df$stage))))
  
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
  source_name     <- "eden_v3b.R"
  institution     <- "USGS"
  qaqc            <- "under review"
  comments        <- "Product derived from RBF interpolation of gages over the EDEN extent"
  
  nc_out <- createNetCDFfile(out.name = output_file,
                             layer.name = out_layer,
                             units = out_units,
                             prec = out_prec,
                             long.name = long_layer_name,
                             daily = TRUE,
                             extent = extent,
                             cell.size = cell_size,
                             fill.value = background,
                             source.name = source_name,
                             institution = institution,
                             qaqc = qaqc,
                             comments = comments,
                             start.dateStr = day1, 
                             t.size = tDim,
                             layer.min = depth_min,
                             layer.max = depth_max
  )
  
  # Convert df of interp to a matrix & export as netcdf
  for(j in 1:tDim){ # j <- 1
    print(paste("Day ", j, " of ", tDim))
    
    dt <- interp_list[[j]]
    # create matrix for netcdf
    out_mat <- acast(dt, X_COORD ~ Y_COORD, value.var = out_layer)
    # rotate matrix
    out_mat <- t(apply(out_mat, 1, rev)) 
    
    vec2nc(nc_out, out_mat, out_layer, j)
  }
  
  closeNetCDF(nc_out) 
  
}