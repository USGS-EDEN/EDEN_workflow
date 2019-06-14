# Function that takes gages from a subarea and runs the RBF function
run_eden_rbf <- function (subarea_df, n_neigh, subarea_name) {
  subarea_index <- grep(subarea_name, names(subareas_aniso))

  # Convert df to SpatialPoints df
  coordinates(subarea_df) <- ~x_aniso + y_aniso
  proj4string(subarea_df) <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

  # Run RBF
  subarea_rbf <- rbf(median ~ x_aniso + y_aniso, data = subarea_df, func = "M", eta = 0, rho = 0, n.neigh = n_neigh, newdata = subareas_aniso[[subarea_index]])

  # Final clean up
  subarea_rbf <- cbind(subareas[[subarea_index]], subarea_rbf$var1.pred)
  colnames(subarea_rbf)[3] <- "stage"

  return (subarea_rbf)
}

eden_raster <- function (eden) {
  coordinates(eden) <- ~X_COORD + Y_COORD
  proj4string(eden) <- CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  gridded(eden) <- T
  eden <- raster(eden)
  #writeRaster()
}

## Runs radial basis functions on the gages to interpolate a water surface over the EDEN extent
interpolate_gages <- function (input_gages, edenmaster) {
  # Merge with edenmaster to add coordinates, subarea classification
  gages <- merge(edenmaster, input_gages, all.x = T, by = "gage")
  class(gages[, "x_nad83_utm17n"]) <- class(gages[, "y_nad83_utm17n"]) <- "numeric"

  ## Add water level values for the 8 'pseudo' gages
  # Add values for the 3 pseudo-gages that were generated from the _Ex files
  gages[gages$gage == "pBCA19+LO1", ]$median <- (gages[gages$gage == "BCA19", ]$median + gages[gages$gage == "MO-214", ]$median) / 2
  gages[gages$gage == "pNP202NE1", ]$median <- (gages[gages$gage == "NP202", ]$median + gages[gages$gage == "NESRS1", ]$median) / 2
  gages[gages$gage == "pS12D_DN", ]$median <- gages[gages$gage == "S12D_T", ]$median

  # Add water level values for the 5 'pseudo' gages
  # - these are the ones manually determined to increase interpolation accuracy
  # - locations are based on pseudo-canal borders from EDEN_v2

  # This one on WCA1 - WCA2A border reduces error in the northern pt
  gages[gages$gage == "pseudo_canal_1", ]$median <- gages[gages$gage == "S10D_T", ]$median

  # Create linear eqns for the four on the WCA3A/B border
  # - adding these reduces error on the 3A side
  upper <- gages[gages$gage == "S151_H", ]$median
  lower <- gages[gages$gage == "S333_H", ]$median
  # 32.57 km between the two gages (as canal distance, not straight-line)
  slope <- (upper - lower) / 32.57 

  # 13.48 km = dist b/t upper gage and pseudo gage 2
  gages[gages$gage == "pseudo_canal_2", ]$median <- upper - (slope * 13.48)
  # 15.88 km = dist b/t upper gage and pseudo gage 3
  gages[gages$gage == "pseudo_canal_3", ]$median <- upper - (slope * 15.88)
  # 25.28 km = dist b/t upper gage and pseudo gage 4
  gages[gages$gage == "pseudo_canal_4", ]$median <- upper - (slope * 25.28)
  # 30.14 km = dist b/t upper gage and pseudo gage 5
  gages[gages$gage == "pseudo_canal_5", ]$median <- upper - (slope * 30.14)

  ## --------------------------------------------------------------------------
  ## Remove gages that don't have measurements for that day
  no_na_values <- sum(is.na(gages$median))
  print(paste0("The number of missing gages on this day is: ", no_na_values))
  if (no_na_values) print(paste0("Missing data are from gage stations: ", gages[is.na(gages$median), ]$gage))
  gages <- na.omit(gages)

  ## --------------------------------------------------------------------------
  # Convert gage data to anisotropic coordinates
  gages_aniso <- as.data.frame(coords.aniso(coords = gages[, c("x_nad83_utm17n", "y_nad83_utm17n")], aniso.pars = c(350 * pi / 180, 31 / 30)))
  colnames(gages_aniso) <- c("x_aniso", "y_aniso")
  gages <- cbind(gages, gages_aniso)

  ## --------------------------------------------------------------------------
  # Create dataframe for each subzone classification
  wca1_gages <- gages[gages$wca1 == 1, ]
  wca2a_gages <- gages[gages$wca2a == 1, ]
  wca2b_gages <- gages[gages$wca2b == 1, ]
  wca3a_gages <- gages[gages$wca3a == 1, ]
  wca3b_gages <- gages[gages$wca3b == 1, ]
  pw_gages <- gages[gages$pw == 1, ]
  l67ext_gages <- gages[gages$l67ext == 1, ]
  other_gages <- gages[gages$other == 1, ]

  ## --------------------------------------------------------------------------
  print("Running RBF interpolation...")

  wca1_rbf <- run_eden_rbf(wca1_gages, 8, "wca1")
  wca2a_rbf <- run_eden_rbf(wca2a_gages, 8, "wca2a")
  wca2b_rbf <- run_eden_rbf(wca2b_gages, 8, "wca2b")
  wca3a_rbf <- run_eden_rbf(wca3a_gages, 8, "wca3a")
  wca3b_rbf <- run_eden_rbf(wca3b_gages, 8, "wca3b")
  pw_rbf <- run_eden_rbf(pw_gages, nrow(pw_gages) - 1, "pw")
  l67ext_rbf <- run_eden_rbf(l67ext_gages, 8, "l67ext")
  other_rbf <- run_eden_rbf(other_gages, 8, "other")

  ## --------------------------------------------------------------------------
  # Bind everything together
  eden <- rbind(wca1_rbf, wca2b_rbf, wca3b_rbf, pw_rbf, other_rbf, wca2a_rbf, l67ext_rbf, wca3a_rbf)

  return (eden)
}

# Create edenmaster list and retrieve gage data for date range
eden <- function (date_range) {
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

  # Create edenmaster list
  master <- read.csv("./input/pseudogage_subareaID.csv", stringsAsFactors = FALSE)
  for (i in 1:dim(gages)[1]) {
    bits <- rev(unlist(strsplit(substr(paste(as.integer(intToBits(gages$area[i])), collapse = ""), 1, 8), "")))
    row <- c(gages$station_name_web[i], round(gages$utm_easting[i], digits = 1), round(gages$utm_northing[i], digits = 1), bits)
    master <- rbind(master, row)
  }
  print(paste("edenmaster generated for", surf_qtr))

  # Retrieve gage data
  gage_list <- setNames(replicate(length(date_range), data.frame()), date_range)
  for (i in 1:length(date_range)) {
    data_query <- paste0("select `stage_", gages$station_name_web, "`+", gages$conv, " as stage from stage where datetime >= ", format(date_range[i], "%Y%m%d010000"), " and datetime < ", format(date_range[i] + 1, "%Y%m%d000001"), " order by datetime")
    dt <- data.frame(gage = gages$station_name_web, median = NA)
    for (j in 1:length(data_query))
      dt$median[j] <- median(as.numeric(unlist(dbGetQuery(con, data_query[j])))) * 12 * 2.54
    gage_list[[i]] <- dt
  }

  # Run interpolation for each day
  interp_list <- lapply(gage_list, interpolate_gages, edenmaster = master)

  return (interp_list, output_file)
}

eden_nc <- function (interp_list, output_file) {
  # Find min and max for netCDF attributes
  depth_min <- min(unlist(lapply(interp_list, function (df) min(df$stage))))
  depth_max <- max(unlist(lapply(interp_list, function (df) max(df$stage))))

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
                             daily = TRUE,
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
    # rotate matrix
    out_mat <- t(apply(out_mat, 1, rev)) 
    
    vec2nc(nc_out, out_mat, out_layer, j)
  }
  
  closeNetCDF(nc_out) 
}
