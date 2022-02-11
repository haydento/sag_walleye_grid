#' @title simple clean up reef information, prepare for plotting
#' @description remove degrees symbol in lat/long, simplify column names, convert to sf object
#' @param x table of reefs information
#'
#' @examples
#' tar_load(dirty_reefs)
#' x <- dirty_reefs
#' clean_reefs(dirty_reefs)

clean_reefs <- function(x = dirty_reefs){
  x[, c("Long", "Lat") := list(as.numeric(sub("\\p{So}", "", x$Long, perl = TRUE)), as.numeric(sub("\\p{So}", "", x$Lat, perl = TRUE)))]
  x <- x[, c("Reef", "Lat", "Long")]
  setnames(x, names(x), tolower(names(x)))
  x <- st_as_sf(x, coords = c("long", "lat"), remove = FALSE, crs = 4326, agr = "constant")
  return(x)
}  


#' @title create custom leaflet interactive map of planning data used for study design
#' @description Creates map that overlays two sources of bathymetry (lidar and GL bathymetry), receiver locations, sentinal tag locations, area of proposed glider patrols, mobile tracking listening stations
#' @param bathy file path to bathymetry layer (tif)
#' @param grid sf points object containing proposed LWF grid

#' @examples
#' tar_load(bathy)
#' tar_load(grid)
#' tar_load(sbay)
#' tar_load(reefs)
#' tar_load(rec_grid)
#' tar_load(sim_dtc_high_perf)
#' tar_load(sim_tracks)
#' sim_fish = sim_tracks
#' tar_load(in_bay)
#' grid_map(bathy, lidar, sentinal, rec_depth, glid_area = glider, mobile = mobile_listen)

# create leaflet map
grid_map <- function(bathy, grid, sbay, reefs, rec_grid, spawn_rivers, bay_mth, pth, sim_fish){

  sim_fish[min_dist <= 2000, col := "red"][is.na(col), col := "grey"]
  trks <- st_as_sf(sim_fish, coords = c("X", "Y"), agr = "constant", crs = 4326)
  trks <- st_transform(trks, 3175)
  setDT(trks)
  trks <- trks[, .(col = col, geometry = st_combine(geometry)), by = fish]
  trks <- st_as_sf(trks)
  trks <- st_cast(trks, "LINESTRING")
  trks <- st_transform(trks, 4326)
  
  bath <- terra::rast(bathy)
#  bath <- terra::aggregate(bath, fact = 4)
  bath <- stars::st_as_stars(bath)
    
  brks_bath <- brks(x = bath, n = 100)
  pal_LH <- leaflet::colorNumeric(c(viridis::viridis(100)), bath[[1]], na.color = "transparent")

  # create leaflet map
  m <- leaflet()
  m <- setView(m, zoom = 15, lat = 45.537 , lng = -83.999)
  m <- addTiles(m)
  m <- leafem::addGeoRaster(m, bath, opacity = 1, colorOptions = leafem::colorOptions(palette = viridis::viridis(256), breaks = brks_bath$brks), group = "bathy (ft)")
  m <- addTiles(m, urlTemplate = "http://tileservice.charts.noaa.gov/tiles/50000_1/{z}/{x}/{y}.png", group = "nav chart")
  m <- addProviderTiles(m, providers$Esri.WorldImagery, group = "satellite")
  m <- addProviderTiles(m, providers$Esri.NatGeoWorldMap, group = "alt")
 # m <- addPolylines(map = m, data = glider_pth, lng = ~lon, lat = ~lat, color = "green")
#  m <- addMarkers(m, lng = -83.58845, lat = 44.08570, label = "release")

  m <- addCircleMarkers(m, data = grid, label = grid$station, color = c("red"), radius = c(4), group = "LWF recs", stroke = FALSE, fillOpacity = 1)
  m <- addCircleMarkers(m, data = reefs, label = ~reef, color = c("blue"),   radius = c(6),  group = "bay reefs",     stroke = FALSE, fillOpacity = 1)
  m <- addCircleMarkers(m, data = rec_grid, color = c("yellow"), radius = c(10), group = "proposed recs (bay grid)", stroke = FALSE, fillOpacity = 1)
  m <- addCircleMarkers(m, data = bay_mth, color = "orange", radius = 10, group = "proposed recs (bay mth)", stroke = FALSE, fillOpacity = 1)
  m <- addMarkers(m, data = spawn_rivers, label = ~river, group = "spawn rivers")
  m <- addGlPolylines(m, data = trks, group = "sim fish trks", color = ~col, weight = 0.5)
                      
#  m <- addCircleMarkers(m, data = sync_100, label = ~label, fillColor = "yellow", radius = 8, group = "sentinel tag", stroke = FALSE, fillOpacity = 1)
#  m <- addCircleMarkers(m, data = all_mob, label = ~label_short, fillColor = "orange", radius = 8, group = "mobile", stroke = FALSE, fillOpacity = 1)
  m <- addPolygons(map = m, data = sbay, color  = "red", fillColor = NA, group = "sag bay")
  m <- leafem::addMouseCoordinates(m)
  m <- addLegend(m, pal = pal_LH , values =  bath[[1]], title = "depth (ft)", opacity = 1, group = "bathy (ft)")
#  m <- addLegend(m, pal = pal_lidar, values = lid[[1]], title = "depth (lidar, ft)", opacity = 1, group = "lidar_depth")
#  m <- addLayersControl(m, overlayGroups = c("LH_depth", "lidar_depth", "receivers", "sentinel tag", "mobile", "glider patrol"), options = layersControlOptions(collapsed = FALSE))
    m <- addMeasure(m, primaryLengthUnit = "meters", secondaryLengthUnit = "kilometers")  
  m <- addLayersControl(m, baseGroups = c("satellite", "nav chart", "alt", "bathy (ft)"), overlayGroups = c("LWF recs", "sag bay", "bay reefs", "proposed recs (bay grid)", "spawn rivers", "proposed recs (bay mth)", "sim fish trks"), position = "bottomright", options = layersControlOptions(collapsed = FALSE))
  
  htmlwidgets::saveWidget(m, pth)

  return(pth)
}



#################
#' @title find grid in polygon
#' @description calculates equally spaced grid within polygon
#' @param sag_poly polygon in which grid is desired
#' @param cellsize distance between receivers in grid (in meters)
#' @param in_crs temporary crs used to calculate grid
#' @param out_crs output crs of grid
#' @value sf point object

#' @examples
#' tar_load(sbay)
#' cellsize = c(10000,10000)
#' in_crs = 3175
#' out_crs = 4326
#' tst <- grid(poly = sbay, cellsize = c(10000,10000), in_crs = 3175, out_crs = 4326)
#' plot(st_geometry(sbay))
#' plot(st_geometry(tst), add = TRUE)

.grid <- function(poly, cellsize, in_crs = 3175, out_crs = 4326){

  y <- st_transform(poly, crs = in_crs)
  x <- st_make_grid(y, what = "centers", cellsize = cellsize)
  x <- st_as_sf(x)
  x <- st_join(x = x, y = y, left = FALSE)
  x <- st_transform(x, crs = out_crs)

  return(x)
}

#' @title import polygon GIS layer and subset with another layer to get intersecting region
#' @description Reads polygon layer and uses sf::st_intersection to extract overlapping layer.  Overlapping layer is the outline of Saginaw Bay.
#' @param x gis layer to load
#' @param y gis layer 2 used to find intersection
#' @examples
#' tar_load(sag_bay_mth_raw)
#' x <- sag_bay_mth_raw
#' tar_load(sbay)
#' y <- sbay
#' 


.intersect <- function(x = sag_bay_mth_raw, y = sbay){
  out <- sf::st_read(x, quiet = TRUE, agr = "constant")
  out <- sf::st_intersection(out,y)
  return(out)
}


#' tar_load(sbay)
#' x <- sbay
#' tar_load(sag_bay_mth)
#' y <- sag_bay_mth


.difference <- function(x = sbay, y = sag_bay_mth){
  in_b <- st_difference(st_union(x), y)
  in_b <- st_as_sf(in_b)
  return(in_b)
}



fix_CRS <- function(){
  data(greatLakesPoly)
  GL <- greatLakesPoly
  crs <- CRS(SRS_string = "EPSG:4326")
  slot(GL, "proj4string") <- crs
  return(GL)
}

#############################

# development/fix of simulation functions
# add in crs info for >proj.6
## data(greatLakesPoly)
## GL <- greatLakesPoly
## crs <- CRS(SRS_string = "EPSG:4326")
## slot(GL, "proj4string") <- crs

# Crop out LH (using sf)
crop <- function(GL, xmin = -84.387, xmax = -82.0915, ymin = 43, ymax = 45){
  GL <- st_as_sf(GL, agr = "constant")
  LH_crop <- st_bbox(c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), crs = 4326, agr = "constant")
  LH <- st_crop(GL, LH_crop, agr = "constant")
  return(LH)
}

#LH_sp <- as(LH, "Spatial")
#plot(st_geometry(LH))

## crw_in_polygon_updated <- function (polyg, theta = c(0, 10), stepLen = 100, initPos = c(NA, 
##     NA), initHeading = NA, nsteps = 30, EPSG = 3175, sp_out = TRUE, 
##     show_progress = TRUE) 
## {
##     if (!inherits(polyg, c("SpatialPolygonsDataFrame", "SpatialPolygons", 
##         "Polygons", "Polygon"))) 
##         polyg <- sp::Polygon(polyg, hole = FALSE)
##     if (!inherits(polyg, c("SpatialPolygonsDataFrame", "SpatialPolygons", 
##         "Polygons"))) 
##         polyg <- sp::Polygons(list(polyg), ID = 1)
##     #    projargs <- paste0("+init=epsg:", EPSG)
##     projargs <- CRS(SRS_string = paste0("EPSG:", EPSG))
##     if (!inherits(polyg, c("SpatialPolygonsDataFrame", "SpatialPolygons"))) {
##       #polyg <- sp::SpatialPolygons(list(polyg), proj4string = sp::CRS(projargs))
##       polyg <- sp::SpatialPolygons(list(polyg))
##       slot(polyg, "proj4string") <- projargs
##       projargs_in <- projargs
##     }
##     else {
##       # projargs_in <- sp::proj4string(polyg)
##       projargs_in <- wkt(polyg) 
##     }
##     #if (!identical(sp::proj4string(polyg), rgdal::CRSargs(sp::CRS(projargs)))) {
##     if (!identical(wkt(polyg), wkt(projargs))){
##       #polyg <- sp::spTransform(polyg, sp::CRS(projargs))
##       polyg <- sp::spTransform(polyg, projargs)
##     }
##     if (any(is.na(initPos))) {
##         inPoly <- FALSE
##         while (inPoly == FALSE) {
##             init <- c(runif(1, floor(sp::bbox(polyg)["x", "min"]), 
##                 ceiling(sp::bbox(polyg)["x", "max"])), runif(1, 
##                 floor(sp::bbox(polyg)["y", "min"]), ceiling(sp::bbox(polyg)["y", 
##                   "max"])))
##             init <- sp::SpatialPoints(matrix(as.numeric(init), 
##                 nrow = 1), proj4string = sp::CRS(sp::proj4string(polyg)))
##             inPoly <- rgeos::gDistance(polyg, init) == 0
##             inPoly <- switch(inPoly + 1, FALSE, TRUE, FALSE, 
##                 FALSE)
##         }
##     }
    
##     if (all(!is.na(initPos))) {
##         init <- sp::SpatialPoints(matrix(as.numeric(initPos), nrow = 1))#, proj4string = sp::CRS(projargs_in))
##         slot(init, "proj4string") <- CRS(projargs_in)
##         init <- sp::spTransform(init, projargs)
##         inPoly <- rgeos::gDistance(polyg, init) == 0
##         if (!inPoly) 
##             stop("initPos is outside polygon boundary.")
##     }
    
##     if (is.na(initHeading)) 
##         initHeading <- runif(1, 0, 360)

##     path.fwd <- data.frame(x = rep(NA, nsteps + 1), y = NA)
##     path.fwd[1, ] <- sp::coordinates(init)
##     xl <- methods::as(polyg, "SpatialLines")
##     rows_i <- 1
##     init_i <- init
##     dist_i <- rgeos::gDistance(init_i, xl)
##     nsteps_i <- (dist_i%/%stepLen) + 1
##     rows_i <- 1 + 1:nsteps_i
##     rows_i <- rows_i[rows_i <= (nsteps + 1)]
##     k <- 0
##     if (show_progress) {
##         message("Simulating tracks...")
##         pb <- txtProgressBar(min = 0, max = nsteps, initial = 0, 
##             style = 3)
##     }
##     while (max(rows_i) <= (nsteps + 1)) {
##         theta_i <- c(theta[1], theta[2] * (1 + 0.1 * k^2))
##         path.fwd.i <- crw(theta = theta_i, stepLen = stepLen, 
##             initPos = as.vector(sp::coordinates(init_i)), initHeading, 
##             nsteps = length(rows_i))

##         check_in_polygon <- function(points, polygon) {
##             points_sp <- sp::SpatialPoints(as.matrix(points))#, 
##             # proj4string = sp::CRS(sp::proj4string(polygon)))
##             slot(points_sp, "proj4string") <- CRS(wkt(polygon))
            
##             inPoly <- sapply(1:nrow(sp::coordinates(points)), 
##                 function(i) rgeos::gDistance(polygon, points_sp[i, 
##                   ]) == 0)
##             return(inPoly)
##         }
##         inPoly <- check_in_polygon(path.fwd.i, polyg)
##         if (all(!inPoly)) {
##             k <- k + 1
##             next
##         }
##         if (any(!inPoly)) 
##             rows_i <- rows_i[inPoly]
##         k <- 0
##         path.fwd[rows_i, ] <- path.fwd.i[inPoly, ]
##         init_i <- sp::SpatialPoints(path.fwd[max(rows_i), ])#, 
##             slot(init_i, "proj4string") <- projargs

##         dist_i <- rgeos::gDistance(init_i, xl)
##         initHeading <- vector_heading(path.fwd$x[max(rows_i) - 
##             1:0], path.fwd$y[max(rows_i) - 1:0])
##         nsteps_i <- (dist_i%/%stepLen) + 1
##         rows_i <- max(rows_i) + 1:nsteps_i
##         rows_i <- min(rows_i[rows_i <= (nsteps + 1)], nsteps + 
##             2)
##         if (show_progress) {
##             setTxtProgressBar(pb, max(rows_i))
##             if (max(rows_i) > (nsteps + 1)) 
##                 close(pb)
##         }
##     }
##     if (show_progress) message("Done.")
##     path.fwd.sp <- sp::SpatialPoints(path.fwd)
##     slot(path.fwd.sp, "proj4string") <- projargs
##     path.fwd.sp <- sp::spTransform(path.fwd.sp, CRSobj = CRS(projargs_in))
##     if (!sp_out) 
##       path.fwd.sp <- as.data.frame(sp::coordinates(path.fwd.sp))
##     return(path.fwd.sp)
## }

########
#' tar_load(sim_tracks)
#' path = sim_tracks[fish == 1,]
#' vel = 0.5
#' delayRng = c(60,180)
#' burstDur = 5
#' EPSG = 3175



transmit_along_path_updated <- function (path = NA, vel = 0.5, delayRng = c(60, 180), burstDur = 5, 
    EPSG = 3175, sp_out = TRUE) 
{
  
  path <- sf::st_as_sf(path, coords = c("X", "Y"), agr = "constant", crs = 4326, remove = FALSE)
  path <- st_transform(path, crs = EPSG)
  projargs_in <- 4326

  path <- data.frame(st_coordinates(path))

  path$cumdistm <- c(0, cumsum(sqrt(diff(path$X)^2 + diff(path$Y)^2)))
  path$etime <- path$cumdistm/vel
  ntrns <- max(path$etime)/(delayRng[1] + burstDur)
  ints <- runif(ntrns, delayRng[1] + burstDur, delayRng[2] + burstDur)
  ints[1] <- runif(1, 0, ints[1])
  etime <- cumsum(ints)
  etime <- etime[etime <= max(path$etime)]
  trns <- data.frame(X = approx(path$etime, path$X, xout = etime)$y, 
                     Y = approx(path$etime, path$Y, xout = etime)$y, et = etime)

trns <- trns[, c("X", "Y", "et")]
trns <- sf::st_as_sf(trns, coords = c("X","Y"), agr = "constant", crs = EPSG)
trns <- st_transform(trns, crs = projargs_in)
trns <- as_Spatial(trns)
  
  if (!sp_out) {
    trns <- as.data.frame(cbind(sp::coordinates(trns), et = trns$et))
    names(trns) <- c("X", "Y", "et")
  }
  return(trns)
}

##################
######################
# detect transmisions


#' trnsLoc = out[fish == 1, c("X", "Y", "et")]
#' tar_load(bay_mth_grd)
#' recLoc <- as.data.table(st_coordinates(bay_mth_grd))
#' pdrf <- function(dm, b = c(0.5, -1/120)){
#'   p <- 1/(1+exp(-(b[1]+b[2]*dm)))
#'   return(p)
#' }
#' detRngFun = pdrf
#' EPSG = 3175
#' sp_out = FALSE
#' show_progress = FALSE
#' detect_transmissions_updated(trnsLoc = trnsLoc, recLoc = recLoc, detRngFun = pdrf, EPSG = 3175, sp_out = FALSE, show_progress = TRUE)

 detect_transmissions_updated <- function (trnsLoc = NA, recLoc = NA, detRngFun = NA, EPSG = 3175, 
    sp_out = TRUE, show_progress = TRUE) 
{
    if (inherits(trnsLoc, "data.frame")) {
        missingCols <- setdiff(c("X", "Y", "et"), names(trnsLoc))
        if (length(missingCols) > 0) 
            stop(paste0("'trnsLoc' must contain the ", "following columns: \n", 
                paste(missingCols, collapse = "\n")))
    }
    if (inherits(recLoc, "data.frame")) {
        missingCols <- setdiff(c("X", "Y"), names(recLoc))
        if (length(missingCols) > 0) 
            stop(paste0("'recLoc' must contain the ", "following columns: \n", 
                paste(missingCols, collapse = "\n")))
    }
    #projargs <- paste0("+init=epsg:", EPSG)
    projargs <- CRS(SRS_string = paste0("EPSG:", EPSG))

    if (!inherits(trnsLoc, c("SpatialPointsDataFrame", "SpatialPoints"))) {
      trnsLoc <- sp::SpatialPointsDataFrame(trnsLoc[, c("X", "Y")], data = trnsLoc)
      slot(trnsLoc, "proj4string") <- CRS(SRS_string = "EPSG:4326")
      projargs_in <- CRS(wkt(trnsLoc))
    }
    else {
        projargs_in <- CRS(wkt(trnsLoc))
    }
    if (!identical(wkt(trnsLoc), wkt(projargs))) {
        trnsLoc <- sp::spTransform(trnsLoc, projargs)
    }
    if (!inherits(recLoc, "data.frame") & all(is.numeric(recLoc))) {
        if (length(recLoc) != 2) 
            stop(paste("recLoc must be data.frame or 2-column vector (x,y)"))
        recLoc <- data.frame(X = recLoc[1], Y = recLoc[2])
    }
    if (!inherits(recLoc, c("SpatialPointsDataFrame", "SpatialPoints"))) {
      recLoc <- sp::SpatialPoints(recLoc)
      slot(recLoc, "proj4string") <- CRS(SRS_string = "EPSG:4326")
        rec_projargs_in <- CRS(wkt(recLoc))
    }
    else {
        rec_projargs_in <- recLoc
    }
    if (!identical(wkt(recLoc), wkt(projargs))) {
        recLoc <- sp::spTransform(recLoc, projargs)
    }
    recLoc <- as.data.frame(sp::coordinates(recLoc))
    trnsLoc <- as.data.frame(cbind(sp::coordinates(trnsLoc), 
        et = trnsLoc$et))
    names(recLoc)[1:2] <- c("X", "Y")
    names(trnsLoc)[1:2] <- c("X", "Y")

    dtc <- data.table(trns_id = NA_real_, recv_id = NA_real_, recv_x = NA_real_, 
                      recv_y = NA_real_, trns_x = NA_real_, trns_y = NA_real_, etime = NA_real_, stringsAsFactors = FALSE)[0,]


    
    ## dtc <- data.frame(trns_id = NA, recv_id = NA, recv_x = NA, 
    ##     recv_y = NA, trns_x = NA, trns_y = NA, etime = NA, stringsAsFactors = FALSE)[0, 
    ##     ]
    for (g in 1:nrow(recLoc)) {
      if (g == 1 & show_progress) {
        pb <- txtProgressBar(min = 0, max = nrow(recLoc), 
                             style = 3)
      }
      distM.g <- sqrt((trnsLoc$X - recLoc$X[g])^2 + (trnsLoc$Y - 
                                                       recLoc$Y[g])^2)
      detP.g <- detRngFun(distM.g)
      succ.g <- as.logical(rbinom(length(detP.g), 1, detP.g))
      if (sum(succ.g) > 0) {
        dtc.g <- data.frame(trns_id = which(succ.g), recv_id = g, 
                            recv_x = recLoc$X[g], recv_y = recLoc$Y[g], trns_x = trnsLoc$X[succ.g], 
                trns_y = trnsLoc$Y[succ.g], etime = trnsLoc$et[succ.g])
            dtc <- rbind(dtc, dtc.g)
        }
        if (show_progress) {
            info <- sprintf("%d%% done", round(g/nrow(recLoc) * 
                100))
            setTxtProgressBar(pb, g)
            if (g == nrow(recLoc)) 
                close(pb)
        }
    }
    dtc <- dtc[order(dtc$etime), ]
    if (nrow(dtc) > 0) {
      dtc_recv <- sp::SpatialPoints(dtc[, c("recv_x", "recv_y")])
      slot(dtc_recv, "proj4string") <- projargs                                 
      dtc_recv <- sp::spTransform(dtc_recv, CRSobj = projargs_in)

      dtc_trns <- sp::SpatialPoints(dtc[, c("trns_x", "trns_y")])
      slot(dtc_trns, "proj4string") <- projargs
      dtc_trns <- sp::spTransform(dtc_trns, CRSobj = projargs_in)
      
      dtc <- as.data.frame(cbind(dtc[, c("trns_id", "recv_id")], 
            sp::coordinates(dtc_recv), sp::coordinates(dtc_trns), 
            etime = dtc$etime))
      

      ## for(i in 1:length(names(dtc))){
      ##   set(dtc, j = names(dtc)[i], value = as.numeric(dtc[[names(dtc)[i]]]))
      ## }
      ## cols <- c("trns_id", "recv_id", "recv_x", "recv_y", "trns_x", "trns_y", "etime")
      ## dtc[, (cols) := list(as.numeric(trns_id), as.numeric(recv_id), as.numeric(recv_x), as.numeric(recv_y), as.numeric(trns_x), as.numeric(trns_y), as.numeric(etime))]
      ## browser()

      ## browser()
      
      if(nrow(dtc) == 0){
        dtc <- data.table(trns_id = NA_real_, recv_id = NA_real_, recv_x = NA_real_, recv_y = NA_real_, trns_x = NA_real_, trns_y = NA_real_, etime = NA_real_)
           
##       dtc[, trns_id := as.numeric(trns_id)]
##       dtc[, recv_id := as.numeric(recv_id)]
##       dtc[, recv_x := as.numeric(recv_x)]
##       dtc[, recv_y := as.numeric(recv_y)]
##       dtc[, trns_x := as.numeric(trns_x)]
##       dtc[, trns_y := as.numeric(trns_y)]
##       dtc[, etime := as.numeric(etime)]
}
      # foo <- data.table(x = TRUE, y = FALSE)
      #bar <- foo[0]
      #str(bar)
     # bar[1, x := NA_real_]
      ## dtc <- rbind(bar, list(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))

        if (sp_out) {
          dtc <- sp::SpatialPointsDataFrame(dtc_recv, data = dtc)  
        }
    }
    return(dtc)
}

##################


     pdrf <- function(dm, b=c(0.5, -1/120)){
       p <- 1/(1+exp(-(b[1]+b[2]*dm)))
       return(p)
     }


#############################################
## tar_load(LH)
## LH_sp <- as(LH, "Spatial")
## tar_load(bay_mth_grd)
## recLoc <- st_coordinates(bay_mth_grd)
## recLoc <- data.table(x = recLoc[,1], y = recLoc[,2])
## ## # https://inbo.github.io/tutorials/tutorials/spatial_crs_coding/

## # simulate one fish movement
## trk  <- crw_in_polygon_updated(polyg = LH_sp, theta = c(0,25), stepLen = 10000, initHeading = 0, nsteps = 100, initPos = c(-83.6258, 43.8824), sp_out = FALSE)

## # simulate multiple fish movements
## trks <- replicate(50, crw_in_polygon_updated(polyg = LH_sp, theta = c(0,25), stepLen = 10000, initHeading = 0, nsteps = 100, initPos = c(-83.6258, 43.8824), sp_out = FALSE, show_progress = FALSE), simplify = FALSE)

## # combine all fish into single data.table
## trks <- rbindlist(trks, idcol = "fish")

# determine distance between track linestring and all receivers
# create linestrings

#' tar_load(sim_tracks)
#' fsh_trks = sim_tracks
#' tar_load(bay_mth_grd)
#' rec_grid <- bay_mth_grd
#' sim_fish_rec_dist(fsh_trks, rec_grid)

sim_fish_rec_dist <- function(fsh_trks, rec_grid){

  trks <- st_as_sf(fsh_trks, coords = c("x", "y"), agr = "constant", crs = 4326)
  trks <- st_transform(trks, crs = 3175)
  setDT(trks)
  trks <- trks[, .(geometry = st_combine(geometry)), by = .(fish)]
  trks <- st_as_sf(trks, agr = "constant")
  trks <- st_cast(trks, "LINESTRING")
  bay_mth_grd <- st_transform(rec_grid, crs = 3175)
  dist <- st_distance(bay_mth_grd, trks)
  min_dist <- apply(dist, 2, min)
  out <-  data.table(fish = 1:max(fsh_trks$fish), min_dist = min_dist)
  return(out)
}



#################
#' tar_load(sim_tag_trans)
#' dtc_trans = sim_tag_trans
#' tar_load(bay_mth_grd)
#' recLoc <- bay_mth_grd
#' tar_load(min_dist)
#' detRngFun = pdrf
#' EPSG = 3175
#' sp_out = FALSE
#' show_progress = FALSE
#' rc_intercept = 0.5
#' rc_slope = -1/120
#' tar_load(sim_tracks)
#' min_dist = sim_tracks
#' ba = c(1.4, -0.00433)
#' .sim_dtc(dtc_trans = sim_tag_trans, recLoc = bay_mth_grd, EPSG = 3175, sp_out = FALSE, show_progress = FALSE, min_dist, ba = c(2.913904, -0.0051440101))


.sim_dtc <- function(dtc_trans, recLoc, EPSG, sp_out, show_progress, min_dist, ba){
  recLoc <- st_coordinates(recLoc)
  recLoc <- data.table(X = recLoc[,1], Y = recLoc[,2])
  min_dist <- data.table(unique(min_dist[, c("fish", "min_dist")], by = "fish"), rc_intercept = ba[1], rc_slope = ba[2]) 

  pdrf <- function(dm, b=ba){
    p <- 1/(1+exp(-(b[1] + b[2]*dm)))
    return(p)
  }
  
  out <- dtc_trans[, detect_transmissions_updated(trnsLoc = .SD,
                                                  detRngFun = pdrf,
                                                  recLoc = recLoc,
                                                  EPSG = EPSG,
                                                  sp_out = sp_out,
                                                  show_progress = show_progress),
                   by = .(fish), .SDcols = c("X", "Y", "et")]

  out <- merge(out, min_dist, all.y = TRUE, by.x = "fish", by.y = "fish")

  # total number of virtual fish
  out[, num_sim_fish := max(fish)]

  # total number of times each fish was detected.  fish not detected = NA 
  out[!is.na(recv_id), times_dtc := nrow(.SD), by = "fish"]

  # total number of fish detected at least once
  out[!is.na(recv_id), num_dtc := uniqueN(fish)]

  # overall detection probabiliity
  out[!is.na(recv_id), array_dtc_prob := num_dtc/num_sim_fish]

  # throw fish that didn't get within 2000 m of any receiver and count
  out[min_dist <= 2000, adj_sim_fish := uniqueN(fish)]

  # count number of fish within 2000 m of receiver that were detected
  out[!is.na(recv_id) & min_dist <= 2000, adj_num_dtc := uniqueN(fish)]
  
  # adjust detection probablity to account for fish that never got close to receiver
  out[min_dist <= 2000, adj_array_dtc_prob := adj_num_dtc/adj_sim_fish]
  
  return(out[])
}

###################

## rng_curve <- raw_log_reg_H_GBE
## rng_crv <- fread(rng_curve)

## all <- CJ(Interval = rng_crv$Interval, tr_dist = seq(0,2000,250))
## rng_crv <- rng_crv[all, on = "Interval"]
## rng_crv[, dtc_p := pdrf(tr_dist, intercept = Intercept, slope = Slope)]

## plot(dtc_p ~ tr_dist, data = rng_crv[Interval == 72,], ylim = c(0,1), type = "o")






## plot(dtc_p ~ tr_dist, data = rng_crv, type = "p")
## plot(dtc_p ~ as.factor(tr_dist), data = rng_crv)
## plot(Slope ~ as.factor(tr_dist), data = rng_crv)
## plot(Intercept ~ as.factor(tr_dist), data = rng_crv)

## quantile(rng_crv[tr_dist == 750, dtc_p], 0.75)

## foo <- rng_crv[dtc_p >= 0.28 & dtc_p <= 0.82 & tr_dist == 750]
## setkey(foo, dtc_p)

# low performance = Intercept = 2.913904, Slope = -0.0051440101
# high performance = Intercept = 3.104209, slope = -0.0021253279


## #' tar_load(raw_log_reg_H_GBE)
## #' rng_curve <- raw_log_reg_H_GBE
## #' tar_load(sim_tag_trans)
## #' dtc_trans = sim_tag_trans
## #' tar_load(bay_mth_grd)
## #' recLoc <- bay_mth_grd
## #' tar_load(min_dist)
## #'
## #' array_dtc_prob(rng_curve = rng_curve, dtc_trans = sim_tag_trans, recLoc = bay_mth_grd, detRngFun = pdrf,  min_dist = min_dist)


## array_dtc_prob <- function(rng_curve, dtc_trans, recLoc, detRngFun,  min_dist){
##   # load up GB logistic regression params
##   rng_crv <- fread(rng_curve)

##   # rename range curve columns
##   setnames(rng_crv, names(rng_crv), paste0("rc_", names(rng_crv)))

##   # do simulation for each slope intercept in "rng_crv"
##   ## rows <- nrow(rng_crv)
##   ## out <- vector(mode = "list", length = rows)

##   ## for( i in 1:rows){
##   ##   sim <- .sim_dtc(dtc_trans = dtc_trans, recLoc = recLoc, detRngFun = pdrf(b = c(rng_crv$rc_Intercept[i],  rng_crv$rc_Slope[i])), EPSG = 3175, sp_out = FALSE, show_progress = FALSE, min_dist = min_dist)

##   ##   out[[i]] <-  cbind(sim, rng_crv[i])
##   ## }

##   rng_crv[rc_Interval == 1, {
##     tst <- .SD[1, c("rc_Intercept", "rc_Slope")]
##     tst1 <- c(tst$rc_Intercept, tst$rc_Slope)
##     tst1}
##     #    .sim_dtc(dtc_trans = dtc_trans, recLoc = recLoc, EPSG = 3175, sp_out = FALSE, show_progress = FALSE, ba = tst1, min_dist = min_dist)}
## , by = .(rc_Interval)]



##   tst_fun <- function(ba) {
##     return(ba)

## tst <- rng_crv[, tst_fun(ba = .SD[1, c("rc_Intercept", "rc_Slope")]), by = .(rc_Interval)]

    
  
## rng_crv[rc_Interval == 1, .

  

## ## apply(rng_crv, 1, FUN = function(x) {.sim_dtc(dtc_trans = dtc_trans, recLoc = recLoc, EPSG = 3175, sp_out = FALSE, show_progress = FALSE, b = x, min_dist = min_dist)}, simplify = FALSE)


  

  
##   return(out)
## }

## ## ptm <- proc.time()
## ## ptoc.time() - ptm

## ## tst <- rbindlist(


##   ####################3


###############
#' tar_load(sim_tracks)
#' path = sim_tracks
#' vel = 0.5
#' delayRng = c(60,180)
#' burstDur = 5
#' EPSG = 3175
#' sp_out = FALSE


.sim_tag_trans <- function(path, vel, delayRng, burstDur, EPSG, sp_out){
  out <-path[, transmit_along_path_updated(path = .SD,
                                           vel = vel,
                                           delayRng = delayRng,
                                           burstDur = burstDur,
                                           EPSG = EPSG,
                                           sp_out = sp_out),
             by = .(fish), .SDcols = c("X", "Y")]
  return(out)
}

#####################
#' make simulated tracks
#' n_trks = 5
#' tar_load(LH)
#' poly = LH
#' theta = c(0,25)
#' stepLen = 1000
#' initHeading = 40
#' nsteps = 150
#' initPos = c(-83.6248, 43.8824)
#' sp_out = FALSE
#' show_progress = FALSE
#' tar_load(bay_mth_grd)
#' recs = bay_mth_grd

.sim_tracks <- function(n_trks, poly, theta, stepLen, initHeading, nsteps, initPos, sp_out, show_progress, recs){

  out <- replicate(n_trks, crw_in_polygon_sf(polyg = poly, theta = theta, stepLen = stepLen, initHeading = initHeading, nsteps = nsteps, initPos = initPos, sp_out = sp_out, show_progress = show_progress), simplify = FALSE)
  out <- rbindlist(out, idcol = "fish")

  tr_dist <- sim_fish_rec_dist(fsh_trks = out, rec_grid = recs)
  
  comb <- tr_dist[out, on = "fish"]

  return(comb)

}

  
sim_fish_rec_dist <- function(fsh_trks, rec_grid){

  trks <- st_as_sf(fsh_trks, coords = c("X", "Y"), agr = "constant", crs = 4326)
  trks <- st_transform(trks, crs = 3175)
  setDT(trks)
  trks <- trks[, .(geometry = st_combine(geometry)), by = .(fish)]
  trks <- st_as_sf(trks, agr = "constant")
  trks <- st_cast(trks, "LINESTRING")
  bay_mth_grd <- st_transform(rec_grid, crs = 3175)
  dist <- st_distance(bay_mth_grd, trks)
  min_dist <- apply(dist, 2, min)
  out <-  data.table(fish = 1:max(fsh_trks$fish), min_dist = min_dist)
  return(out)
}

## #########################
## library(viridis)

## plot(st_geometry(LH))
## points(recLoc, pch = 15, col = "blue")

## fsh <- unique(trks$fish)
## col_pal <- viridis(length(fsh), option = "D")
## col_pal <- data.table(fish = fsh, color = col_pal)
## trks <- trks[col_pal, on = "fish"]

## trans <- trans[col_pal, on = "fish"]



## for(i in 1:length(fsh)){
##   trks.i <- trks[fish == fsh[i],]
##   lines(y ~ x, data = trks.i, col = color, lwd = 2)
##   trans.i <- trans[fish == fsh[i],]
## #  points(y ~ x, data = trans.i, col = color, pch = 16, cex = )
##   points(trns_y ~ trns_x, data = dtc_pts, col = "red", pch = 16, cex = 2)
## }


#dist <- seq(0,2000,25)
# b = c(intercept, slope)
#b = c(114.590585954034,-0.189486287548403)
#prob <- pdrf(dm = dist, b = b)

## pdrf <- function(dm, b=c(0.5, -1/120)){
##        p <- 1/(1+exp(-(b[1]+b[2]*dm)))
##        return(p)
##      }

## plot(x = dist, y = prob, type = "o")



## tar_load(sim_tracks)
## foo <- st_as_sf(sim_tracks, coords = c("x", "y"), agr = "constant", crs = 4326)
## bar <- st_transform(foo, 3175)
## setDT(bar)
## bar <- bar[, .(geometry = st_combine(geometry)), by = fish]
## bar <- st_as_sf(bar)
## bar <- st_cast(bar, "LINESTRING")
## bar <- st_transform(bar, 4326)

## .intersect <- function(x = sag_bay_mth_raw, y = sbay){
##   out <- sf::st_read(x, quiet = TRUE, agr = "constant")
##   out <- sf::st_intersection(out,y)
##   return(out)
## }




#############################################

#' Simulate a correlated random walk inside a polygon
#'
#' Uses \link{crw} to simulate a random walk as series of equal-length steps
#' with turning angles drawn from a normal distribution inside a polygon.
#'
#' @param polyg A spatial polygon object (class must be
#'   \code{\link[sp]{SpatialPolygonsDataFrame}},
#'   \code{\link[sp]{SpatialPolygons}}, or \code{\link[sf]{sf}} or
#'   \code{\link[sf]{sfc}} object containing a 'POLYGON' feature); \cr \emph{OR}
#'   \cr A polygon defined as data frame with numeric columns x and y.
#'
#' @param theta A 2-element numeric vector with turn angle parameters (theta[1]
#'   = mean; theta[2] = sd) from normal distribution.
#'
#' @param stepLen A numeric scalar with total distance moved in each step. Units
#'   are same as the units of the coordinate reference system specified by
#'   argument \code{EPSG} (meters for the default Great Lakes projected
#'   coordinate system).
#'
#'
#' @param initPos A 2-element numeric vector with initial position
#'   (initPos[1]=x, initPos[2]=y) in same coordinate reference system as
#'   \code{polyg}.
#'
#' @param initHeading A numeric scalar with initial heading in degrees. E.g., 0
#'   = North; 90 = East, 180 = South, 270 = West; etc.
#'
#' @param nsteps A numeric scalar with number of steps to simulate.
#'
#' @param EPSG Numeric EPSG code of coordinate system used for simulations.
#'   Default is 3175, a projected coordinate system for the North American Great
#'   Lakes Basin and St. Lawrence River system.
#'   \url{https://spatialreference.org/ref/epsg/nad83-great-lakes-and-st-lawrence-albers/}.
#'    Must be a projected (Cartesian) coordinate system.
#'
#' @param sp_out A logical scalar that determines if returned object is a
#'   so-called spatial class. If \code{TRUE} (default value) and input
#'   \code{polyg} is class \code{\link[sp]{SpatialPolygonsDataFrame}},
#'   \code{\link[sp]{SpatialPolygons}}, \code{\link[sf]{sf}}, or
#'   \code{\link[sf]{sfc}} (must be 'POLYGON' feature) then class of returned
#'   object will be same as input object \code{polyg}. If \code{TRUE} and input
#'   \code{polyg} is a \code{data.frame} then returned object will be of class
#'   \code{\link[sp]{SpatialPolygonsDataFrame}}. If \code{FALSE} then returned
#'   object will be of class \code{data.frame}.
#'
#' @param show_progress Logical. Progress bar and status messages will be shown
#'   if TRUE (default) and not shown if FALSE.
#'
#' @details If initPos = NA, then a starting point is randomly selected within
#'   the polygon boundary. A path is simulated forward using the crw function.
#'   Initial heading is also randomly selected if initHeading = NA. When a step
#'   crosses the polygon boundary, a new heading for that step is drawn and the
#'   turn angle standard deviation is enlarged slightly for each subsequent
#'   point that lands outside the polygon.
#'
#' @details If polyg object is a data frame with x and y columns and
#'   \code{sp_out = TRUE}, then \code{\link[sp]{SpatialPoints}} output object
#'   will have coordinate system of \code{EPSG}.  Coordinate system on output
#'   will be same as input if polyg object is \code{\link[sp]{SpatialPolygons}}
#'   or \code{\link[sf]{sf}}.
#'
#'
#' @return A \code{\link[sf]{sf}}, \code{\link[sp]{SpatialPoints}}, or
#'   \code{data.frame}, object in the same coordinate system as the input
#'   \code{polyg} object. \cr \emph{OR} \cr A two-column data frame containing:
#'   \item{x}{x coordinates} \item{y}{y coordinates} in the same units as
#'   \code{polyg}. \cr See argument \code{sp_out}.
#'
#' @author C. Holbrook \email{cholbrook@@usgs.gov}
#'
#' @seealso \link{crw}
#'
#' @note The path is constructed in segments based on the minimum distance
#'   between the previous point and the closest polygon boundary.
#'
#'   Simulations are conducted within the coordinate system specified by
#'   argument \code{EPSG}. The default EPSG (3175), covers only the Great Lakes
#'   of North America. Simulations conducted in other areas will need to specify
#'   a valid EPSG representing an appropriate projected (Cartesian) coordinate
#'   system for the study area.
#'
#' @examples
#'
#' #Simple box example
#' mypolygon <- data.frame(x = c(-50,-50, 50, 50), y = c(-50,50,50,-50))
#' foo <- crw_in_polygon(mypolygon, theta = c(0, 20), stepLen = 10,
#'   initPos=c(0,0), initHeading=0, nsteps=50)
#' class(foo) #note object is SpatialPoints
#' plot(sp::coordinates(foo), type = "o", pch = 20, asp = c(1,1),
#'   xlim = range(mypolygon$x), ylim = range(mypolygon$y))
#' polygon(mypolygon, border = "red")
#'
#'
#' #Great Lakes Example
#' data(greatLakesPoly)
#'
#' #simulate in great lakes polygon
#' foo2 <- crw_in_polygon(greatLakesPoly,theta=c(0,25), stepLen=10000,
#'   initHeading=0, nsteps=100, sp_out = TRUE)
#'
#' #plot
#' sp::plot(greatLakesPoly, col = "lightgrey", border = "grey")
#' points(foo2,type="o", pch = 20, col = "red")
#'
#' #zoom in
#' sp::plot(greatLakesPoly, col = "lightgrey", border = "grey",
#'   xlim = sp::bbox(foo2)[1,], ylim = sp::bbox(foo2)[2,])
#' points(foo2,type="o", pch = 20, col = "red")
#'
#' @export

crw_in_polygon_sf <- function(polyg, theta = c(0,10), stepLen = 100, 
  initPos = c(NA,NA), initHeading = NA, nsteps = 30, 
  EPSG = 3175, sp_out = TRUE, show_progress = TRUE){          
  
  #check polyg
  if(!inherits(polyg, c("data.frame", "sf", "sfc", "SpatialPolygonsDataFrame", 
                        "SpatialPolygons"))) 
    stop("Input 'polyg' must be of class 'data.frame', 'sf', 'sfc', ",
          "'SpatialPolygonsDataFrame', or 'SpatialPolygons'.")
  
  #check that sf geometry is POLYGON
  if(inherits(polyg, c("sf", "sfc"))) {
    if(!("POLYGON" %in% sf::st_geometry_type(polyg)))
    stop("Input object 'polyg' must contain geometry of type 'POLYGON' when ",
          "class is 'sf'.")
    polyg_sf <- polyg
  } else if(inherits(polyg, "data.frame")){
    #check for names
    if(!all(c("x", "y") %in% names(polyg))) stop("Input data.frame 'polyg' ",
                                                 "must have columns named ",
                                                 "'x' and 'y'.")
    
    #close polyg if needed
    if(!identical(polyg[1,], tail(polyg, 1))) polyg <- rbind(polyg, polyg[1,])
    
    polyg_sf <- sf::st_polygon(list(as.matrix(polyg[c("x","y")])))
    polyg_sf <- sf::st_sfc(polyg_sf, crs = EPSG)
    polyg_sf <- sf::st_sf(ID=1:length(polyg_sf), geom = polyg_sf)
  }
  
  
  #convert to sf_polygon if polyg is SpatialPolygonsDataFrame or SpatialPolygons
  if(inherits(polyg, c("SpatialPolygonsDataFrame", "SpatialPolygons"))){

    polyg_sf <- sf::st_as_sf(polyg)
  }
  
  #set CRS
  polyg_sf <- sf::st_transform(polyg_sf, crs = EPSG)
  
  #if any initPos were not given
  #randomly select one point in the study area
  if(any(is.na(initPos))) init <- sf::st_sample(polyg_sf, size = 1)

  
  #if initPos are both given, check to see if in polyg
  if(all(!is.na(initPos))) {
    init_crs <- sf::st_crs(polyg)
    if(is.na(init_crs)) init_crs <- sf::st_crs(EPSG)
    init <- sf::st_as_sf(data.frame(x=initPos[1], y = initPos[2]),
                         coords = c("x", "y"),
                         crs = init_crs)
    init <- sf::st_transform(init, crs = sf::st_crs(EPSG))
    inPoly <- any(sf::st_contains(polyg_sf, sparse = FALSE))
    if(!inPoly) stop("initPos is outside polygon boundary.")
  } #end if   
  
  
  #randomly select heading if not given
  if(is.na(initHeading)) initHeading <- runif(1, 0, 360)
  
  #preallocate
  path_fwd <- data.frame(x = rep(NA, nsteps + 1), y = NA) 
  path_fwd[1,] <- sf::st_coordinates(init)
  
  #create lines object from polyg (for distance measurement)
  xl <- sf::st_cast(polyg_sf, "MULTILINESTRING")

  rows_i <- 1
  init_i <- init
  dist_i <- min(sf::st_distance(init_i, xl, sparse = TRUE), na.rm = TRUE)
  nsteps_i <- (as.numeric(dist_i) %/% stepLen) + 1
  rows_i <- 1 + 1:nsteps_i 
  rows_i <- rows_i[rows_i <= (nsteps + 1)]  
  
  #initalize counter for boundary failures
  k <- 0

  #initialize progress bar
  if(show_progress) {
    message("Simulating tracks...")
    pb <- txtProgressBar(min = 0, max = nsteps + 2, initial = 0, style = 3)	  
  }
  
  while(length(rows_i) > 0){

    #calculate theta based on k (failed boundary attempts)
    theta_i <- c(theta[1], theta[2] * (1 + 0.1 * k^2))
    
    #operate on temporary object for ith window
    path_fwd_i <- crw(theta = theta_i, stepLen = stepLen, 
                      initPos = as.vector(sf::st_coordinates(init_i)),
                      initHeading, nsteps = length(rows_i))
    
    inPoly <- check_in_polygon(path_fwd_i, polyg_sf, EPSG)
    
    if(all(!inPoly)) {
      k <- k + 1 #counter
      next #repeat this iteration if all outside polygon
    }
    if(any(!inPoly)) rows_i <- rows_i[inPoly] #retain only rows inside
    
    k <- 0
    
    #update path.fwd
    path_fwd[rows_i , ] <- path_fwd_i[inPoly, ]
    
    
    #simulate track forward
    init_i <- sf::st_as_sf(path_fwd[max(rows_i), ], 
                           coords = c("x", "y"),
                           crs = EPSG)
    
    #smallest distance to boundary
    dist_i <- min(sf::st_distance(init_i, xl, sparse = TRUE), na.rm = TRUE)
    
    #calculate heading at end (start of next)
    initHeading <- vector_heading(path_fwd$x[max(rows_i) - 1:0],
                                  path_fwd$y[max(rows_i) - 1:0]) 
    

    #update progress bar
    if(show_progress){
      setTxtProgressBar(pb, max(rows_i))
      if(max(rows_i) > (nsteps + 1)) close(pb)		
    }

    #conservative estimate of the number of rows/steps to simulate
    #i.e., without reaching barrier
    nsteps_i <- (as.numeric(dist_i) %/% stepLen) + 1
    rows_i <- max(rows_i) + 1:nsteps_i 
    rows_i <- rows_i[rows_i < nsteps + 2]    
    
  } #end while
  
  if(show_progress) message("Done.")
  
  #set output CRS to polygon CRS if polygon has CRS; EPSG otherwise
  if(!is.na(sf::st_crs(polyg))) { 
    crs_out <- sf::st_crs(polyg) } else { crs_out <- sf::st_crs(EPSG) }

  #coerce to sf in output crs
  path_fwd_sf <- sf::st_as_sf(path_fwd, coords = c("x", "y"), 
                              crs = EPSG)
  path_fwd_sf <- sf::st_transform(path_fwd_sf, crs = crs_out)
  
  if(sp_out){
    if(inherits(polyg, c("sf", "sfc"))) return(path_fwd_sf)

    #return sp object if input is sp or data.frame
    return(sf::as_Spatial(path_fwd_sf))  
  } else {
    return(as.data.frame(sf::st_coordinates(path_fwd_sf)))
  }
} 
 
#check if in polygon
check_in_polygon <- function(points, polygon, EPSG){
  points_sf <- sf::st_as_sf(points,
                            coords = c("x", "y"),
                            crs = EPSG)
  #identify points contains in any polygon
  inPoly <- apply(sf::st_contains(polygon, points_sf, sparse = FALSE), 2 , 
                  any)
  return(inPoly)
} 
 



######################
#' tar_load(reefs)
#' tar_load(in_bay_grd)
#' tar_load(sbay)
## nearest <- st_nearest_feature(reefs, in_bay_grd)
## ls <- st_nearest_points(reefs, in_bay_grd[nearest,], pairwise = TRUE)

## plot(st_geometry(sbay))
## plot(st_geometry(reefs), add = TRUE, col = "red", pch = 16)
## plot(st_geometry(in_bay_grd), add = TRUE, col = "blue", pch = 16)
## plot(ls, add = TRUE, col = "green")


## dist <- st_distance(reefs, in_bay_grd[nearest,], by_element = TRUE)


.grid <- function(poly, cellsize, in_crs = 3175, out_crs = 4326, what = "polygons", square = TRUE){

  y <- st_transform(poly, crs = in_crs)
  x <- st_make_grid(y, what = "centers", cellsize = cellsize, square = TRUE)
  x <- st_as_sf(x)
  x <- st_join(x = x, y = y, left = FALSE)
  x <- st_transform(x, crs = out_crs)

  return(x)
}

################
#' @title create a grid of receivers, taking into account mandatory receivers deployed on reefs
#' @description Creates grid of receivers within polygon.  One receiver is located at center of each grid unless a "reef" (or other location) that needs a receiver is supplied.  If a grid contains another location "mandatory deployment" location, then other receiver deployed in grid is removed such that only one grid receiver or 1 or more mandatory receivers are deployed in each cell of grid.  Function does not assign a grid receiver to cells that contain less than 0.10*area of full cell.
#' @param bbox bounding box of where receivers area to be deployed.  input value must be acceptable to sf::st_bbox
#' @param inner_bay_poly polygon object containing outline of where receivers are to be deployed
#' @param cellsize size of grid cell (x,y). Input must be appropriate for sf::st_grid and in METERS
#' @param reefs sf points object that contains locations for required points.  In this use, they represent spawning reefs
#' @return function returns a sf points object for each receiver location

#' @examples
#' tar_load(in_bay)
#' inner_bay_poly <- in_bay
#' tar_load(reefs)
#' reefs <- reefs
#' bbox = c(xmin = -83.948193, xmax = -82.946270, ymin = 43.595602, ymax = 44.277664)
#' cellsize = c(15000,15000 )

.inner_bay_rec_grid <- function(bbox = c(xmin = -83.948193, xmax = -82.946270, ymin = 43.595602, ymax = 44.277664), inner_bay_poly = in_bay, cellsize = c(15000, 15000), reefs){

  # create bounding box for inner bay and convert to crs 3175
  bay_box  <- st_bbox(bbox, crs = 4326)
  bay_box <- st_bbox(st_transform(st_as_sfc(bay_box), 3175))
  in_bay <- st_transform(inner_bay_poly, 3175)

  # make inner bay grid constrained by bay outline polygon
  grd <- st_make_grid(bay_box, cellsize = cellsize)
  grd <- st_intersection(grd, in_bay)
  grd <- st_transform(grd, 4326)
  in_bay <- st_transform(inner_bay_poly, 4326)

  # calculate centers of each grid
  centers <- st_centroid(grd)
  centers <- st_sf(ID = "grd", geometry = st_sfc(centers), crs = 4326)

  # combine all grid and receiver points into a single object
  reefs$ID <- "reef"
  all_pts <- rbind(reefs[,c("ID", "geometry")], centers)

  # calculate area of cell
  cell_area <- as.numeric(cellsize[1]*cellsize[2])

  # pull all info for grid together, including number of points (either grid or reefs) in each cell.
  grd <- st_sf(area = st_area(grd), pt_ct = lengths(st_intersects(grd, all_pts)), geometry = grd)
  grd$prop_max <- as.numeric(grd$area)/cell_area

  # Assign each point to cells that contain it.  This seems to duplicate previous command???
  bar <- st_intersection(grd, all_pts)

  # select out all reefs and center points in grid cells that do not contain reefs
  out <- bar[bar$prop_max > 0.10 & (bar$pt_ct == 1 | bar$ID == "reef"),]

  return(out)
}
  



## out$col <- ifelse(out$ID == "reef", "red", "blue")
## plot(st_geometry(in_bay))
## plot(st_geometry(grd), add = TRUE)
## plot(st_geometry(out), add = TRUE, pch = 16, col = out$col, cex = 2)












