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
#' grid_map(bathy, lidar, sentinal, rec_depth, glid_area = glider, mobile = mobile_listen)

# create leaflet map
grid_map <- function(bathy, grid, sbay, reefs, rec_grid, spawn_rivers, bay_mth, pth){

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
  m <- addCircleMarkers(m, data = rec_grid, label = ~STATE, color = c("yellow"), radius = c(10), group = "proposed recs (bay grid)", stroke = FALSE, fillOpacity = 1)
  m <- addCircleMarkers(m, data = bay_mth, color = "orange", radius = 10, group = "proposed recs (bay mth)", stroke = FALSE, fillOpacity = 1)
  m <- addMarkers(m, data = spawn_rivers, label = ~river, group = "spawn rivers")
                      
#  m <- addCircleMarkers(m, data = sync_100, label = ~label, fillColor = "yellow", radius = 8, group = "sentinel tag", stroke = FALSE, fillOpacity = 1)
#  m <- addCircleMarkers(m, data = all_mob, label = ~label_short, fillColor = "orange", radius = 8, group = "mobile", stroke = FALSE, fillOpacity = 1)
  m <- addPolygons(map = m, data = sbay, color  = "red", fillColor = NA, group = "sag bay")
  m <- leafem::addMouseCoordinates(m)
  m <- addLegend(m, pal = pal_LH , values =  bath[[1]], title = "depth (ft)", opacity = 1, group = "bathy (ft)")
#  m <- addLegend(m, pal = pal_lidar, values = lid[[1]], title = "depth (lidar, ft)", opacity = 1, group = "lidar_depth")
#  m <- addLayersControl(m, overlayGroups = c("LH_depth", "lidar_depth", "receivers", "sentinel tag", "mobile", "glider patrol"), options = layersControlOptions(collapsed = FALSE))
    m <- addMeasure(m, primaryLengthUnit = "meters", secondaryLengthUnit = "kilometers")  
  m <- addLayersControl(m, baseGroups = c("satellite", "nav chart", "alt", "bathy (ft)"), overlayGroups = c("LWF recs", "sag bay", "bay reefs", "proposed recs (bay grid)", "spawn rivers", "proposed recs (bay mth)"), position = "bottomright", options = layersControlOptions(collapsed = FALSE))
  
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

crw_in_polygon_updated <- function (polyg, theta = c(0, 10), stepLen = 100, initPos = c(NA, 
    NA), initHeading = NA, nsteps = 30, EPSG = 3175, sp_out = TRUE, 
    show_progress = TRUE) 
{
    if (!inherits(polyg, c("SpatialPolygonsDataFrame", "SpatialPolygons", 
        "Polygons", "Polygon"))) 
        polyg <- sp::Polygon(polyg, hole = FALSE)
    if (!inherits(polyg, c("SpatialPolygonsDataFrame", "SpatialPolygons", 
        "Polygons"))) 
        polyg <- sp::Polygons(list(polyg), ID = 1)
    #    projargs <- paste0("+init=epsg:", EPSG)
    projargs <- CRS(SRS_string = paste0("EPSG:", EPSG))
    if (!inherits(polyg, c("SpatialPolygonsDataFrame", "SpatialPolygons"))) {
      #polyg <- sp::SpatialPolygons(list(polyg), proj4string = sp::CRS(projargs))
      polyg <- sp::SpatialPolygons(list(polyg))
      slot(polyg, "proj4string") <- projargs
      projargs_in <- projargs
    }
    else {
      # projargs_in <- sp::proj4string(polyg)
      projargs_in <- wkt(polyg) 
    }
    #if (!identical(sp::proj4string(polyg), rgdal::CRSargs(sp::CRS(projargs)))) {
    if (!identical(wkt(polyg), wkt(projargs))){
      #polyg <- sp::spTransform(polyg, sp::CRS(projargs))
      polyg <- sp::spTransform(polyg, projargs)
    }
    if (any(is.na(initPos))) {
        inPoly <- FALSE
        while (inPoly == FALSE) {
            init <- c(runif(1, floor(sp::bbox(polyg)["x", "min"]), 
                ceiling(sp::bbox(polyg)["x", "max"])), runif(1, 
                floor(sp::bbox(polyg)["y", "min"]), ceiling(sp::bbox(polyg)["y", 
                  "max"])))
            init <- sp::SpatialPoints(matrix(as.numeric(init), 
                nrow = 1), proj4string = sp::CRS(sp::proj4string(polyg)))
            inPoly <- rgeos::gDistance(polyg, init) == 0
            inPoly <- switch(inPoly + 1, FALSE, TRUE, FALSE, 
                FALSE)
        }
    }
    
    if (all(!is.na(initPos))) {
        init <- sp::SpatialPoints(matrix(as.numeric(initPos), nrow = 1))#, proj4string = sp::CRS(projargs_in))
        slot(init, "proj4string") <- CRS(projargs_in)
        init <- sp::spTransform(init, projargs)
        inPoly <- rgeos::gDistance(polyg, init) == 0
        if (!inPoly) 
            stop("initPos is outside polygon boundary.")
    }
    
    if (is.na(initHeading)) 
        initHeading <- runif(1, 0, 360)

    path.fwd <- data.frame(x = rep(NA, nsteps + 1), y = NA)
    path.fwd[1, ] <- sp::coordinates(init)
    xl <- methods::as(polyg, "SpatialLines")
    rows_i <- 1
    init_i <- init
    dist_i <- rgeos::gDistance(init_i, xl)
    nsteps_i <- (dist_i%/%stepLen) + 1
    rows_i <- 1 + 1:nsteps_i
    rows_i <- rows_i[rows_i <= (nsteps + 1)]
    k <- 0
    if (show_progress) {
        message("Simulating tracks...")
        pb <- txtProgressBar(min = 0, max = nsteps, initial = 0, 
            style = 3)
    }
    while (max(rows_i) <= (nsteps + 1)) {
        theta_i <- c(theta[1], theta[2] * (1 + 0.1 * k^2))
        path.fwd.i <- crw(theta = theta_i, stepLen = stepLen, 
            initPos = as.vector(sp::coordinates(init_i)), initHeading, 
            nsteps = length(rows_i))

        check_in_polygon <- function(points, polygon) {
            points_sp <- sp::SpatialPoints(as.matrix(points))#, 
            # proj4string = sp::CRS(sp::proj4string(polygon)))
            slot(points_sp, "proj4string") <- CRS(wkt(polygon))
            
            inPoly <- sapply(1:nrow(sp::coordinates(points)), 
                function(i) rgeos::gDistance(polygon, points_sp[i, 
                  ]) == 0)
            return(inPoly)
        }
        inPoly <- check_in_polygon(path.fwd.i, polyg)
        if (all(!inPoly)) {
            k <- k + 1
            next
        }
        if (any(!inPoly)) 
            rows_i <- rows_i[inPoly]
        k <- 0
        path.fwd[rows_i, ] <- path.fwd.i[inPoly, ]
        init_i <- sp::SpatialPoints(path.fwd[max(rows_i), ])#, 
            slot(init_i, "proj4string") <- projargs

        dist_i <- rgeos::gDistance(init_i, xl)
        initHeading <- vector_heading(path.fwd$x[max(rows_i) - 
            1:0], path.fwd$y[max(rows_i) - 1:0])
        nsteps_i <- (dist_i%/%stepLen) + 1
        rows_i <- max(rows_i) + 1:nsteps_i
        rows_i <- min(rows_i[rows_i <= (nsteps + 1)], nsteps + 
            2)
        if (show_progress) {
            setTxtProgressBar(pb, max(rows_i))
            if (max(rows_i) > (nsteps + 1)) 
                close(pb)
        }
    }
    if (show_progress) message("Done.")
    path.fwd.sp <- sp::SpatialPoints(path.fwd)
    slot(path.fwd.sp, "proj4string") <- projargs
    path.fwd.sp <- sp::spTransform(path.fwd.sp, CRSobj = CRS(projargs_in))
    if (!sp_out) 
      path.fwd.sp <- as.data.frame(sp::coordinates(path.fwd.sp))
    return(path.fwd.sp)
}

########
transmit_along_path_updated <- function (path = NA, vel = 0.5, delayRng = c(60, 180), burstDur = 5, 
    EPSG = 3175, sp_out = TRUE) 
{
  
  path <- sf::st_as_sf(path, coords = c("x", "y"), agr = "constant", crs = 4326, remove = FALSE)
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
  trns <- data.frame(x = approx(path$etime, path$X, xout = etime)$y, 
                     y = approx(path$etime, path$Y, xout = etime)$y, et = etime)

trns <- trns[, c("x", "y", "et")]
trns <- sf::st_as_sf(trns, coords = c("x","y"), agr = "constant", crs = EPSG)
trns <- st_transform(trns, crs = projargs_in)
trns <- as_Spatial(trns)
  
  if (!sp_out) {
    trns <- as.data.frame(cbind(sp::coordinates(trns), et = trns$et))
    names(trns) <- c("x", "y", "et")
  }
  return(trns)
}

##################
######################
# detect transmisions


## trnsLoc = out[fish == 1, c("x", "y", "et")]
## tar_load(bay_mth_grd)
## recLoc <- as.data.table(st_coordinates(bay_mth_grd))
## setnames(recLoc, c("X", "Y"), c("x", "y"))
## pdrf <- function(dm, b = c(0.5, -1/120)){
##   p <- 1/(1+exp(-(b[1]+b[2]*dm)))
##   return(p)
## }
## detRngFun = pdrf
## EPSG = 3175
## sp_out = FALSE
## show_progress = FALSE
## detect_transmissions_updated(trnsLoc = trnsLoc, recLoc = recLoc, detRngFun = pdrf, EPSG = 3175, sp_out = FALSE, show_progress = TRUE)

 detect_transmissions_updated <- function (trnsLoc = NA, recLoc = NA, detRngFun = NA, EPSG = 3175, 
    sp_out = TRUE, show_progress = TRUE) 
{
    if (inherits(trnsLoc, "data.frame")) {
        missingCols <- setdiff(c("x", "y", "et"), names(trnsLoc))
        if (length(missingCols) > 0) 
            stop(paste0("'trnsLoc' must contain the ", "following columns: \n", 
                paste(missingCols, collapse = "\n")))
    }
    if (inherits(recLoc, "data.frame")) {
        missingCols <- setdiff(c("x", "y"), names(recLoc))
        if (length(missingCols) > 0) 
            stop(paste0("'recLoc' must contain the ", "following columns: \n", 
                paste(missingCols, collapse = "\n")))
    }
    #projargs <- paste0("+init=epsg:", EPSG)
    projargs <- CRS(SRS_string = paste0("EPSG:", EPSG))

    if (!inherits(trnsLoc, c("SpatialPointsDataFrame", "SpatialPoints"))) {
      trnsLoc <- sp::SpatialPointsDataFrame(trnsLoc[, c("x", "y")], data = trnsLoc)
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
        recLoc <- data.frame(x = recLoc[1], y = recLoc[2])
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
    names(recLoc)[1:2] <- c("x", "y")
    names(trnsLoc)[1:2] <- c("x", "y")

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
      distM.g <- sqrt((trnsLoc$x - recLoc$x[g])^2 + (trnsLoc$y - 
                                                       recLoc$y[g])^2)
      detP.g <- detRngFun(distM.g)
      succ.g <- as.logical(rbinom(length(detP.g), 1, detP.g))
      if (sum(succ.g) > 0) {
        dtc.g <- data.frame(trns_id = which(succ.g), recv_id = g, 
                            recv_x = recLoc$x[g], recv_y = recLoc$y[g], trns_x = trnsLoc$x[succ.g], 
                trns_y = trnsLoc$y[succ.g], etime = trnsLoc$et[succ.g])
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
#' .sim_dtc(dtc_trans = sim_tag_trans, recLoc = bay_mth_grd, detRngFun = pdrf, EPSG = 3175, sp_out = FALSE, show_progress = FALSE, min_dist)


.sim_dtc <- function(dtc_trans, recLoc, detRngFun, EPSG, sp_out, show_progress, min_dist){
  recLoc <- st_coordinates(recLoc)
  recLoc <- data.table(x = recLoc[,1], y = recLoc[,2])
  out <- dtc_trans[, detect_transmissions_updated(trnsLoc = .SD,
                                                  recLoc = recLoc,
                                                  detRngFun = pdrf,
                                                  EPSG = EPSG,
                                                  sp_out = sp_out,
                                                  show_progress = show_progress),
                   by = .(fish), .SDcols = c("x", "y", "et")]

  out <- merge(out, min_dist, all.y = TRUE)

  # total number of virtual fish
  out[, num_sim_fish := max(fish)]

  # total number of times each fish was detected.  fish not detected = NA 
  out[!is.na(recv_id), times_dtc := nrow(.SD), by = "fish"]

  # total number of fish detected at least once
  out[!is.na(recv_id), num_dtc := uniqueN(fish)]

  # overall detection probabiliity
  out[!is.na(recv_id), array_dtc_prob := num_dtc/num_sim_fish]

  return(out[])
}

###############
#' tar_load(sim_tracks)

.sim_tag_trans <- function(path, vel, delayRng, burstDur, EPSG, sp_out){
  out <-path[, transmit_along_path_updated(path = .SD,
                                           vel = vel,
                                           delayRng = delayRng,
                                           burstDur = burstDur,
                                           EPSG = EPSG,
                                           sp_out = sp_out),
             by = .(fish), .SDcols = c("x", "y")]
  return(out)
}

#####################
#' make simulated tracks
#'

.sim_tracks <- function(n_trks, poly, theta, stepLen, initHeading, nsteps, initPos, sp_out, show_progress){

  poly <- as(poly, "Spatial")
  out <- replicate(n_trks, crw_in_polygon_updated(polyg = poly, theta = theta, stepLen = stepLen, initHeading = initHeading, nsteps = nsteps, initPos = initPos, sp_out = sp_out, show_progress = show_progress), simplify = FALSE)
  out <- rbindlist(out, idcol = "fish")
  return(out)
}

                   









## # calculate all transmissions for all fish separately, then combine with data.table
##  trans <- trks[, transmit_along_path_updated(path = .SD, vel = 0.5, delayRng = c(60,180), burstDur = 5, EPSG = 3175, sp_out = FALSE), by = .(fish), .SDcols = c("x", "y")]

## #  determine all detection transmissions
## out <- trans[, detect_transmissions_updated(trnsLoc = .SD, recLoc = recLoc, detRngFun = pdrf, EPSG = 3175, sp_out = FALSE, show_progress = FALSE), by = .(fish), .SDcols = c("x", "y", "et")]














# detect transmissions for all tracks
## fsh <- unique(trans$fish)
## out <- vector(mode = "list", length = length(fsh))

## for(i in 1:length(fsh)){
##   trans.i <- trans[fish == fsh[i],]

##   foo <- detect_transmissions_updated(trnsLoc = trans.i, recLoc = recLoc, detRngFun = pdrf, EPSG = 3175, sp_out = FALSE, show_progress = FALSE)

## out[[i]] <- foo
## }

## dtc_pts <- rbindlist(out)


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






