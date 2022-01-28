# function file contains only functions called directly to create targets. 

#' @title prepare bathymetry layer, clean up
#' @description bring in bathymetry geotiff, crop to study area, convert depth in feet to meters, convert CRS to 4326 (wgs84) and write out results as geotiff
#' @param bathy1 file path to bathymetry data file (geotiff)
#' @param xmin_out crop boundary (longitude, LLC)
#' @param xmax_out crop boundary (longitude, LRC)
#' @param ymin_out crop boundary (latitude, LLC)
#' @param ymax_out crop boundary (latitude, ULC)
#' 
#' @examples
#' library(terra)
#' prep_bathy("data/LH_bathy/huron_lld.tif", xmin_out = -84.06762, xmax_out = -82.60408, ymin_out = 43.41871, ymax_out = 44.96670)

# prepare bathymetry layer
prep_bathy <- function(bathy1, xmin_out, xmax_out,  ymin_out, ymax_out, out_pth = "~/Documents/glider_range_test_receiver_planning_2021/output/bathy.tif"){
  bathy1 <- terra::rast(bathy1)
  e <- terra::ext(c(xmin_out, xmax_out, ymin_out, ymax_out ) )
  bathy1 <- terra::crop(bathy1, e)
  # remove land
  bathy1[bathy1 > 0] <- NA
  
  # convert meters to feet
  bathy1 <- bathy1 * -3.28084
  template <- bathy1
  crs(template) <- "epsg:4326"
  bathy1 <- project(bathy1, template)

  writeRaster(bathy1, out_pth, overwrite = TRUE)
  return(out_pth)
}

#' @title extract depths from bathy and lidar at each receiver location
#' @description extract depth using  bathymetry  at specified points
#' @param raw_recs points to extract depth from bathymetry layers, input is either path to shapfile or sfc points spatial object
#' @param bathy bathymetric layer produced by "prep_bathy" function
#' @param lidar bathymetric layer produced by "lidar_prep" function
#' 
#' @examples 
#' tar_load(all_mob)
#' #tar_load(all_rings)
#' #raw_recs <- all_rings
#' raw_recs = all_mob
#' tar_load(mobile_listen)
#' tar_load(all_rings)
#' #depth_extract(raw_recs = mobile_listen, bathy, lidar)
#' depth_extract(all_rings, bathy, lidar)#' 
#'
#' tar_load(bathy)
#' tar_load(parallel_lines_SB)
#' raw_recs <- parallel_lines_SB
#' lidar <- NULL
#' depth_extract(all_mob, bathy, lidar)
#' depth_extract(parallel_lines, bathy)

depth_extract <- function(raw_recs, bathy, lidar= NULL){
  
  if(is.character(raw_recs)){
    recs <- sf::st_read(raw_recs, quiet = TRUE)
    recs <- sf::st_transform(recs, crs = 4326)
    recs <- terra::vect(recs)
  }

  if(!is.character(raw_recs)){
    recs <- sf::st_as_sf(x = raw_recs, crs = 4326, coords = c("lon", "lat"), remove = FALSE )
    recs <- terra::vect(recs)
  }
  
  bath <- terra::rast(bathy)
  
  # compile all the points with lat/lon
  out <-as.data.table(cbind(as.data.frame(terra::geom(recs)), terra::as.data.frame(recs)))
  out_bathy <- data.table::as.data.table(terra::extract(bath, recs, xy = FALSE))
  out[out_bathy, depth_ft_bathy := huron_lld, on = .(geom = ID)]

  if(!is.null(lidar)){
    lid <- terra::rast(lidar)
    out_lidar <- data.table::as.data.table(terra::extract(lid, recs, xy = FALSE))
    out[out_lidar, depth_ft_lidar := Job627873_001_001, on = .(geom = ID)]
  }


  if(is.character(raw_recs)){
    out <-  out[, c("geom", "x", "y", "glat_array", "station_no", "depth_ft_bathy", "depth_ft_lidar")]
    setnames(out, c("geom", "x", "y", "glat_array"), c("id", "lon", "lat", "glatos_array"))
  }

  if(!is.character(raw_recs) & !is.null(lidar)){
    out <- out[, c("geom", "x", "y", "glatos_array", "station_no", "depth_ft_bathy", "depth_ft_lidar")]
    setnames(out, c("geom", "x", "y"), c("id", "lon", "lat"))
  }

  if(!is.character(raw_recs) & is.null(lidar)){
    out <- out[, c("geom", "x", "y", "array", "station", "depth_ft_bathy")]
    setnames(out, c("geom", "x", "y", "array", "station", "depth_ft_bathy"), c("id", "lon", "lat", "glatos_array", "site", "depth_ft"))

    substrRight <- function(x, n){
      substr(x, nchar(x)-n+1, nchar(x))
    }

    out[, station_no := substrRight(site, 3)]
    out[, site_label := sprintf("%s, %2.0f feet", site, depth_ft)]  
  }
  
  # create label info for leaflet
  if(!is.character(raw_recs) & !is.null(lidar)){
    out[, site_label := sprintf("site id %s-%s\n water depth %2.0f feet (LIDAR)\n water depth %2.0f feet (DEM)\n", glatos_array, station_no, depth_ft_lidar, depth_ft_bathy)]
    out[, label_short := depth_ft_bathy]
    out[!is.nan(depth_ft_lidar), label_short := depth_ft_lidar]
    out[, label_short := sprintf("%s-%s, %2.0f ft", glatos_array, station_no, label_short)]
  }

  return(out[])
}


#' @title function finds sentinal tag location (lat/lon, WGS84) a specified distance between two locations
#' @description returns location between two locations with offset of specified distance (meters)
#' @param rec_depth receiver locations
#' @param glatos_array glatos array ID for point one.
#' @param start_station_no station number for point one.
#' @param end_station_no station number for point two
#' @param dist offset distance.  Calculates from point one to point two in a straight line
#' @examples
#' tar_load(rec_depth)
#' setDT(rec_depth)
#' glatos_array = c("AC4", "AC3", "AC2")
#' start_station_no = c("010", "003", "008")
#' end_station_no = c("011", "004", "009")
#' dist = 100

#'  bar <- find_sent(rec_depth = rec_depth, start_glatos_array = c("AC4", "AC3", "AC2"), end_glatos_array = c("AC4", "AC3", "AC2"), start_station_no = c("010", "003", "008"), end_station_no = c("011", "004", "009"), dist = 100)


find_sent <- function(rec_depth, glatos_array = c("AC4", "AC3", "AC2"), start_station_no = c("010", "003", "008"), end_station_no = c("011", "004", "009"), dist = 100){

  tst <- data.table(glatos_array, start_station_no, end_station_no, id = 1:(length(glatos_array)))
  tst <- melt(tst, id.vars = c("glatos_array", "id"), measure_vars = c("start_station_no", "end_station_no"), variable.factor = FALSE, value.name = "station_no")
  
  tst[rec_depth, c("lat", "lon") := list(lat, lon), on = .(glatos_array, station_no)]
  tst <- dcast(tst, glatos_array + id ~ variable, value.var = c("lat", "lon"))
  setnames(tst, c("lat_end_station_no", "lat_start_station_no", "lon_end_station_no", "lon_start_station_no", "id"), c("end_lat", "start_lat", "end_lon", "start_lon", "station_no"))
  ## # calculate point 100 m from starting point
  tst[, c("lon", "lat") := sentinal_pos(from_lon = .SD[,c("start_lon")], from_lat = .SD[,c("start_lat")],
                                        to_lon = .SD[,c("end_lon")], to_lat = .SD[,c("end_lat")], offset = 100), by = .(station_no)]

  return(tst) 
}


#' @title create custom leaflet interactive map of planning data used for study design
#' @description Creates map that overlays two sources of bathymetry (lidar and GL bathymetry), receiver locations, sentinal tag locations, area of proposed glider patrols, mobile tracking listening stations
#' @param bathy file path to bathymetry layer (tif)
#' @param lidar file path to lidar data layer (tif)
#' @param sentinal sf point object containing points for sentinel tags
#' @param rec_depth data.table object containing depths for proposed receivers
#' @param glid_area sf polygon object containing area for proposed glider patrols
#' @param mobile file path to mobile tracking locations

#' @examples
#' tar_load(bathy)
#' tar_load(lidar)
#' tar_load(sentinal)
#' tar_load(rec_depth)
#' tar_load(glider)
#' tar_load(all_mob)
#' leaflet_map(bathy, lidar, sentinal, rec_depth, glid_area = glider, mobile = mobile_listen)

# create leaflet map
leaflet_map <- function(bathy, pth, lines, lidar, glider_lat = c(45.5361667, 45.545333, 45.597333), glider_lon = c(-84.000, -84.022, -84.110833) ){

  glider_pth <- data.frame(lat = glider_lat, lon = glider_lon)
  lines <- sf::st_as_sf(lines, crs = 4326, agr = "constant", coords = c("lon", "lat"))
  bath <- terra::rast(bathy)
#  bath <- terra::aggregate(bath, fact = 4)
  bath <- stars::st_as_stars(bath)
  
  ## lid <- terra::rast(lidar)
  ## lid <- terra::aggregate(lid, fact = 4)
  ## lid <- stars::st_as_stars(lid)
  
  brks_bath <- brks(x = bath, n = 100)
##  brks_lidar <- brks(x = lid, n = 100)
  pal_LH <- leaflet::colorNumeric(c(viridis::viridis(100)), bath[[1]], na.color = "transparent")
##  pal_lidar <- leaflet::colorNumeric(c(viridis::viridis(100)), lid[[1]], na.color = "transparent")

  # create leaflet map
  m <- leaflet()
  m <- setView(m, zoom = 15, lat = 45.537 , lng = -83.999)
  m <- addTiles(m)
  m <- leafem::addGeoRaster(m, bath, opacity = 1, colorOptions = leafem::colorOptions(palette = viridis::viridis(256), breaks = brks_bath$brks), group = "bathy (ft)")
  m <- addTiles(m, urlTemplate = "http://tileservice.charts.noaa.gov/tiles/50000_1/{z}/{x}/{y}.png", group = "nav chart")
  m <- addProviderTiles(m, providers$Esri.WorldImagery, group = "satellite")
  m <- addProviderTiles(m, providers$Esri.NatGeoWorldMap, group = "alt")
  m <- addPolylines(map = m, data = glider_pth, lng = ~lon, lat = ~lat, color = "green")
#  m <- addMarkers(m, lng = -83.58845, lat = 44.08570, label = "release")
  m <- addCircleMarkers(m, data = lines, label = ~site_label, color = c("red", "red", "red", "red", "red", "red", "blue", "red", "red", "blue"), radius = c(4,4,4,4,4,4,10,4,4,10), group = "recs", stroke = FALSE, fillOpacity = 1)
                    
  
#  m <- addCircleMarkers(m, data = sync_100, label = ~label, fillColor = "yellow", radius = 8, group = "sentinel tag", stroke = FALSE, fillOpacity = 1)
#  m <- addCircleMarkers(m, data = all_mob, label = ~label_short, fillColor = "orange", radius = 8, group = "mobile", stroke = FALSE, fillOpacity = 1)
#  m <- addPolygons(map = m, data = glider, color = "red", group = "glider patrol")
  m <- leafem::addMouseCoordinates(m)
#  m <- addLegend(m, pal = pal_LH , values =  bath[[1]], title = "depth (ft)", opacity = 1, group = "LH_depth")
#  m <- addLegend(m, pal = pal_lidar, values = lid[[1]], title = "depth (lidar, ft)", opacity = 1, group = "lidar_depth")
#  m <- addLayersControl(m, overlayGroups = c("LH_depth", "lidar_depth", "receivers", "sentinel tag", "mobile", "glider patrol"), options = layersControlOptions(collapsed = FALSE))
    m <- addMeasure(m, primaryLengthUnit = "meters", secondaryLengthUnit = "kilometers")  
  m <- addLayersControl(m, baseGroups = c("satellite", "nav chart", "alt", "bathy (ft)"), overlayGroups = c("recs"), position = "bottomright", options = layersControlOptions(collapsed = FALSE))
  
  htmlwidgets::saveWidget(m, pth)

  return(pth)
  }


#' @title create polygon of region identified as glider patrol area
#' @description create gpkg that contains polygon of area idenfitied for area patrolled by glider.  creates concentric area around release location.
#' @param shoreline path to shoreline outline (polygon)
#' @param in_dist minimum distance from release location where patrol is started
#' @param out_dist maximum distance from release location where patrol ends
#' @param release_lat latitude of release location
#' @param release_lon longitude of release location
#' @examples
#' tar_load(shoreline_data)
#' tst <- glider_zone(shoreline = shoreline_data, in_dist = 4000, out_dist = 5000, release_lon = -83.58845, release_lat = 44.08570)
 
glider_zone <- function(shoreline, in_dist, out_dist, release_lon, release_lat){

  shore <- sf::st_read(shoreline, agr = "constant", quiet = TRUE)
  inner <- geobuffer::geobuffer_pts(xy = data.frame(lon = c(release_lon, release_lon), lat = c(release_lat, release_lat) ), dist = c(in_dist, out_dist), output = "sf")
  st_agr(inner) <- "constant"
  inner <- st_transform(inner, 4326)
  all <- st_difference(shore, inner[1,])
  st_agr(all) <- "constant"
  bar <- st_intersection(all, inner[2,])
  st_agr(bar) <- "constant"

  pth <- "~/Documents/cisco_stocking_survival_pilot/plan_map/output/glider_zone.gpkg"
  sf::st_write(bar, pth, append = FALSE)
  
  return(pth)
}


#' @title randomly select listening stations for mobile tracking
#' @description calculates buffer around release location, and extracts points from area around release location.  Can control minimum distance between listening receivers and other mobile tracking locations
#' @param shore path to shoreline shapefile
#' @param recs path to receiver locations
#' 
#' @examples
#' tar_load(shoreline_data)
#' tar_load(recs_raw)
#' mobile_station(shore = shoreline_data, recs = recs_raw)

mobile_station <- function(shore, recs){


  # load shoreline and convert to crs 3175
  shore <- sf::st_read(shore, agr = "constant", quiet = TRUE)
  shore <- sf::st_transform(shore, 3175)

  # create buffer around release location
  # convert to crs 3175 before
  # intersect buffer with shoreline
  inner <- st_sfc(st_point(c(-83.58845, 44.08570)), crs = 4326)
  inner <- st_transform(inner, 3175)
  inner <- st_buffer(inner, dist = 4500, nQuadSegs = 120)
  nearshore <- st_intersection(shore, inner)

  # load in receiver points
  # create buffer around points (250 m)
  # buffer around points represents detection range of receiver
  recs_raw <- sf::st_read(recs, agr = "constant", quiet = TRUE)
  recs_buffer <- st_buffer(recs_raw, dist = 250)

  # Create polygon layer with holes representing receiver detection range
  # polygon is region of water around receivers out to 4500 m from release,
  # corresponds to last arc of receivers
  nearshore <- st_coordinates(nearshore)
  holes <- st_coordinates(recs_buffer)
  holes[,4] <- holes[,4] + 1
  poly <- rbind(nearshore, holes)
  poly <- lapply(split(poly[,c(1,2)], poly[,4]), matrix, ncol = 2)
  poly <- st_sfc(st_polygon(poly), crs = 3175)

  #plot(poly, col = "blue")

  # select 100 random sites to monitor within polygon
  # The next bit of code eliminates sites within 200 m of 250 buffers around each receiver 
  smps <- st_sample(poly, size = 10000, type = "random", exact = TRUE)
  original <- smps

  # calculate 200 m buffers
  buffer <- st_buffer(smps, 200)
  offending <- st_contains(poly, buffer, sparse = FALSE)
  smps <- buffer[offending]

  ############

  # next code block adds a minimum distance of 250 m between listening stations
  i <- 1

  buffer_size = 250

  repeat( {
    buffer <- st_buffer(smps[i], buffer_size)
    offending <- st_intersects(smps, buffer, sparse = FALSE)
    offending[i] <- FALSE
    smps <-  smps[!offending]

    if (i >= length(smps)) {
      break
    } else {
      i <- i + 1
    }
  }
  )

  cents_pt <- st_centroid(smps)
  cents_pt <- st_transform(cents_pt, 4326)
  
  pth <- "~/Documents/cisco_stocking_survival_pilot/plan_map/output/random_mobile_track.gpkg"
  sf::st_write(cents_pt, pth, append = FALSE)
  
  return(pth)

}
  

#' @title calculate geographic coordinates for receivers in concentric circles.
#' @description User provides distance of arc from central point (m), inter-receiver distance (m), and minimum distance distance from endpoints to shoreline and function returns an equally spaced series of points on concentric circles.  End points exactly argument values but inter-receiver spacing is not exactly equal to inter-receiver distance argument.  This is because function finds equally spaced intervals and depending on the distance of inter-receiver circle, inter-receiver distance may vary a bit.
#' @param shoreline path to shoreline (polygon, geopackage file format)
#' @param arc_dist distance in meters from center of arc to concentric circle
#' @param lat latitude of central point (i.e., release location, WGS 84)
#' @param lon longitude of central point (i.2., release location, WGS 84)
#' @param end_dist distance in meters from end of concentric circle to intersection of concentric circle and shoreline
#' @param rec_dist distance in meters between each point (inter-receiver distance)
#' @param label_prefix id column for each point
#' @return returns a list of (1) arc (sf object, linestring), (2) equally spaced points, centered between each endpoint on concentric circle (i.e., receiver points, sf points object), (3) offset value used to center points on concentric circle
#' @examples 

#' tar_load(shoreline_data)
#' shoreline = shoreline_data
#' shoreline <- sf::st_read(shoreline, agr = "constant", quiet = TRUE)
#' lon = -83.58845
#' lat = 44.08570
#' arc_dist = 4000
#' rec_dist = 500
#' end_dist = 250
#' label_prefix = "mob"
#' x <- concentric(shoreline_data, arc_dist = 1000, lat = 44.08570, lon = -83.58845, end_dist = 500, rec_dist = 1000, label_prefix = "AC") 


concentric <- function(shoreline, arc_dist, lat, lon, shore_dist, end_dist, rec_dist, label_prefix = "AC"){
  if(is.character(shoreline)){
    shore <- sf::st_read(shoreline, agr = "constant", quiet = TRUE)
    shoreline <- sf::st_transform(shore, 4326)
  }
  
  release <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
  buffer <- geobuffer::geobuffer_pts(cbind(lon,lat), dist_m = arc_dist, step_dg = 1, output = "sf")
  buffer <- st_transform(buffer, crs = 4326)
  buffer <- sf::st_cast(buffer, "LINESTRING")
  buffer <- sf::st_intersection(shoreline, buffer)
  buffer <- sf::st_cast(buffer, "LINESTRING", warn = FALSE)
  buffer$id <- sf::st_length(buffer)
  buffer <- buffer[which.max(buffer$id),]
  arc_len <- as.numeric(buffer$id) 
  effective_dist <- arc_len - end_dist
  tst <- sf::st_coordinates(buffer)
  tst1 <-   cumsum(c(0, (geosphere::distGeo(tst[,1:2]))))
  tst1 <- tst1[!is.na(tst1)]
  seq_pts <- seq(from = end_dist, to = effective_dist, length.out = (effective_dist %/% rec_dist) + 1)
  observed_rec_dist <- mean(diff(seq_pts))  #"extra"
  int_pts_lon <- approx(tst1, tst[,1], xout = seq_pts)
  int_pts_lat <- approx(tst1, tst[,2], xout = seq_pts)
  tst2 <- data.table(dist_m = int_pts_lon$x, int_lon = int_pts_lon$y, int_lat = int_pts_lat$y)
  tst2[, glatos_array := paste0(label_prefix, gsub("0+$", "", as.character(arc_dist)))]
  tst2[, station_no := formatC(1:nrow(tst2), width = 3, format = "d", flag = "0")]
  tst2[, dist_m := geosphere::distGeo(tst2[,2:3])]
  tst2 <- sf::st_as_sf(tst2, agr = "constant", crs = 4326, coords = c("int_lon", "int_lat"))
  out <- list(arc = buffer, pts = tst2, observed_rec_dist = observed_rec_dist)
  return(out)
}


# function extracts and combines multiple sf files into a single one for plotting
# a bit hacky- see https://github.com/ropensci/targets/issues/271
#' @title combine multiple concentric rings into a single sf points object
#' @description extracts all layers representing a single concentric circle with specified prefix and combines into single spatial sf points object
#' @param prefix  starting string for multiple concentric circles made by static branching by targets.

combine_pts <- function(prefix = "arc"){
  envir <- new.env()
  tar_load(starts_with(prefix), envir = envir)
  foo <- as.list(envir)
  bar <- lapply(1:(length(foo)), function(x)foo[[x]]$pts)
  out <- do.call(rbind, bar)
  return(out)
}


#' @title combined receivers and sentinal coordinates and create gpx file for use on boat sounder
#' @description Combines coordinate files (sf objects) and then writes out file for boat coordinates.  Function subsets out only name and coordinates for use in gpx file.
#' @param sentinal sentinal tag coordinates
#' @param receivers receiver tag coordinates
#' @param pth path to output gpx file

#' @examples
#' tar_load(all_rings)
#' tar_load(sentinal_depth)

#' make_gear_gpx(sentinal = sentinal_depth, receivers = all_rings, pth = "output/Sag_bay_cisco_deployments.gpx")

make_gear_gpx <- function(sentinal, receivers, pth = "output/Sag_bay_cisco_deployments.gpx"){
  sent <- sf::st_as_sf(sentinal, coords = c("lon", "lat"), crs = 4326)
  sent$name <- paste("snt", sent$id, sep = "-")
  sent <- sent[, c("name")]
  receivers$name <- paste(receivers$glatos_array, receivers$station_no, sep = "-")                       
  receivers <- receivers[, c("name")]
  out <- rbind(receivers, sent)
  sf::st_write(out, pth, driver = "GPX", append = FALSE )
  return(pth)
}


#' @title make gpx file
#' @description writes basic gpx file using sf
#' @param x object (data.table) that contains lat, lon, site location
#' @param pth output path to gpx file
#' @param idcol identifies id column (site location) to be used for "name" column in gpx file
#'
#' @examples
#' tar_load(recs)
#' make_gpx(recs, pth = "output/glider_recs.gpx", idcol = "site")
#' 
make_gpx <- function(x, pth = "output/glider_rec.gpx", idcol = "site"){
  setnames(x, idcol, "name")
  x <- sf::st_as_sf(x, coords = c("lon", "lat"), crs = 4326)
  x <- x[, c("name")]
  sf::st_write(x, pth, driver = "GPX", append = FALSE)
  return(pth)
}

#' @title make mobile tracking gpx file
#' @description creates file for upload on sounder using common GPX file format for mobile tracking.
#' @param x data.table containing coordinates and ID for mobile tracking listening stations
#' @param pth output path to write file
#'
#' @examples
#' tar_load(mob_depths)
#' make_mobile_gpx(x = mob_depths, pth = "output/sag_bay_cisco_mobile.gpx")

make_mobile_gpx <- function(x, pth = "output/sag_bay_cisco_mobile.gpx"){
  x[, name := paste(glatos_array, station_no, sep = "-")]
  x <- sf::st_as_sf(x[, c("name", "lon", "lat")], coords = c("lon", "lat"), crs = 4326)
  sf::st_write(x, pth, driver = "GPX", append = FALSE)
  return(pth)
}

######################
#' @title find coordinates for parallel lines of receivers
#' @description User provides starting point and then parallel lines are calculated for lines of multiple receivers
#' @param start_lat starting lat
#' @param start_lon starting lon
#' @param offset distance in meters between parallel lines
#' @param line_direction character value specifying spatial orientation of lines
#' @param parallel_line_direction character value specifying location of parallel line in relation to first line
#' @param inter_pt_dist distance between receivers within a line
#' @param line_dist total distance of line

#' @examples
#' start_lat = 45.537
#' start_lon = -83.999
#' offset = 100
#' line_direction = "N"
#' parallel_line_direction = "E"
#' inter_pt_dist = 500
#' line_dist = 2000
#' find_parallel_lines(start_lat, start_lon, offset, line_direction, parallel_line_direction, inter_pt_dist)

find_parallel_lines <- function(start_lat, start_lon, offset, line_direction = "N", parallel_line_direction = "E", inter_pt_dist = 500, line_dist){

 # find line 1 points
  pt1 <- data.table::data.table(lon = start_lon, lat = start_lat, id = "start", dist = 0)
  pt2 <- data.table::data.table(point_offset(lon = start_lon, lat = start_lat, offsetDist = line_dist, offsetDir = line_direction, distUnit = "m"), id = "end", dist = line_dist)
  line1 <- rbind(pt1, pt2)
  lon_int <- approx(x = line1$dist, y = line1$lon, xout = seq(from = line1[["dist"]][1], to = line1[["dist"]][2], by = inter_pt_dist))$y
  lat_int <- approx(x = line1$dist, y = line1$lat, xout = seq(from = line1[["dist"]][1], to = line1[["dist"]][2], by = inter_pt_dist))$y
  line1 <- data.table::data.table(lon = lon_int, lat = lat_int, id = "line1")
  
  # find line 2 points
  line2_pt1 <- data.table::data.table(point_offset(lon = start_lon, lat = start_lat, offsetDist = offset, offsetDir = parallel_line_direction, distUnit = "m"), id = "start", dist = 0)
  line2_pt2 <- data.table::data.table(point_offset(lon = line2_pt1[["lon"]], lat = line2_pt1[["lat"]], offsetDist = line_dist, offsetDir = line_direction, distUnit = "m"), id = "end", dist = line_dist)
  line2 <- rbind(line2_pt1, line2_pt2)
  lon_int <- approx(x = line2$dist, y = line2$lon, xout = seq(from = line2[["dist"]][1], to = line2[["dist"]][2], by = inter_pt_dist))$y
  lat_int <- approx(x = line2$dist, y = line2$lat, xout = seq(from = line2[["dist"]][1], to = line2[["dist"]][2], by = inter_pt_dist))$y
  line2 <- data.table::data.table(lon = lon_int, lat = lat_int, id = "line2")

  # combine
  out <- rbind(line1, line2)
  out[c(1,(nrow(out))), c("array", "station") := list(c("MBU", "MBU"), c("001", "002"))]
  out[!c(1,(nrow(out))), c("array", "station") := list(c(rep("VPS",8)), c("001", "002", "003", "004", "005", "006", "007", "008"))]
  out[, station := paste(array, station, sep = "-")]

  
return(out)
  
}
 
######################
#' @title find coordinates for parallel lines of receivers
#' @description User provides starting point and then parallel lines are calculated equidistant on both sides of start point.  Used geosphere::bearing to figure out bearing of line input to "bearing" argument in this function.
#' @param start_lat starting lat
#' @param start_lon starting lon
#' @param offset distance in meters between parallel lines
#' @param line_direction character value specifying spatial orientation of lines
#' @param parallel_line_direction character value specifying location of parallel line in relation to first line
#' @param inter_pt_dist distance between receivers within a line
#' @param line_dist total distance of line

#' @examples
#' start_lat = 45.5361667
#' start_lon = -84.00
#' split_dist = 250
#' bearing = 360-59.33614
#' end_lat = 45.545333
#' end_lon = -84.022
#' inter_pt_dist = 500
#'
#' find_parallel_lines_split_start(start_lat=start_lat, start_lon=start_lon, end_lon=end_lon, end_lat=end_lat, bearing =bearing, split_dist = split_dist,  inter_pt_dist=inter_pt_dist)

find_parallel_lines_split_start <- function(start_lat, start_lon, end_lon, end_lat, bearing, split_dist, inter_pt_dist = 500){
  
  # find coords of points equidistant from transect start and end points, positioned parallel to each other at the appropriate distance and bearing
  end1 <- data.table(pt_id = c(1,2), geosphere::destPoint(p = c(start_lon, start_lat), c((bearing + 90)%%360, (bearing-90)%%360), d = split_dist))
  end2 <- data.table(pt_id = c(1,2), geosphere::destPoint(p = c(end_lon, end_lat), c((bearing+90)%%360, (bearing-90)%%360), d = split_dist))    

  d1 <- distm(as.matrix(end1[1, 2:3]), as.matrix(end2[1, 2:3]))
  d2 <- distm(as.matrix(end1[2,2:3]), as.matrix(end2[2,2:3]))
  
  
  dist_ends_side1 <- c(0, d1)
  dist_ends_side2 <- c(0, d2)
  dist_seq_side1 <- seq(0, 2001, by = inter_pt_dist)

  lon_int_side1 <- approx(x = dist_ends_side1, y = c(end1[pt_id == 1,"lon"], end2[pt_id == 1, "lon"]), xout = c(dist_seq_side1))$y
  lat_int_side1 <- approx(x = dist_ends_side1, y = c(end1[pt_id == 1,"lat"], end2[pt_id == 1, "lat"]), xout = c(dist_seq_side1))$y

  lon_int_side2 <- approx(x = dist_ends_side2, y = c(end1[pt_id == 2,"lon"], end2[pt_id == 2, "lon"]), xout = c(dist_seq_side1))$y
  lat_int_side2 <- approx(x = dist_ends_side2, y = c(end1[pt_id == 2,"lat"], end2[pt_id == 2, "lat"]), xout = c(dist_seq_side1))$y
  
  # combine

  s1 <- as.data.table(cbind(lon = lon_int_side1, lat = lat_int_side1))
  s2 <- as.data.table(cbind(lon = lon_int_side2, lat = lat_int_side2))
  s1 <- s1[!c(1,5),]
  s2 <- s2[!c(1,5),]
  s_all <- rbind(s1, s2)
  s_all[, c("array", "station") := list(c(rep("VPS",6)), c("002", "003", "004", "005", "006", "007"))]
  

  ends <- as.data.table(rbind(end1[,2:3], end2[,2:3]))
  ends[c(1,4), c("array", "station") := list(c("MBU", "MBU"), c("001", "002"))]
  ends[c(2,3), c("array", "station") := list(c("VPS", "VPS"), c("001", "008"))] 
  out <- rbind(s_all, ends)
  out[, station := paste(array, station, sep = "-")]
  
  return(out[])
  
}

  

  
  
  
## foo <- rbind(end1, end2)#, lat_int_side1)
## side2 <- cbind(lon_int_side2, lat_int_side2)
## side1 <- cbind(lon_int_side1, lat_int_side1)


##   plot(foo[,c("lon", "lat")])
## tst <-  rbind(c(-84.000, 45.5361667), c(-84.022, 45.545333))
## lines(tst)  
##  points(side1, pch = 16, col = "red")
##  points(side2, pch = 16, col = "blue")


  
## plot(out)

## points(foo, pch =16, col = "red")  
## points(bar, pch = 16, col = "blue")
  


  
 



  
  
