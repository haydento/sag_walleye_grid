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
grid_map <- function(bathy, grid, sbay, reefs, rec_grid, spawn_rivers,  pth){

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
  m <- addCircleMarkers(m, data = rec_grid, label = ~STATE, color = c("yellow"), radius = c(10), group = "proposed recs", stroke = FALSE, fillOpacity = 1)
  m <- addMarkers(m, data = spawn_rivers, label = ~river, group = "spawn rivers")
                      
#  m <- addCircleMarkers(m, data = sync_100, label = ~label, fillColor = "yellow", radius = 8, group = "sentinel tag", stroke = FALSE, fillOpacity = 1)
#  m <- addCircleMarkers(m, data = all_mob, label = ~label_short, fillColor = "orange", radius = 8, group = "mobile", stroke = FALSE, fillOpacity = 1)
  m <- addPolygons(map = m, data = sbay, color  = "red", fillColor = NA, group = "sag bay")
  m <- leafem::addMouseCoordinates(m)
  m <- addLegend(m, pal = pal_LH , values =  bath[[1]], title = "depth (ft)", opacity = 1, group = "bathy (ft)")
#  m <- addLegend(m, pal = pal_lidar, values = lid[[1]], title = "depth (lidar, ft)", opacity = 1, group = "lidar_depth")
#  m <- addLayersControl(m, overlayGroups = c("LH_depth", "lidar_depth", "receivers", "sentinel tag", "mobile", "glider patrol"), options = layersControlOptions(collapsed = FALSE))
    m <- addMeasure(m, primaryLengthUnit = "meters", secondaryLengthUnit = "kilometers")  
  m <- addLayersControl(m, baseGroups = c("satellite", "nav chart", "alt", "bathy (ft)"), overlayGroups = c("LWF recs", "sag bay", "bay reefs", "proposed recs", "spawn rivers"), position = "bottomright", options = layersControlOptions(collapsed = FALSE))
  
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





