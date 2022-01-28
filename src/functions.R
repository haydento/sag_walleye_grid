#' @title create custom leaflet interactive map of planning data used for study design
#' @description Creates map that overlays two sources of bathymetry (lidar and GL bathymetry), receiver locations, sentinal tag locations, area of proposed glider patrols, mobile tracking listening stations
#' @param bathy file path to bathymetry layer (tif)
#' @param grid sf points object containing proposed LWF grid

#' @examples
#' tar_load(bathy)
#' tar_load(grid)
#' tar_load(sbay)
#' grid_map(bathy, lidar, sentinal, rec_depth, glid_area = glider, mobile = mobile_listen)

# create leaflet map
grid_map <- function(bathy, grid, sbay, pth){

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
  m <- addCircleMarkers(m, data = grid, label = ~station, color = c("red"), radius = c(4), group = "recs", stroke = FALSE, fillOpacity = 1)
                    
  
#  m <- addCircleMarkers(m, data = sync_100, label = ~label, fillColor = "yellow", radius = 8, group = "sentinel tag", stroke = FALSE, fillOpacity = 1)
#  m <- addCircleMarkers(m, data = all_mob, label = ~label_short, fillColor = "orange", radius = 8, group = "mobile", stroke = FALSE, fillOpacity = 1)
  m <- addPolygons(map = m, data = sbay, color  = "red", fillColor = NA, group = "sag bay")
  m <- leafem::addMouseCoordinates(m)
  m <- addLegend(m, pal = pal_LH , values =  bath[[1]], title = "depth (ft)", opacity = 1, group = "bathy (ft)")
#  m <- addLegend(m, pal = pal_lidar, values = lid[[1]], title = "depth (lidar, ft)", opacity = 1, group = "lidar_depth")
#  m <- addLayersControl(m, overlayGroups = c("LH_depth", "lidar_depth", "receivers", "sentinel tag", "mobile", "glider patrol"), options = layersControlOptions(collapsed = FALSE))
    m <- addMeasure(m, primaryLengthUnit = "meters", secondaryLengthUnit = "kilometers")  
  m <- addLayersControl(m, baseGroups = c("satellite", "nav chart", "alt", "bathy (ft)"), overlayGroups = c("recs", "sag bay"), position = "bottomright", options = layersControlOptions(collapsed = FALSE))
  
  htmlwidgets::saveWidget(m, pth)

  return(pth)
  }

