# utils file are utility functions that are called within "functions.R" file.

#' @title utility function- adjust resolution of geotiff spatial tiles.
#' @description unify tile resolution of multiple tiles.  Used in "lidar_prep" function.
 
unify_tile_res <- function(x, template){
  temp2 <- x
  terra::res(temp2) <- terra::res(template)
  bar <- terra::resample(x, temp2)
  return(bar)
}


#' @title utility function- find geogrpahic point a  specified distance from first point in line with second point 
#' @description input two points and function finds point a specified distance from 1st point in line with second point.
#' @param from_lon longitude of point one (WGS84, 1-value vector)
#' @param from_lat latitude of point one (WGS84, 1-value vector)
#' @param to_lon longitudee of second point (WGS84, 1-value vector)
#' @param to_lat latitude of second point (WGS84, 1-value vector)
#' @param offset distance from first point to calculated point
#'
#' @examples 

# calculate position 100 m from receiver for sentinel tags
sentinal_pos <- function(from_lon, from_lat, to_lon, to_lat, offset = 100){
  from_lon <- from_lon[[1]]
  from_lat <- from_lat[[1]]
  to_lon <- to_lon[[1]]
  to_lat <- to_lat[[1]]
  dist <- geosphere::distHaversine(c(from_lon, from_lat), c(to_lon, to_lat))
  lon <- approx(x = c(0, dist), y = c(from_lon, to_lon), xout = offset)$y
  lat <- approx(x = c(0, dist), y = c(from_lat, to_lat), xout = offset)$y
  out <- data.table(lon, lat)
  return(out)
}



#' @title utility function- classifies terra objects and create equal number of breaks used for plotting rasters with color shading in leaflet
#' @description creates brakes for leaflet
#' @param x lidar/bathy terra object.  Converts to vector
#' @param n_brks number of breaks desired in color bar

  # create breaks for color bars
  brks <- function(x, n_brks = 100){
    x <- as.vector(x[[1]])
    x <- x[!is.na(x)]
    out <- classInt::classIntervals(x, n = n_brks)
    return(out)
  }

#' @title utility function for formatting data reported out in receiver points table
#' @description formats columns and names for output in receiver points table created by rec_points_2021.rmd.  This function changes adds id, site, depth_ft, lat, lon and changes order of columns
#' @param y data table input from sentinal_depth and rec_depth targets
#' 
formatter <- function(y){
  data.table::setDT(y)
  x <- data.table::copy(y)
  setorder(x, glatos_array, station_no)
  set(x, j = "id", value = 1:nrow(x))
  set(x, j = "site", value = paste(x$glatos_array, x$station_no, sep = "-")) 
  set(x, j = "depth_ft", value = round(x$depth_ft_lidar, 1))
  x[is.nan(depth_ft), depth_ft := round(depth_ft_bathy, 1)]
  x <- x[, c("id", "site", "lon", "lat", "depth_ft")]
  return(x)
}
  
##########################

#' @title utility function that creates basic table of coordinates using flextable
#' @description creates simple flextable for use within rmarkdown to output list of coordinates
#' @param out_tbl data.table of information to output in report

coords_table <- function(out_tbl){#, path = "output/juv_coords.html"){
  flex <- flextable::flextable(out_tbl)
  flex <- flextable::fontsize(flex, part = "all", size = 12)
  flex <- flextable::bold(flex, part = "header")
  flex <- flextable::theme_zebra(flex)
  flex <- flextable::autofit(flex)
  flex <- flextable::align(flex, align = "center", part = "all")
#  flex <- flextable::add_header_row(flex, values = "", colwidths = 6)
#  flex <- flextable::add_header_row(flex, values = paste("as of:", as.Date(Sys.time()), sep = " "),
#                                    colwidths = 6)
#  flex <- flextable::add_header_row(flex, values = "Juvenile cisco gear deployment", colwidths = 6)
#  flex <- flextable::fontsize(flex, i = 1, part = "header", size = 24)
#  flex <- flextable::bold(flex, part = "header", i = 1)
#  flex <- flextable::fontsize(flex, i = 2, part = "header", size = 12)
#  flex <- flextable::align(flex, align = "center", part = "header", i = 2)
  #save_as_html(flex, path = path, title = "Juv cisco") 
  #return(path)
  return(flex)
}



