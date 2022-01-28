library(targets)
library(tarchetypes)
source("src/functions.R")
source("src/glider_funcs.R")
source("src/util_funcs.R")
options(tidyverse.quiet = TRUE)

tar_option_set(packages = c("data.table", "sf", "glatos", "geosphere", "viridisLite",  "ggplot2", "raster", "flextable", "terra", "geosphere"))

list(
  # load path to LWF receivers
  tar_target(
    raw_grid,
    "data/us_receivers_20201215.csv",
    format = "file"
  ),

  # load path to Sag Bay polygon from EPA-AOC
  tar_target(
    raw_sbay,
    "data/aoc_mi_saginaw_river_bay/AOC_MI_SAGINAW_RIVER_BAY.shp",
    format = "file"
  ),

  # Convert SB polygon of SB to wgs84
  tar_target(
    sbay,
    {shp <- st_read(raw_sbay, quiet = TRUE);
      shp <- shp[shp$AOC == "AREA OF CONERN",];
      st_transform(shp, crs = 4326)
      },
    format = "rds"
  ),

  # Convert LWF grid to sf object
  tar_target(
    all_grid,
    {grd <- fread(raw_grid);
      st_as_sf(grd, coords = c("longitude", "latitude"), remove = FALSE, crs = 4326, agr = "constant")
    },
    format = "rds"
),

  tar_target(
    raw_bathy,
    "data/LH_bathy/huron_lld.tif", # raw file for lake huron bathymetry data
    format = "file"
  ),
  
  tar_target(
    bathy_SB,
    prep_bathy(raw_bathy, xmin_out = -83.61763, xmax_out = -83.45284, ymin_out = 44.04096, ymax_out = 44.18836, out_pth = "~/Documents/WA_sources_of_reproduction_2021/grid_planing/output/bathy_SB.tif"),
    format = "file"
  )


  ## tar_target(
  ##   clean_grid,
  ##   .clean_grid(),
  ##   format = "rds"
)


