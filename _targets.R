library(targets)
library(tarchetypes)
source("src/functions.R")
source("src/glider_funcs.R")
source("src/util_funcs.R")
options(tidyverse.quiet = TRUE)

tar_option_set(packages = c("data.table", "sf", "glatos", "geosphere", "viridisLite",  "ggplot2", "raster", "flextable", "terra", "geosphere", "leaflet", "readxl"))

list(
  # load path to range curve data collected by Binder in 2016 in GB with V13 tags
  tar_target(
    raw_log_reg_H_GBE,
    "data/Results/Results/GBE/LogisticRegressions/HighPower/logisticResultsH.csv",
    format = "file"
  ),
  
  # load path to Saginaw Bay Reef locations
  tar_target(
    raw_SB_reefs,
    "data/Saginaw Bay Reef Locations.xlsx",
    format = "file"
    ),
  
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
    grid,
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
    bathy,
    prep_bathy(raw_bathy, xmin_out = -83.96, xmax_out = -82.535, ymin_out = 43.5, ymax_out = 44.44, out_pth = "~/Documents/WA_sources_of_reproduction_2021/grid_planing/output/bathy_SB.tif"),
    format = "file"
  ),

tar_target(
  grid_depth,
  depth_extract(raw_recs = grid, bathy = bathy, lidar = NULL),
  format = "rds"
  ),

tar_target(
  map,
  grid_map(bathy, grid = grid_depth, sbay, reefs = reefs, rec_grid, spawn_rivers = spawn_rivs, bay_mth = bay_mth_grd, pth = "docs/index.html"),
  format = "file"
),

tar_target(
  rec_grid,
  .grid(poly = sbay, cellsize = c(10000, 10000), in_crs = 3175, out_crs = 4326),
  format = "rds"
),

# read in Sag Bay reefs, write out an object for future debugging
tar_target(
  dirty_reefs,
  {out <- as.data.table(read_xlsx(path = raw_SB_reefs, range = "Reefs!D6:I15"));
   #fwrite(out, "data/SB_reefs.csv")
  },
  format = "fst_dt"
),

tar_target(
  reefs,
  clean_reefs(x = dirty_reefs),
  format = "rds"
),

tar_target(
  raw_spawn_riv,
  "data/spawning_rivers.csv",
  format = "file"
),

tar_target(
  spawn_rivs,
  {out <- fread(raw_spawn_riv);
    st_as_sf(out, agr = "constant", remove = FALSE, coords = c("long", "lat"), crs = 4326)
  },
  format = "rds"
),

tar_target(
  sag_bay_mth_raw,
  "data/SB_mouth.gpkg",
  format = "file"
),

tar_target(
  bay_mth,
  .intersect(sag_bay_mth_raw, sbay),
  format = "rds"
),

tar_target(
  bay_mth_grd,
  .grid(poly = bay_mth, cellsize = c(6000, 6000), in_crs = 3175, out_crs = 4326),
  format = "rds"
),

tar_target(
  GL_poly,
  fix_CRS(),
  format = "rds"
),

tar_target(
  LH,
  crop(GL_poly, xmin = -84.387, xmax = -82.0915, ymin = 43, ymax = 45),
  format = "rds"
),

# create simulated tracks.  Adjust inputs in "crw_in_polygon_updated" to change nature of tracks
tar_target(
  sim_tracks,
  .sim_tracks(
    n_trks = 50,
    poly = LH,
    theta = c(0,25),
    stepLen = 10000,
    initHeading = 0,
    nsteps = 100,
    initPos = c(-83.6248, 43.8824),
    sp_out = FALSE,
    show_progress = FALSE),
  format = "fst_dt"
  ),

# calculate min distance between fish and all receivers in grid
tar_target(
  min_dist,
  sim_fish_rec_dist(fsh_trks = sim_tracks, rec_grid = bay_mth_grd),
  format = "fst_dt"
),

# calculate transmissions for all simulated fish tracks and simulated tags
tar_target(
  sim_tag_trans,
  .sim_tag_trans(
    path = sim_tracks,
    vel = 0.5,
    delayRng = c(60,180),
    burstDur = 5,
    EPSG = 3175,
    sp_out = FALSE),
  format = "fst_dt"
),

# determine which transmission were detected or not.
tar_target(
  sim_dtc,
  .sim_dtc(dtc_trans = sim_tag_trans,
           recLoc = bay_mth_grd,
           detRngFun = pdrf,
           EPSG = 3175,
           sp_out = FALSE,
           show_progress = FALSE,
           min_dist = min_dist),
  format = "fst_dt"
)




) 



