
# R preamble --------------------------------------------------------------

library(DBI)
library(ggfx)
library(ggrepel)
library(here)
library(maps)
library(sf)
library(tidyverse)
library(tigris)

# load custom functions
# doing it this way to mask the functions in the global environment
sys.source(
  here("R", "get_basemap.R"), # requires httr and jsonlite
  envir = attach(NULL, name = "basemap")
)

gpkg <- here("data", "chaco.gpkg")

dbc <- dbConnect(RSQLite::SQLite(), gpkg)

epsg <- 5070

qa <- 1280/720

# data --------------------------------------------------------------------

project_area <- read_sf(gpkg, "project-area")

watersheds <- read_sf(gpkg, "watersheds")

greathouses <- dbReadTable(dbc, "great-houses") |> as_tibble()

# basemap -----------------------------------------------------------------

bb8 <- project_area |> st_buffer(90000) |> st_transform(epsg) |> st_bbox()

dy <- bb8[["ymax"]] - bb8[["ymin"]]
dx <- dy * qa

mid_x <- (bb8[["xmax"]] + bb8[["xmin"]])/2

bb8[["xmax"]] <- mid_x + (dx/2)
bb8[["xmin"]] <- mid_x - (dx/2)

basemap <- get_basemap(
  bb8,
  map = "imagery", 
  size = c(1280, 720),
  imageSR = epsg
)

# map ---------------------------------------------------------------------

cover <- st_sym_difference(
  bb8 |> st_as_sfc(),
  project_area |> st_transform(epsg)
)

greathouses_sf <- watersheds |> 
  st_transform(epsg) |> 
  st_centroid() |> 
  semi_join(
    greathouses |> group_by(watershed) |> summarize(),
    by = "watershed"
  )

chaco <- greathouses_sf |> filter(watershed == "1408010606")
chaco_xy <- chaco |> st_coordinates() |> c()

ggplot() +
  annotation_raster(
    basemap,
    bb8[["xmin"]], bb8[["xmax"]],
    bb8[["ymin"]], bb8[["ymax"]]
  ) +
  geom_sf(
    data = cover,
    color = "transparent",
    fill = alpha("white", 0.7)
  ) +
  geom_sf(
    data = project_area,
    color = "black",
    fill = "transparent"
  ) +
  with_outer_glow(
    geom_sf(
      data = greathouses_sf,
      color = "#00749d",
      size = 2.5
    ),
    colour = "white",
    sigma = 2,
    expand = 4
  ) +
  geom_label_repel(
    aes(chaco_xy[1], chaco_xy[2], label = "Chaco Canyon"),
    nudge_x = 85000,
    nudge_y = 38000,
    label.padding = unit(0.3, "lines"),
    size = 20/.pt
  ) +
  geom_sf(
    data = chaco |> st_buffer(7000),
    fill = "white", 
    color = "black",
    linewidth = 0.5
  ) +
  geom_sf(
    data = chaco,
    color = "#00749d",
    size = 3
  ) +
  coord_sf(
    xlim = bb8[c("xmin", "xmax")],
    ylim = bb8[c("ymin", "ymax")],
    crs = epsg,
    datum = NA,
    expand = FALSE
  ) +
  theme_void()

ggsave(
  here("figures", "chaco-world.png"),
  width = 10 * qa,
  height = 10,
  dpi = 300
)

# disconnect! -------------------------------------------------------------

dbDisconnect(dbc)
