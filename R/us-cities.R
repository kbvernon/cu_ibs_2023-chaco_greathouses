
# R preamble --------------------------------------------------------------

library(here)
library(maps)
library(sf)
library(tidyverse)
library(tigris)

# data --------------------------------------------------------------------

exclude <- c("MP", "AK", "HI", "VI", "PR", "AS", "GU")

us_states <- states() |> 
  filter(!(STUSPS %in% exclude)) |> 
  st_union()

metros <- here("data", "uscities.csv") |>
  read_csv() |>
  st_as_sf(coords = c("lng", "lat"), crs = 4326) |>
  filter(!(state_id %in% exclude))


# map ---------------------------------------------------------------------

ggplot() +
  geom_sf(data = us_states, fill = "white") +
  geom_sf(
    data = metros, 
    color = "#264500",
    size = 0.5,
    alpha = 0.3
  ) +
  coord_sf(crs = 5070, datum = NA, expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin()
  )

# quarto revealjs slide aspect ratio
qa <- 700/1050

ggsave(
  here("figures", "us-cities.png"),
  width = 10,
  height = 10 * qa,
  dpi = 300
)

