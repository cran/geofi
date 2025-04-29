## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)
library(geofi)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)

## -----------------------------------------------------------------------------
# munis <- ogc_get_statfi_area(year = 2022, scale = 4500, tessellation = "kunta")
# print(munis)

## -----------------------------------------------------------------------------
# ggplot(munis) +
#   geom_sf() +
#   theme_minimal() +
#   labs(title = "Finnish Municipalities (2022)")

## -----------------------------------------------------------------------------
# bbox <- "200000,6600000,500000,6900000"  # In EPSG:3067
# munis_south <- ogc_get_statfi_area(
#   year = 2022,
#   scale = 4500,
#   tessellation = "kunta",
#   bbox = bbox,
#   crs = 3067
# )

## -----------------------------------------------------------------------------
# ggplot(munis_south) +
#   geom_sf() +
#   theme_minimal() +
#   labs(title = "Municipalities in Southern Finland (2022)")

## -----------------------------------------------------------------------------
# wellbeing <- ogc_get_statfi_area(
#   year = 2022,
#   tessellation = "hyvinvointialue",
#   scale = 4500
# )

## -----------------------------------------------------------------------------
# pop_data <- ogc_get_statfi_area_pop(year = 2021, crs = 3067)
# print(pop_data)

## -----------------------------------------------------------------------------
# ggplot(pop_data) +
#   geom_sf(aes(fill = population_total)) +
#   scale_fill_viridis_c(option = "plasma") +
#   theme_minimal() +
#   labs(title = "Population by Administrative Area (2021)", fill = "Population")

## -----------------------------------------------------------------------------
# bbox <- "200000,6600000,500000,6900000"
# pop_south <- ogc_get_statfi_area_pop(year = 2021, bbox = bbox, crs = 3067)

## -----------------------------------------------------------------------------
# grid_data <- ogc_get_statfi_statistical_grid(year = 2021, resolution = 5000)
# print(grid_data)

## -----------------------------------------------------------------------------
# ggplot(grid_data) +
#   geom_sf(aes(fill = population_total), color = NA) +
#   scale_fill_viridis_c(option = "magma") +
#   theme_minimal() +
#   labs(title = "Population by 5km Grid Cells (2021)", fill = "Population")

## -----------------------------------------------------------------------------
# bbox <- "200000,6600000,500000,6900000"
# grid_south <- ogc_get_statfi_statistical_grid(
#   year = 2021,
#   resolution = 1000,
#   bbox = bbox
# )

