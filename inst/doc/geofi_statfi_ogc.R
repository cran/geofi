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
# muni <- ogc_get_statfi_area(year = 2022, scale = 4500, tessellation = "kunta")

## -----------------------------------------------------------------------------
# ggplot(muni) +
#   geom_sf() +
#   theme_minimal() +
#   labs(title = "Finnish Municipalities (2022)")

## -----------------------------------------------------------------------------
# bbox_finland_south <- "18.797607,59.573288,30.476074,61.695082"
# muni_south <- ogc_get_statfi_area(
#   year = 2022,
#   scale = 4500,
#   tessellation = "kunta",
#   bbox = bbox_finland_south,
#   crs = 3067
# )

## -----------------------------------------------------------------------------
# ggplot(muni_south) +
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

## -----------------------------------------------------------------------------
# ggplot(data=pop_data |>
#          filter(grepl("^kunta", areaStatisticalUnit_inspireId_localId))) +
#     geom_sf(aes(fill = female_percentage)) +
#     scale_fill_viridis_c(option = "plasma") +
#     theme_minimal() +
#     labs(title = "Population by Administrative Area (2021)", fill = "share of females (%)")

## -----------------------------------------------------------------------------
# pop_south <- ogc_get_statfi_area_pop(year = 2021, bbox = bbox_finland_south, crs = 4326)
# ggplot(data=pop_south |> filter(grepl("^kunta", areaStatisticalUnit_inspireId_localId))) +
#     geom_sf(aes(fill = female_percentage)) +
#     scale_fill_viridis_c(option = "plasma") +
#     theme_minimal() +
#     labs(title = "Population by Administrative Area (2021)", fill = "share of females (%)")

## -----------------------------------------------------------------------------
# grid_data <- ogc_get_statfi_statistical_grid(year = 2021, resolution = 5000, bbox = bbox_finland_south)

## -----------------------------------------------------------------------------
# ggplot(grid_data) +
#   geom_sf(aes(fill = total_count), color = NA) +
#   scale_fill_viridis_c(option = "magma", trans='sqrt') +
#   theme_minimal() +
#   labs(title = "Population by 5km Grid Cells (2021)", fill = "Population")

## -----------------------------------------------------------------------------
# bbox_capital_region <- "24.441147,60.102168,25.285034,60.369071"
# grid_capital <- ogc_get_statfi_statistical_grid(
#   year = 2021,
#   resolution = 1000,
#   bbox = bbox_capital_region
# )

## -----------------------------------------------------------------------------
# ggplot(grid_capital) +
#   geom_sf(aes(fill = total_count), color = NA) +
#   scale_fill_viridis_c(option = "magma", trans='sqrt') +
#   theme_minimal() +
#   labs(title = "Population by 1km Grid Cells (2021)", fill = "Population")

