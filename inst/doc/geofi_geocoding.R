## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# # Install from GitHub
# devtools::install_github("rOpenGov/geofi")
# # Install from CRAN
# install.packages("geofi")

## -----------------------------------------------------------------------------
# 
# library(geofi)
# library(sf)
# library(ggplot2)
# 

## ----eval = FALSE-------------------------------------------------------------
# options(geofi_mml_api_key = "your_api_key_here")

## -----------------------------------------------------------------------------
# suomenlinna <- geocode(
#   search_string = "Suomenlinna",
#   source = "geographic-names",
#   crs = 4326
# )
# print(suomenlinna)

## -----------------------------------------------------------------------------
# address <- geocode(
#   search_string = "Mannerheimintie 100, Helsinki",
#   source = "addresses",
#   crs = 3067,
#   size = 5
# )
# print(address)

## -----------------------------------------------------------------------------
# ggplot(data = address) +
#   geom_sf(color = "blue", size = 3) +
#   labs(
#     title = "Geocoded Location: Mannerheimintie 100, Helsinki",
#     subtitle = "CRS: ETRS-TM35FIN (EPSG:3067)"
#   ) +
#   theme_minimal()

## -----------------------------------------------------------------------------
# focused <- geocode(
#   search_string = "Helsinki",
#   source = "geographic-names",
#   options = "focus.point.lat=60.1699&focus.point.lon=24.9384"
# )

## -----------------------------------------------------------------------------
# # Create an sf point
# parliament_point <- data.frame(lon = 24.933333, lat = 60.1725) |>
#   sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
# 
# # Reverse geocode
# places <- geocode_reverse(
#   point = parliament_point,
#   sources = "geographic-names"
# )
# print(places)

## -----------------------------------------------------------------------------
# json_results <- geocode_reverse(
#   point = parliament_point,
#   boundary_circle_radius = 1,
#   return = "json"
# )
# print(json_results)

## -----------------------------------------------------------------------------
# ggplot() +
#   geom_sf(data = parliament_point, color = "red", size = 3, shape = 17) +
#   geom_sf(data = places, color = "blue", size = 3) +
#   labs(
#     title = "Reverse Geocoded Location: Eduskuntatalo",
#     subtitle = "Red: Input Point, Blue: Geocoded Result (EPSG:4326)"
#   ) +
#   theme_minimal()

