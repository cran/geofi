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

## ----eval = FALSE-------------------------------------------------------------
# options(geofi_mml_api_key = "your_api_key_here")

## -----------------------------------------------------------------------------
# collections <- ogc_get_maastotietokanta_collections()
# head(collections)

## -----------------------------------------------------------------------------
# cemeteries <- ogc_get_maastotietokanta(collection = "hautausmaa", crs = 4326)
# cemeteries

## -----------------------------------------------------------------------------
# cemeteries_helsinki <- ogc_get_maastotietokanta(
#   collection = "hautausmaa",
#   bbox = "24.5,60.1,25.5,60.5",
#   crs = 4326
# )

## -----------------------------------------------------------------------------
# ggplot(cemeteries_helsinki) +
#   geom_sf() +
#   theme_minimal() +
#   labs(title = "Cemeteries near Helsinki")

## ----eval = FALSE-------------------------------------------------------------
# bogs <- ogc_get_maastotietokanta(
#   collection = "suo",
#   max_pages = 15
# )

## -----------------------------------------------------------------------------
# kainu_places <- ogc_get_nimisto(search_string = "kainu")
# print(kainu_places)

## -----------------------------------------------------------------------------
# ggplot(kainu_places) +
#   geom_sf() +
#   geom_sf_text(aes(label = spelling), size = 3, check_overlap = TRUE) +
#   theme_minimal() +
#   labs(title = "Place Names Containing 'Kainu'")

## -----------------------------------------------------------------------------
# kainu_bbox <- ogc_get_nimisto(
#   search_string = "kainu*",
#   bbox = "27.515259,63.450509,30.531006,64.524823",
#   crs = 4326
# )

## -----------------------------------------------------------------------------
# swimming_beaches_filtered_by_municipality_number <- ogc_get_nimisto(
#   search_string = "*uimaranta*",
#   custom_params = "municipality=091"
# )
# swimming_beaches_filtered_by_municipality_number

