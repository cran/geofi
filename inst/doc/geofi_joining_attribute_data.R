## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  
  collapse = TRUE,
  comment = "#>",
  message = FALSE, 
  warning = FALSE,
  fig.height = 7, 
  fig.width = 7,
  dpi = 75
)

## ----eval = FALSE-------------------------------------------------------------
#  # install from CRAN
#  install.packages("geofi")
#  
#  # Install development version from GitHub
#  remotes::install_github("ropengov/geofi")

## ----include = FALSE, eval = TRUE---------------------------------------------
# Let's first create a function that checks if the suggested 
# packages are available
check_namespaces <- function(pkgs){
  return(all(unlist(sapply(pkgs, requireNamespace,quietly = TRUE))))
}
apiacc <- geofi::check_api_access()
pkginst <- check_namespaces(c("sotkanet","dplyr","tidyr","janitor","ggplot2"))
apiacc_pkginst <- all(apiacc,pkginst)

## ----municipality_map, eval = apiacc_pkginst----------------------------------
library(geofi)
muni <- get_municipalities(year = 2023)

library(sotkanet)
library(dplyr)
sotkadata_swedish_speaking_pop <- GetDataSotkanet(indicators = 2433, years = 2000:2022) %>% 
  filter(region.category == "KUNTA") %>% 
  mutate(municipality_code = as.integer(region.code))

## ----bind_data, eval = apiacc_pkginst-----------------------------------------
map_data <- right_join(muni, 
                       sotkadata_swedish_speaking_pop, 
                       by = c("municipality_code" = "municipality_code"))

## ----plot1, fig.width = 10, fig.height = 7, eval = apiacc_pkginst-------------

library(ggplot2)
map_data %>% 
  ggplot(aes(fill = primary.value)) + 
  geom_sf() + 
  labs(title = unique(sotkadata_swedish_speaking_pop$indicator.title.fi)) +
  theme(legend.position = "top")

## ----zipcode_with_statistics_finland, eval = apiacc_pkginst-------------------
library(pxweb)
# lets get all zipcodes and all variables
pxweb_query_list <- 
  list("Postinumeroalue"=c("*"),
       "Tiedot"=c("he_vakiy"),
       "Vuosi"=c("2021"))

px_raw <- 
  pxweb_get(url = "https://statfin.stat.fi/PXWeb/api/v1/en/Postinumeroalueittainen_avoin_tieto/uusin/paavo_pxt_12ey.px",
            query = pxweb_query_list)


px_data <- as_tibble(
  as.data.frame(px_raw, 
                column.name.type = "text", 
                variable.value.type = "text")
  ) %>% setNames(janitor::make_clean_names(names(.)))
px_data %>% 
  filter(postal_code_area != "Finland")

## ----get_zipcodes, eval = apiacc_pkginst--------------------------------------
px_data$posti_alue <- sub(" .+$", "", px_data$postal_code_area)

# Lets join with spatial data and plot the area of each zipcode
zipcodes19 <- get_zipcodes(year = 2019) 
zipcodes_map <- left_join(zipcodes19, 
                          px_data)
ggplot(zipcodes_map) + 
  geom_sf(aes(fill = inhabitants_total_he), 
          color  = alpha("white", 1/3)) +
  labs(title = "Total number of inhabitants, 2021", 
       fill = NULL)

