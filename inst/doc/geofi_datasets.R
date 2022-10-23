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

## ---- eval = FALSE------------------------------------------------------------
#  # install from CRAN
#  install.packages("geofi")
#  
#  # Install development version from GitHub
#  remotes::install_github("ropengov/geofi")

## -----------------------------------------------------------------------------
# Let's first create a function that checks if the suggested 
# packages are available
check_namespaces <- function(pkgs){
  return(all(unlist(sapply(pkgs, requireNamespace,quietly = TRUE))))
}

## ----municipality_keys--------------------------------------------------------
library(geofi)
library(dplyr)
d <- data(package = "geofi")
as_tibble(d$results) %>% 
  select(Item,Title) %>% 
    filter(grepl("municipality_key", Item))

## ----municipality_key_names---------------------------------------------------
names(geofi::municipality_key_2020)

## ----municipality_key_maakunta------------------------------------------------
geofi::municipality_key_2021 %>% 
  count(maakunta_code,maakunta_name_fi,maakunta_name_sv,maakunta_name_en)

## ----municipality_map, fig.height = 7, fig.width = 4--------------------------
municipalities <- get_municipalities(year = 2020, scale = 4500)
plot(municipalities["municipality_name_fi"], border = NA)

## ----muni_pop_map1, fig.height = 7, fig.width = 4-----------------------------
get_municipality_pop(year = 2020) %>%  
  subset(select = miehet_p) %>% 
  plot()

## ----muni_pop_map2, fig.height = 7, fig.width = 4-----------------------------
get_municipality_pop(year = 2020) %>%  
  group_by(hyvinvointialue_name_fi) %>%  
  summarise(vaesto = sum(vaesto)) %>%  
  subset(select = vaesto) %>% 
  plot()

## ----muni_pop_map3, fig.height = 7, fig.width = 4-----------------------------
get_municipality_pop(year = 2020) %>%  
  dplyr::group_by(hyvinvointialue_name_fi) %>% 
  summarise(vaesto = sum(vaesto),
            miehet = sum(miehet)) %>% 
  mutate(share = miehet/vaesto*100) %>% 
  subset(select = share) %>% 
  plot()

## ----zipcode_map, fig.height = 7, fig.width = 4-------------------------------
zipcodes <- get_zipcodes(year = 2015) 
plot(zipcodes["nimi"], border = NA)

## ----statisticsl_grid_data, fig.height = 7, fig.width = 4---------------------
stat_grid <- get_statistical_grid(resolution = 5, auxiliary_data = TRUE)
plot(stat_grid["euref_x"], border = NA)

## ----population_grid_data, fig.height = 7, fig.width = 4----------------------
pop_grid <- get_population_grid(year = 2018, resolution = 5)
plot(pop_grid["kunta"], border = NA)

## ----central_localities, fig.height = 7, fig.width = 4------------------------
plot(municipality_central_localities["teksti"])

## ----geofacets----------------------------------------------------------------
d <- data(package = "geofi")
as_tibble(d$results) %>% 
  select(Item,Title) %>% 
    filter(grepl("grid", Item)) %>% 
  print(n = 100)

## ----geofacet, fig.height = 8, fig.width = 10---------------------------------
libs <- c("pxweb","geofacet","ggplot2")
if (check_namespaces(pkgs = libs)) {
  library(pxweb)
  # Let pull population data from Statistics Finland
  pxweb_query_list <- 
    list("Alue 2019"=c("*"),
         "Tiedot"=c("M411"),
         "Vuosi"=c("*"))
  
  # Download data 
  px_data <- 
    pxweb_get(url = "https://pxdata.stat.fi/PXWeb/api/v1/fi/Kuntien_avainluvut/2019/kuntien_avainluvut_2019_aikasarja.px",
              query = pxweb_query_list)
  
  # Convert to data.frame 
  px_data <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
  names(px_data) <- c("kunta_name","year","value")
  
  # lets aggregate population data
  dat <- left_join(geofi::municipality_key_2021 %>% select(-year),
                   px_data) %>% 
    group_by(maakunta_code, maakunta_name_fi,year) %>% 
    summarise(population = sum(value, na.rm = TRUE)) %>% 
    na.omit() %>% 
    ungroup() %>% 
    rename(code = maakunta_code, name = maakunta_name_fi)
  
  library(geofacet)
  library(ggplot2)
  
  ggplot(dat, aes(x = year, y = population/1000, group = name)) + 
    geom_line() + 
    facet_geo(facets = ~name, grid = grid_maakunta, scales = "free_y") +
    theme(axis.text.x = element_text(size = 6)) +
    scale_x_discrete(breaks = seq.int(from = 1987, to = 2018, by = 5)) +
    labs(title = "Population 1987-2018", y = "population (1000)")
} else {
  message("'pxweb' not available")
}

