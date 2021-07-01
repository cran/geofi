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

## ----municipality_map, fig.width = 5------------------------------------------
library(geofi)
polygon <- get_municipalities(year = 2021, scale = 4500)
point <- geofi::municipality_central_localities
# municipality code into integer
point$municipality_code <- as.integer(point$kuntatunnus)
library(sf) # for spatial data operations later

## ----base, fig.width = 5------------------------------------------------------
# dev.off()
plot(st_geometry(polygon["municipality_code"]))
plot(polygon["municipality_code"], add = TRUE, border="white")
plot(st_geometry(point["municipality_code"]), add = TRUE, color = "black")

## ----gg, fig.width = 5--------------------------------------------------------
libs <- c("ggplot2")
if (check_namespaces(pkgs = libs)) {
library(ggplot2)
ggplot() + 
  geom_sf(data = polygon, aes(fill = municipality_code)) +
  geom_sf(data = point)
} else {
  message("One or more of the following packages is not available: ", 
          paste(libs, collapse = ", "))
}

## ----uusimaa, fig.width=8, fig.height=4---------------------------------------
libs <- c("ggplot2")
if (check_namespaces(pkgs = libs)) {
library(dplyr)
polygon_uusimaa <- polygon %>% filter(maakunta_name_fi %in% "Uusimaa")
point_uusimaa <- point %>% filter(municipality_code %in% polygon_uusimaa$municipality_code)
ggplot() + 
  theme_light() +
  geom_sf(data = polygon_uusimaa, alpha = .3) + 
  geom_sf(data = point_uusimaa) + 
  geom_sf_text(data = point_uusimaa, aes(label = teksti))
} else {
  message("One or more of the following packages is not available: ", 
          paste(libs, collapse = ", "))
}

## ----uusimaa_repel, fig.width=8, fig.height=4---------------------------------
libs <- c("ggplot2")
if (check_namespaces(pkgs = libs)) {
ggplot() + 
  theme_light() +
  geom_sf(data = polygon_uusimaa, alpha = .3) + 
  geom_sf(data = point_uusimaa) + 
  ggrepel::geom_text_repel(data = point_uusimaa %>%
                        sf::st_set_geometry(NULL) %>%
                        bind_cols(point_uusimaa %>% 
                                    sf::st_centroid() %>% 
                                    sf::st_coordinates() %>% as_tibble()),
                     aes(label = teksti, x = X, y = Y))
} else {
  message("One or more of the following packages is not available: ", 
          paste(libs, collapse = ", "))
}

## -----------------------------------------------------------------------------
pop_data <- bind_rows(
  tibble(
    municipality_code = polygon$municipality_code
  ) %>% 
    mutate(population = rnorm(n = nrow(.), mean = 2000, sd = 250),
           time = 2020),
  tibble(
    municipality_code = polygon$municipality_code
  ) %>% 
    mutate(population = rnorm(n = nrow(.), mean = 2000, sd = 250),
           time = 2021)
  )
pop_data

## ----facet,  fig.height=7-----------------------------------------------------
pop_map <- right_join(polygon, pop_data)

libs <- c("ggplot2")
if (check_namespaces(pkgs = libs)) {
ggplot(pop_map, 
       aes(fill = population)) +
  geom_sf() +
  facet_grid(~time)
} else {
  message("One or more of the following packages is not available: ", 
          paste(libs, collapse = ", "))
}

## ----patchwork, fig.width = 8, fig.height=10----------------------------------
libs <- c("ggplot2","patchwork")
if (check_namespaces(pkgs = libs)) {
library(patchwork)
p_municipalities <- ggplot(polygon, aes(fill = municipality_code)) + 
  geom_sf() + 
  theme(legend.position = "top")
p_regions <- ggplot(polygon %>% count(maakunta_code), aes(fill = maakunta_code)) + 
  geom_sf() + 
  theme(legend.position = "top")
p_uusimaa <- ggplot(polygon_uusimaa, aes(fill = municipality_code)) + 
  geom_sf() + 
  theme(legend.position = "top")

(p_municipalities | p_regions) /
p_uusimaa + plot_layout(nrow = 2, heights = c(1,0.6)) +
  plot_annotation(title = "Combining multiple maps into a single (gg)plot")
} else {
  message("One or more of the following packages is not available: ", 
          paste(libs, collapse = ", "))
}

## ---- fig.height = 5----------------------------------------------------------
libs <- c("ggplot2")
if (check_namespaces(pkgs = libs)) {
ggplot(polygon_uusimaa, aes(fill = municipality_code)) +
  geom_sf(color = alpha("white", 1/3)) +
  scale_fill_fermenter(palette = "YlGnBu") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top"
        ) +
  labs(title = "Municipality code", 
       fill = NULL)
} else {
  message("One or more of the following packages is not available: ", 
          paste(libs, collapse = ", "))
}

## ----leaflet, out.width="90%"-------------------------------------------------
libs <- c("leaflet")
if (check_namespaces(pkgs = libs)) {
polygon_wgs84 <- sf::st_transform(x = polygon, crs = "+proj=longlat +datum=WGS84")
point_wgs84 <- sf::st_transform(x = point, crs = "+proj=longlat +datum=WGS84")

library(leaflet)
# lets create a palette for polygon fill (municipality codes)
pal <- leaflet::colorNumeric(palette = "Blues", 
                            domain = polygon_wgs84$municipality_code)
# labels for localities
labels <- sprintf(
  "<strong>%s</strong> (%s)",
  point_wgs84$teksti, point_wgs84$kuntatunnus
) %>% lapply(htmltools::HTML)

# popup for polygons
popup <- sprintf(
  "<strong>%s</strong> (%s)",
  polygon_wgs84$municipality_name_fi, polygon_wgs84$municipality_code
) %>% lapply(htmltools::HTML)


leaflet(polygon_wgs84) %>% 
  addProviderTiles(provider = providers$CartoDB.Positron) %>%
   addPolygons(fillColor = ~pal(municipality_code),
              color = "black",
              weight = 1,
              opacity = 1,
              dashArray = "3",
              fillOpacity = 0.4,
              popup = popup, 
              highlight = highlightOptions(
                weight = 2,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.4,
                bringToFront = TRUE)
  )  %>% 
  addMarkers(data = point_wgs84,
              label = labels,
              clusterOptions = markerClusterOptions(),
              labelOptions = labelOptions(opacity = .7,
                                          style = list("font-weight" = "normal",
                                                       padding = "2px 4px"),
                                          textsize = "12px",
                                          direction = "auto"))
} else {
  message("One or more of the following packages is not available: ", 
          paste(libs, collapse = ", "))
}

