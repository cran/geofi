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

## -----------------------------------------------------------------------------
library(geofi)
library(sf)
library(dplyr)
muni <- get_municipalities()
point <- municipality_central_localities
crs <- st_crs(muni)
crs$input

## ----crss---------------------------------------------------------------------
muni_4326 <- st_transform(muni, "EPSG:4326")
crs <- st_crs(muni_4326)
crs$input

libs <- c("ggplot2","patchwork")
if (check_namespaces(pkgs = libs)) {
library(ggplot2)
  
p1 <- ggplot(muni %>% st_union()) + 
  geom_sf() + 
  labs(subtitle = "EPSG:3067")
p2 <- ggplot(muni_4326 %>% st_union()) + 
  geom_sf() +
  labs(subtitle = "EPSG:4326")
library(patchwork)
wrap_plots(list(p1,p2), nrow = 1) +
  plot_annotation(title = "Map of Finland in two different CRS")
} else {
  message("One or more of the following packages is not available: ", 
          paste(libs, collapse = ", "))
}

## ----largest, fig.width=5-----------------------------------------------------
libs <- c("ggplot2")
if (check_namespaces(pkgs = libs)) {
# compute area
muni$area <- sf::st_area(muni)
# largest
muni %>% 
  arrange(desc(area)) %>% 
  slice(1:10) %>% 
  ggplot() + 
  geom_sf() + 
  geom_sf_label(aes(label = name_fi)) +
  labs(title = "largest 10")
  # smallest
muni %>% 
  arrange(area) %>% 
  slice(1:10) %>% 
  ggplot() + 
  geom_sf() + 
  geom_sf_label(aes(label = name_fi)) +
  labs(title = "smallest 10")
} else {
  message("One or more of the following packages is not available: ", 
          paste(libs, collapse = ", "))
}

## ----subsetting---------------------------------------------------------------
greater_helsinki <- c('Espoo','Helsinki','Vantaa','Hyvinkää',
                      'Järvenpää','Kauniainen','Kerava','Kirkkonummi',
                      'Mäntsälä','Nurmijärvi','Pornainen','Sipoo','Tuusula','Vihti')
greater_helsinki_polygon <- muni %>% filter(municipality_name_fi %in% greater_helsinki)

if (check_namespaces(pkgs = libs)) {
ggplot(greater_helsinki_polygon) + 
  geom_sf() +
  geom_sf(data = point %>% 
            filter(teksti %in% toupper(greater_helsinki)))
} else {
  message("One or more of the following packages is not available: ", 
          paste(libs, collapse = ", "))
}

## ----bb_poly------------------------------------------------------------------
bounding_box_polygon <- st_as_sfc(st_bbox(muni %>% filter(municipality_name_fi %in% greater_helsinki)))

if (check_namespaces(pkgs = libs)) {
ggplot(st_intersection(bounding_box_polygon, muni)) + 
  geom_sf() +
  geom_sf(data = point %>% filter(teksti %in% toupper(greater_helsinki)))
} else {
  message("One or more of the following packages is not available: ", 
          paste(libs, collapse = ", "))
}

## ----bb_point-----------------------------------------------------------------
bounding_box_point <- st_as_sfc(st_bbox(point %>% filter(teksti %in% toupper(greater_helsinki))))

if (check_namespaces(pkgs = libs)) {
ggplot(st_intersection(bounding_box_point, muni)) + 
  geom_sf() +
  geom_sf(data = point %>% filter(teksti %in% toupper(greater_helsinki)))
} else {
  message("One or more of the following packages is not available: ", 
          paste(libs, collapse = ", "))
}

## ----neighbours,  fig.height = 5----------------------------------------------
helsinki <- muni %>%  filter(municipality_name_fi == "Helsinki")
neigbour_codes <- st_intersection(muni,helsinki) %>% 
  pull(municipality_code)

if (check_namespaces(pkgs = libs)) {
ggplot(muni %>% filter(municipality_code %in% neigbour_codes)) +
  geom_sf() +
  geom_sf_label(aes(label = municipality_name_fi))
} else {
  message("One or more of the following packages is not available: ", 
          paste(libs, collapse = ", "))
}

## ----unioin-------------------------------------------------------------------
muni$area_class <- cut_number(x = as.numeric(muni$area), n = 3)
# 
# 
if (check_namespaces(pkgs = libs)) {
muni %>% 
  filter(area_class == levels(muni$area_class)[1]) %>% 
  st_union() %>% 
  ggplot() +
  geom_sf()
} else {
  message("One or more of the following packages is not available: ", 
          paste(libs, collapse = ", "))
}

## ----union2-------------------------------------------------------------------
if (check_namespaces(pkgs = libs)) {
muni %>% 
  group_by(area_class) %>%
  summarise() %>% 
  ggplot() +
  geom_sf(aes(fill = area_class))
} else {
  message("One or more of the following packages is not available: ", 
          paste(libs, collapse = ", "))
}

## ----centroids, fig.width=5---------------------------------------------------
muni_centroids <- st_centroid(muni)

if (check_namespaces(pkgs = libs)) {
ggplot() +
  geom_sf(data = muni) +
  geom_sf(data = muni_centroids, color = "blue") +
  # plot also the municipality_central_localities
  geom_sf(data = municipality_central_localities, color = "red")
} else {
  message("One or more of the following packages is not available: ", 
          paste(libs, collapse = ", "))
}

## ----buffers, fig.width=5-----------------------------------------------------
muni_centroids_buffer <- muni_centroids %>%
    st_buffer(dist = 15000)

if (check_namespaces(pkgs = libs)) {
ggplot() +
  geom_sf(data = muni) +
  geom_sf(data = muni_centroids_buffer) +
  geom_sf(data = muni_centroids, shape = 3)
} else {
  message("One or more of the following packages is not available: ", 
          paste(libs, collapse = ", "))
}

## ----muonio, fig.width=5------------------------------------------------------
muonio <- muni %>% filter(municipality_name_fi == "Muonio")

grid_sf <- st_make_grid(muonio, cellsize = c(2000,4000), what="polygons") %>%
    st_sf()

grid_clip <- st_intersection(grid_sf, muonio)
grid_clip$rank <- 1:nrow(grid_clip)

if (check_namespaces(pkgs = libs)) {
ggplot(grid_clip) +
  geom_sf(aes(fill = rank), color = alpha("white", 1/3), size = 3) +
  scale_fill_viridis_c() +
  theme_minimal()
} else {
  message("One or more of the following packages is not available: ", 
          paste(libs, collapse = ", "))
}

## ----voronoi, fig.height = 9--------------------------------------------------
library(geofi)
library(sf)

muni_voronoi <- municipality_central_localities %>% 
  st_union() %>%
  st_voronoi() %>% 
  st_cast() %>% 
  st_sf() %>% 
  st_intersection(st_union(muni)) %>% 
  mutate(rnk = 1:nrow(.))

if (check_namespaces(pkgs = libs)) {
ggplot(muni_voronoi) + 
  geom_sf(aes(fill = rnk)) +
  geom_sf(data = municipality_central_localities, shape = 4) +
  scale_fill_fermenter(palette = "YlGnBu") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")
} else {
  message("One or more of the following packages is not available: ", 
          paste(libs, collapse = ", "))
}

