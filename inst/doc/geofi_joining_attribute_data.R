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

## ----municipality_map---------------------------------------------------------
library(geofi)
muni <- get_municipalities(year = 2019)

libs <- c("pxweb","dplyr","tidyr","janitor","ggplot2")
if (check_namespaces(pkgs = libs)) {
library(pxweb)
pxweb_query_list <-
  list("Alue 2020"=c("*"),
       "Tiedot"=c("*"),
       "Vuosi"=c("2019"))

px_raw <-
  pxweb_get(url = "https://pxnet2.stat.fi/PXWeb/api/v1/en/Kuntien_avainluvut/2020/kuntien_avainluvut_2020_aikasarja.px",
            query = pxweb_query_list)

library(dplyr)
library(tidyr)
library(janitor)
library(sf)
px_data <- as_tibble(
  as.data.frame(px_raw, 
                column.name.type = "text", 
                variable.value.type = "text")
  ) %>% setNames(make_clean_names(names(.))) %>% 
  pivot_longer(names_to = "information", values_to = "municipal_key_figures", 3:ncol(.))
px_data
} else {
  message("One or more of the following packages is not available: ", 
          paste(libs, collapse = ", "))
}

## ----municipality_map2--------------------------------------------------------
if (check_namespaces(pkgs = libs)) {
count(px_data, region_2020)
  } else {
  message("One or more of the following packages is not available: ", 
          paste(libs, collapse = ", "))
}

## -----------------------------------------------------------------------------
if (check_namespaces(pkgs = libs)) {
map_data <- right_join(muni, 
                       px_data, 
                       by = c("municipality_name_fi" = "region_2020"))
  } else {
  message("One or more of the following packages is not available: ", 
          paste(libs, collapse = ", "))
}

## ---- fig.width = 10, fig.height = 7------------------------------------------
if (check_namespaces(pkgs = libs)) {
library(ggplot2)
map_data %>% 
  filter(grepl("swedish|foreign", information)) %>% 
  ggplot(aes(fill = municipal_key_figures)) + 
  geom_sf() + 
  facet_wrap(~information) +
  theme(legend.position = "top")
  } else {
  message("One or more of the following packages is not available: ", 
          paste(libs, collapse = ", "))
}

## -----------------------------------------------------------------------------
if (FALSE){
  library(readr)
  cols(
    Area = col_character(),
    Time = col_date(format = ""),
    val = col_double()
  ) -> cov_cols
  
  thl_korona_api <- "https://sampo.thl.fi/pivot/prod/en/epirapo/covid19case/fact_epirapo_covid19case.csv?row=dateweek20200101-508804L&column=hcdmunicipality2020-445222L"
  status <- httr::status_code(httr::GET(thl_korona_api))

  xdf_raw <- read_csv2(thl_korona_api, col_types = cov_cols)
  xdf <- xdf_raw %>% 
    # filter(!grepl("Kaikki", Alue)) %>% 
    rename(date = Time, 
           shp = Area, 
           day_cases = val) %>% 
    group_by(shp) %>% 
    arrange(shp,date) %>% 
    filter(!is.na(day_cases)) %>% 
    mutate(total_cases = cumsum(day_cases)) %>% 
    ungroup() %>% 
    group_by(shp) %>% 
    filter(date == max(date, na.rm = TRUE)) %>% 
    ungroup()
} 
xdf <- structure(list(shp = c("Åland", "All areas", "Central Finland Hospital District", 
"Central Ostrobothnia Hospital District", "Helsinki and Uusimaa Hospital District", 
"Itä-Savo Hospital District", "Kainuu Hospital District", "Kanta-Häme Hospital District", 
"Kymenlaakso Hospital District", "Länsi-Pohja Hospital District", 
"Lappi Hospital District", "North Karelia Hospital District", 
"North Ostrobothnia Hospital District", "North Savo Hospital District", 
"Päijät-Häme Hospital District", "Pirkanmaa Hospital District", 
"Satakunta Hospital District", "South Karelia Hospital District", 
"South Ostrobothnia Hospital District", "South Savo Hospital District", 
"Southwest Finland Hospital District", "Vaasa Hospital District"
), date = structure(c(18674, 18674, 18674, 18674, 18674, 18674, 
18674, 18674, 18674, 18674, 18674, 18674, 18674, 18674, 18674, 
18674, 18674, 18674, 18674, 18674, 18674, 18674), class = "Date"), 
    day_cases = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0), total_cases = c(120, 51047, 1850, 177, 
    29519, 141, 227, 873, 854, 478, 465, 516, 2265, 850, 1243, 
    2662, 671, 348, 534, 573, 4803, 1878)), row.names = c(NA, 
-22L), class = c("tbl_df", "tbl", "data.frame"))

xdf %>% 
    count(shp)
  
muni <- get_municipalities(year = 2021) 
muni %>% 
  st_drop_geometry() %>% 
  count(sairaanhoitop_name_en)

## -----------------------------------------------------------------------------
libs <- c("ggplot2")
if (check_namespaces(pkgs = libs)) {
muni %>% 
  count(sairaanhoitop_name_en) %>% 
  left_join(xdf, by = c("sairaanhoitop_name_en" = "shp")) %>% 
  ggplot(aes(fill = total_cases)) +
  geom_sf() +
  geom_sf_text(aes(label = paste0(sairaanhoitop_name_en, "\n", total_cases)), 
               color = "white") +
  labs(title = "Number of total COVID-19 cases reported since January 2020", 
       fill = NULL)
} else {
  message("One or more of the following packages is not available: ", 
          paste(libs, collapse = ", "))
}

## ----zipcode_with_statistics_finland------------------------------------------
libs <- c("ggplot2","pxweb","janitor")
if (check_namespaces(pkgs = libs)) {
library(pxweb)
# lets get all zipcodes and all variables
pxweb_query_list <- 
  list("Postinumeroalue"=c("*"),
                           "Tiedot"=c("*"))

# Download data 
px_raw <- 
 pxweb_get(url = "https://pxnet2.stat.fi/PXWeb/api/v1/en/Postinumeroalueittainen_avoin_tieto/2019/paavo_1_he_2019.px",
           query = pxweb_query_list)

px_data <- as_tibble(
  as.data.frame(px_raw, 
                column.name.type = "text", 
                variable.value.type = "text")
  ) %>% setNames(make_clean_names(names(.)))
px_data %>% 
  filter(postal_code_area != "Finland")
} else {
  message("One or more of the following packages is not available: ", 
          paste(libs, collapse = ", "))
}

## -----------------------------------------------------------------------------
libs <- c("ggplot2","pxweb","janitor")
if (check_namespaces(pkgs = libs)) {
px_data$posti_alue <- sub(" .+$", "", px_data$postal_code_area)

# Lets join with spatial data and plot the area of each zipcode
zipcodes19 <- get_zipcodes(year = 2019) 
zipcodes_map <- left_join(zipcodes19, 
                          px_data %>% filter(data == "Average age of inhabitants, 2017 (HE)"))
ggplot(zipcodes_map) + 
  geom_sf(aes(fill = paavo_open_data_by_postal_code_area_2019), 
          color  = alpha("white", 1/3)) +
  labs(title = "Average age of inhabitants, 2017 (HE)", 
       fill = NULL)
} else {
  message("One or more of the following packages is not available: ", 
          paste(libs, collapse = ", "))
}

