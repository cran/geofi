
.onAttach <- function(lib, pkg) {
#   packageStartupMessage("\ngeofi R package: tools for open GIS data for Finland.\nPart of rOpenGov <ropengov.org>.\n
# **************\n
# Please note that the content of variables 'hyvinvointialue_name_*' and 'hyvinvointialue_code' has changed in 1.0.6 release.\nNew content follows the classification by Statistics Finland.\n
# **************")
}

# Create an empty environment to hold the WFS provider data
wfs_providers <- new.env()

#' @importFrom yaml read_yaml
#' @importFrom purrr map2

.onLoad <- function(lib, pkg) {

  # Read the WFS providers definition data from YAML file in inst/extdata
  wfs_data <- yaml::read_yaml(system.file("extdata", "wfs_providers.yaml",
                                          package = "geofi"))
  # Map the providers and their data (URL, WFS version) to the environment
  purrr::map2(names(wfs_data), wfs_data, assign, envir = wfs_providers)
}
