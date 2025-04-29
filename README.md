
<!-- badges: start -->

[![rOG-badge](https://ropengov.github.io/rogtemplate/reference/figures/ropengov-badge.svg)](https://ropengov.org/)
[![R build
status](https://github.com/rOpenGov/geofi//workflows/R-CMD-check/badge.svg)](https://github.com/rOpenGov/geofi/actions/)
[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/)
[![codecov](https://codecov.io/gh/rOpenGov/geofi/branch/master/graph/badge.svg?token=yJSHHMSSKs)](https://app.codecov.io/gh/rOpenGov/geofi)
[![Watch on
GitHub](https://img.shields.io/github/watchers/ropengov/geofi.svg?style=social)](https://github.com/ropengov/geofi/watchers/)
[![Star on
GitHub](https://img.shields.io/github/stars/ropengov/geofi.svg?style=social)](https://github.com/ropengov/geofi/stargazers/)
[![CRAN
published](https://www.r-pkg.org/badges/version/geofi)](https://www.r-pkg.org/pkg/geofi)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/geofi)](https://cran.r-project.org/package=geofi)
[![Downloads](http://cranlogs.r-pkg.org/badges/geofi)](https://cran.r-project.org/package=geofi)
<!-- badges: end -->

# geofi - Access Finnish Geospatial Data <a href='https://ropengov.github.io/geofi/'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- README.md is generated from README.Rmd. Please edit that file -->

Access Finnish Geospatial Data.

## Installation and use

``` r
# install from CRAN
install.packages("geofi")

# Install development version from GitHub
remotes::install_github("ropengov/geofi")
```

With `geofi`-package you can download geospatial data on municipalities,
zipcodes and population and statistical grids from Statistics Finland
[WFS-api](https://stat.fi/org/avoindata/paikkatietoaineistot_en.html).
In addition, you have on-board municipality keys for aggregating
municipality-level data into higher level regional distributions based
Statistics Finland [classification
API](https://data.stat.fi/api/classifications/v2/).

Below are few examples of the data you can access using `geofi`. Please
have a closer look at the
[vignettes](https://ropengov.github.io/geofi/articles/index.html) for
more comprehensive use cases.

``` r
library(geofi)
d1 <- get_municipalities(year = 2025)
d2 <- get_zipcodes(year = 2025)
d3 <- get_statistical_grid(resolution = 5)
d4 <- get_population_grid(resolution = 5)
d5 <- municipality_central_localities()

library(ggplot2)
library(dplyr)
theme_set(
  theme_minimal(base_family = "Arial") +
  theme(legend.position= "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()
        )
)
p1 <- ggplot(d1, aes(fill = kunta)) + geom_sf(colour = alpha("white", 1/3)) + labs(subtitle = "municipalities")
p2 <- ggplot(d1 |> count(maakunta_code), aes(fill = maakunta_code)) + geom_sf(colour = alpha("white", 1/3)) + labs(subtitle = "Aggregated municipality data \nat region (maakunta) level \n(one of many!)")
p3 <- ggplot(d2, aes(fill = as.integer(posti_alue))) + geom_sf(colour = alpha("white", 1/3)) + labs(subtitle = "zipcodes")
p4 <- ggplot(d3, aes(fill = nro)) + geom_sf(colour = alpha("white", 1/3)) + labs(subtitle = "statistical grid")
p5 <- ggplot(d4, aes(fill = id_nro)) + geom_sf(colour = alpha("white", 1/3)) + labs(subtitle = "population grid")
p6 <- ggplot(d5, aes(color = as.integer(kuntatunnus))) + geom_sf() + labs(subtitle = "Central municipality localities")

library(patchwork)
wrap_plots(list(p1,p2,p3,p4,p5,p6), ncol = 3) + 
  patchwork::plot_annotation(title = "Spatial data in geofi-package")
```

![](man/figures/readme_map-1.png)<!-- -->

### Contribute

Contributions are very welcome:

- [Use issue tracker](https://github.com/ropengov/geofi/issues) for
  feedback and bug reports.
- [Send pull requests](https://github.com/ropengov/geofi/)
- [Star us on the Github page](https://github.com/ropengov/geofi/)

### Acknowledgements

**Kindly cite this work** as follows:

``` text
citation("geofi")
Kindly cite the geofi R package as follows:

To cite 'geofi' in publications use:

  Kainu M, Lehtomaki J, Parkkinen J, Miettinen J, Kantanen P, Vesanen
  S, Lahti L (2025). _geofi: Access Finnish Geospatial Data_.
  doi:10.32614/CRAN.package.geofi
  <https://doi.org/10.32614/CRAN.package.geofi>, R package version
  1.1.0, <https://github.com/rOpenGov/geofi>.

A BibTeX entry for LaTeX users is

  @Manual{R-geofi,
    title = {{geofi: Access Finnish Geospatial Data}},
    doi = {10.32614/CRAN.package.geofi},
    author = {Markus Kainu and Joona Lehtomaki and Juuso Parkkinen and Jani Miettinen and Pyry Kantanen and Sampo Vesanen and Leo Lahti},
    year = {2025},
    version = {1.1.0},
    note = {R package version 1.1.0},
    url = {https://github.com/rOpenGov/geofi},
  }

Many thanks for all contributors! For more info, see:
https://github.com/rOpenGov/geofi
```

We are grateful to all
[contributors](https://github.com/rOpenGov/geofi/graphs/contributors).
This project is part of [rOpenGov](https://ropengov.org).
