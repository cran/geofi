% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_codes_as_character.R
\name{convert_municipality_key_codes}
\alias{convert_municipality_key_codes}
\title{Convert regional codes in on-board municipality key data sets into variable length characters}
\usage{
convert_municipality_key_codes(muni_key = geofi::municipality_key)
}
\arguments{
\item{muni_key}{a municipality key from geofi-package}
}
\value{
tibble with codes converted to variable length characters as provided by Statistics Finland
}
\description{
Statistics Finland provides numerical codes of regions as two or three digit characters.
By default, those are converted to integers by geofi for convenience, but can be converted back
using this function.
}
\examples{
 \dontrun{
 convert_municipality_key_codes(muni_key = geofi::municipality_key)
 }

}
\author{
Markus Kainu \href{mailto:markus.kainu@kapsi.fi}{markus.kainu@kapsi.fi}, Pyry Kantanen
}
