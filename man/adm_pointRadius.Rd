\name{geo_adm}

\alias{geo_adm}


\title{
Find coordinates and uncertainty for adminsitrative areas
}

\description{
Find coordinates and uncertainty for adminsitrative areas
}


\usage{
adm_pointRadius(country, adm, cache_path=NULL, method="high_probability")
}
 
\arguments{
  \item{country}{country name or country ISO code, or a SpatVector of polygons}
  \item{adm}{positive integer to indicated admin level}
  \item{cache_path}{path to pass the geodata::gadm to cache the gadm data. Use NULL to set a default path}
  \item{method}{character. "high_probability" or "low_uncertainty"}
}

\value{
data.frame
}


\examples{
v <- terra::vect(system.file("ex/lux.shp", package="terra"))[, c("NAME_1", "NAME_2")]
adm_pointRadius(v, method="high_probability")

}