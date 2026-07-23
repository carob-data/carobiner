\name{adm_pointRadius}

\alias{adm_pointRadius}


\title{
Find coordinates and uncertainty for administrative area
}

\description{
Find coordinates and uncertainty for administrative areas
}


\usage{
adm_pointRadius(country, adm, cache_path=NULL, method="high_probability", geo=NULL)
}
 
\arguments{
  \item{country}{country name or country ISO code, or a SpatVector of polygons}
  \item{adm}{positive integer to indicated admin level}
  \item{cache_path}{path to pass the geodata::gadm to cache the gadm data. Use NULL to set a default path}
  \item{method}{character. "high_probability" or "low_uncertainty"}
  \item{geo}{data.frame to compute the uncertainty based on pre-defined coordinates instead of adminstrative area centroids. The data.frame must have variables "longitude", "latitude" and "adm_" (replace underscore to match argument \code{adm}) with names that match the GADM admistrative area names. The locations should be inside the polygons with the matching names}
}

\value{
data.frame
}


\examples{
v <- terra::vect(system.file("ex/lux.shp", package="terra"))[, c("NAME_1", "NAME_2")]
a <- adm_pointRadius(v, method="high_probability")
dfput(a[1:3, ])
} 