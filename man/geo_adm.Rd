\name{geo_adm}

\alias{geo_adm}


\title{
Find coordinates and uncertainty for adminsitrative areas
}

\description{
Find coordinates and uncertainty for adminsitrative areas
}


\usage{
geo_adm(country, adm, cache_path=NULL)
}
 
\arguments{
  \item{country}{country name. Can be ISO code}
  \item{adm}{positive integer to indicated admin level}
  \item{path}{path to cache the gadm data. Use NULL to set a default path}
}

\value{
data.frame
}


