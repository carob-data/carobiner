\name{dms_to_dd}

\alias{dms_to_dd}

\title{
Convert degrees-minutes-seconds (DMS) coordinates to decimal degrees
}

\description{
Convert coordinates in degrees, minutes, and seconds to signed decimal degrees. Accepts either separate component vectors (\code{d}, \code{m}, \code{s}, \code{hem}) or a single character vector of DMS strings (e.g. \code{"14\u00b0 22' 30\" S"}). Validates hemisphere axis consistency and requires an explicit \code{default_hem} when hemisphere values are missing.
}

\usage{
dms_to_dd(d, m=0, s=0, hem=NULL, default_hem=NULL)
}

\arguments{
  \item{d}{numeric vector of degrees, or character vector of DMS strings (when \code{m}, \code{s}, and \code{hem} are omitted)}
  \item{m}{numeric (or character) vector of minutes (default \code{0})}
  \item{s}{numeric (or character) vector of seconds (default \code{0})}
  \item{hem}{optional character vector of hemispheres (\code{"N"}, \code{"S"}, \code{"E"}, \code{"W"})}
  \item{default_hem}{optional single character (\code{"N"}, \code{"S"}, \code{"E"}, or \code{"W"}) used when \code{hem} is missing/empty}
}

\value{
numeric vector of signed decimal degrees (negative for \code{"S"} and \code{"W"}).
}

\examples{
dms_to_dd(14, 22, 30, hem = "S")
dms_to_dd(c(34, 34), c(12, 15), c(0, 30), hem = c("E", NA), default_hem = "E")
dms_to_dd(c("14 22 30 S", "14 25 00"), default_hem = "S")
}
