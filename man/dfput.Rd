\name{dfput}
\alias{dfput}

\title{Print a data.frame as copy-pasteable R code}

\description{
Format a data.frame as a \code{data.frame(...)} call with column vectors, for pasting into Carob scripts. Unlike \code{dput}, the output omits \code{structure()}, \code{row.names}, and \code{class} and uses readable column-wise \code{c(...)} vectors.
}

\usage{
dfput(x, name=NULL, digits = NULL, indent = 4, drop=NULL)
}

\arguments{
  \item{x}{data.frame}
  \item{name}{optional object name; if set, prints \code{name <- data.frame(...)}}
  \item{digits}{optional number of digits for rounding numeric columns}
  \item{indent}{number of spaces before each column line inside \code{data.frame}}
  \item{drop}{character vector of column names to omit}
}

\value{
The formatted code string (invisibly); also printed for interactive use.
}

\examples{
xy <- data.frame(
  adm1 = c("Dodoma", "Manyara"),
  adm2 = c("Kongwa", "Kiteto"),
  longitude = c(36.6104, 36.8266),
  latitude = c(-5.9767, -5.1963),
  geo_undertainty = c(56168, 84918)
)

dfput(xy, name = "geo")
}
