\name{on_github}

\alias{on_github}

\title{
What datasets have been processed?
}

\description{
Get a data.frame with the datasets that have been processed and are on the reagro/carob github site
}


\usage{
on_github(uri=NULL)
}
 
\arguments{
  \item{uri}{character. One or more URIs (or script basenames) to look up.
  If \code{NULL}, all scripts are returned.}
}

\value{
data.frame with columns:
\itemize{
  \item{\code{uri}: clean identifier (see \code{\link[yuri]{simpleURI}})}
  \item{\code{file}: script path under the repository
    (e.g. \code{scripts/agronomy/doi_10.21223_HMMWZH.R}), or empty if not found}
  \item{\code{note}: \code{""}, \code{"to be completed"}, \code{"REJECTED"},
    \code{"not found"}, or \code{"not a valid URI"}}
}

When \code{uri} is supplied, one row is returned per input value (order and
duplicates are preserved).
}


\examples{
\dontrun{
all <- on_github()

test <- on_github(c("doi:10.21223/HMMWZH", "doi:10.7910/DVN/NDL2T3",
                    "doi_10.7910_DVN_NDL2T3.R"))
}
}
