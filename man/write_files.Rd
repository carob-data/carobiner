\name{write_files}

\alias{write_files}

\title{
Write Carob dataset files
}

\description{
Write Carob dataset files. This calls \code{\link{check_terms}}
}


\usage{
write_files(path=NULL, metadata, wide, long=NULL, wth=NULL, var_meta=NULL, options=NULL)
}
 
\arguments{
  \item{path}{character. \code{NULL}, or the path to the local Carob repo}
  \item{metadata}{data.frame. One row with dataset level descriptive variables}
  \item{wide}{data.frame. Dataset with records in the "wide" format (one row for each observational unit)}
  \item{long}{data.frame. Dataset with records in the "long" format to capture multiple observations over time or depth for a single variable and observational unit}
  \item{wth}{data.frame with weather data}
  \item{var_meta}{data.frame. Metadata associated with speficic variables (e.g. extraction method used in chemical soil analysis)}
  \item{options}{list}
}

\value{
if \code{path=NULL} a list of data.frames (metadata and records). Otherwise a logical value indicating if the standardized dataset was succesfully written to the Carob repo.
}
