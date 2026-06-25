\name{check_terms}

\alias{check_terms}

\title{
Check the terms uses in a dataset
}

\description{
Check the terms used in a dataset. A data.frame is returned that lists: unknown variables; required variables that are missing; character values that are not accepted; numeric variables that are out of the expected range; and variables with the wrong data type.
}

\usage{
check_terms(records=NULL, metadata=NULL, longrecs=NULL, wth=NULL, soil=NULL, group="", check="all")
}
 
\arguments{
  \item{records}{data.frame with observations in "wide" format}
  \item{metadata}{data.frame with metadata}
  \item{longrecs}{data.frame with "long" format observations, for when there are multiple observations in time or depth for a single observational unit}
  \item{wth}{data.frame with weather data}
  \item{soil}{data.frame with soil data}
  \item{group}{character. Optional group name}
  \item{check}{character. "none" to do no checking; "nogeo" to not check coordinates. "all" and all other values to do all checks}
}

\value{
data.frame
}

\examples{

records <- utils::read.csv(system.file("ex/records.csv", package = "carobiner"))
meta <- utils::read.csv(system.file("ex/metadata.csv", package = "carobiner"))

x <- check_terms(records, meta)
x

}
