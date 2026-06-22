\name{make_report}
\alias{make_report}

\title{
Write an HTML data report
}

\description{
Build a Carob HTML from data supplied in memory
}

\usage{
make_report(records, metadata = NULL, issues = NULL, group = "",
	path = NULL, file = NULL, check = "nogeo", open = FALSE)
}

\arguments{
  \item{records}{data.frame. Wide-format observation records.}
  \item{metadata}{optional data.frame with one row, or a list, with Carob metadata fields.}
  \item{issues}{optional data.frame with columns \code{check} and \code{msg} from
    \code{\link{check_terms}}. If missing, \code{check_terms} is run.}
  \item{group}{character. Group name for checks, evaluation, and script links.}
  \item{path}{character. Directory for the HTML file. Defaults to \code{tempdir()}.}
  \item{file}{character. Output file name.}
  \item{check}{character. Check level passed to \code{\link{check_terms}} when
    \code{issues} is not supplied.}
  \item{open}{logical. Open the report in a browser?}
}

\value{
The path to the written HTML file (invisibly).
}

\seealso{
\code{\link{check_terms}}
}

\examples{
\dontrun{
records <- utils::read.csv(system.file("ex/records.csv", package = "carobiner"))
make_report(records, open = TRUE)
}
}
