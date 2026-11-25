\name{parse_dates}

\alias{parse_dates}

\title{
Parse mixed-format spreadsheet dates to ISO character strings
}

\description{
Parse a vector containing mixed date representations commonly produced by Excel workbooks (Excel serial numbers, Unix timestamps in seconds, ISO dates and datetimes, \code{Date}/\code{POSIXt} objects, named-month dates in English, Spanish, or French, and numeric \code{d/m/y} or \code{m/d/y} strings) into standardized \code{"YYYY-MM-DD"} character strings without emitting coercion warnings.
}

\usage{
parse_dates(x, order = c("auto", "dmy", "mdy", "ymd"),
	ambiguous = c("na", "dmy", "mdy"),
	min_year = 1950L, max_year = 2035L,
	origin = "1899-12-30", partial = FALSE,
	na = c("", "-", "\u2013", "NA", "N/A", "n/a", "#N/A", ".", "NULL"))
}

\arguments{
  \item{x}{character, numeric, \code{Date}, or \code{POSIXt} vector of dates}
  \item{order}{character. Component order for numeric 3-part dates such as \code{"05/06/2018"}. With \code{"auto"} (the default), the function inspects all numeric 3-part dates in \code{x} and infers \code{"dmy"} if any first component exceeds 12 (and no second exceeds 12) or \code{"mdy"} if any second component exceeds 12 (and no first exceeds 12)}
  \item{ambiguous}{character. How to handle a numeric 3-part date when \code{order = "auto"} cannot resolve day/month order across \code{x} (for example, all values have both day and month \code{<= 12} and unequal): \code{"na"} (default, return \code{NA}), \code{"dmy"}, or \code{"mdy"}}
  \item{min_year}{integer. Minimum accepted year; dates outside \code{[min_year, max_year]} return \code{NA}}
  \item{max_year}{integer. Maximum accepted year}
  \item{origin}{character. Origin for Excel date serial numbers (default \code{"1899-12-30"})}
  \item{partial}{logical. If \code{TRUE}, also preserve valid \code{"YYYY"} and \code{"YYYY-MM"} strings}
  \item{na}{character vector of strings treated as missing values}
}

\value{
character vector of ISO dates (\code{"YYYY-MM-DD"}, or \code{"YYYY"} / \code{"YYYY-MM"} when \code{partial = TRUE}), with \code{NA} for missing or unparseable entries.
}

\examples{
parse_dates(c("41780", "2018-06-15", "15-Aug-18", "15/ago/2017", "25/06/2018", "05/06/2018"))
parse_dates(c("06/05/2018", "06/25/2018"))
parse_dates("06/11/2022", order = "dmy")
parse_dates("06/11/2022", order = "auto")
}
