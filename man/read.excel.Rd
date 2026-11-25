\name{read.excel}

\alias{read.excel}
\alias{read.biff2}

\title{
Read an excel file
}

\description{
Read an excel file and return a data.frame without generating warnings. \code{read.excel} falls back to \code{read.biff2} for legacy Excel 2.1 (BIFF2) \code{.xls} files that \code{\link[readxl]{read_excel}} cannot open.
}


\usage{
read.excel(f, fix_names=FALSE, lower=FALSE, ...)
read.biff2(f, col_names=TRUE, skip=0L, na="", fix_names=FALSE, lower=FALSE)
}
 
\arguments{
  \item{f}{character (filename)}
  \item{col_names}{logical. If \code{TRUE}, the first non-skipped row is used as column names}
  \item{skip}{integer. Number of leading rows to skip before reading data}
  \item{na}{character vector of strings to interpret as missing values}
  \item{fix_names}{logical. If \code{TRUE} variable names are made valid. Spaces and other characters such as parenthesis are replaced by a dot. Multiple dots are replaced by a single dot}
  \item{lower}{logical. If \code{TRUE}, returned column names are all in lowercase}
  \item{...}{additional arguments passed on to \code{\link[readxl]{read_excel}}}.
}

\value{
data.frame
}

\seealso{
\code{\link{read.excel.hdr}}
}
