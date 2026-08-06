\name{replace_values}

\alias{replace_values}

\title{
Replace values in a vector
}

\description{
Replace selected values in a vector. You can pass parallel \code{from}/\code{to} vectors, or a single named vector as \code{from} (with \code{to = NULL}) whose names are matched and replaced by the corresponding values.
}

\usage{
replace_values(x, from, to=NULL, must_have=TRUE)
}
 
\arguments{
  \item{x}{vector}
  \item{from}{values to be changed; or a named vector mapping old values (names) to new values when \code{to} is \code{NULL}}
  \item{to}{replacement values aligned with \code{from}; if \code{NULL}, \code{from} must be a named vector and its names/values are used as from/to}
  \item{must_have}{logical. Give an error if a value in \code{from} is not in \code{x}}
}

\value{
vector
}


\examples{
v <- LETTERS[1:5]
replace_values(v, c("A", "B", "C"), c("first", "abc", "third"))

# named map: only listed values are changed; others are left as-is
crop_map <- c(forages = "forage crop", commonbean = "common bean")
replace_values(c("maize", "forages", "commonbean"), crop_map)
}


