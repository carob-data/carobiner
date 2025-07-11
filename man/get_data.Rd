\name{get_data}

\alias{get_data}

\title{
Download a data set
}

\description{
Download the files associated with a URI. 
}


\usage{
get_data(uri, path, group, files=NULL, cache=TRUE, recursive=FALSE, filter=TRUE, protocol="", username, password)
}
 
\arguments{
  \item{uri}{character. The URI of a dataset}
  \item{path}{character. The path to the Carob repo}
  \item{group}{character. group name}
  
  \item{files}{character. If not \code{NULL} these files are copied to the raw data folder; and \code{uri} is not used to get the data. This can be useful for datasets that do not have a URI or are not hosted on a website with a standard protocol for data retrieval such as dataverse or CKAN. It can also be useful to process local files before they are published. Files can be either *all* have a http address, or *all* on the local file system}
  \item{cache}{logical. if \code{TRUE} the data are only downloaded again if they are not locally available}
  \item{recursive}{logical. if \code{TRUE} also return filenames in sub-directories}
  \item{filter}{logical. if \code{TRUE} files that probably do not have the primary data are filtered out (files ending on .pdf, .doc(x), .json)}
 
  \item{protocol}{character}
  \item{username}{character}
  \item{password}{character}
}

\value{
character
}
