\name{Extract.wrap}
\alias{Extract.wrap}
\title{Extractor wrapper}
\usage{
  Extract.wrap(verbose = TRUE,
    dropbox = "C:/Users/florian_o/Dropbox/mobility/SIPP")
}
\arguments{
  \item{verbose}{}

  \item{dropbox}{path to folder where to save this}
}
\value{
  NULL saves subset data.tables into dropbox
}
\description{
  Selects variables from SIPP database and does some
  initial subsetting. This is an interface to
  \code{\link{ExtractorSippDB}}
}

