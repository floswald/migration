\name{Extract.wrap}
\alias{Extract.wrap}
\title{Extractor wrapper}
\usage{
  Extract.wrap(verbose = TRUE,
    which = paste0(c(1996, 2001, 2004, 2008)),
    dropbox = "C:/Users/florian_o/Dropbox/mobility/SIPP")
}
\arguments{
  \item{verbose}{}

  \item{dropbox}{path to folder where to save this}

  \item{which}{names of datasets to extract. Names are:
  "1996", "2001", "2004", "2008", "Mig_2008"}
}
\value{
  NULL saves subset data.tables into dropbox
}
\description{
  Selects variables from SIPP database and does some
  initial subsetting. This is an interface to
  \code{\link{ExtractorSippDB}}
}

