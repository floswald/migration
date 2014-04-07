\name{InitBBLData}
\alias{InitBBLData}
\title{Initiate a BBL simulation dataset}
\usage{
  InitBBLData(logi, BBLpars,
    saveto = "~/Dropbox/mobility/output/model/BBL/BBLSimData.RData")
}
\description{
  builds the initial BBL dataset. Selects a random sample
  from SIPP data at it's first observation in the data.
}
\examples{
load("~/Dropbox/mobility/SIPP/Sipp_aggby_age.RData")
BBL <- list(maxAge=60,FE=TRUE,n=0.1)
l <- InitBBLData(merged,RE.coefs,BBLpars=BBL)
}

