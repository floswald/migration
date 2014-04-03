\name{buildBBLData}
\alias{buildBBLData}
\title{Make a BBL simulation dataset}
\usage{
  buildBBLData(logi, RE.coefs, BBLpars,
    saveto = "~/Dropbox/mobility/output/model/BBL/BBLSimData.RData")
}
\arguments{
  \item{varlist}{character vector of variables to take from
  full data}
}
\description{
  builds the initial BBL dataset
}
\examples{
load("~/Dropbox/mobility/SIPP/Sipp_aggby_age.RData")
load("~/Dropbox/mobility/output/model/BBL/income-REcoefs.RData")
BBL <- list(maxAge=60,FE=TRUE,n=0.1)
l <- buildBBLData(merged,RE.coefs,BBLpars=BBL)
}

