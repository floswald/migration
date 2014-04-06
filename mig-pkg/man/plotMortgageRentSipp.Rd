\name{plotMortgageRentSipp}
\alias{plotMortgageRentSipp}
\title{plot monthly rent and mortgage by state}
\usage{
  plotMortgageRentSipp(sipp,
    path = "~/Dropbox/mobility/output/data/sipp/")
}
\description{
  create a errorbar plot marking both mean mortgage payment
  as well as mortgage payment from my sipp sample
}
\examples{
load("~/Dropbox/mobility/SIPP/Sipp_aggby_age.RData")
plotMortgageRentSipp(sipp=merged)
}

