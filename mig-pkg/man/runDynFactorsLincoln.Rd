\name{runDynFactorsLincoln}
\alias{runDynFactorsLincoln}
\title{Dynamic Factor model for Lincoln Home Values}
\usage{
  runDynFactorsLincoln()
}
\description{
  Dynamic Factor model for Lincoln Home Values
}
\examples{
data(HValue96_dynF_quarterly)
m <- dynPrices(dat=dat,facs=3,maxite=300,demean=TRUE,detrend=TRUE,meth="BFGS")
}

