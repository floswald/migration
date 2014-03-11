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
data(HomeValues,package="EconData")
dat <- HomeValues
data(CPIHOSSL,package="EconData")
cpi.h <- xts::to.quarterly(CPIHOSSL)[,1]
coredata(cpi.h) <- coredata(cpi.h) / as.numeric(cpi.h['1996-01-01'])
names(cpi.h) <- "cpiH"
cpi <- data.table(qtr=index(cpi.h),cpiH=coredata(cpi.h),key="qtr")
setnames(cpi,c("qtr","cpiH"))
setkey(cpi,qtr)
setkey(dat,qtr)
dat <- cpi[dat]
dat[,HValue96 := Home.Value / cpiH ]
setnames(dat,c("qtr","HValue96","State"),c("quarter","y","state"))
m <- dynPrices(dat=dat,facs=3,maxite=300,demean=TRUE,detrend=TRUE,meth="BFGS")
}

