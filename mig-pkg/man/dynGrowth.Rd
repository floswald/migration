\name{dynGrowth}
\alias{dynGrowth}
\title{MARSS Dynamic Factor Model on growth by Division}
\usage{
  dynGrowth(dat, facs, maxite = 50, trans = 1,
    meth = "BFGS")
}
\arguments{
  \item{dat}{dataset}

  \item{detrend}{formula for detrending outcome var}

  \item{demean}{TRUE if want to do a z-transformation on
  detrended outcome}

  \item{meth}{method for maximizing the likelihood}

  \item{facs}{number of hidden factors}
}
\description{
  estimates a dynamic factor model
}
\examples{
data(HValue96_dynF_yearly)
div=dat[,list(HV=mean(y)),by=list(Division,date)]
setnames(div,c("state","date","y"))
d <- dynGrowth(div,facs=2,trans=1)
sim <- MARSSsimulate(d$marss,tSteps=20,nsim=10)
#matplot(t(sim$sim.states[ , , 1]),type="l",main="simulated hidden factors")
#matplot(t(sim$sim.data[ , , 1]),type="l",main="simulated outcomes")
}

