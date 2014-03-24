\name{dynPrices}
\alias{dynPrices}
\title{MARSS Dynamic Factor Model}
\usage{
  dynPrices(dat, facs, maxite = 50, trans = 1,
    meth = "BFGS")
}
\arguments{
  \item{dat}{dataset}

  \item{trans}{integer which transformation}
}
\description{
  estimates a dynamic factor model
}
\examples{
data(HValue96_dynF_yearly)
dd <- copy(dat)
l <- dynPrices(dat=dd,facs=2,maxite=5000,trans="pct.increase")
sim <- MARSSsimulate(l$marss,tSteps=20,nsim=1)
#matplot(t(sim$sim.states[ , , 1]),type="l",main="simulated hidden factors")
#matplot(t(sim$sim.data[ , , 1]),type="l",main="simulated outcomes")
}

