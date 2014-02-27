\name{dynPrices}
\alias{dynPrices}
\title{MARSS Dynamic Factor Model for Prices}
\usage{
  dynPrices(dat, facs, maxite = 50, detrend = TRUE,
    demean = TRUE, meth = "kem")
}
\arguments{
  \item{dat}{dataset from
  data(FHFA_states,package="EconData")}

  \item{facs}{number of hidden factors}
}
\description{
  estimates a dynamic factor model
}
\examples{
data(FHFA_states,package="EconData")
dd <- states$qtr[state \%in\% sample(unique(state),size=20)]
l <- dynPrices(dat=dd,facs=3,maxite=300,demean=TRUE,detrend=TRUE,meth="BFGS")
sim <- MARSSsimulate(l,tSteps=20,nsim=1)
matplot(t(sim$sim.states[ , , 1]),type="l",main="simulated hidden factors")
matplot(t(sim$sim.data[ , , 1]),type="l",main="simulated outcomes")
}

