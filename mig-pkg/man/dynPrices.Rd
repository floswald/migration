\name{dynPrices}
\alias{dynPrices}
\title{MARSS Dynamic Factor Model}
\usage{
  dynPrices(dat, facs, maxite = 50,
    detrend = formula(y ~ state * date), demean = TRUE,
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
data(HValue96_dynF_quarterly)
dd <- copy(dat[state \%in\% sample(unique(state),size=5)])
l <- dynPrices(dat=dd,facs=3,maxite=300)
sim <- MARSSsimulate(l$marss,tSteps=20,nsim=1)
#matplot(t(sim$sim.states[ , , 1]),type="l",main="simulated hidden factors")
#matplot(t(sim$sim.data[ , , 1]),type="l",main="simulated outcomes")
}

