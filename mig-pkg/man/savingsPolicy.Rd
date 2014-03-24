\name{savingsPolicy}
\alias{savingsPolicy}
\title{Savings policy function}
\usage{
  savingsPolicy(d, quants = NULL,
    path = "~/Dropbox/mobility/output/model/BBL",
    plot = FALSE)
}
\arguments{
  \item{d}{dataset}

  \item{quants}{at which quantiles of savings to run
  quantile regression (default NULL)}

  \item{path}{where to save results}

  \item{plot}{if to plot mean/median savings}
}
\description{
  Savings policy function
}
\examples{
load("~/Dropbox/mobility/SIPP/Sipp4mn.RData")
s <- savingsPolicy(d=merged4mn,quants=0.5)
}
\seealso{
  Other FirstStage: \code{\link{housingModel}}
}

