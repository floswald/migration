\name{buildAggregatePrices}
\alias{buildAggregatePrices}
\title{Build aggregate price series}
\usage{
  buildAggregatePrices(agg)
}
\arguments{
  \item{agg}{data.frame with columns \code{price} and
  \code{income}}
}
\description{
  takes two series for log income and log house values at
  national level and constructs census division prices and
  income from simulations
}
\examples{
a <- makeAggPricesFromData(N=3)
buildAggregatePrices(agg=a)
}
\seealso{
  Other ExpectationsModel:
  \code{\link{buildDivDeviations}},
  \code{\link{estimateDivDeviations}},
  \code{\link{makeDivDifferences}},
  \code{\link{plotDivDeviations}},
  \code{\link{simDivDeviations}}
}

