\name{plotDivDeviations}
\alias{plotDivDeviations}
\title{make plots of deviations models}
\usage{
  plotDivDeviations(d, path = NULL)
}
\description{
  make plots of deviations models
}
\examples{
d <- buildDivDeviations(n=20,N=3)
plotDivDeviations(d)
d <- buildDivDeviations(n=20,N=3)

## example 2
agg <- makeAggPricesFromData(N=3)
a = buildAggregatePrices(agg)
plotDivDeviations(a)
}
\seealso{
  Other ExpectationsModel:
  \code{\link{buildAggregatePrices}},
  \code{\link{buildDivDeviations}},
  \code{\link{estimateDivDeviations}},
  \code{\link{makeAggPricesFromData}},
  \code{\link{makeDivDifferences}},
  \code{\link{simDivDeviations}}
}

