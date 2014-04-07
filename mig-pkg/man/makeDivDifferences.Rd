\name{makeDivDifferences}
\alias{makeDivDifferences}
\title{log differences between US average and location prices from Data}
\usage{
  makeDivDifferences(path = NULL)
}
\description{
  create dataset with of log differences between US average
  and location prices from census median income and Lincoln
  house values in 96 dollars
}
\examples{
data(US_medinc,package="EconData")
makeHPIDivDifferences()a
}
\seealso{
  Other ExpectationsModel:
  \code{\link{buildAggregatePrices}},
  \code{\link{buildDivDeviations}},
  \code{\link{estimateDivDeviations}},
  \code{\link{makeAggPricesFromData}},
  \code{\link{plotSimDivDeviations}},
  \code{\link{simDivDeviations}}
}

