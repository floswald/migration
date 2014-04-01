\name{simMovesFromLogit}
\alias{simMovesFromLogit}
\title{simulate number of moves from mnlogit result}
\usage{
  simMovesFromLogit(res, newdata = NULL, GOF = FALSE,
    print.to = "~/Dropbox/mobility/output/model/BBL/")
}
\arguments{
  \item{res}{mnlogit result}

  \item{print.to}{location to print table}
}
\description{
  simulate number of moves from mnlogit result
}
\seealso{
  Other LogitModel: \code{\link{DTSetChoice}},
  \code{\link{buildLogit}}, \code{\link{mergeHomeValues}},
  \code{\link{mergePredIncomeMovingHist}},
  \code{\link{printLogitModel}}
}

