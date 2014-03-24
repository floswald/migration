\name{printLogitModel}
\alias{printLogitModel}
\title{print Logit Model Results}
\usage{
  printLogitModel(res, omit,
    path = "~/Dropbox/mobility/output/model/BBL")
}
\arguments{
  \item{res}{mnlogit result}

  \item{omit}{regex of coefficients to omit from latex
  table. html tables don't omit anything.}

  \item{path}{where to save tables}
}
\value{
  NULL
}
\description{
  print Logit Model Results
}
\seealso{
  Other LogitModel: \code{\link{DTSetChoice}},
  \code{\link{buildLogit}}, \code{\link{mergeHomeValues}},
  \code{\link{mergePredIncomeMovingHist}},
  \code{\link{simMovesFromLogit}}
}

