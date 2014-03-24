\name{makePrediction1}
\alias{makePrediction1}
\title{income prediciton level 1}
\usage{
  makePrediction1(y, RE.coefs, with.FE, State_dist)
}
\description{
  merge random effects id-specific intercept and adjust as
  deviation from state s population intercept for each
  upid,age combination, take the guys random effect, find
  difference from region j's intercept, predict population
  income in k, and finally add the personal effect. format
  ====== logit model requires long data. strategy: take
  copy of 'income' by state to get id age state y 1 29 WI
  1000 1 30 WI 1030 1 31 WI 1100 ...
}
\seealso{
  Other IncomePrediction: \code{\link{RE.HHincome}},
  \code{\link{getHomeValues}}, \code{\link{getIncomeM}},
  \code{\link{getREintercept}},
  \code{\link{makePrediction2}}
}

