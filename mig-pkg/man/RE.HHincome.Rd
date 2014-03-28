\name{RE.HHincome}
\alias{RE.HHincome}
\title{Linear Random Effects Panel Model of Income}
\usage{
  RE.HHincome(dat,
    path = "~/Dropbox/mobility/output/model/BBL")
}
\arguments{
  \item{dat}{data set of income relevant variables}
}
\value{
  list for each state with coefficients and fixed effects
  for each individual. saves data.
}
\description{
  estimate a linear panel of incoem with AR(1) error term
  fit a model similar to the one in Baltagi (2005, 5.2.1).
}
\details{
  model is for income in location j is \itemize{ \item
  log(y_ijt) = beta_j + gamma'X_it + mu_i + v_it, where
  \itemize{ \item y_ijt is log income of HH i in MONTH t in
  location j \item v_it = rho v_it-1 + eps_it \item eps ~
  iid N(0,sigma_eps), \item mu ~ iid N(0,sigma_mu), \item
  mu independent of v_it. v_i0 ~ N(0,sigma_eps^2 /
  (1-rho^2)) \item The random effects assumption is
  incorporated by assuming that \item mu_i is iid
  (uncorrelated with X). }}
}
\examples{
load("~/Dropbox/mobility/SIPP/Sipp_aggby_NULL.RData")
l <- RE.HHincome(dat=merged[state \%in\% c("AZ","AL","AR")],path="~/Dropbox/mobility/output/model/BBL",type="html")
}
\seealso{
  Other IncomePrediction: \code{\link{getHomeValues}},
  \code{\link{getIncomeM}}, \code{\link{getREintercept}},
  \code{\link{makePrediction1}},
  \code{\link{makePrediction2}}
}

