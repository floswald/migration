\name{housingModel}
\alias{housingModel}
\title{Housing Status Policy Function Model}
\usage{
  housingModel(d,
    path = "~/Dropbox/mobility/output/model/BBL",
    marginal = FALSE)
}
\arguments{
  \item{path}{}

  \item{marginal}{TRUE if marginal effects}
}
\description{
  estimates the reduced form for housing status changes
  form data
}
\examples{
load("~/Dropbox/mobility/SIPP/Sipp_aggby_age.RData")
h <- housingModel(d=merged)
}
\seealso{
  Other FirstStage: \code{\link{savingsPolicy}}
}

