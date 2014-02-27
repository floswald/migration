\name{buildLogit}
\alias{buildLogit}
\title{Make a MN Logit estimation datset}
\usage{
  buildLogit(logi, RE.coefs, with.FE = FALSE,
    verbose = TRUE,
    saveto = "~/Dropbox/mobility/output/model/BBL/logit.RData")
}
\arguments{
  \item{income}{data.table from \code{\link{subset.all}}}

  \item{RE.coefs}{regression coefs from
  \code{\link{RE.HHincome}}}

  \item{verbose}{TRUE/FALSE}

  \item{saveto}{if not NULL, where to save}

  \item{with.FE}{TRUE if you want to make prediction in
  state k with individual fixed effect estimated for state
  j.}
}
\value{
  data.table with predicted incomes
}
\description{
  takes output of \code{\link{RE.HHincome}} and predicts
  income in all locations. same for house prices
}
\details{
  We compute the the linear predictor of the model in
  \code{\link{RE.HHincome}}, i.e. shocks are irrelevant.
}
\examples{
load("~/Dropbox/mobility/SIPP/prelogit.RData")
load("~/Dropbox/mobility/output/model/BBL/income-REcoefs.RData")
l <- buildLogit(prelogit,RE.coefs)
}

