\name{buildLogit}
\alias{buildLogit}
\title{Make a MN Logit estimation datset}
\usage{
  buildLogit(logi, RE.coefs, with.FE = FALSE,
    verbose = TRUE,
    saveto = "~/Dropbox/mobility/output/model/BBL/logit.RData",
    savetosmall = "~/Dropbox/mobility/output/model/BBL/logit30.RData",
    movreg)
}
\arguments{
  \item{income}{data.table from \code{\link{subset.all}}}

  \item{RE.coefs}{regression coefs from
  \code{\link{RE.HHincome}}}

  \item{verbose}{TRUE/FALSE}

  \item{saveto}{if not NULL, where to save}

  \item{saveto}{if not NULL, where to save small data}

  \item{movreg}{data.table with movers register. this is a
  data.table as produced by \code{\link{Sipp.Sumstats}},
  with columns \code{from,to,upid,age} at the minium.}

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
load("~/Dropbox/mobility/output/data/sipp/sumstats.RData")
mvt <- sumstats$mvtab
mvt[,c("wave","mid","from") := NULL]
l <- buildLogit(prelogit,RE.coefs,movreg=mvt)
}
\seealso{
  Other IncomePrediction: \code{\link{RE.HHincome}},
  \code{\link{getHomeValues}}, \code{\link{getIncomeM}},
  \code{\link{getREintercept}},
  \code{\link{makePrediction1}},
  \code{\link{makePrediction2}}
}

