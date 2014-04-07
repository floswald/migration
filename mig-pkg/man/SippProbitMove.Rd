\name{SippProbitMove}
\alias{SippProbitMove}
\title{SIPP probit of staying vs moving}
\usage{
  SippProbitMove(d, path = NULL)
}
\description{
  descriptive evidence
}
\examples{
load("~/Dropbox/mobility/SIPP/Sipp_aggby_age_svy.RData")
pr <- SippProbitMove(d=des,"~/Dropbox/mobility/output/data/sipp")

## goodness of fit

dat <- copy(des$variables)
dat[,probmove := predict(pr,type="response")]
dat[,pred.move := runif(n=nrow(dat))<probmove]
print(dat[,list(actual.moves=sum(S2S),predicted=sum(pred.move))])
}

