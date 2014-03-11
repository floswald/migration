\name{runMNLogit}
\alias{runMNLogit}
\title{Run multinomial logit model of location choice}
\usage{
  runMNLogit(d,
    saveto = "~/Dropbox/mobility/output/model/BBL/logit30Res.RData")
}
\description{
  Run multinomial logit model of location choice
}
\examples{
if (Sys.info()["user"] == "florianoswald" ){
load("~/Dropbox/mobility/output/model/BBL/logit30.RData")
} else {
load("C:/Users/florian_o/Dropbox/mobility/output/model/BBL/logit30.RData")
}
res <- runMNLogit(d=l)
# plot(pr1[-grep("ID",colnames(pr1))])
}

