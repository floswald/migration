\name{plotNormalization}
\alias{plotNormalization}
\title{Plot normalization of Prices}
\usage{
  plotNormalization(d, mod,
    saveto = "~/Dropbox/mobility/output/model/factor")
}
\description{
  Plot normalization of Prices
}
\examples{
data(HValue96_dynF_quarterly)
m <- makeTimeMatrix(dat)
plotNormalization(d=dat,mod=m$detmod,saveto=NULL)
}

