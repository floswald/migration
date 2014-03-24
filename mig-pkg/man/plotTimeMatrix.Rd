\name{plotTimeMatrix}
\alias{plotTimeMatrix}
\title{Plot factor model Time Matrix}
\usage{
  plotTimeMatrix(m, FD,
    saveto = "~/Dropbox/mobility/output/model/factor")
}
\description{
  Plot factor model Time Matrix
}
\examples{
data(HValue96_dynF_quarterly)
m <- makeTimeMatrix(dat)
p <- plotTimeMatrix(m,FD=FALSE)
}

