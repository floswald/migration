\name{Sipp.SumStats}
\alias{Sipp.SumStats}
\title{Summary Statistics from Sipp}
\usage{
  Sipp.SumStats(d,
    saveto = "~/Dropbox/mobility/output/data/sipp/sumstats.RData")
}
\arguments{
  \item{save}{file to save}
}
\description{
  Summary Statistics from Sipp
}
\examples{
load("~/Dropbox/mobility/SIPP/SippFull.RData")
z = Sipp.SumStats(merged,NULL)
}

