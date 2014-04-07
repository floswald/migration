\name{plotHousePrices}
\alias{plotHousePrices}
\title{Plot different house price index series}
\usage{
  plotHousePrices(d, response, FD = FALSE,
    saveto = "~/Dropbox/mobility/output/data/FHFA/")
}
\arguments{
  \item{d}{dataset. must contain columns Date, State and a
  response variable}

  \item{response}{string of response var name}

  \item{saveto}{location where to save plots}

  \item{FD}{wether to first difference the series}
}
\value{
  list of ggplots
}
\description{
  groups series by states, region and division and plots
}
\examples{
## FHFA indices
data(FHFA_states,package="EconData")
d <- copy(FHFA_states$qtr)
plotHousePrices(d=d,response="index_sa")
rm(d)
## Lincoln House values
data(HValue96_dynF_quarterly)
d <- copy(dat)
d[,date := as.Date(date)]
setnames(d,"y","HValue96")
plotHousePrices(d=d,response="HValue96",saveto="~/Dropbox/mobility/output/data/Lincoln/")
}

