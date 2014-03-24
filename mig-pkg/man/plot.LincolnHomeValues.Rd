\name{plot.LincolnHomeValues}
\alias{plot.LincolnHomeValues}
\title{Plot different Lincoln Inst. House Values}
\usage{
  plot.LincolnHomeValues(dat,
    saveto = "~/Dropbox/mobility/output/model/factor")
}
\description{
  Plot different Lincoln Inst. House Values
}
\examples{
data(HomeValues,package="EconData")
dd <- HomeValues[State \%in\% sample(unique(State),size=20)]
plot.LincolnHomeValues(dd)
}

