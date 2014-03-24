\name{plot.FHFAindex}
\alias{plot.FHFAindex}
\title{Plot different FHFA house prices indices}
\usage{
  plot.FHFAindex(dat,
    saveto = "~/Dropbox/mobility/output/model/factor/fhfa.pdf")
}
\description{
  Plot different FHFA house prices indices
}
\examples{
data(FHFA_states,package="EconData")
dd <- FHFA.states$qtr[state \%in\% sample(unique(state),size=20)]
plot.FHFAindex(dd)
}

