\name{plot.predict.income}
\alias{plot.predict.income}
\title{Diagnostic Plot for Income Prediction}
\usage{
  plot.predict.income(n, l)
}
\arguments{
  \item{n}{number of randomly drawn individuals}

  \item{l}{output of income prediction}
}
\value{
  a ggplot
}
\description{
  plots incomes as predicted by
  \code{\link{predict.income}}
}
\examples{
load("~/Dropbox/mobility/output/model/BBL/predIncome.RData")
p <- plot.predict.income(n=4,l)
}

