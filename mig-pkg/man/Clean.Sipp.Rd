\name{Clean.Sipp}
\alias{Clean.Sipp}
\title{Clean Sipp Data}
\usage{
  Clean.Sipp(path = "~/Dropbox/mobility/SIPP",
    TM.idx = list(p96 = c(3, 6, 9, 12), p01 = c(3, 6, 9), p04 = c(3, 6), p08 = c(4, 7, 10)),
    verbose = TRUE, subset.4months = TRUE)
}
\arguments{
  \item{TM.idx}{list with one index vector of TM waves to
  use per panel. Name list elements like "p96" [panel 96]}

  \item{path}{to output from \code{\link{Extract.wrap}}}

  \item{subset.4months}{TRUE if want to subset monthly data
  to only the final reference month (srefmon 4). the
  assumption is events in srefmon 4 are representative of
  the previous three months.}
}
\value{
  NULL. Saves 2 data.tables to dropbox.
}
\description{
  take output from \code{\link{Extract.wrap}} and clean
  data. apply labels, account for missing vars. merge
  topical and core data. output two datasets, differing in
  time resolution (monthly of 4-monthly).
}
\details{
  Data is cleaned for inconsistencies across SIPP panels
  1996-2008, merged with house price indices by state, and
  dollar denoted variables are deflated to 1996 as a base
  year using the US cpi. House price indices are normalized
  to to the state with lowest index value in 1995:Q4
}

