\name{Clean.Sipp}
\alias{Clean.Sipp}
\title{Clean Sipp Data}
\usage{
  Clean.Sipp(path = "~/Dropbox/mobility/SIPP",
    TM.idx = list(p96 = c(3, 6, 9, 12), p01 = c(3, 6, 9), p04 = c(3, 6), p08 = c(4, 7, 10)),
    agg.by = "age", use.hvalue.for.p2y = TRUE,
    verbose = TRUE)
}
\arguments{
  \item{agg.by}{list of variable names by which to
  aggregate. presumably those should be time variables like
  qtr, year, age etc}

  \item{TM.idx}{list with one index vector of TM waves to
  use per panel. Name list elements like "p96" [panel 96]}

  \item{path}{to output from \code{\link{Extract.wrap}}
  events in srefmon 4 are representative of the previous
  three months.}
}
\value{
  NULL. Saves 2 data.tables to dropbox.
}
\description{
  take output from \code{\link{Extract.wrap}} and clean
  data. apply labels, account for missing vars. merge
  topical and core data. output two datasets, differing in
  time resolution (monthly or 4-monthly).
}
\details{
  Data is cleaned for inconsistencies across SIPP panels
  1996-2008, merged with house price indices by state, and
  dollar denoted variables are deflated to 1996 as a base
  year using the US cpi. All dollar values are denoted in
  1000s of US dollars. The SIPP can be cast at different
  time resolutions, i.e. you can look at monthly data
  quarterly data, annual, etc. This function outputs
  monthly data and a dataset subset to the fourth (i.e.
  last) reference month of each wave.
}

