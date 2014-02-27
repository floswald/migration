\name{ExtractorSippDB}
\alias{ExtractorSippDB}
\title{Extract data.tables from SIPP database}
\usage{
  ExtractorSippDB(dbfile, ck, which.core, which.tm,
    which.wgt, tk, subset = "", outfile, verbose,
    test = FALSE)
}
\arguments{
  \item{dbfile}{location of database}

  \item{ck}{string of variable names from core data to
  keep}

  \item{which.core}{numeric vector of which core waves to
  keep}

  \item{which.tm}{numeric vector of which topical modules
  to keep}

  \item{which.wgt}{character vector of name of weight
  tables}

  \item{tk}{list of character vectors of variable names
  from topical data to keep, one vector for each topical
  module}

  \item{subset}{SQL string for selecting from database}

  \item{outfile}{filename of where to save results}

  \item{test}{if TRUE extract only a short test dataset}
}
\description{
  Select variables and build a data.tables from the SIPP
  database, downloaded and built with anthony damico's
  usgsd tools. selects ALL waves from coredata, but only
  selected waves from topical modules.
}
\details{
  don't use this function but the easier to use
  \code{\link{Extract.wrap}}
}
\references{
  \url{https://github.com/ajdamico/usgsd},
  \url{http://www.asdfree.com/},
}

