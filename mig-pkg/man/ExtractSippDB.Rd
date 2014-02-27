\name{ExtractSippDB}
\alias{ExtractSippDB}
\title{Extract data.tables from SIPP database}
\usage{
  ExtractSippDB(dbfile, ck, which.core, which.tm, tk,
    subset = "", outfile)
}
\arguments{
  \item{dbfile}{location of database}

  \item{ck}{string of variable names from core data to
  keep}

  \item{which.core}{numeric vector of which core waves to
  keep}

  \item{which.tm}{numeric vector of which topical modules
  to keep}

  \item{which.wgt}{name of weight tables}

  \item{tk}{string of variable names from topical data to
  keep}

  \item{subset}{SQL string for selecting from database}

  \item{outfile}{filename of where to save results}
}
\description{
  Select variables and build a data.tables from the SIPP
  database, downloaded and built with ajdamico's usgsd
  tools. selects ALL waves from coredata, but only selected
  waves from topical modules.
}
\references{
  \url{https://github.com/ajdamico/usgsd}
}

