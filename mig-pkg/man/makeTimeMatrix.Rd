\name{makeTimeMatrix}
\alias{makeTimeMatrix}
\title{construct a numeric matrix with time varying across columns}
\usage{
  makeTimeMatrix(dat, trans)
}
\arguments{
  \item{dat}{data.frame in long format with columns y,
  quarter and state}

  \item{trans}{transformation}
}
\value{
  list with detrended data matrix and data.table with means
  and sds
}
\description{
  construct a numeric matrix with time varying across
  columns
}

