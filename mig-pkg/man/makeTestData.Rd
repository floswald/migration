\name{makeTestData}
\alias{makeTestData}
\title{makes a Test Dataset of Individuals for income prediction}
\usage{
  makeTestData(n, A, nm)
}
\arguments{
  \item{n}{number of inds}

  \item{A}{number of maximal years in panel}

  \item{nm}{number of movers}
}
\description{
  makes a Test Dataset of Individuals for income prediction
}
\examples{
tt <- makeTestData(10,3,1)
}
\seealso{
  Other testFunctions: \code{\link{makeTestREcoefs}}
}

