\name{makeTestREcoefs}
\alias{makeTestREcoefs}
\title{makes a Test Dataset of income regression coefs}
\usage{
  makeTestREcoefs(te,
    X = c("(Intercept)", "age", "I(age^2)", "cohort1920", "cohort1940", "cohort1960", "cohort1980"))
}
\arguments{
  \item{te}{test dataset from \code{\link{makeTestData}}}

  \item{X}{char vector of explanatory variables, including
  "(Intercept)"}
}
\description{
  makes a Test Dataset of income regression coefs
}
\seealso{
  Other testFunctions: \code{\link{makeTestData}}
}

