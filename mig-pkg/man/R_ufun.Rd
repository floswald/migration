\name{R_ufun}
\alias{R_ufun}
\title{CRRA Utility Function}
\usage{
  R_ufun(cons, gamma, theta)
}
\arguments{
  \item{cons}{scalar consumption. must be non-negative.}

  \item{gamma}{scalar CRRA parameter}

  \item{theta}{scalar utility weight of housing. theta = 0
  for renters.}
}
\description{
  this is a devel function. only used to check accuracy of
  C++ production code. the function is defined by u(c,h) =
  (1/(1-gamma)) * c^(1-gamma) + theta*h
}

