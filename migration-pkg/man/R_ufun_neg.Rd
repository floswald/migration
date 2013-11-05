\name{R_ufun_neg}
\alias{R_ufun_neg}
\title{CRRA Utility Function with neg Cons penalty}
\usage{
  R_ufun_neg(cons, gamma, cutoff)
}
\arguments{
  \item{cons}{scalar consumption. must be non-negative.}

  \item{gamma}{scalar CRRA parameter}

  \item{cutoff}{minimum level of consumption below which
  approximation is used}
}
\value{
  list with utility and gradient
}
\description{
  this is a devel function. only used to check accuracy of
  C++ production code. the function is defined by u(c) =
  (1/(1-gamma)) * c^(1-gamma) if c > cstar u(c) =
  (1/(1-gamma)) * cstar^(1-gamma) + u'(cstar)*(cstar-c) +
  0.5*u''(cstar)*(cstar-c)^2 else
}

