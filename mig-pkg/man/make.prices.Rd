\name{make.prices}
\alias{make.prices}
\title{make a growing price grid}
\usage{
  make.prices(m, plot = FALSE)
}
\value{
  list with aggregate price matrix where row i is the price
  grid for age i and with a 3D array that has age by price
  by location prices
}
\description{
  taking maximal and minimal price growth rates, constructs
  grids for each period
}

