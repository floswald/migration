\name{dev_blitz2}
\alias{dev_blitz2}
\title{Blitz mini discrete choice model 2}
\usage{
  dev_blitz2(data)
}
\description{
  in this example we map vectors to C
}
\examples{


# minimal blitz example.
# checks mapping R to blitz

x  <- sample(1:120,size=120,replace=F)
option1 <- x[1:60]
option2 <- x[61:100]
savings  <- runif(n=120)
dataR <- list(option1 = option1, option2 = option2, savings=savings,dims = c(2,5,6))
blitz <- dev_blitz2(data=dataR)
r = list()
As <- array(option1,c(2,5,6)) - array(savings,c(2,5,6))
A2s <- array(option2,c(2,5,6)) - array(savings,c(2,5,6))
V1 = apply(As,c(1,2),max)
V2 = apply(A2s,c(1,2),max)
V12 = array(c(V1,V2),c(2,5,2))
r$vmax = apply(V12,c(1,2),max)
r$dchoice = apply(V12,c(1,2),which.max)
print(all.equal(r,blitz))
}

