\name{devC}
\alias{devC}
\title{devC}
\usage{
  devC(data)
}
\description{
  devC
}
\examples{

nA <- 5L; nY <- 3L; nT <- 3L
s1 <- data.table(expand.grid(a=seq(0,10,length=nA),y=1:nY,it=1:nT))
s1[,cash := a + y + 0.3*it]
s2 <- copy(s1)
s2[,cash := a + y + 0.2*it + 0.09*it^2]
save <- seq(0,10,length=nA)
dataR <- list( cash1 = s1[["cash"]],
               cash2 = s2[["cash"]],
               dims = c(nA,nY,nT),
               savings = save)
blitz <- devC(data=dataR)
s3 <- data.table(expand.grid(a=seq(0,10,length=nA),y=1:nY,it=1:nT,save=save))
s3[,cons := a + y + 0.3*it - save]
print(all.equal(s3[,array(cons,c(nA,nY,nT,nA))],blitz$vmax))
}

