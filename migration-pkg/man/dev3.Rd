\name{dev3}
\alias{dev3}
\title{dev3}
\usage{
  dev3(data)
}
\description{
  dev3
}
\examples{

nA <- 5L; nY <- 3L; nT <- 3L
s1 <- data.table(expand.grid(a=seq(0,10,length=nA),y=1:nY,it=1:nT))
s1[,cash := a + y + 0.3*it]
save <- seq(0,10,length=nA)
s3 <- data.table(expand.grid(a=seq(0,10,length=nA),y=1:nY,it=1:nT,save=save))
s3[,cons := a + y + 0.3*it - save]
dataR <- list( cash1 = s1[["cash"]],
               dims = c(nA,nY,nT),
               savings = save,
               theta = 1.2,beta=0.95,myNA=-99)
blitz <- dev3(data=dataR)

######################################################
# Calculating an R solution to this 3-period lifecycle
######################################################

V = array(0,c(nA,nY,nT))
V[ , ,3] = s1[it==3,log(cash)]
C = s3[,array(cons,c(nA,nY,nT,nA))]
W = array(0,c(nA,nY,nT,nA))
for (ti in 2:1) {
    for (ia in 1:nA) {
         for(iy in 1:nY) {
             for (ja in 1:nA){
                 if (C[ia,iy,ti,ja] > 0){
		     		W[ia,iy,ti,ja] =  log(C[ia,iy,ti,ja])  + dataR$beta*V[ja,iy,ti+1]
			     } else {
			        W[ia,iy,ti,ja] = dataR$myNA
			     }
		     }
			V[ia,iy,ti] = max(W[ia,iy,ti, ])
         }
     }
}
############################################
# comparing R and blitz++ solutions
############################################

print(all.equal(V,blitz$V1))
print(all.equal(W[ , ,1, ],blitz$w))
print(all.equal(C,blitz$cons))
}

