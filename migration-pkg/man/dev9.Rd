\name{dev9}
\alias{dev9}
\title{dev9: continuous optimization, utility function}
\usage{
  dev9(data)
}
\description{
  dev9: continuous optimization, utility function
}
\examples{


# example for dev9
# ================

# savings: continuous optimization
# utility: CRRA 

# this is a tester for the continuous optimization.
# code not used for production.

library(migration)

nA <- 50L; nY <- 2L; nT <- 20L
G <- rouwenhorst(rho=0.9,n=nY,sigma=0.1)$Pmat
dims <- c(nA,nY,nT)
names(dims) <- c("a","y","age")
dataR <- list( dims=dims,
               theta = 0.2,beta=0.95,gamma=1.4,cutoff = 0.1,verbose = 1,
			   R = 1.04,
               G = as.numeric(rouwenhorst(rho=0.9,n=nY,sigma=0.1)$Pmat))
               
grids <- list()
grids$a <- grid.maker(bounds=c(-2,10),num.points = nA,spacing = "log.g2")
grids$arent <- grid.maker(bounds=c(0,10),num.points = nA,spacing = "log.g2")
grids$y <- seq(1,2,length=nY)

# OTHER's statespace: add dimension "here" AND "there".

# owner neg and pos assets, renter only pos
SSo <- data.table(rbind(expand.grid(a=grids$a,y=grids$y,it=1:(nT-1)),expand.grid(a=grids$arent,y=grids$y,it=nT)))
SSr <- data.table(expand.grid(a=grids$arent,y=grids$y,it=1:nT))

# both income and prices are mappings from here and there to y and p.
# for now no location specific costs/differences in prices. add that later
SSo[,cstay := a + y]	
SSr[,csell := a + y ]
SSo[it==nT,cstay := a ]	
SSr[it==nT,csell := a ]

# restrictions
# ============

# stayer: 

# seller: can have negative assets
# seller: but cannot borrow. must pay off housing debt upon sale.

# tensors
# =======


ResS <- SSr[,array(csell,c(dataR$dims))]
ResO <- SSo[,array(cstay,c(dataR$dims))]	# "O" is for "Owner", i.e. "stay"

dataR$ResS <- as.numeric(ResS)
dataR$ResO <- as.numeric(ResO)
dataR$agrid_own <- grids$a
dataR$agrid_rent <- grids$arent

d <- dev9(dataR)


}

