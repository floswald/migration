

# what's the difference between a lifecycle model with implicit and explicit borrowing constraint?

rm(list=ls())
library(data.table)
source('~/git/RUtils/R/tools.r')

nT <- 10
nA <- 30
nY <- 3

# bounds
alow <- -1
ahigh <- 3
ylow <- 1
yhigh <- 2

# borrowign constraints?
savelow <- -1
savehigh <- ahigh

# params
params <- list()
params$beta <- 0.95
params$R <- 1.04

P <- rouwenhorst(rho=0.8,sigma=0.1,mu=0,n=nY)$Pmat

grids <- list()
grids$a <- seq(alow,ahigh,length=nA)
grids$y <- seq(ylow,yhigh,length=nY)
st <- data.table(expand.grid(ia=1:nA,iy=1:nY,it=1:nT),expand.grid(a=grids$a,y=grids$y))
st[,cash := a + y]

# utility
ufun <- function(x) {
	out <- x
	out[x>0] <- log(x[x>0])
	out[x<0] <- -999999999
	return(out)
}

object <- function(x,csh,pars){
	r <- - (ufun(csh-x) + pars$beta*myfun(pars$R*x))
	return(r)
}


# final period
setkey(st,it,iy,ia)
st[.(nT), value := ufun(a)]
st[,savings := -319]

# time loop

for (ti in (nT-1):1){

	for (yi in 1:nY){
	
		EVtmp <- st[.(ti+1,yi)][, P %*% matrix(value,nY,nA,byrow=T)]
		myfun = approxfun(x=grids$a,y=EVtmp[yi,],rule=2)


		for (ai in 1:nA){
		cat('period, income, asset',ti,yi,ai,'\n')

			tmp <- optim(par=0.51,fn=object,csh=st[.(ti,yi,ai)][,cash],pars=params,method="Brent",lower=savelow,upper=savehigh) 
			st[.(ti,yi,ai), c("value","savings") := list(-tmp$value,tmp$par)]
			st[.(ti,yi,ai), cons := cash - savings]
		#             st[.(ti,yi,ia), value := (-1) * val]
		}

	}

}


# look at that
st[,mean(savings),by=it]



