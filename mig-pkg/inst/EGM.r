

# solve the euler equation

# do some integration

rm(list=ls())


nA <- 50L; nY <- 3L; nT <- 5L

library(rutils)
library(data.table)

               

dataR <- list( dims = c(nA,nY,nT),
               beta=0.95,
			   myNA=-99,rent=0.05,
			   r=0.04,
			   R=(1+0.04),
			   CRRA=1.4,
			   down=0.2)
dataR$a <- exp(exp(exp(seq(0.01,log(log(log(10+1)+1)+1),length=nA))-1)-1)-1
# dataR$a <- seq(0.00,10,length=nA)
dataR$y <- seq(0.1,3,le=nY) 
dataR$G <- rouwenhorst(n=nY,rho=0.8,sigma=1,mu=0)$Pmat

# utility function
ufun <- function(x,pars){

	y <- x
	y[x<=0] <- -pars$myNA

	if (pars$CRRA==1){
		return(log(y[x>0]))
	} else {
		return( (1/(1-pars$CRRA )) * y[x>0] ^ (1-pars$CRRA ))
	}
}

# marginal utility function
du <- function(x,pars){
	y <- x
	y[x<=0] <- pars$myNA

	if (pars$CRRA==1){
		y[x>0] <- 1/x[x>0]
	} else {
		y[x>0] <- x[x>0] ^ -pars$CRRA
	}
	return(y)
}

# inverse marginal utility function
duinv <- function(x,pars){
	y <- x
	y[x<=0] <- pars$myNA

	if (pars$CRRA==1){
		y[x>0] <- x[x>0]
	} else {
		y[x>0] <- x[x>0] ^ -(1/pars$CRRA)
	}
	return(y)
}

# future value approximation
EVfun <- function(newx,grid,values,derivs=0){

	fx <- splinefun(x=grid,y=values)
	return(fx(newx,deriv=derivs))
}

# objective function: euler equation
obj <- function(x,cash,evplus,pars){

	cons <- cash - x 

	mu <- du(x=cons,pars)

	EV <- EVfun(x*pars$R,pars$a,evplus,derivs=1)

	return( mu - pars$beta*(pars$R)*EV )
}


# sR means you are renter
s <- data.table(expand.grid(a=dataR$a,y=dataR$y,it=1:nT))
s <- cbind(s,expand.grid(ia=1:nA,iy=1:nY))
s[,cash := a + y ]
s[,rcash := dataR$R*cash]


# tensors
val  <- array(0,dataR$dims)
err  <- array(0,dataR$dims)
sav  <- array(0,dataR$dims)
con  <- array(0,dataR$dims)
val[ , ,nT] <- s[it==nT,matrix(10*ufun(a+y,dataR),nA,nY)] 	# final period value 
EV   <- array(0,dataR$dims)

# tensor functions
library(RcppSimpleTensor)
f <- tensorFunction(R[i,j] ~ V[i,k] * G[k,j])

EV[ , ,nT] <- f(val[ , ,nT],t(dataR$G))

# test objective
par(mfcol=c(1,2))
plot(x=seq(0,0.5,le=100),y=obj(seq(0,0.5,le=100),cash=0.5,EV[ ,1,nT],dataR),type="l",ylab="euler equation",xlab="savings choice",main="euler equation with cash=0.5")
abline(h=0,col="red")
plot(x=seq(0,2,le=100),y=obj(seq(0,2,le=100),cash=2,EV[ ,1,nT],dataR),type="l",ylab="euler equation",xlab="savings choice",main="euler equation with cash=2")
abline(h=0,col="red")
par(mfcol=c(1,1))

setkey(s,ia,iy,it)




eps <- 0.001
# loop over states
for (ti in (nT-1):1){
	for (yi in 1:nY){
		evtmp <- EV[ ,yi,ti+1]
		for (ai in 1:nA){
			tmp <- uniroot(f=obj,
						   interval=c(-2,s[.(ai,yi,ti)][["rcash"]]-eps),
						   cash=s[.(ai,yi,ti)][["cash"]],
						   evplus = evtmp,
						   pars=dataR)
			err[ai,yi,ti] <- tmp$f.root
			sav[ai,yi,ti] <- tmp$root
			con[ai,yi,ti] <- s[.(ai,yi,ti)][["cash"]] - (1/dataR$R) * sav[ai,yi,ti]
			val[ai,yi,ti] <- ufun(con[ai,yi,ti],pars=dataR) + dataR$beta*EVfun(sav[ai,yi,ti],dataR$a,evtmp,derivs=0)
		}
	}
	EV[ , ,ti] <- f(val[ , ,ti],t(dataR$G))
}

persp(x=dataR$a,y=dataR$y,sav[ , ,1])
persp(x=dataR$a,y=dataR$y,con[ , ,1])



# test against value function optimization

value.val  <- array(0,dataR$dims)
sav.val <- array(0,dataR$dims)
con.val  <- array(0,dataR$dims)
# objective function: value funcion
obj.val <- function(x,cash,evplus,pars){

	cons <- cash - x 

	u <- ufun(x=cons,pars)

	EV <- EVfun(pars$R*x,pars$a,evplus,derivs=0)

	return( u + pars$beta*EV )
}

# loop over states
for (ti in (nT-1):1){
	for (yi in 1:nY){
		evtmp <- EV[ ,yi,ti+1]
		for (ai in 1:nA){
			tmp <- optimize(f=obj.val,
						   interval=c(-1,s[.(ai,yi,ti)][["rcash"]]-eps),
						   cash=s[.(ai,yi,ti)][["cash"]],
						   evplus = evtmp,
						   pars=dataR,maximum=TRUE)
			sav.val[ai,yi,ti] <- tmp$maximum
			con.val[ai,yi,ti] <- s[.(ai,yi,ti)][["cash"]] -  sav.val[ai,yi,ti]
			value.val[ai,yi,ti] <- ufun(con.val[ai,yi,ti],pars=dataR) + dataR$beta*EVfun(dataR$R * sav.val[ai,yi,ti],dataR$a,evtmp,derivs=0)
		}
	}
	EV[ , ,ti] <- f(value.val[ , ,ti],t(dataR$G))
}




#' evaluate interpolation scheme with linear extraplation if outside grid
#'
#' does linear inter- and extrapolation
#' @param x grid values to interpolate
#' @param y values, y = f(x) to interpolate
#' @param newx values of x at which to evaluate the interpolation
InterpExtrap <- function(x,y,newx,spline=FALSE,deriv=0){

	n <- length(newx)
	m <- length(x)
	r <- rep(0,n)

	if (spline){

		interp <- splinefun(x,y,"natural")

		for (i in 1:n){

			if (newx[i]>max(x)){

				# extrapolate above x
				slope <- (interp(x[m],deriv) - interp(x[m-1],deriv)) / (x[m] - x[m-1])
				r[i] <- interp(x[m],deriv) + (newx[i] - x[m]) * slope

			} else if (newx[i]<min(x)) {

				# extrapolate below x
				slope <- 1
				r[i] <- interp(x[1],deriv) + (newx[i] - x[1]) * slope

			} else {

				r[i] <- interp(newx[i],deriv)

			}
		}
	} else {

		interp <- approxfun(x,y)

		for (i in 1:n){

			if (newx[i]>max(x)){

				# extrapolate above x
				slope <- (interp(x[m]) - interp(x[m-1])) / (x[m] - x[m-1])
				r[i] <- interp(x[m]) + (newx[i] - x[m]) * slope

			} else if (newx[i]<min(x)) {

				# extrapolate below x
				slope <- 1
				r[i] <- interp(x[1]) + (newx[i] - x[1]) * slope

			} else {

				r[i] <- interp(newx[i])

			}
		}
		if (deriv==1){
			z <- diff(r) / diff(newx)
			z <- c(z,tail(z,1)+0.5*tail(diff(z)[diff(z)!=0],1))
			r <- z
		}
	}
	return(r)
}

source('http://www.stat.colostate.edu/~meyer/pensplall.R')

## endogenous grid method
e <- list()
e$V <- array(0,c(nA,nY,nT))
e$EV <- array(0,c(nA,nY,nT))
e$c  <- array(0,c(nA,nY,nT))
e$m  <- array(0,c(nA,nY,nT))
e$V[ , ,nT] <- matrix(ufun(outer(dataR$a,dataR$y,"+"),dataR),nA,nY)
e$EV[ , ,nT] <- f(e$V[ , ,nT],t(dataR$G))

# get expected value for each state y today
derivs <- lapply(1:nY,function(x) InterpExtrap(x=dataR$a,y=e$EV[,x,nT],newx=dataR$a*dataR$R,spline=FALSE,deriv=1))

# get T-1 consumption
for (yi in 1:nY){
	e$c[ ,yi,nT-1] <- duinv( derivs[[yi]], dataR)
}

e$m[ , ,nT-1] <- e$c[ , ,nT-1] + matrix(dataR$a,nA,nY)

e$V[ , ,nT-1] <- matrix(ufun(e$c[ ,,nT-1],dataR),nA,nY) + dataR$beta * e$EV[ , ,nT]
e$EV[ , ,nT-1] <- f(e$V[ , ,nT-1],t(dataR$G))




# newx should be next period's consumption function!


ti=nT-2
derivs <- lapply(1:nY,function(x) InterpExtrap(x=dataR$a,y=e$c[,x,ti+1],newx=dataR$a*dataR$R,spline=FALSE,deriv=1))

# get T-2 consumption
for (yi in 1:nY){
	e$c[ ,yi,ti] <- duinv( derivs[[yi]], dataR)
}

e$m[ , ,ti] <- e$c[ , ,ti] + matrix(dataR$a,nA,nY)

e$V[ , ,ti] <- matrix(ufun(e$c[ ,,ti],dataR),nA,nY) + dataR$beta * e$EV[ , ,ti+1]
e$EV[ , ,ti] <- f(e$V[ , ,ti],t(dataR$G))


ti=nT-3
derivs <- lapply(1:nY,function(x) InterpExtrap(x=dataR$a,y=e$EV[,x,ti+1],newx=dataR$a*dataR$R,spline=FALSE,deriv=1))

# get T-3 consumption
for (yi in 1:nY){
	e$c[ ,yi,ti] <- duinv( derivs[[yi]], dataR)
}

e$m[ , ,ti] <- e$c[ , ,ti] + matrix(dataR$a,nA,nY)

e$V[ , ,ti] <- matrix(ufun(e$c[ ,,ti],dataR),nA,nY) + dataR$beta * e$EV[ , ,ti+1]
e$EV[ , ,ti] <- f(e$V[ , ,ti],t(dataR$G))


ti=nT-4
derivs <- lapply(1:nY,function(x) InterpExtrap(x=dataR$a,y=e$EV[,x,ti+1],newx=dataR$a*dataR$R,spline=FALSE,deriv=1))

# get T-1 consumption
for (yi in 1:nY){
	e$c[ ,yi,ti] <- duinv( derivs[[yi]], dataR)
}

e$m[ , ,ti] <- e$c[ , ,ti] + matrix(dataR$a,nA,nY)

e$V[ , ,ti] <- matrix(ufun(e$c[ ,,ti],dataR),nA,nY) + dataR$beta * e$EV[ , ,ti+1]
e$EV[ , ,ti] <- f(e$V[ , ,ti],t(dataR$G))





for (ti in (nT-2):1){
	
	derivs <- lapply(1:nY,function(x) InterpExtrap(x=dataR$a,y=e$EV[,x,ti+1],newx=dataR$a*dataR$R,spline=FALSE,deriv=1))

	# get T-1 consumption
	for (yi in 1:nY){
		e$c[ ,yi,ti] <- duinv( derivs[[yi]], dataR)
	}

	e$m[ , ,ti] <- e$c[ , ,ti] + matrix(dataR$a,nA,nY)

	e$V[ , ,ti] <- matrix(ufun(e$c[ ,,ti],dataR),nA,nY) + dataR$beta * e$EV[ , ,ti+1]
	e$EV[ , ,ti] <- f(e$V[ , ,ti],t(dataR$G))

}








