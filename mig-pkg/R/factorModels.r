


#' Principal Component Scree plot
#' 
#' @param m matrix (T by N)
scree <- function(m){
  p <- princomp(m)
  screeplot(p)
}


#' transform R into a compact interval
#'
y2z.transform <- function(y,high,low=0){

	stopifnot(high > low)

	z <- log( (y - low) / (1 - y/high))
	return(z)
}

#' inverse of transform R into a compact interval
#'
z2y.transform <- function(z,high,low=0){

	stopifnot(high > low)
	y <- (low + exp(z) ) / (1 + exp(z)/high)
	return(y)
}



#' construct a numeric matrix with time varying across columns
#'
#' @param dat data.frame in long format with columns y, quarter and state
#' @param trans transformation
#' @return list with detrended data matrix and data.table with means and sds
makeTimeMatrix <- function(dat,trans=1){

	stopifnot( c("y","date","state") %in% names(dat) )

	w <- copy(dat)
	setkey(w,state,date)


	# z transform and growth

	if (trans==1){



		w[,z := .SD[,(y - mean(y))/sd(y)], by=state]
		w[,dz := .SD[,diff(z)] ,by=state ]
		m <- reshape(w[,list(state,date,dz)],timevar="date",idvar="state",direction="wide")

	} else {

		stop("no other trans defined")

	}

	# bring into right shape
	st <- m[,state]
	ti <- w[,unique(date)]

	m[,state:=NULL]
	m <- as.matrix(m)

	rownames(m) <- st
	colnames(m) <- ti

	idx <- w[,unique(date)]

	out <- list(m=m,idx=idx,w=w)
	return(out)

}
	
	
	





#' Dynamic Factor model for Lincoln Home Values
#'
#' @examples
#' data(HValue96_dynF_quarterly)
#' m <- dynPrices(dat=dat,facs=3,maxite=300,demean=TRUE,detrend=TRUE,meth="BFGS")
runDynFactorsLincoln <- function(){
	NULL
}



#' make a home values dataset for factor models
#'
#' contains a variable y that is home value in 96
#' dollars, by state 
makeHomeValues <- function(freq,path="~/git/migration/mig-pkg/data/"){

	data(HomeValues,package="EconData")
	dat <- copy(HomeValues)
	data(CPIHOSSL,package="EconData")

	if (freq=="quarterly"){

		cpi.h <- xts::to.quarterly(CPIHOSSL)[,1]
		coredata(cpi.h) <- coredata(cpi.h) / as.numeric(cpi.h['1996-01-01'])
		names(cpi.h) <- "cpiH"
		cpi <- data.table(qtr=index(cpi.h),cpiH=coredata(cpi.h),key="qtr")
		setnames(cpi,c("qtr","cpiH"))
		setkey(cpi,qtr)
		setkey(dat,qtr)
		dat <- cpi[dat]
		dat[,HValue96 := Home.Value / cpiH ]
		setkey(dat,State,qtr)
		setnames(dat,c("qtr","HValue96","State"),c("date","y","state"))

	} else if (freq=="yearly") {

		# must agg homevalues first
		dat <- dat[,list(Home.Value=mean(Home.Value)),by=list(State,year(qtr))]

	
		cpi.h <- xts::to.yearly(CPIHOSSL)[,1]
		coredata(cpi.h) <- coredata(cpi.h) / as.numeric(cpi.h['1996'])
		names(cpi.h) <- "cpiH"
		cpi <- data.table(year=year(index(cpi.h)),cpiH=coredata(cpi.h),key="year")
		setnames(cpi,c("year","cpiH"))
		setkey(cpi,year)
		setkey(dat,year)
		dat <- cpi[dat]
		dat[,HValue96 := Home.Value / cpiH ]
		setkey(dat,State,year)
		setnames(dat,c("year","HValue96","State"),c("date","y","state"))

	} else {

		stop("misspelled.")

	}

	fname <- paste0("HValue96_dynF_",freq,".RData")
	save(dat,file=file.path(path,fname))

	return(dat)

}


#' MARSS Dynamic Factor Model
#'
#' estimates a dynamic factor model
#' 
#' @param dat dataset 
#' @param detrend formula for detrending outcome var
#' @param demean TRUE if want to do a z-transformation on detrended outcome
#' @param meth method for maximizing the likelihood
#' @param facs number of hidden factors
#' @examples
#' data(HValue96_dynF_yearly)
#' dd <- copy(dat)
#' l <- dynPrices(dat=dd,facs=2,maxite=5000)
#' sim <- MARSSsimulate(l$marss,tSteps=20,nsim=1)
#' #matplot(t(sim$sim.states[ , , 1]),type="l",main="simulated hidden factors")
#' #matplot(t(sim$sim.data[ , , 1]),type="l",main="simulated outcomes")
dynPrices <- function(dat,facs,maxite=50,detrend=formula(y~state*date),demean=TRUE,FD=FALSE,meth="BFGS"){

	mmat <- makeTimeMatrix(dat=dat,detrend=detrend,demean=demean,FD=FD)

	m <- mmat$m


	# setup model

	# build z matrix
	Z.vals <- list()
	for (i in 1:nrow(m)){
	for (j in 1:facs){

		if (i<facs){
			if (j>i){
			   	Z.vals[[i+(j-1)*nrow(m)]] <- 0
			} else {
				Z.vals[[i+(j-1)*nrow(m)]] <- paste0("z",i,j)
			}

		} else {

			Z.vals[[i+(j-1)*nrow(m)]] <- paste0("z",i,j)
		}
	}}

	Z <- matrix(Z.vals,nrow(m),facs)
	

	Q <- B <- diag(1,facs)

	# structure of error matrix on outcome equation
	R = "diagonal and unequal"

	x0 <- U <- A <- "zero"

	# initial variance
	V0 <- diag(5,facs)

	# list to pass to MARSS
	dfa <- list(Z=Z,A=A,R=R,B=B,U=U,Q=Q,x0=x0,V0=V0)
	cntl.list <- list(maxit=maxite,trace=1)

	mod <- MARSS(m,model=dfa,control=cntl.list,method=meth)

	out <- list(marss=mod,datmod=mmat,orig=mmat$orig)

	return(out)

}

#' MARSS Dynamic Factor Model on growth by Division
#'
#' estimates a dynamic factor model
#' 
#' @param dat dataset 
#' @param detrend formula for detrending outcome var
#' @param demean TRUE if want to do a z-transformation on detrended outcome
#' @param meth method for maximizing the likelihood
#' @param facs number of hidden factors
#' @examples
#' data(HValue96_dynF_yearly)
#' div=dat[,list(HV=mean(y)),by=list(Division,date)]
#' setnames(div,c("state","date","y"))
#' d <- dynGrowth(div,facs=2,trans=1)
#' sim <- MARSSsimulate(d$marss,tSteps=20,nsim=10)
#' #matplot(t(sim$sim.states[ , , 1]),type="l",main="simulated hidden factors")
#' par(mfcol=c(3,3))
#' for (i in 1:9) matplot(t(sim$sim.data[ , , i]),type="l",main=sprintf("sim number %d",i))
#' par(mfcol=c(1,1))
dynGrowth <- function(dat,facs,maxite=50,trans=1,meth="BFGS"){

	mmat <- makeTimeMatrix(dat=dat,trans)

	m <- mmat$m


	# setup model

	# build z matrix
	Z.vals <- list()
	for (i in 1:nrow(m)){
	for (j in 1:facs){

		if (i<facs){
			if (j>i){
			   	Z.vals[[i+(j-1)*nrow(m)]] <- 0
			} else {
				Z.vals[[i+(j-1)*nrow(m)]] <- paste0("z",i,j)
			}

		} else {

			Z.vals[[i+(j-1)*nrow(m)]] <- paste0("z",i,j)
		}
	}}

	Z <- matrix(Z.vals,nrow(m),facs)
	

	Q <- B <- diag(1,facs)

	# structure of error matrix on outcome equation
	R = "diagonal and unequal"

	x0 <- U <- A <- "zero"

	# initial variance
	V0 <- diag(5,facs)

	# list to pass to MARSS
	dfa <- list(Z=Z,A=A,R=R,B=B,U=U,Q=Q,x0=x0,V0=V0)
	cntl.list <- list(maxit=maxite,trace=1)

	mod <- MARSS(m,model=dfa,control=cntl.list,method=meth)

	out <- list(marss=mod,datmod=mmat)

	return(out)

}


#' Back out price levels from MARSS simulation
#'
#' @param l the output of \code{\link{dynPrices}}
#' @param n number of periods to simulate
#' @param N number of different simulations
backOutPrices <- function(l,n,N=1,trans){

	# simulate series
	sim <- MARSSsimulate(l$marss,tSteps=n,nsim=N)

	# states involved
	st <- rownames(l$marss$model$data)

	if (l$datmod$w[,class(date)=="yearqtr"]){
		dates=l$datmod$w[,as.yearqtr(seq(from=min(as.Date(date)),length.out=n,by="3 months"))]
	} else if (l$datmod$w[,class(date)=="integer"]){
		dates=l$datmod$w[,seq(from=min(date),length.out=n,by=1)]
	}

	si <- sim$sim.data
	dimnames(si) = list(state=st,date=dates,sim=paste0(1:N))
	dimnames(sim$sim.data) = list(state=st,date=dates,sim=paste0(1:N))


	# box in si!
	ra <- l$datmod$w[,range(dz)]
	si = z2y.transform(si,high=ra[2],low=ra[1])



	if (trans==1){

		# undo differencing

		z <- array(0,dim(si))
		dimnames(z) = list(state=st,date=dates,sim=paste0(1:N))

		z0 <- l$datmod$w[,head(z,1),by=list(state)][["V1"]]


		for (nS in 1:N){

			z[, 1,nS] <- z0 

			for (nt in 2:n){
				
				z[, nt,nS] <- z[, nt-1,nS] + si[, nt,nS]

			}
		}


		# undo z transformation
		y <- data.table(melt(z),key="state")
		ms <- d$datmod$w[,list(mean=mean(y),sd=sd(y)),by=state]
		setkey(ms,state)

		y[ms]

		y[,unzed := z*sd + mean]

	} else {

		stop("not defined")

	}
	


	# factors
	facs <- sim$sim.states
	dimnames(facs) = list(fac=paste0(1:nrow(l$marss$states)),date=dates,sim=paste0(1:N))

	# add linear trend to that, if you had one
	# newdata for prediction

	# melt
	m <- melt(y)
	mf <- melt(facs)
	out <- list(arr=y,molten=m,factors=facs)

	# plots
	out$t <- ggplot(l$datmod$orig,aes(x=date,y=y,color=state))+ geom_line()
	out$p <- ggplot(m,aes(x=date,y=value,color=state)) + geom_line() + facet_wrap(~sim)
	out$f <- ggplot(mf,aes(x=date,y=value,color=factor(fac))) + geom_line() + facet_wrap(~sim)

	return(out)

}

















































	
