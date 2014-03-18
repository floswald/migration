


#' Principal Component Scree plot
#' 
#' @param m matrix (T by N)
scree <- function(m){
  p <- princomp(m)
  screeplot(p)
}





#' construct a numeric matrix with time varying across columns
#'
#' @param dat data.frame in long format with columns y, quarter and state
#' @param demean TRUE/FALSE
#' @param detrend TRUE/FALSE
#' @return list with detrended data matrix and data.table with means and sds
makeTimeMatrix <- function(dat,detrend=formula(y~state*date),demean=TRUE,FD=FALSE,log=FALSE){

	stopifnot( c("y","date","state") %in% names(dat) )

	if (log) {
		dat[,y := log(y)]
		logmod <-TRUE 
	} else {
		logmod <- FALSE

	}


	# detrend data
	if (!is.null(detrend)){
		#detmod <- lm(y ~ state*bs(date,degree=3),data=dat)
		detmod <- lm(formula=detrend,data=dat)
		dat[,y := residuals(detmod)]
	} else {
		detmod <- NULL
	}


	# demean series
	if (demean){
		zmod <- dat[,list(mean=mean(y),sd=sd(y)),by=state]
		dat[,y := .SD[,(y- mean(y))/sd(y)], by=state]
	} else {
		zmod <- NULL
	}


	# first difference
	if (FD){
		setkey(dat,state,date)
		FDmod <- copy(dat)
		dat[,y := diff(y), by=state]
	} else {
		FDmod <- NULL
	}

	# plot all
	st = dat[,unique(state)]

	# bring into right shape
	m <- reshape(dat[,list(state,date,y)],timevar="date",idvar="state",direction="wide")
	st <- m[,state]
	ti <- dat[,unique(date)]

	m[,state:=NULL]
	m <- as.matrix(m)

	rownames(m) <- st
	colnames(m) <- ti

	idx <- dat[,unique(date)]

	out <- list(m=m,detmod=detmod,zmod=zmod,FDmod=FDmod,idx=idx,orig=dat,logmod=logmod)
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

#' MARSS Dynamic Factor Model on growth
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
#' data(US_states,package="EconData")
#' setkey(dat,state)
#' setkey(US_states,state)
#' US_states[,c("FIPS","STATE","Reg_ID","Div_ID"):=NULL]
#' dat=US_states[dat]
#' div=dat[,list(HV=mean(y)),by=list(Division,date)]
#' setnames(div,c("state","date","y"))
#' d <- dynGrowth(div,facs=2,detrend=NULL,maxite=1000,demean=TRUE,FD=TRUE)
#' sim <- MARSSsimulate(d$marss,tSteps=20,nsim=10)
#' #matplot(t(sim$sim.states[ , , 1]),type="l",main="simulated hidden factors")
#' #matplot(t(sim$sim.data[ , , 1]),type="l",main="simulated outcomes")
dynGrowth <- function(dat,facs,maxite=50,detrend=formula(y~state*date),demean=TRUE,FD=FALSE,log=TRUE,meth="BFGS"){

	mmat <- makeTimeMatrix(dat=dat,detrend=detrend,demean=demean,FD=FD,log=log)

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

	out <- list(marss=mod,datmod=mmat,orig=dat)

	return(out)

}


#' Back out price levels from MARSS simulation
#'
#' @param l the output of \code{\link{dynPrices}}
#' @param n number of periods to simulate
#' @param N number of different simulations
backOutPrices <- function(l,n,N=1){

	# simulate series
	sim <- MARSSsimulate(l$marss,tSteps=n,nsim=N)

	# states involved
	st <- rownames(l$marss$model$data)

	if (l$orig[,class(date)=="yearqtr"]){
		dates=l$orig[,as.yearqtr(seq(from=min(as.Date(date)),length.out=n,by="3 months"))]
	} else if (l$orig[,class(date)=="integer"]){
		dates=l$orig[,seq(from=min(date),length.out=n,by=1)]
	}

	pr <- sim$sim.data
	dimnames(pr) = list(state=st,date=dates,sim=paste0(1:N))
	dimnames(sim$sim.data) = list(state=st,date=dates,sim=paste0(1:N))


	if(!is.null(l$datmod$FDmod)){
		setkey(d$orig,state,date)

		for (ni in 1:N){
		pr[,1,ni ] <- d$orig[list(unique(state),min(date))][["y"]]
		for (ist in 1:length(st)){
			for (iN in 2:n){
				pr[ist,iN, ni] <- pr[ist,iN-1,ni ] + pr[ist,iN,ni ]
			}
		}
		}
	}

	if (!is.null(l$datmod$zmod)){	

		# un-z-score simulation values
		for (i in st) pr[i, , ] <- pr[i, , ] * l$datmod$zmod[state==i][["sd"]] + l$datmod$zmod[state==i][["mean"]]

	}

	
	if (!is.null(l$datmod$detmod)){

		newd <- data.table(expand.grid(date=dates,state=st))
		for (i in st) pr[i, , ] <- pr[i, , ] + predict(l$datmod$detmod,newdata=newd[state==i])

	}

	if (l$datmod$logmod) pr <- exp(pr)

	


	# factors
	facs <- sim$sim.states
	dimnames(facs) = list(fac=paste0(1:nrow(l$marss$states)),date=dates,sim=paste0(1:N))

	# add linear trend to that, if you had one
	# newdata for prediction

	# melt
	m <- melt(pr)
	mf <- melt(facs)
	out <- list(arr=pr,molten=m,factors=facs)

	# plots
	out$t <- ggplot(l$orig,aes(x=date,y=y,color=state))+ geom_line()
	out$p <- ggplot(m,aes(x=date,y=value,color=state)) + geom_line() + facet_wrap(~sim)
	out$f <- ggplot(mf,aes(x=date,y=value,color=factor(fac))) + geom_line() + facet_wrap(~sim)

	return(out)

}

















































	
