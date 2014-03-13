


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
makeTimeMatrix <- function(dat,detrend=formula(y~state*date),demean=TRUE,FD=FALSE){

	stopifnot( c("y","date","state") %in% names(dat) )

	# detrend data
	if (!is.null(detrend)){
		#detmod <- lm(y ~ state*bs(date,degree=3),data=dat)
		detmod <- lm(formula=detrend,data=dat)
		dat[,detr := residuals(detmod)]
	} else {
		dat[,detr := y]
		detmod <- NULL
	}


	# demean series
	if (demean){
		dat[,y := .SD[,(detr- mean(detr))/sd(detr)], by=state]
		zmod <- dat[,list(mean=mean(detr),sd=sd(detr)),by=state]
	} else {
		dat[,y := detr]
		zmod <- NULL
	}


	# first difference
	if (FD){
		setkey(dat,date,state)
		dat[,y := c(diff(y),NA), by=state]
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

	out <- list(m=m,detmod=detmod,zmod=zmod,idx=idx)
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
#' data(HValue96_dynF_quarterly)
#' dd <- copy(dat[state %in% sample(unique(state),size=5)])
#' l <- dynPrices(dat=dd,facs=3,maxite=300)
#' sim <- MARSSsimulate(l$marss,tSteps=20,nsim=1)
#' #matplot(t(sim$sim.states[ , , 1]),type="l",main="simulated hidden factors")
#' #matplot(t(sim$sim.data[ , , 1]),type="l",main="simulated outcomes")
dynPrices <- function(dat,facs,maxite=50,detrend=formula(y~state*date),demean=TRUE,meth="BFGS"){

	mmat <- makeTimeMatrix(dat=dat,detrend=detrend,demean=demean)

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
	dates=l$orig[,as.yearqtr(seq(from=min(as.Date(date)),length.out=n,by="3 months"))]




	# un-z-score simulation values
	unzi <- array(0,dim=dim(sim$sim.data))
	dimnames(unzi) = list(state=st,date=dates,sim=paste0(1:N))
	dimnames(sim$sim.data) = list(state=st,date=dates,sim=paste0(1:N))
	for (i in st) unzi[i, , ] <- sim$sim.data[i, , ] * l$datmod$zmod[state==i][["sd"]] + l$datmod$zmod[state==i][["mean"]]

	# add linear trend to that
	# newdata for prediction
	newd <- data.table(expand.grid(date=dates,state=st))
	for (i in st) unzi[i, , ] <- unzi[i, , ] + predict(l$datmod$detmod,newdata=newd[state==i])

	# melt
	m <- melt(unzi)
	out <- list(arr=unzi,molten=m)

	ggplot(m,aes(x=date,y=value,color=state)) + geom_line() + facet_wrap(~sim)

	return(unzi)

}

















































	
