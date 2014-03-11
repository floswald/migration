


#' Principal Component Scree plot
#' 
#' @param m matrix (T by N)
scree <- function(m){
  p <- princomp(m)
  screeplot(p)
}



#' Plot different FHFA house prices indices
#'
#' @examples
#' data(FHFA_states,package="EconData")
#' dd <- FHFA.states$qtr[state %in% sample(unique(state),size=20)]
#' plot.FHFAindex(dd)
plot.FHFAindex <- function(dat,saveto="~/Dropbox/mobility/output/model/factor/fhfa.pdf"){

	# detrend data
		detmod <- lm(index_sa ~ quarter*state,data=dat)
		dat[,detr := residuals(detmod)]


	# demean series
		dat[,y1 := .SD[,(index_sa- mean(index_sa))/sd(index_sa)], by=state]
		dat[,y2 := .SD[,(detr- mean(detr))/sd(detr)], by=state]
		zmod <- dat[,list(mean=mean(detr),sd=sd(detr)),by=state]

	# plot all
	st = dat[,unique(state)]

	pdf(file=saveto)
	par(mfcol=c(2,2))


	r <- dat[,range(index_sa)]
	dat[state==st[1],plot(Date,index_sa,type="l",ylim=r,main="observed index")]
	for (i in st[-1]) dat[state==i,lines(Date,index_sa)]

	r <- dat[,range(detr)]
	dat[state==st[1],plot(Date,detr,type="l",ylim=r,main="detrended index\n(time dummies)")]
	for (i in st[-1]) dat[state==i,lines(Date,detr)]

	r <- dat[,range(y1)]
	dat[state==st[1],plot(Date,y1,type="l",ylim=r,main="z-score of observed index",ylab="z")]
	for (i in st[-1]) dat[state==i,lines(Date,y1,ylab="")]

	r <- dat[,range(y2)]
	dat[state==st[1],plot(Date,y2,type="l",ylim=r,main="z-score of deviation from\n time (dummy) trend",ylab="z'")]
	for (i in st[-1]) dat[state==i,lines(Date,y2,ylab="")]

	par(mfcol=c(1,1))
	dev.off()
}

#' Plot different Lincoln Inst. House Values
#'
#' @examples
#' data(HomeValues,package="EconData")
#' dd <- HomeValues[State %in% sample(unique(State),size=20)]
#' plot.LincolnHomeValues(dd)
plot.LincolnHomeValues <- function(dat,saveto="~/Dropbox/mobility/output/model/factor"){

	# detrend data
	setnames(dat,"Home.Price.Index","hpi")

	detmod <- lm(hpi ~ qtr*State,data=dat)
	dat[,detr := residuals(detmod)]


	# demean series
	dat[,y1 := .SD[,(hpi- mean(hpi))/sd(hpi)], by=State]
	dat[,y2 := .SD[,(detr- mean(detr))/sd(detr)], by=State]
	zmod <- dat[,list(mean=mean(detr),sd=sd(detr)),by=State]

	# plot all
	st = dat[,unique(State)]

	pdf(file=file.path(saveto,"LincolnHPI.pdf"))
	par(mfcol=c(2,2))


	r <- dat[,range(hpi)]
	dat[State==st[1],plot(qtr,hpi,type="l",ylim=r,main="observed index")]
	for (i in st[-1]) dat[State==i,lines(qtr,hpi)]

	r <- dat[,range(detr)]
	dat[State==st[1],plot(qtr,detr,type="l",ylim=r,main="detrended index\n(time dummies)")]
	for (i in st[-1]) dat[State==i,lines(qtr,detr)]

	r <- dat[,range(y1)]
	dat[State==st[1],plot(qtr,y1,type="l",ylim=r,main="z-score of observed index",ylab="z")]
	for (i in st[-1]) dat[State==i,lines(qtr,y1,ylab="")]

	r <- dat[,range(y2)]
	dat[State==st[1],plot(qtr,y2,type="l",ylim=r,main="z-score of deviation from\n time (dummy) trend",ylab="z'")]
	for (i in st[-1]) dat[State==i,lines(qtr,y2,ylab="")]

	par(mfcol=c(1,1))
	dev.off()
	
	# same for HomeValues
	# ===================

	# with and without inflation adustment

	# without
	detmod <- lm(Home.Value ~ qtr*State,data=dat)
	dat[,detr := residuals(detmod)]


	# demean series
	dat[,y1 := .SD[,(Home.Value- mean(Home.Value))/sd(Home.Value)], by=State]
	dat[,y2 := .SD[,(detr- mean(detr))/sd(detr)], by=State]
	zmod <- dat[,list(mean=mean(detr),sd=sd(detr)),by=State]

	# plot all
	st = dat[,unique(State)]

	pdf(file=file.path(saveto,"LincolnHomeValues.pdf"))
	par(mfcol=c(2,2))


	r <- dat[,range(Home.Value)]
	dat[State==st[1],plot(qtr,Home.Value,type="l",ylim=r,main="observed values",ylab="current dollars")]
	for (i in st[-1]) dat[State==i,lines(qtr,Home.Value,ylab="")]

	r <- dat[,range(detr)]
	dat[State==st[1],plot(qtr,detr,type="l",ylim=r,main="detrended values\n(time dummies)",ylab="current dollars")]
	for (i in st[-1]) dat[State==i,lines(qtr,detr,ylab="")]

	r <- dat[,range(y1)]
	dat[State==st[1],plot(qtr,y1,type="l",ylim=r,main="z-score of observed values",ylab="z")]
	for (i in st[-1]) dat[State==i,lines(qtr,y1,ylab="")]

	r <- dat[,range(y2)]
	dat[State==st[1],plot(qtr,y2,type="l",ylim=r,main="z-score of value deviation \nfrom time (dummy) trend",ylab="z'")]
	for (i in st[-1]) dat[State==i,lines(qtr,y2,ylab="")]

	par(mfcol=c(1,1))
	dev.off()


	# with

	data(CPIHOSSL,package="EconData")
	cpi.h <- xts::to.quarterly(CPIHOSSL)[,1]
		
	coredata(cpi.h) <- coredata(cpi.h) / as.numeric(cpi.h['1996-01-01'])
	names(cpi.h) <- "cpiH"

	cpi <- data.table(qtr=index(cpi.h),cpiH=coredata(cpi.h),key="qtr")
	setnames(cpi,c("qtr","cpiH"))
	setkey(cpi,qtr)
	setkey(dat,qtr)

	dat <- cpi[dat]
	dat[,HValue96 := Home.Value / cpiH ]

	detmod <- lm(HValue96 ~ qtr*State,data=dat)
	dat[,detr := residuals(detmod)]


	# demean series
	dat[,y1 := .SD[,(HValue96- mean(HValue96))/sd(HValue96)], by=State]
	dat[,y2 := .SD[,(detr- mean(detr))/sd(detr)], by=State]
	zmod <- dat[,list(mean=mean(detr),sd=sd(detr)),by=State]

	# plot all
	st = dat[,unique(State)]

	pdf(file=file.path(saveto,"LincolnHomeValuesInflation.pdf"))
	par(mfcol=c(2,2))


	r <- dat[,range(HValue96)]
	dat[State==st[1],plot(qtr,HValue96,type="l",ylim=r,ylab="1996 dollars")]
	title(main="observed values",sub="inflation adjusted")
	for (i in st[-1]) dat[State==i,lines(qtr,HValue96,ylab="")]

	r <- dat[,range(detr)]
	dat[State==st[1],plot(qtr,detr,type="l",ylim=r,ylab="1996 dollars")]
	title(main="detrended values\n(time dummies)",sub="inflation adjusted")
	for (i in st[-1]) dat[State==i,lines(qtr,detr,ylab="")]

	r <- dat[,range(y1)]
	dat[State==st[1],plot(qtr,y1,type="l",ylim=r,ylab="z")]
	title(main="z-score of observed values",sub="inflation adjusted")
	for (i in st[-1]) dat[State==i,lines(qtr,y1,ylab="")]

	r <- dat[,range(y2)]
	dat[State==st[1],plot(qtr,y2,type="l",ylim=r,ylab="z'")]
	title(main="z-score of value deviation \nfrom time (dummy) trend",sub="inflation adjusted")
	for (i in st[-1]) dat[State==i,lines(qtr,y2,ylab="")]

	par(mfcol=c(1,1))
	dev.off()
}


#' construct a numeric matrix with time varying across columns
#'
#' @param dat data.frame in long format with columns y, quarter and state
#' @param demean TRUE/FALSE
#' @param detrend TRUE/FALSE
#' @return list with detrended data matrix and data.table with means and sds
makeTimeMatrix <- function(dat,detrend=TRUE,demean=TRUE){

	stopifnot( c("y","quarter","state") %in% names(dat) )

	# detrend data
	if (detrend){
		detmod <- lm(y ~ quarter*state,data=dat)
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

	# plot all
	st = dat[,unique(state)]

	# bring into right shape
	m <- reshape(dat[,list(state,quarter,y)],timevar="quarter",idvar="state",direction="wide")
	st <- m[,state]
	ti <- dat[,unique(quarter)]

	m[,state:=NULL]
	m <- as.matrix(m)

	rownames(m) <- st
	colnames(m) <- ti

	out <- list(m=m,detmod=detmod,zmod=zmod)
	return(out)

}
	


#' Dynamic Factor model for Lincoln Home Values
#'
#' @examples
#' data(HomeValues,package="EconData")
#' dat <- HomeValues
#' data(CPIHOSSL,package="EconData")
#' cpi.h <- xts::to.quarterly(CPIHOSSL)[,1]
#' coredata(cpi.h) <- coredata(cpi.h) / as.numeric(cpi.h['1996-01-01'])
#' names(cpi.h) <- "cpiH"
#' cpi <- data.table(qtr=index(cpi.h),cpiH=coredata(cpi.h),key="qtr")
#' setnames(cpi,c("qtr","cpiH"))
#' setkey(cpi,qtr)
#' setkey(dat,qtr)
#' dat <- cpi[dat]
#' dat[,HValue96 := Home.Value / cpiH ]
#' setnames(dat,c("qtr","HValue96","State"),c("quarter","y","state"))
#' m <- dynPrices(dat=dat,facs=3,maxite=300,demean=TRUE,detrend=TRUE,meth="BFGS")
runDynFactorsLincoln <- function(){
	NULL
}



#' MARSS Dynamic Factor Model for House Prices
#'
#' estimates a dynamic factor model
#' 
#' @param dat dataset of House Prices
#' @param facs number of hidden factors
#' @examples
#' data(FHFA_states,package="EconData")
#' dd <- FHFA.states$qtr[state %in% sample(unique(state),size=20)]
#' l <- dynPrices(dat=dd,facs=3,maxite=300,demean=TRUE,detrend=TRUE,meth="BFGS")
#' sim <- MARSSsimulate(l,tSteps=20,nsim=1)
#' matplot(t(sim$sim.states[ , , 1]),type="l",main="simulated hidden factors")
#' matplot(t(sim$sim.data[ , , 1]),type="l",main="simulated outcomes")
dynPrices <- function(dat,facs,maxite=50,detrend=TRUE,demean=TRUE,meth="kem"){

	mmat <- makeTimeMatrix(dat,detrend,demean)

	m <- mmat$m

	# plot for 6 states
	#sst <- sample(st,5)

	#par(mfcol=c(3,2))
	#for (i in sst){
		#plot(m[i,],xlab="",ylab="hpi",bty="L",pch=16,col="blue",type="b",main=i)
	#}
	#par(mfcol=c(1,1))

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

	return(mod)

}


















































	
