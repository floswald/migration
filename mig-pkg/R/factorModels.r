


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
#' plot.TimeMatrix(dd)
plot.TimeMatrix <- function(dat,saveto="~/Dropbox/mobility/output/model/factor/fhfa.pdf"){

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

		dat[state==st[1],plot(Date,index_sa,type="l",ylim=c(90,350),main="observed index")]
		for (i in st[-1]) dat[state==i,lines(Date,index_sa)]

		dat[state==st[1],plot(Date,detr,type="l",ylim=c(-100,120),main="detrended index")]
		for (i in st[-1]) dat[state==i,lines(Date,detr)]

		dat[state==st[1],plot(Date,y1,type="l",ylim=c(-3,3),main="z-score of observed index")]
		for (i in st[-1]) dat[state==i,lines(Date,y1)]

		dat[state==st[1],plot(Date,y2,type="l",ylim=c(-3,3),main="z-score of deviation from\n linear trend")]
		for (i in st[-1]) dat[state==i,lines(Date,y2)]

	par(mfcol=c(1,1))
	dev.off()
}


#' construct a numeric matrix with time varying across columns
#'
#' @param dat data.frame in long format from data(FHFA_states,package="EconData")
#' @param demean TRUE/FALSE
#' @param detrend TRUE/FALSE
#' @return list with detrended data matrix and data.table with means and sds
makeTimeMatrix <- function(dat,detrend=TRUE,demean=TRUE){

	# detrend data
	if (detrend){
		detmod <- lm(index_sa ~ quarter*state,data=dat)
		dat[,detr := residuals(detmod)]
	} else {
		dat[,detr := index_sa]
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

	if (demean & detrend){

		dat[state==st[1],plot(Date,y,type="l",ylim=c(-3,3),main="z-score of deviation from linear trend for all states")]
		for (i in st[-1]) dat[state==i,lines(Date,y)]

	} else if (demean & !detrend){

		dat[state==st[1],plot(Date,y,type="l",ylim=c(-3,3),main="z-score of observed index")]
		for (i in st[-1]) dat[state==i,lines(Date,y)]

	} else if (!deman & detrend){
		
		dat[state==st[1],plot(Date,y,type="l",ylim=c(90,350),main="demeaned index")]
		for (i in st[-1]) dat[state==i,lines(Date,y)]

	} else {

		dat[state==st[1],plot(Date,y,type="l",ylim=c(90,350),main="observed index")]
		for (i in st[-1]) dat[state==i,lines(Date,y)]

	}

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


#' MARSS Dynamic Factor Model for Prices
#'
#' estimates a dynamic factor model
#' @param dat dataset from data(FHFA_states,package="EconData")
#' @param facs number of hidden factors
#' @examples
#' data(FHFA_states,package="EconData")
#' dd <- FHFA.states$qtr[state %in% sample(unique(state),size=20)]
#' l <- dynPrices(dat=dd,facs=3,maxite=300,demean=TRUE,detrend=TRUE,meth="BFGS")
#' sim <- MARSSsimulate(l,tSteps=20,nsim=1)
#' matplot(t(sim$sim.states[ , , 1]),type="l",main="simulated hidden factors")
#' matplot(t(sim$sim.data[ , , 1]),type="l",main="simulated outcomes")
dynPrices <- function(dat,facs,maxite=50,detrend=TRUE,demean=TRUE,meth="kem"){

	m <- makeTimeMatrix(dat,detrend,demean)

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


















































	
