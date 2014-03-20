


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
makeTimeMatrix <- function(dat,trans){

	stopifnot( c("y","date","state") %in% names(dat) )

	w <- copy(dat)
	setkey(w,state,date)
	#w[,y := residuals( lm(y~date ) )]


	# z transform and growth

	if (trans=="ztransform"){



		w[,z := .SD[,(y - mean(y))/sd(y)], by=state]
		w[,dz := .SD[,diff(z)] ,by=state ]
		attr(w,"trans") <- trans
		m <- reshape(w[,list(state,date,dz)],timevar="date",idvar="state",direction="wide")

	} else if (trans=="pct.increase") {

		# compute percentage change of y
		w[,dz  := .SD[, diff(y)/y[-length(y)]] ,by=state]
		attr(w,"trans") <- trans
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
	
	
#' inverse diff function
#'
#' performs revers operation of \code{diff}
idiff <- function(x0=0,x){

	y <- c(x0,rep(0,length(x)))

	for (i in 1:length(x)){

		y[i+1] <- y[i] + x[i]

	}
	return(y)
}

	
#' grow x by a percentage increase
#'
ipercent <- function(x0=0,x){

	y <- c(x0,rep(0,length(x)))

	for (i in 1:length(x)){

		y[i+1] <- y[i]*(1 + x[i])

	}
	return(y)
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
	data(US_states,package="EconData")
	US_states[,c("FIPS","STATE","Reg_ID","Div_ID","Region") := NULL]
	setkey(US_states,state)

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
		setkey(dat,State)
		dat <- US_states[dat]
		setkey(dat,state,qtr)
		setnames(dat,c("qtr","HValue96"),c("date","y"))

	} else if (freq=="yearly") {

		# must agg homevalues first
		dat <- dat[,list(Home.Value=mean(Home.Value),hpi=mean(Home.Price.Index)),by=list(State,year(qtr))]

	
		cpi.h <- xts::to.yearly(CPIHOSSL)[,1]
		coredata(cpi.h) <- coredata(cpi.h) / as.numeric(cpi.h['1996'])
		names(cpi.h) <- "cpiH"
		cpi <- data.table(year=year(index(cpi.h)),cpiH=coredata(cpi.h),key="year")
		setnames(cpi,c("year","cpiH"))
		setkey(cpi,year)
		setkey(dat,year)
		dat <- cpi[dat]
		dat[,HValue96 := Home.Value / cpiH ]
		setkey(dat,State)
		dat <- US_states[dat]
		setkey(dat,state,year)
		setnames(dat,c("year","HValue96"),c("date","y"))

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
#' @param trans integer which transformation
#' @examples
#' data(HValue96_dynF_yearly)
#' dd <- copy(dat)
#' l <- dynPrices(dat=dd,facs=2,maxite=5000,trans="pct.increase")
#' sim <- MARSSsimulate(l$marss,tSteps=20,nsim=1)
#' #matplot(t(sim$sim.states[ , , 1]),type="l",main="simulated hidden factors")
#' #matplot(t(sim$sim.data[ , , 1]),type="l",main="simulated outcomes")
dynPrices <- function(dat,facs,maxite=50,trans=1,meth="BFGS"){

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
#' div=dat[,list(HV=mean(hpi)),by=list(Division,date)]
#' setnames(div,c("state","date","y"))
#' d <- dynGrowth(div,facs=2,trans="pct.increase",maxite=1000)
#' sim <- MARSSsimulate(d$marss,tSteps=20,nsim=10)
#' #matplot(t(sim$sim.states[ , , 1]),type="l",main="simulated hidden factors")
#' par(mfrow=c(3,3))
#' for (i in 1:9) {matplot(t(sim$sim.data[ , , i]),type="l",main=sprintf("sim number %d",i));abline(a=0,b=0,col="red")}
#' par(mfrow=c(1,1))
dynGrowth <- function(dat,facs,maxite=50,trans="pct.increase",meth="BFGS",rwalks=1){

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
	Q <- diag(1,facs)
	
   # if (rwalks==1){

		#Bvals <- list()

		#for (i in 1:facs){
			#for (j in 1:facs){
				#if (i==1){
					#if (j==1){
						#Bvals[[i+(j+1)*facs]] <- 1
					#} else {

						#Bvals[[i+(j+1)*facs]] <- 0
					#}
				#} else if (i==j){
						
					#Bvals[[i+(j+1)*facs]] <- paste0("b",i,j)

				#} else {
					#Bvals[[i+(j+1)*facs]] <- 0
				#}
			#}
		#}
		#B <- matrix(Bvals,facs,facs)

	#} else {

		B <- diag(1,facs)

	#}

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
backOutLevels <- function(l,n,N=1,x0=1,saveto=NULL){

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
	dimnames(si) = list(state=st,date=dates,sim=paste0("simulation ",1:N))
	dimnames(sim$sim.data) = list(state=st,date=dates,sim=paste0("simulation ",1:N))


	if (attr(l$datmod$w,"trans")=="pct.increase"){

		# model simulate percentage increases
		# construct levels by going forward on x0

		z <- array(0,dim(si))
		dimnames(z) = list(state=st,date=dates,sim=paste0("simulation ",1:N))
		z1 <- array(0,dim(si))
		dimnames(z1) = list(state=st,date=dates,sim=paste0("simulation ",1:N))

		if (length(x0)==1) x0 <- rep(x0,length(st))
		names(x0) <- st

		# for each simulation
		for (nS in 1:N){

			# for each state
			for (iS in st){

				z[iS, ,nS] <- ipercent(x0=x0[iS],x=si[iS, ,nS])[-1]
				z1[iS, ,nS] <- z[iS, ,nS]

			}
		}
		# factors
		facs <- sim$sim.states
		dimnames(facs) = list(fac=paste0(1:nrow(l$marss$states)),date=dates,sim=paste0("simulation ",1:N))

		# add linear trend to that, if you had one
		# newdata for prediction

		# melt
		m <- melt(z)
		msi <- melt(si)
		mf <- melt(facs)
		out <- list(arr=z,arr1=z1,sim=si,molten=m,factors=facs)

		# plots
		out$t <- ggplot(l$datmod$w,aes(x=date,y=y,color=state))+ geom_line()  + ggtitle('True levels')
		out$dt <- ggplot(l$datmod$w,aes(x=date,y=dz,color=state))+ geom_line() + ggtitle('True Percentage Growth')
		out$p <- ggplot(m,aes(x=date,y=value,color=state)) + geom_line() + facet_wrap(~sim,scales="free_y")  + ggtitle('Simulated levels')
		out$si <- ggplot(msi,aes(x=date,y=value,color=state)) + geom_line() + facet_wrap(~sim,scales="free_y") + geom_hline(yintercept=0) + ggtitle('Simulated Percentage Growth')
		out$f <- ggplot(mf,aes(x=date,y=value,color=factor(fac))) + geom_line() + facet_wrap(~sim)

		if (!is.null(saveto)){
			pdf(saveto,width=20,heigh=16)
			multiplot(plotlist=out[c("t","dt","p","si")],layout=matrix(1:4,nrow=2,byrow=T))
			dev.off()
		} else {
			out$mplot <- multiplot(plotlist=out[c("t","dt","p","si")],layout=matrix(1:4,nrow=2,byrow=T))
		}

		return(out)

	} else {

		z <- si

	}


}





#' objective function to estimate a model of regional price growth
#'
growthobj <- function(dat,pars,N){

	# pars = [beta, sigma1, ... , sigmaK]
	pars <- as.numeric(pars)
	pdt <- data.table(cd=dat[cd!="USA",unique(cd)])
	pdt[,sigma := pars[-1]]
	pdt[,beta  := pars[1]]
	setkey(pdt,cd)

	t0 <- copy(dat[cd!="USA"])
	t0 <- t0[,list(dmaxdev=max(dx),dmindev=min(dx),dmeandev=mean(dx)),by=cd]


	# simulate N datasets
	sdat <- copy(dat[cd!="USA"])
	sdat[,reps := N]

	setkey(sdat,reps)
	sdat <- sdat[J(rep(unique(reps),unique(reps))),allow.cartesian=TRUE]

	sdat[,smple := rep(1:N,each=nrow(dat[cd!="USA"]))]

	sdat[,reps:=NULL]

	
	# draw shocks
	setkey(sdat,cd) 

	sdat <- sdat[pdt]

	setkey(sdat,yr,cd)

	sdat[,eps :=0]

	for (iyr in 1992:2013){

		for (icd in sdat[,unique(cd)]){

			sdat[list(iyr,icd),eps := rnorm(n=N,mean= sdat[list(iyr-1,icd)][,beta*eps], sd=sigma )]
		}

	}

	t1 <- sdat[,list(maxdev=max(eps),mindev=min(eps),meandev=mean(eps)),by=cd]

	setkey(t1,cd)
	setkey(t0,cd)

	t2 <- t1[t0]
	t2[,omaxdev := (maxdev-dmaxdev)^2]
	t2[,omindev := (mindev-dmindev)^2]
	t2[,omeandev := (meandev-dmeandev)^2]
	t2[,alldev := omaxdev + omindev + omeandev]

	obj <- t2[, sum(alldev)]

	return(obj)

}


#' create dataset with HPI difference to national index
#'
#' @examples
#' data(HomeValues,package="EconData")
#' makeHPIDivDifferences(dat)
makeHPIDivDifferences <- function(d){

	us = d[,list(US96=mean(y)),by=date]
	div = d[,list(divmean=mean(y)),by=list(date,Division)]
	setkey(div,date)
	setkey(us,date)
	div=us[div]
	div[,dev := divmean - US96]


	pal <- getcbPalette(n=ncol(p2)-1)
	pal[length(pal)] <- "red"
	p <- ggplot(div,aes(x=date,y=dev,color=Division)) + geom_line() + scale_color_manual(values=pal)

	out <- list(dat=div,p=p)
	return(out)
}














































	
