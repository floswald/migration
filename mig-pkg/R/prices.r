	


# functions for aggregate prices
# and deviations models





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






#' log differences between US average and location prices from Data
#'
#' create dataset with of log differences between US average and location prices
#' from census median income and Lincoln house values in 96 dollars
#' @family ExpectationsModel
#' @examples
#' data(US_medinc,package="EconData")
#' makeDivDifferences()
makeDivDifferences <- function(path=NULL){

	r <- list()

	pal <- getcbPalette(n=9)
	# add a red color to palette for USA
	pal <- c("red",pal)

	# income
	data(US_medinc_current,package="EconData")
	d <- data.table(medinc_current$inc)

	# in 1996 dollars
	data("CPIAUCSL",package="EconData")
	cpi           <- to.yearly(CPIAUCSL)[,1]
	coredata(cpi) <- coredata(cpi)/ as.numeric(cpi['1996'])
	cpi           <- data.table(Year=year(index(cpi)),cpi=coredata(cpi),key="Year")
	setnames(cpi,"cpi.CPIAUCSL.Open","cpi")


	# must aggregate over those 2 groups of states that are missing
	# huge pain.

	


	d[,State := tolower(State)]
	d[,Year  := as.numeric(as.character(Year))]
	setkey(d,Year)
	d <- cpi[d]
	d[,medinc := medinc / cpi]

	# merge with divisions
	data(US_states,package="EconData")
	US_states[,c(1,4,5,6) := NULL]
	US_states[,STATE := tolower(STATE)]
	setkey(US_states,STATE)
	setkey(d,State)

	d <- US_states[d]
	d[STATE=="     united states",c("state","Division") := list("USA","USA")]
	d[,STATE := NULL]
	
	div <- d[,list(meanmedinc=mean(log(medinc)),minmedinc=min(log(medinc)),maxmedinc=max(log(medinc))),by=list(Year,Division)]

	# normalizing constant for model:
	normalize = d[Year==1996,mean(medinc)]

	# define "deviation" as difference in logs i.e. percentage difference
	div[,dev := meanmedinc - .SD[Division=="USA"][["meanmedinc"]] ]
	div[,Division := factor(Division)]
	div[,Division := relevel(Division,"USA")]
		
	div2 <- div[Division!="USA",list(meddev=mean(dev),mindev=min(dev),maxdev=max(dev)),by=Division]
	div2[,Division := abbreviate(Division,3)]
	setkey(div2,Division)

	# collect results

	r$income <- list()
	r$income$d <- div
	r$income$d2 <- div2

	# plots
	r$income$plevel <- ggplot(div,aes(x=Year,y=meanmedinc,color=Division,size=Division)) + geom_line() + scale_color_manual(values=pal) + ggtitle('median log income levels') + scale_size_manual(values=c(1.5,rep(1,9)))+ theme_bw()

	r$income$pdevs  <- ggplot(div,aes(x=Year,y=dev,color=Division,size=Division)) + geom_line() + scale_color_manual(values=pal) + ggtitle('percent deviations from US median log income') + scale_size_manual(values=c(1.5,rep(1,9))) + theme_bw()


	# Lincoln House values in 96 dollars
	# TODO you may want to change that series
	data(HValue96_dynF_yearly)
	divH <- dat[,list(p=mean(log(y))),by=list(date,Division)]

	# national house value
	US <- divH[,list(p=mean(p)),by=date]
	US[,Division:="USA"]

	divH <- rbind(divH,US,use.names=TRUE)
	divH[,Division := factor(Division)]
	divH[,Division := relevel(Division,"USA")]
	divH[,dev := p - .SD[Division=="USA"][["p"]] ]

	r$price <- list()
	r$price$meanstate <- dat[,list(mean=mean(log(y)),min=min(log(y)),max=max(log(y))),by=state]
	div2 <- div[Division!="USA",list(meddev=mean(dev),mindev=min(dev),maxdev=max(dev)),by=Division]
	div2[,Division := abbreviate(Division,3)]
	setkey(div2,Division)
	r$price$meandiv <- div2
	r$price$d <- divH

	r$price$plevel <- ggplot(divH,aes(x=date,y=p,color=Division,size=Division)) + geom_line() + scale_color_manual(values=pal) + ggtitle('mean log house values') + scale_size_manual(values=c(1.5,rep(1,9)))+ theme_bw()

	r$price$pdevs  <- ggplot(divH,aes(x=date,y=dev,color=Division,size=Division)) + geom_line() + scale_color_manual(values=pal) + ggtitle('percent deviation from national mean house value') + scale_size_manual(values=c(1.5,rep(1,9))) + scale_y_continuous(name="log(p_d) - log(P)")+ theme_bw()


	#  US national price to income ratio
	# ==================================

	r$price$p2y = data.table(year=seq(from=as.Date("1984-1-1"),to=as.Date("2012-1-1"),by="year"),p2y=r$price$d[date>1983 & date<2013 & Division=="USA", exp(p)] / r$inc$d[Division=="USA",exp(meanmedinc)])

	rexp = r$price$p2y
	rexp[,year := year(year)]

	r$price$p_p2y <- ggplot(r$price$p2y,aes(x=year,y=p2y)) + geom_line(size=1.2) + theme_bw() + scale_y_continuous(name="US Price to Income ratio")

	write.csv(r$income$d2,file="~/Dropbox/mobility/output/model/R2julia/divincome.csv")
	write.csv(r$price$meandiv,file="~/Dropbox/mobility/output/model/R2julia/divprice.csv")
	write.csv(rexp,file="~/Dropbox/mobility/output/model/R2julia/p2y.csv")
	write.csv(normalize,file="~/Dropbox/mobility/output/model/R2julia/normalize.csv")

	if (!is.null(path)){



		ggsave(plot=r$price$plevel,filename=file.path(path,"DivisionPriceLevel.pdf"),width=23,height=15,units="cm")
		ggsave(plot=r$price$pdevs,filename=file.path(path,"DivisionPriceDevs.pdf"),width=23,height=15,units="cm")
		ggsave(plot=r$income$plevel,filename=file.path(path,"DivisionIncomeLevel.pdf"),width=23,height=15,units="cm")
		ggsave(plot=r$income$pdevs,filename=file.path(path,"DivisionIncomeDevs.pdf"),width=23,height=15,units="cm")
		ggsave(plot=r$price$p_p2y,filename=file.path(path,"price2income.pdf"),width=23,height=15,units="cm")

	}


	class(r) <- c("list","divData")
	return(r)
}


#' Estimate deviations models
#' 
#' @family ExpectationsModel
estimateDivDeviations <- function(dat,path="~/Dropbox/mobility/output/model/R2julia"){

	stopifnot( is(dat,c("list","divData")) )

	# estimate income AR1s
	r <- list()

	tmp <- copy(dat$income$d[Division!="USA"])
	setkey(tmp,Division,Year)
	tmp[,Ldev :=  tmp[list(Division,Year-1)][["dev"]]]

	tmpl <- lapply(tmp[,unique(Division)], function(x) lm(dev~Ldev,tmp[Division==x]))
	names(tmpl) <- tmp[,unique(Division)]

	tmpd <- data.frame(lapply(tmpl,coef))
	tmpd <- rbind(tmpd,data.frame(lapply(tmpl,function(x) summary(x)$sigma)))
	tmpd <- t(tmpd)
	colnames(tmpd)[3] <- "sigma"

	r$inc <- tmpd

	# estimate prices AR1s
	tmp <- copy(dat$price$d[Division!="USA"])
	setkey(tmp,Division,date)
	tmp[,Ldev :=  tmp[list(Division,date-1)][["dev"]]]

	tmpl <- lapply(tmp[,unique(Division)], function(x) lm(dev~Ldev,tmp[Division==x]))
	names(tmpl) <- tmp[,unique(Division)]

	tmpd <- data.frame(lapply(tmpl,coef))
	tmpd <- rbind(tmpd,data.frame(lapply(tmpl,function(x) summary(x)$sigma)))
	tmpd <- t(tmpd)
	colnames(tmpd)[3] <- "sigma"

	r$price <- tmpd

	write.csv(r$price,file=file.path(path,"rho-price.csv"))
	write.csv(r$inc,file=file.path(path,"rho-income.csv"))



	class(r) <- c("list","divModels")

	return(r)

}


#' Simulate data from Deviation Models
#'
#' @family ExpectationsModel
simDivDeviations <- function(d,n=20,N=1,burn){

	stopifnot(  is(d,c("list","divModels")) )

	r <- list()

	r$inc <- lapply(rownames(d$inc), function(x) replicate(n=N, simAR1(x0=0,n,intercept=d$inc[x,"(Intercept)"],rho1=d$inc[x,"Ldev"],sigma=d$inc[x,"sigma"],burnIn=burn) ) )
	names(r$inc) <- rownames(d$inc)

	r$price <- lapply(rownames(d$price), function(x) replicate(n=N, simAR1(x0=0,n,intercept=d$price[x,"(Intercept)"],rho1=d$price[x,"Ldev"],sigma=d$price[x,"sigma"],burnIn=burn) ))
	names(r$price) <- rownames(d$price)

	return(r)
}


#' Build Deviations Models
#'
#' @family ExpectationsModel
buildDivDeviations <- function(n=20,N=2,burn=40){

	d <- makeDivDifferences()
	e <- estimateDivDeviations(d)
	s <- simDivDeviations(d=e,n,N,burn)

	return(s)
}







#' Build aggregate price series
#'
#' takes two series for log income 
#' and log house values at national
#' level and constructs census division
#' prices and income from simulations
#'
#' @param agg data.frame with columns
#' \code{price} and \code{income}
#' @family ExpectationsModel
#' @examples
#' a <- makeAggPricesFromData(N=3)
#' buildAggregatePrices(agg=a)
buildAggregatePrices <- function(agg){

	n <- dim(agg)[1]
	N <- dim(agg)[3]

	s <- buildDivDeviations(n,N)

	r <- list()

	r$inc <- lapply(s$inc,function(x){ agg[ ,"medinc", ] - x }) 
	r$price <- lapply(s$price,function(x){ agg[ ,"p", ] - x  }) 

	return(r)

}


#' make aggregate prices from real data
#'
#' @param N number of simulations
#' @family ExpectationsModel
makeAggPricesFromData <- function(N){

   x   <- makeDivDifferences()
   agg <- x$income$d[Division=="USA",list(date=Year,medinc)][order(date)]
   p   <- x$price$d[Division=="USA",list(date,p)]
   setkey(agg,date)
   setkey(p,date)
   agg <- p[agg]
   agg <- replicate(n=N,as.matrix(agg))
   return(agg)
}






#' simulate univariate AR1/AR2
#'
simAR1 <- function(x0,n,intercept=0,rho1,rho2=0,mu=0,sigma,shocks=NULL,burnIn=NULL){

	if (!is.null(burnIn)){
		n <- n+burnIn
	}

	x <- rep(0,n)
	if (intercept!=0){
		#x[1] <- intercept / (1-rho1-rho2)
		# doing this because South.Atlantic has crazy mean (near unit root)
		# if I dont.
		x[1] <- intercept 
	} else {
		x[1] <- x0 
	}
	if (is.null(shocks)){
		shocks <- rnorm(n=n,mean=mu,sd=sigma)
	}

	if (rho2!=0){
		x[2] <- x0
		for (i in 3:n) x[i] <- intercept + rho1*x[i-1] + rho2*x[i-2]+ shocks[i]
	} else {
		for (i in 2:n) x[i] <- intercept + rho1*x[i-1] + shocks[i]
	}


	if (!is.null(burnIn)) x <- x[-(1:burnIn)]

	return(x)
}

#' compare AR1 and AR2 simulations
#'
compareAR1andAR2 <- function(n){

	pars <- list(ar1=c(0.9,0),ar2=c(0.9,-0.3),ar1.2=c(0.7,0),ar2.2=c(0.7,0.3))
	shocks <- list()
	shocks[[1]] <- rnorm(n,mean=0,sd=1)
	shocks[[2]] <- shocks[[1]]
	shocks[[3]] <- rnorm(n,mean=0,sd=1)
	shocks[[4]] <- shocks[[3]]

	p <- list()
	for (i in 1:4){
		p[[i]] <- as.ts(simAR1(x0=0,n,intercept=0,rho1=pars[[i]][1],rho2=pars[[i]][2],mu=0,sigma=1,shocks[[i]]))
	}

	names(p) <- names(pars)



	par(mfrow=c(2,2))
	lapply(names(p),function(x) plot(p[[x]],main=sprintf("rho1=%g,rho2=%g",pars[[x]][1],pars[[x]][2])))
	par(mfrow=c(1,1))

	return(p)
}

#' estimate Division Deviations models
#'
divDeviations <- function(){

	r <- list()

	# load division house value data
	data(HValue96_dynF_yearly)
	hv <- makeHPIDivDifferences(dat)

	# estimate linear model for each d
	r$houses <- list()

	# load division median income data
	data(US_medinc_2012,package="EconData")
	m <- medinc_2012$zinc

	# estimate linear model for each d
	r$incomes <- list()

	# define class

	class(r) <- c("list","devmodels")

	# return
	return(r)

}
