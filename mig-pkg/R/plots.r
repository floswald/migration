

#' Multiple plot function
#
#' ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#' 
#' @param plotlist list of ggplots
#' @param cols Number of columns in layout
#' @param layout matrix specifying the layout. If present, 'cols' is ignored.
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#' @author http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL) {

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# plotting functions




#' Plot median incomes
#'
plotMedianIncome <- function(saveto="~/Dropbox/mobility/output/data/census"){

	data(US_medinc_2012,package="EconData")
	data(US_states,package="EconData")

	d <- data.table(medinc_2012$incl)
	d[, State := tolower(State)]
	setnames(d,"State","STATE")
	setkey(d,STATE)

	s <- US_states[,list(STATE,state,Region,Division)]
	s[,STATE := tolower(STATE)]
	s <- s[complete.cases(s)]
	setkey(s,STATE)

	# merge
	d <- d[s]
	d[,Date := as.Date(paste(Year,"01","01",sep="-"))]


	# aggregate
	reg <- d[,list(medinc=mean(medinc)),by=list(Region,year(Date))]
	div <- d[,list(medinc=mean(medinc)),by=list(Division,year(Date))]
	reg[,Date := as.Date(paste(year,"01","01",sep="-"))]
	div[,Date := as.Date(paste(year,"01","01",sep="-"))]

	# normal simulation
	d[,c("meanY","sdY") := list(mean(medinc),sd(medinc)),by=state]
	d[,sim := rnorm(n=1,mean=meanY,sd=sdY),by=list(state,Date)]
	setkey(d,state,Date)

	# plots
	plist <- list()
	tstring <- "median income 1000s 2012$"
	yscale <- scale_y_continuous(name=tstring,limits=d[,range(medinc/1000)])
	yscale2 <- scale_y_continuous(name=tstring)

	cbPalette <- c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
	plist$regs <- ggplot(reg,aes(x=Date,y=medinc/1000,color=Region,group=Region)) + geom_line(size=1)+ ggtitle(tstring)  + yscale2+theme_bw()
	plist$divs <- ggplot(div,aes(x=Date,y=medinc/1000,color=Division)) + geom_line(size=1)+ ggtitle(tstring)  + yscale2 + scale_color_manual(values=cbPalette)+theme_bw()

	plist$divstate <- lapply(d[,unique(Division)], function(x) ggplot(d[Division==x], aes(x=Date,y=medinc/1000,color=state)) + geom_line(size=1)+ ggtitle(x) + yscale +theme_bw())
	plist$sim_divstate <- lapply(d[,unique(Division)], function(x) ggplot(d[Division==x], aes(x=Date,y=sim/1000,color=state)) + geom_line(size=1)+ ggtitle(x) +theme_bw()+ scale_y_continuous(name="simulate income",limits=d[,range(medinc/1000)]) )
	plist$regstate <- lapply(d[,unique(Region)], function(x) ggplot(d[Region==x], aes(x=Date,y=medinc/1000,color=state)) + geom_line(size=1)+ ggtitle(x) +theme_bw() + yscale)

	pdf(file=file.path(saveto,"reg.pdf"))
	print(plist$regs)
	dev.off()
	
	pdf(file=file.path(saveto,"divs.pdf"))
	print(plist$divs)
	dev.off()

	pdf(file=file.path(saveto,"regstate.pdf"),width=12,height=9)
	multiplot(plotlist=plist$regstate,layout=matrix(1:6,nrow=2,byrow=TRUE))
	dev.off()
	
	pdf(file=file.path(saveto,"divstate.pdf"),width=12,height=9)
	multiplot(plotlist=plist$divstate,layout=matrix(1:9,nrow=3,byrow=TRUE))
	dev.off()

	pdf(file=file.path(saveto,"sim_divstate.pdf"),width=12,height=9)
	multiplot(plotlist=plist$sim_divstate,layout=matrix(1:9,nrow=3,byrow=TRUE))
	dev.off()


	return(plist)

}
	

#' Analyse median Income statistically
#'
#' @param saveto
analyzeMedianIncome <- function(demean=FALSE,division=FALSE,saveto="~/Dropbox/mobility/output/data/census"){

	data(US_medinc,package="EconData")
	data(US_states,package="EconData")

	d <- data.table(medinc.in2012$incl)
	d[, State := tolower(State)]
	setnames(d,"State","STATE")
	setkey(d,STATE)

	s <- US_states[,list(STATE,state,Region,Division)]
	s[,STATE := tolower(STATE)]
	s <- s[complete.cases(s)]
	setkey(s,STATE)

	# merge
	d <- d[s]
	d[,year := as.numeric(as.character(Year))]

	# division
	if (division) {
		d <- d[,list(medinc=mean(medinc)),by=list(year,Division)]
		setnames(d,"Division","state")
	}

	if (demean) {
		d[,USmean := mean(medinc),by=year]
		d[,medinc := medinc - .SD[,USmean],by=state]
	}
	

	setkey(d,state,year)
	d[,Ly := d[list(state,year-1)][["medinc"]] ]

	l <- lapply(d[,unique(state)], function(j) lm(medinc~Ly,d[state==j]))
	names(l) <- d[,unique(state)]

	adf0 <- lapply(d[,unique(state)], function(j) d[state==j,tseries::adf.test(medinc,k=1,alternative="stationary")])
	adf1 <- lapply(d[,unique(state)], function(j) d[state==j,tseries::adf.test(medinc,k=1,alternative="explosive")])
	adf <- data.frame(state=names(l), Pval.H0.stationary=unlist(lapply(adf0,function(x) x$p.value)), Pval.H0.explosive=unlist(lapply(adf1,function(x) x$p.value)))

	df <- data.frame(state=names(l),intercept=unlist(lapply(l,function(x) coef(x)[1])),slope=unlist(lapply(l,function(x) coef(x)[2])),slope.pvalue=unlist(lapply(l,function(x) coef(summary(x))["Ly","Pr(>|t|)"])),r.squared=unlist(lapply(l,function(x) summary(x)[["r.squared"]])))
	rownames(df) <- NULL

	if (!is.null(saveto)){

		print(xtable(df,digits=c(1,1,2,2,4,2)),include.rownames=FALSE,file=file.path(saveto,"IncomeAR1Reg.tex"),floating=FALSE,booktabs=TRUE)
		print(xtable(adf,digits=c(1,1,3,3)),include.rownames=FALSE,file=file.path(saveto,"Dickey-Fuller.tex"),floating=FALSE,booktabs=TRUE)

	}

	return(list(d=d,df=df,adf=adf,l=l))
}



#' Simulate AR1 models from \code{\link{analyzeMedianIncome}}
#'
simMedianIncome <- function(x,n){

	l <- x$l

	simAR1 <- function(b0=0,b1,sigma,n){

		r <- rep(b0,n)
		for (i in 2:n){
			r[i] <- b0 + b1*r[i-1] + rnorm(n=1,mean=0,sd=sigma)
		}
		return(r)
	}

	r <- as.data.frame(lapply(l, function(z) simAR1(b0=coef(z)[[1]],b1=coef(z)[[2]],sigma=summary(z)$sigma,n) ))

	return(r)
}





#' Make Tables with House Value stats
#'
makeTablesHValue96 <- function(path="~/Dropbox/mobility/output/data/Lincoln"){
	data(HValue96_dynF_yearly)
	d <- copy(dat) 
	data(US_states,package="EconData")
	US_states[,c("FIPS","STATE","Reg_ID","Div_ID") := NULL]
	setkey(d,state)
	setkey(US_states,state)
	# merge
	d <- US_states[d]
	setkey(d,state,date)
	d[,dHomeval := c(diff(y),NA),by=state]
	d[,logdHomeval := c(diff(log(y)),NA),by=state]
	d[,divVal := mean(y),by=list(date,Division)]

	ta <- list()
	ta$growth <- d[,list(bound=c("lower","upper"),level=range(y,na.rm=T),growth=range(dHomeval,na.rm=T),loggrowh=range(logdHomeval,na.rm=T),meanloggrowth=mean(logdHomeval,na.rm=T),varloggrowth=var(logdHomeval,na.rm=T)),by=Division]
						  

	ta$means <- d[,.SD[,list(MeanDevFromDiv=mean(y)-mean(divVal)),by=state],by=Division]

	print(xtable(ta$growth,digits=c(1,1,1,0,0,3,4,4)),include.rownames=FALSE,file=file.path(path,"growthHValue96.tex"),floating=FALSE,booktabs=TRUE)
	print(xtable(ta$means),include.rownames=FALSE,digits=4,file=file.path(path,"meanHValue96.tex"),floating=FALSE,booktabs=TRUE)

	# make some acf plots as well
	div = d[,list(HV = mean(Home.Value)),by=list(date,Division)]
	pdf(file.path(path,"Div_PACF.pdf"))
	par(mfcol=c(3,3))
	for (i in div[,unique(Division)]){
		div[Division==i,acf(diff(HV),type="partial",main=paste0("growth in ",i))]
	}
	par(mfcol=c(1,1))
	dev.off()

	return(ta)
}









#' Plot different house price index series
#'
#' groups series by states, region and division and plots
#' 
#' @param d dataset. must contain columns Date, State and a response variable
#' @param response string of response var name
#' @param saveto location where to save plots
#' @param FD wether to first difference the series
#' @return list of ggplots
#' @examples
#' ## FHFA indices
#' data(FHFA_states,package="EconData")
#' d <- copy(FHFA_states$qtr)
#' plotHousePrices(d=d,response="index_sa")
#' rm(d)
#' ## Lincoln House values
#' data(HValue96_dynF_quarterly)
#' d <- copy(dat)
#' d[,date := as.Date(date)]
#' setnames(d,"y","HValue96")
#' plotHousePrices(d=d,response="HValue96",saveto="~/Dropbox/mobility/output/data/Lincoln/")
plotHousePrices <- function(d,response,FD=FALSE,saveto="~/Dropbox/mobility/output/data/FHFA/"){

	
	setnames(d,names(d),tolower(names(d)))
	response <- tolower(response)
	stopifnot(c("state","date") %in% names(d))
	stopifnot( !(c("y") %in% names(d)))

	setnames(d,response,"y")

	# group by census division

	data(US_states,package="EconData")
	US_states[,c("FIPS","STATE","Reg_ID","Div_ID") := NULL]

	setkey(d,state)
	setkey(US_states,state)

	# merge
	d <- US_states[d]
	setkey(d,state,date)

	# don't do any first differencing


	d[,FDindex := c(diff(y),NA),by=state]
	d <- d[complete.cases(d)]

	div <- d[,unique(Division)]
	reg <- d[,unique(Region)]

	plist <- list()

	# my color scale
	cbPalette <- c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

	# compare divisions
	dd <- d[,list(y= mean(y),FDindex=mean(FDindex)),by=list(Division,date)]
	plist$divs <- ggplot(dd,aes(x=date,y=y,color=Division)) + geom_line(size=1) + theme_bw() + ggtitle(paste0(response,' by Census Division')) + scale_color_manual(values=cbPalette)

	pdf(file=file.path(saveto,"div.pdf"))
	print(plist$divs)
	dev.off()


	# compare regions
	dd <- d[,list(y = mean(y),FDindex=mean(FDindex)),by=list(Region,date)]
	plist$regs <- ggplot(dd,aes(x=date,y=y,color=Region)) + geom_line(size=1) + theme_bw() + ggtitle(paste0(response,' by Census Region') )

	pdf(file=file.path(saveto,"reg.pdf"))
	print(plist$regs)
	dev.off()


	# plots grouping states by divisions
	yscale <- scale_y_continuous(name=response,limits=d[,range(y)] )


	plist$bydiv <- lapply(div, function(z) ggplot(d[Division==z],aes(x=date,y=y,color=state)) + geom_line(size=1) + ggtitle( z )+ theme_bw() + yscale)

	pdf(file=file.path(saveto,"bydiv.pdf"),width=12,height=9)
	multiplot(plotlist=plist$bydiv,layout=matrix(1:9,nrow=3,byrow=TRUE))
	dev.off()


	# plots grouping states by regions
	plist$byreg <- lapply(reg, function(z) ggplot(d[Region==z],aes(x=date,y=y,color=state)) + geom_line(size=1) + ggtitle( z )+ theme_bw() + yscale)

	pdf(file=file.path(saveto,"byreg.pdf"),width=12,height=9)
	multiplot(plotlist=plist$byreg,layout=matrix(1:4,nrow=2,byrow=TRUE))
	dev.off()


	# first differencing plots

	if (FD){

		# compare FD of divisions
		plist$FDdivs <- ggplot(dd,aes(x=date,y=FDindex,color=Division)) + geom_line(size=1) + theme_bw() + ggtitle(paste0('FD of ',response,' by Census Division') )

		pdf(file=file.path(saveto,"FDdiv.pdf"))
		print(plist$FDdivs)
		dev.of()

		# FD
		plist$FDregs <- ggplot(dd,aes(x=date,y=FDindex,color=Region)) + geom_line(size=1) + theme_bw() + ggtitle(paste0('FD of ',response,' by Census Region') )

		pdf(file=file.path(saveto,"FDreg.pdf"))
		print(plist$FDregs)
		dev.off()
		# FD
		
		plist$FDbydiv <- lapply(div, function(z) ggplot(d[Division==z],aes(x=date,y=FDindex,color=state)) + geom_line(size=1) + ggtitle( z )+ theme_bw())

		pdf(file=file.path(saveto,"FDbydiv.pdf"),width=14,height=8)
		multiplot(plotlist=plist$FDbydiv,layout=matrix(1:9,nrow=3,byrow=TRUE))
		dev.off()


		# FD
		plist$FDbyreg <- lapply(reg, function(z) ggplot(d[Region==z],aes(x=date,y=FDindex,color=state)) + geom_line(size=1) + ggtitle( z )+ theme_bw() )

		pdf(file=file.path(saveto,"FDbyreg.pdf"),width=14,height=9)
		multiplot(plotlist=plist$FDbyreg,layout=matrix(1:4,nrow=2,byrow=TRUE))
		dev.off()

	}


	return(plist)

}






#' Plot normalization of Prices
#'
#' @examples
#' data(HValue96_dynF_quarterly)
#' m <- makeTimeMatrix(dat)
#' plotNormalization(d=dat,mod=m$detmod,saveto=NULL)
plotNormalization <- function(d,mod,saveto="~/Dropbox/mobility/output/model/factor"){

	# sample 4 states
	d <- d[state %in% sample(unique(state),size=4)]

	if (!is.null(saveto))	pdf(file=file.path(saveto,"normalization.pdf"))
	par(mfcol=c(2,2))
	for (i in d[,unique(state)] ){

		newd <- d[state==i]
		d[state==i,plot(date,Home.Value,main=i)]
		#abline(a=sum(coef(m$detmod)[c("(Intercept)","stateCA")]),b=coef(m$detmod)["date"])
		lines(newd[,date],predict(mod,newdata=newd),col="red")

	}
	if (!is.null(saveto))	dev.off()
	par(mfcol=c(1,1))

}


#' Plot factor model Time Matrix
#'
#' @examples
#' data(HValue96_dynF_quarterly)
#' m <- makeTimeMatrix(dat)
#' p <- plotTimeMatrix(m,FD=FALSE)
plotTimeMatrix <- function(m,FD,saveto="~/Dropbox/mobility/output/model/factor"){

	stopifnot(c("m","detmod","zmod","idx") %in% names(m))

	mm <- m$m
	nas <- (1:ncol(mm))[!is.na(mm[1, ])]

	mm <- mm[,nas]

	d <- data.table(mm)
	setnames(d,as.character(m$idx))
	d[, state := rownames(mm)]

	dm <- melt(d,id.vars="state",variable.name="variable",value.name="price")
	dm[,date := as.Date(as.yearqtr(variable))]
	dm[,variable :=NULL]

	p <- plotHousePrices(d=dm,response="price",FD,saveto)

	return(p)
}



#' Plot different FHFA house prices indices
#'
#' @examples
#' data(FHFA_states,package="EconData")
#' dd <- FHFA.states$qtr[state %in% sample(unique(state),size=20)]
#' plot.FHFAindex(dd)
plot.FHFAindex <- function(dat,saveto="~/Dropbox/mobility/output/model/factor/fhfa.pdf"){

	# detrend data
		detmod <- lm(index_sa ~ quarter,data=dat)
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
	dat[state==st[1],plot(Date,detr,type="l",ylim=r,main="detrended index\n(linear trend)")]
	for (i in st[-1]) dat[state==i,lines(Date,detr)]

	r <- dat[,range(y1)]
	dat[state==st[1],plot(Date,y1,type="l",ylim=r,main="z-score of observed index",ylab="z")]
	for (i in st[-1]) dat[state==i,lines(Date,y1,ylab="")]

	r <- dat[,range(y2)]
	dat[state==st[1],plot(Date,y2,type="l",ylim=r,main="z-score of deviation from\n linear trend",ylab="z'")]
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

	detmod <- lm(hpi ~ qtr + state,data=dat)
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
	dat[State==st[1],plot(qtr,detr,type="l",ylim=r,main="detrended index\n(linear trend)")]
	for (i in st[-1]) dat[State==i,lines(qtr,detr)]

	r <- dat[,range(y1)]
	dat[State==st[1],plot(qtr,y1,type="l",ylim=r,main="z-score of observed index",ylab="z")]
	for (i in st[-1]) dat[State==i,lines(qtr,y1,ylab="")]

	r <- dat[,range(y2)]
	dat[State==st[1],plot(qtr,y2,type="l",ylim=r,main="z-score of deviation from\n linear trend",ylab="z'")]
	for (i in st[-1]) dat[State==i,lines(qtr,y2,ylab="")]

	par(mfcol=c(1,1))
	dev.off()
	
	# same for HomeValues
	# ===================

	# with and without inflation adustment

	# without
	detmod <- lm(Home.Value ~ qtr,data=dat)
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
	dat[State==st[1],plot(qtr,detr,type="l",ylim=r,main="detrended values\n(linear trend)",ylab="current dollars")]
	for (i in st[-1]) dat[State==i,lines(qtr,detr,ylab="")]

	r <- dat[,range(y1)]
	dat[State==st[1],plot(qtr,y1,type="l",ylim=r,main="z-score of observed values",ylab="z")]
	for (i in st[-1]) dat[State==i,lines(qtr,y1,ylab="")]

	r <- dat[,range(y2)]
	dat[State==st[1],plot(qtr,y2,type="l",ylim=r,main="z-score of value deviation \nfrom linear trend",ylab="z'")]
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

	detmod <- lm(HValue96 ~ qtr,data=dat)
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
	title(main="detrended values\n(linear trend)",sub="inflation adjusted")
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

	

getcbPalette <- function(n){

	cbPalette <- c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

	if (n > 9){
		cbPalette <- c(cbPalette, cbPalette[n-length(cbPalette)] )
	}

	return(cbPalette)
}



#' make plots of deviations model and true deviations
#'
plotSimTruthDeviations <- function(path=NULL) {

	N <- 3
	d <- makeDivDifferences()
	s <- buildDivDeviations(n=d$price$d[,length(unique(date))],N=N,burn=40)

	# price

	dp <- d$price$d[Division!="USA"]
	dp[,Replication :="truth"]

	ds <- lapply(s$price,as.data.frame)
	for (si in 1:length(ds)) {
		ds[[si]] <- cbind(ds[[si]],dp[,unique(date)],names(ds)[si])
		names(ds[[si]]) <- c(paste0("rep.",1:N),"date","Division")
	}
	ds <- rbindlist(ds)	# coerces to data.table!

	ds <- melt(ds,id.vars=c("date","Division"),variable.name="Replication")
	ds[,Division := gsub("\\."," ",Division)]

	# base plot: true deviations
	p <- ggplot(dp) + geom_line(aes(x=date,y=dev*100,color=Replication),size=1) + geom_hline(aes(yintercept=0),color="grey",linetype="dashed")

	p <- p + geom_line(data=ds,aes(x=date,y=value*100,color=Replication)) + facet_wrap(~Division) + theme_bw()
	p <- p + scale_y_continuous(name="percent deviation") + scale_color_manual(values=c("blue","green","red","black"))

	if (!is.null(path))	ggsave(filename=file.path(path,"priceSimTruthDevs.pdf"),plot=p,width=23,height=15,units="cm")

	# income

	s <- buildDivDeviations(n=d$income$d[,length(unique(Year))],N=N,burn=300)
	dy <- d$inc$d[Division!="USA"]
	dy[,Replication :="truth"]

	ds <- lapply(s$inc,as.data.frame)
	for (si in 1:length(ds)) {
		ds[[si]] <- cbind(ds[[si]],dy[,unique(Year)],names(ds)[si])
		names(ds[[si]]) <- c(paste0("rep.",1:N),"Year","Division")
	}
	ds <- rbindlist(ds)	# coerces to data.table!

	ds <- melt(ds,id.vars=c("Year","Division"),variable.name="Replication")
	ds[,Division := gsub("\\."," ",Division)]

	# base plot: true deviations
	y <- ggplot(dy) + geom_line(aes(x=Year,y=dev*100,color=Replication),size=1)+ geom_hline(aes(yintercept=0),color="grey",linetype="dashed")

	y <- y + geom_line(data=ds,aes(x=Year,y=value*100,color=Replication)) + facet_wrap(~Division) + theme_bw()
	y <- y + scale_y_continuous(name="percent deviation") + scale_color_manual(values=c("blue","green","red","black"))

	if (!is.null(path))ggsave(filename=file.path(path,"incomeSimTruthDevs.pdf"),plot=y,width=23,height=15,units="cm")

	return(list(price=p,income=y))

}


#' make plots of deviations models
#'
#' @family ExpectationsModel
#' @examples
#' d <- buildDivDeviations(n=20,N=3)
#' plotDivDeviations(d)
#' d <- buildDivDeviations(n=20,N=3)
#' 
#' ## example 2
#' agg <- makeAggPricesFromData(N=3)
#' a = buildAggregatePrices(agg)
#' plotDivDeviations(a)
plotSimDivDeviations <- function(d,path=NULL){


	divs <- names(d$price)
	rp <- lapply(d$price,range)
	rp <- c(min(unlist(rp)),max(unlist(rp)))

	ry <- lapply(d$inc,range)
	ry <- c(min(unlist(ry)),max(unlist(ry)))

	if (is.null(path)){
		par(ask=TRUE)
		par(mfrow=c(3,3))
		for (i in 1:9){
			matplot(d$price[[i]],type="l",main=paste0("price ",divs[i]),ylim=rp)
			abline(a=0,b=0)
		}
		par(mfrow=c(3,3))
		for (i in 1:9){
			matplot(d$inc[[i]],type="l",main=paste0("income ",divs[i]),ylim=ry)
			abline(a=0,b=0)
		}
		par(mfrow=c(1,1))
		par(ask=FALSE)

	} else {
		pdf(file.path(path,"simDivPrices.pdf"))
		par(mfrow=c(3,3))
		for (i in 1:9){
			matplot(d$price[[i]],type="l",main=paste0("price ",divs[i]),ylim=rp)
			abline(a=0,b=0)
		}
		dev.off()

		pdf(file.path(path,"simDivIncomes.pdf"))
		par(mfrow=c(3,3))
		for (i in 1:9){
			matplot(d$inc[[i]],type="l",main=paste0("income ",divs[i]),ylim=ry)
			abline(a=0,b=0)
		}
		dev.off()
	}
}



#' plot monthly rent and mortgage by state
#'
#' create a errorbar plot marking both mean
#' mortgage payment as well as mortgage payment
#' from my sipp sample 
#' @examples
#' load("~/Dropbox/mobility/SIPP/Sipp_aggby_age.RData")
#' plotMortgageRentSipp(sipp=merged)
plotMortgageRentSipp <- function(sipp,path="~/Dropbox/mobility/output/data/sipp/"){

	
	m1=sipp[mortg.rent>0,list(mort=.SD[own==TRUE,weighted.mean(mortg.rent,HHweight,na.rm=T)],rent=.SD[own==FALSE,weighted.mean(mortg.rent,HHweight,na.rm=T)]),by=state]
	m1[,own.rate := merged[age>25&age<65,weighted.mean(own,HHweight),by=list(state)][,V1]]
	m1[,mort2rent := mort / rent]
	m1[order(mort2rent),plot(mort,own.rate)]

	m2=sipp[mortg.rent>0,list(mort=.SD[own==TRUE,weighted.mean(mortg.rent,HHweight,na.rm=T)],rent=.SD[own==FALSE,weighted.mean(mortg.rent,HHweight,na.rm=T)]),by=list(state,year)]
	m2 <- melt(m2,id=c("state","year"))

	m3=sipp[mortg.rent>0,list(mort=.SD[own==TRUE,weighted.mean(mortg.rent,HHweight,na.rm=T)],mortmed=.SD[own==TRUE,wtd.quantile(mortg.rent,HHweight,probs=0.5,na.rm=T)],rent=.SD[own==FALSE,weighted.mean(mortg.rent,HHweight,na.rm=T)],rentmed=.SD[own==FALSE,wtd.quantile(mortg.rent,HHweight,probs=0.5,na.rm=T)]),by=Division]
	m3[,mort2rent := mort / rent]
	m3[,mort2rentMed := mortmed / rentmed]

	m4=sipp[mortg.rent>0,list(mort=.SD[own==TRUE,weighted.mean(mortg.rent,HHweight,na.rm=T)],mortmed=.SD[own==TRUE,wtd.quantile(mortg.rent,HHweight,probs=0.5,na.rm=T)],rent=.SD[own==FALSE,weighted.mean(mortg.rent,HHweight,na.rm=T)],rentmed=.SD[own==FALSE,wtd.quantile(mortg.rent,HHweight,probs=0.5,na.rm=T)]),by=list(Division,year)]
	m4[,mort2rent := mort / rent]
	m4[,mort2rentmed := mortmed / rentmed]

	p <- list()
	# p$rent <- ggplot(m1,aes(x=state,ymin=rent*1000/12,ymax=mort*1000/12)) + geom_point(aes(y=rent*1000/12),color="red",size=2.5) + geom_point(aes(y=mort*1000/12),color="blue",size=2.5) + geom_linerange() + coord_flip() + scale_y_continuous(name="monthly rent (red) or mortgage (blue) payment in 1996 USD") + ggtitle('Average monthly rent and mortgage payments 1996-2012') + theme_bw()
	p$rent <- ggplot(m1,aes(x=state,ymin=rent*1000/12,ymax=mort*1000/12)) + geom_point(aes(y=rent*1000/12),color="red",size=2.5) + geom_point(aes(y=mort*1000/12),color="blue",size=2.5) + geom_linerange() + coord_flip() + labs(y="monthly rent (red) or mortgage (blue) payment in 1996 USD") + ggtitle('Average monthly rent and mortgage payments 1996-2012') + theme_bw() + theme(axis.title.x = element_text(colour = c(rep("black",2),"red",rep("black",2),"blue",rep("black",4))))


# + theme(axis.title.y = element_text(colour = c(rep("black",2),"red",rep("black",2),"blue",rep("black",4))))


	p$rentDiv <- ggplot(m3,aes(x=Division,ymin=rent*1000/12,ymax=mort*1000/12)) + geom_point(aes(y=rent*1000/12),color="red",size=2.5) + geom_point(aes(y=mort*1000/12),color="blue",size=2.5) + geom_linerange() + coord_flip() + scale_y_continuous(name="monthly rent (red) or mortgage (blue) payment in 1996 USD") + ggtitle('Average monthly rent and mortgage payments 1996-2012') + theme_bw()
	p$rentDivMedian <- ggplot(m3,aes(x=Division,ymin=rentmed*1000/12,ymax=mortmed*1000/12)) + geom_point(aes(y=rentmed*1000/12),color="red",size=2.5) + geom_point(aes(y=mortmed*1000/12),color="blue",size=2.5) + geom_linerange() + coord_flip() + scale_y_continuous(name="monthly rent (red) or mortgage (blue) payment in 1996 USD") + ggtitle('median monthly rent and mortgage payments 1996-2012') + theme_bw()

	p$rentDivYear <- ggplot(m4,aes(x=Division,ymin=rent*1000/12,ymax=mort*1000/12)) + geom_point(aes(y=rent*1000/12),color="red",size=2.5) + geom_point(aes(y=mort*1000/12),color="blue",size=2.5) + geom_linerange() + coord_flip() + scale_y_continuous(name="monthly rent (red) or mortgage (blue) payment in 1996 USD") + ggtitle('Average monthly rent and mortgage payments 1996-2012') + facet_wrap(~year) + theme_bw()
	p$rentDivYearMedian <- ggplot(m4,aes(x=Division,ymin=rentmed*1000/12,ymax=mortmed*1000/12)) + geom_point(aes(y=rentmed*1000/12),color="red",size=2.5) + geom_point(aes(y=mortmed*1000/12),color="blue",size=2.5) + geom_linerange() + coord_flip() + scale_y_continuous(name="monthly rent (red) or mortgage (blue) payment in 1996 USD") + ggtitle('Median monthly rent and mortgage payments 1996-2012') + theme_bw()+ facet_wrap(~year) 

	m1 <- m1[order(mort2rent)]
	m1[,state := factor(state)]
	p$m2r  <- ggplot(m1,aes(x=state,y=mort2rent)) + geom_line()  + coord_flip() 
	p$dist <- ggplot(m2,aes(x=value*1000,color=factor(year))) + geom_density() + ggtitle("distribution of monthly rent or mortgage payments") + facet_wrap(~variable) + scale_x_continuous(name="monthly rent or mortgage payment in 1996 USD")

	pdf(file=file.path(path,"mort-rent-state.pdf"))
	print(p$rent)
	dev.off()
	pdf(file=file.path(path,"mort-rent-div.pdf"))
	print(p$rentDiv)
	dev.off()
	pdf(file=file.path(path,"mort-rent-divYear.pdf"))
	print(p$rentDivYear)
	dev.off()

	pdf(file=file.path(path,"mort-rent-dist.pdf"))
	print(p$dist)
	dev.off()
	return(NULL)
}






#' Plot Sipp Transition matrix
#'
#' @examples
#' load('~/Dropbox/mobility/SIPP/Sipp_aggby_age.RData')
#' tt <- merged[from!=to,table(from,to)]
#' PlotSippTransitionMatrix(tt)
PlotSippTransitionMatrix <- function(ttable,path="~/Dropbox/mobility/output/data/sipp"){

	bins <- c(0,1,3,7,10,15,20,25)
	cats <- cut(as.numeric(ttable),breaks=bins,labels=FALSE,right=FALSE)

	pal = getcbPalette(7)

	p <- ggplot(data.frame(expand.grid(from=1:nrow(ttable),to=1:nrow(ttable)),z=factor(cats,labels=c(paste(c(0,1,3,7,10,15)),"> 20"))),aes(x=from,y=to,fill=z)) + geom_tile() + scale_fill_manual(values=pal,name="number\nof movers")

	# add scales
	p <- p + scale_x_continuous(name="from",breaks=1:nrow(ttable),labels=rownames(ttable)) + scale_y_continuous(name="to",breaks=1:nrow(ttable),labels=rownames(ttable)) 

	# add 45deg angle on x names
	p <- p + theme(axis.text.x = element_text(angle=45,vjust=0.5), panel.grid.minor=element_blank()) + ggtitle('SIPP transition matrix')

	if (!is.null(path)){
		pdf(file=file.path(path,"SIPP-transition.pdf"),width=11,height=9)
		print(p)
		dev.off()
	}

	return(p)

}


#' Plot Sipp Migration Rates by Age
#'
PlotSippMigrationRates <- function(){

	data(Sipp_age)

	 m=merged[age>29&age<61&college==TRUE,list(moved.S2S = weighted.mean(S2S,HHweight,na.rm=T),moved.D2D = weighted.mean(D2D,HHweight,na.rm=T)),by=list(age,h=factor(own))][order(age)]
	 m[,type := "Renter"]
	 m[h==TRUE,type := "Owner"]

	 p1 <- ggplot(m,aes(age,y=moved.S2S*100,color=type)) + geom_point(size=2.5) + geom_smooth(formula=y~ns(x,3),method="rlm",size=1) + theme_bw() + ggtitle('Sipp Raw Data: Proportion of Cross-State movers by age') + scale_color_manual(values=c("blue","red")) 
	 p2 <- ggplot(m,aes(age,y=moved.D2D*100,color=type)) + geom_point(size=2.5) + geom_smooth(formula=y~ns(x,3),method="rlm",size=1) + theme_bw() + ggtitle('Sipp Raw Data: Proportion of Cross-Division movers by age') + scale_color_manual(values=c("blue","red")) + scale_y_continuous(name="% of sample moved")
	 ggsave(plot=p1,file="~/Dropbox/mobility/output/data/sipp/raw-moversS2S.pdf",width=13,height=9,scale=0.6)
	 ggsave(plot=p2,file="~/Dropbox/mobility/output/data/sipp/raw-moversD2D.pdf",width=13,height=9,scale=0.6)

	 return(list(S2S=p1,D2D=p2))


}
#' load("~/Dropbox/mobility/SIPP/Sipp_aggby_age.RData")
#' m=merged[age>29&age<61&college==TRUE,list(proportion.moved = weighted.mean(S2S,HHweight,na.rm=T)),by=list(age,h=factor(own))][order(age)]
#' p <- ggplot(m,aes(age,y=proportion.moved,color=h)) + geom_point(size=2.5) + geom_smooth(formula=y~ns(x,3),method="rlm",size=1) + theme_bw() + ggtitle('Sipp Raw Data: Proportion of Cross-State movers by age') + scale_color_manual(values=c("blue","red"))
#' ggsave(plot=p,file="~/Dropbox/mobility/output/data/sipp/raw-movers.pdf",width=13,height=9,scale=0.6)






