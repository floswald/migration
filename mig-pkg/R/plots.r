

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



#' Diagnostic Plot for Income Prediction
#'
#' plots incomes as predicted by \code{\link{predict.income}}
#' @param n number of randomly drawn individuals
#' @param l output of income prediction
#' @family diagplots
#' @return a ggplot
#' @examples
#' load("~/Dropbox/mobility/output/model/BBL/predIncome.RData")
#' p <- plot.predict.income(n=4,l)
plot.predict.income <- function(n,l){

	# choose n random guys from 
	i <- l[,sample(unique(upid),size=n)]

	# choose 5 random states
	#s <- l[,sample(unique(state),size=5)]
	#s <- paste0("logHHincome.",s)

	setkey(l,upid)
	d <- data.frame()
	inum <- 0
	for (id in i){

		inum <- inum+1
		d <- rbind(d,l[.(id)][,list(id,age,logHHincome,logHHincome.AK,logHHincome.AR,logHHincome.CA,logHHincome.MI,logHHincome.PA)])

	}
  
  # TODO 
  # in facet label add the value of the intercept
  x <- l[i]
  int = x[,round(unique(intercept),2),by=upid]
  

	m <- melt(d,id.vars=c("inum","age"))
	names(m) <- c("id","age","income.type","value")
	p <- ggplot(m,aes(age,exp(value)*1000,color=income.type)) + geom_line() + ggtitle('predicted monthly HHincome') + facet_wrap(~id,scales="free")
	
	return(p)
}



#' Plot median incomes
#'
plotMedianIncome <- function(saveto="~/Dropbox/mobility/output/data/census"){

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
	d[,Date := as.Date(paste(Year,"01","01",sep="-"))]



	# aggregate
	reg <- d[,list(medinc=mean(medinc)),by=list(Region,year(Date))]
	div <- d[,list(medinc=mean(medinc)),by=list(Division,year(Date))]
	reg[,Date := as.Date(paste(year,"01","01",sep="-"))]
	div[,Date := as.Date(paste(year,"01","01",sep="-"))]

	# plots
	plist <- list()
	yscale <- scale_y_continuous(name="median annual income in 1000 dollars",limits=c(30,80))
	yscale2 <- scale_y_continuous(name="median annual income in 1000 dollars")

	plist$regs <- ggplot(reg,aes(x=Date,y=medinc/1000,color=Region,group=Region)) + geom_line(size=1)+ ggtitle('median income income in 2012 dollars')  + yscale2
	plist$divs <- ggplot(div,aes(x=Date,y=medinc/1000,color=Division)) + geom_line(size=1)+ ggtitle('median income in 2012 dollars')  + yscale2

	plist$divstate <- lapply(d[,unique(Division)], function(x) ggplot(d[Division==x], aes(x=Date,y=medinc/1000,color=STATE)) + geom_line(size=1)+ ggtitle(x) + yscale )

	plist$regstate <- lapply(d[,unique(Region)], function(x) ggplot(d[Region==x], aes(x=Date,y=medinc/1000,color=STATE)) + geom_line(size=1)+ ggtitle(x)  + yscale)

	pdf(file=file.path(saveto,"reg.pdf"))
	print(plist$regs)
	dev.off()
	
	pdf(file=file.path(saveto,"divs.pdf"))
	print(plist$divs)
	dev.off()

	pdf(file=file.path(saveto,"regstate.pdf"),width=20,height=18)
	do.call(grid.arrange,plist$regstate)
	dev.off()
	
	pdf(file=file.path(saveto,"divstate.pdf"),width=20,height=18)
	do.call(grid.arrange,plist$divstate)
	dev.off()



	return(plist)

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
#' d <- copy(FHFA.states$qtr)
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

	# compare divisions
	dd <- d[,list(y= mean(y),FDindex=mean(FDindex)),by=list(Division,date)]
	plist$divs <- ggplot(dd,aes(x=date,y=y,color=Division)) + geom_line(size=1) + theme_bw() + ggtitle(paste0(response,' by Census Division')) 

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

	pdf(file=file.path(saveto,"bydiv.pdf"),width=14,height=8)
	multiplot(plotlist=plist$bydiv,layout=matrix(1:9,nrow=3,byrow=TRUE))
	dev.off()


	# plots grouping states by regions
	plist$byreg <- lapply(reg, function(z) ggplot(d[Region==z],aes(x=date,y=y,color=state)) + geom_line(size=1) + ggtitle( z )+ theme_bw() + yscale)

	pdf(file=file.path(saveto,"byreg.pdf"),width=14,height=9)
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

