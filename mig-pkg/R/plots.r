


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
	









#' Plot FHFA house price index series
#'
#' group states by region and plot
#' price series
#' @param saveto location where to save plots
#' @return list of ggplots
plotHousePrices <- function(saveto="~/Dropbox/mobility/output/data/FHFA/"){

	data(FHFA_states,package="EconData")

	d <- states$qtr
	d[,note:= NULL]

	# group by census division

	data(US_states,package="EconData")
	US_states[,c("FIPS","STATE","Reg_ID","Div_ID") := NULL]

	setkey(d,state)
	setkey(US_states,state)

	# merge
	d <- US_states[d]
	div <- d[,unique(Division)]
	reg <- d[,unique(Region)]

	plist <- list()

	# compare divisions
	dd <- d[,list(index_sa = mean(index_sa)),by=list(Division,Date)]
	plist$divs <- ggplot(dd,aes(x=Date,y=index_sa,color=Division)) + geom_line(size=1) + theme_bw() + ggtitle('FHFA indices by Census Division') 

	pdf(file=file.path(saveto,"div.pdf"))
	print(plist$divs)
	dev.off()

	# compare regions
	dd <- d[,list(index_sa = mean(index_sa)),by=list(Region,Date)]
	plist$regs <- ggplot(dd,aes(x=Date,y=index_sa,color=Region)) + geom_line(size=1) + theme_bw() + ggtitle('FHFA indices by Census Region') 

	pdf(file=file.path(saveto,"reg.pdf"))
	print(plist$regs)
	dev.off()

	# plots grouping states by divisions
	yscale <- scale_y_continuous(name="FHFA index",limits=c(85,360))


	plist$bydiv <- lapply(div, function(z) ggplot(d[Division==z],aes(x=Date,y=index_sa,color=state)) + geom_line(size=1) + ggtitle( z )+ theme_bw() + yscale)

	pdf(file=file.path(saveto,"bydiv.pdf"),width=14,height=8)
	do.call(grid.arrange,plist$bydiv)
	dev.off()

	# plots grouping states by regions
	plist$byreg <- lapply(reg, function(z) ggplot(d[Region==z],aes(x=Date,y=index_sa,color=state)) + geom_line(size=1) + ggtitle( z )+ theme_bw() + yscale)

	pdf(file=file.path(saveto,"byreg.pdf"),width=14,height=9)
	do.call(grid.arrange,plist$byreg)
	dev.off()



	return(plist)

}

