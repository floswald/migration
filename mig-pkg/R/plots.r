


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


