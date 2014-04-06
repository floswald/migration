

#' Housing Status Policy Function Model
#'
#' estimates the reduced form for housing status changes form data
#' @param path
#' @param marginal TRUE if marginal effects 
#' @family FirstStage
#' @examples
#' load("~/Dropbox/mobility/SIPP/Sipp_aggby_age.RData")
#' h <- housingModel(d=merged)
housingModel <- function(d,path="~/Dropbox/mobility/output/model/BBL",marginal=FALSE){

	# loaded data aggregated by age

	# throw away renters with positive house value
	# d <- d[(own==TRUE) | (own==FALSE & hvalue==0)]

	# throw away negative incomes
	d <- d[HHincome>0]

	# CAUTION
	# remember that HHIncome is MONTHLY INCOME!!

	#rent <- d[own==FALSE,list(state,qtr,HValue96,income,numkids,HHweight,educ,age,age2,sex,wealth,mortg.rent,duration_at_current,born.here,p2y,p2w,buy,dkids)]

	## throw out all cases with some NA
	#rent = rent[complete.cases(rent)]

	#own <- d[own==TRUE,list(state,qtr,hvalue,income,numkids,HHweight,educ,age,age2,sex,mortg.rent,home.equity,duration_at_current,born.here,p2y,p2w,sell,wealth,dkids)]

	#own = own[complete.cases(own)]


	# models
	m <- list()
	m$buylinear <- glm(buy ~ age + age2+dkids+ p2y + p2w  + mortg.rent,data=d[own==FALSE&buy<2],family=binomial(link="probit"),x=TRUE) 
	m$buyspline <- glm(buy ~ age + age2+dkids+ bs(p2y,knots=c(3,5),degree=1) +  mortg.rent ,data=d[own==FALSE&buy<2],family=binomial(link="probit"),x=TRUE) 

	
	m$selllinear <- glm(sell ~ age + age2+dkids+HHincome+ home.equity + mortg.rent + duration,data=d[own==TRUE&sell<2],family=binomial(link="probit"),x=TRUE)
	m$sellspline <- glm(sell ~ age + age2+dkids+HHincome+ bs(home.equity,df=3,degree=1) + mortg.rent + duration,data=d[own==TRUE&sell<2],family=binomial(link="probit"),x=TRUE)


	# compute marginal effects
	mab <- lapply(m,erer::maBina)

	# get coefficients
	coefs <- lapply(m,coef)


	if (!is.null(path)){

		save(coefs,file=file.path(path,"housingCoefs.RData"))

		if (marginal){

			texreg(mab[c("buylinear","buyspline")],custom.model.names=c("Pr(buy|rent)","Pr(buy|rent)"),stars=c(0.01,0.05,0.1),digits=4,file=file.path(path,"buy.tex"),caption="marginal effects at sample mean of x",table=FALSE)
			htmlreg(mab[c("buylinear","buyspline")],custom.model.names=c("Pr(buy|rent)","Pr(buy|rent)"),stars=c(0.01,0.05,0.1),digits=4,file=file.path(path,"buy.html"),caption="marginal effects at sample mean of x")

			texreg(mab[c("selllinear","sellspline")],custom.model.names=c("Pr(sell|own)","Pr(sell|own)"),stars=c(0.01,0.05,0.1),digits=4,file=file.path(path,"sell.tex"),caption="marginal effects at sample mean of x",table=FALSE)
			htmlreg(mab[c("selllinear","sellspline")],custom.model.names=c("Pr(sell|own)","Pr(sell|own)"),stars=c(0.01,0.05,0.1),digits=4,file=file.path(path,"sell.html"),caption="marginal effects at sample mean of x")

		} else {


			texreg(m[c("buylinear","buyspline")],stars=c(0.01,0.05,0.1),digits=4,file=file.path(path,"buy.tex"),caption="coefficient estimates",table=FALSE)
			htmlreg(m[c("buylinear","buyspline")],custom.model.names=c("linear","spline"),stars=c(0.01,0.05,0.1),digits=4,file=file.path(path,"buy.html"),caption="coefficient estimates")

			texreg(m[c("selllinear","sellspline")],stars=c(0.01,0.05,0.1),digits=4,file=file.path(path,"sell.tex"),caption="coefficient estimates",table=FALSE)
			htmlreg(m[c("selllinear","sellspline")],custom.model.names=c("linear","spline"),stars=c(0.01,0.05,0.1),digits=4,file=file.path(path,"sell.html"),caption="coefficient estimates")
		
		}

	}

	screenreg(m,digits=4,custom.model.names=names(m),stars=c(0.01,0.05,0.1))
	screenreg(mab,digits=4,custom.model.names=paste0("ME of ",names(m)),stars=c(0.01,0.05,0.1))
	return(m)

}



#' Savings policy function
#' @param d dataset
#' @param quants at which quantiles of savings to run quantile regression (default NULL)
#' @param path where to save results
#' @param plot if to plot mean/median savings
#' @family FirstStage
#' @examples
#' load("~/Dropbox/mobility/SIPP/Sipp4mn.RData")
#' s <- savingsPolicy(d=merged4mn,quants=0.5)
savingsPolicy <- function(d,quants=NULL,path="~/Dropbox/mobility/output/model/BBL",plot=FALSE){
	
	# TODO this should be by calendar year
	# not by every 4 months
	# of course people will save less within 4 months thatn withing 1 year!

	# TODO redefine savings variable! saving_t = Assets_t+1 - Assets_t, where 
	# Assets_t = wealth_t - equity_t.


	tab         <- d[,list(mean=weighted.mean(saving,w2),median=Hmisc::wtd.quantile(saving,weights=w2,probs=0.5)),by=age][order(age)]
	#tab         <- d[,list(mean=mean(saving),median=median(saving)),by=age][order(age)]
	mtab        <- melt(tab,"age")
	setnames(mtab,c("age","savings","value"))
	p           <- ggplot(mtab,aes(x=age,y=value,color=savings)) + geom_line(size=1) + theme_bw() + scale_y_continuous(name="amount in bank account. 1000 of 1996 dollars")

	m <- list()

	# should weight that regression
	# TODO

	m$OLS1 <- lm(saving ~ HHincome + wealth + age + age2 + mortg.rent + numkids,data=d)
	m$OLS2 <- lm(saving ~ ns(HHincome,df=3) + ns(wealth,df=3) + age + age2 + mortg.rent + numkids,data=d)
	

	t1 <- proc.time()[3]
	cat("entering quantile regression 1. stay tuned.\n")
	m$quantreg <- quantreg::rq(saving ~ HHincome + wealth + age + age2 + mortg.rent + numkids,data=d,tau=quants,method="pfn")
	cat(sprintf("quantile regression 1 took %g seconds\n",proc.time()[3]-t1))

	# summaries
	#s <- list()
	#s$quantreg <- summary(m$quantreg,cov=TRUE)
	
	# coefs
	save.coefs <- list()
	save.coefs$OLS <- coef(m$OLS)
	save.coefs$quantreg <- coef(m$quantreg)
	#save.coefs$quantreg <- s$quantreg$coefficients


	# print to tex

	if (!is.null(path)){

		if (plot){
		pdf(file.path(path,"median-saving.pdf"))
		print(p)
		dev.off()
		}

		save(save.coefs,file=file.path(path,"savings.RData"))

		texreg(m,custom.model.names=names(m),stars=c(0.01,0.05,0.1),digits=4,file=file.path(path,"savings.tex"),table=FALSE)
		htmlreg(m,custom.model.names=names(m),stars=c(0.01,0.05,0.1),digits=4,file=file.path(path,"savings.html"),caption="savings policy estimates")

	}

	return(m)
}




simHousing <- function(newdat,obj){

	if (is.null(newdat)){

		p <- predict(obj)
		return(p)

	} else {
		p <- predict(obj,newdat,probability=TRUE)
		newdat[,newH := as.numeric( runif(nrow(newdat)) > p)]
		return(newdat)
	}
}


simSaving <- function(newdat,obj){

	newdat[,newAsset := predict(obj,newdata=newdat)]
	return(newdat)
}
