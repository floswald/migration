

#' Housing Status Policy Function Model
#'
#' estimates the reduced form for housing status changes form data
#' @param path
#' @param marginal TRUE if marginal effects 
#' @family FirstStage
#' @examples
#' load("~/Dropbox/mobility/SIPP/Sipp_aggby_age_svy.RData")
#' h <- housingModel(d=subset(des,HHincome>0))
housingModel <- function(d,path="~/Dropbox/mobility/output/model/BBL",marginal=FALSE){

	# models
	m <- list()
	m$buylinear <- svyglm(buy ~ age + age2+dkids+ p2y + p2w  + mortg.rent + nonh_wealth,design=subset(d,own==FALSE & buy<2),family=binomial(link="probit"),x=TRUE) 
	m$buyspline <- svyglm(buy ~ -1 + age + age2+dkids+ bs(p2y,knots=c(3,5),degree=1) +  mortg.rent +nonh_wealth + state,design=subset(d,own==FALSE & buy<2),family=binomial(link="probit"),x=TRUE) 

	
	m$selllinear <- svyglm(sell ~ age + age2+dkids+HHincome+ home.equity + mortg.rent + duration + nonh_wealth,design=subset(d,own==TRUE & sell < 2),family=binomial(link="probit"),x=TRUE)
	m$sellspline <- svyglm(sell ~ age + age2+dkids+HHincome+ bs(home.equity,df=3,degree=1) + mortg.rent + duration+ nonh_wealth + state,design=subset(d,own==TRUE & sell < 2),family=binomial(link="probit"),x=TRUE)


	# compute marginal effects
	mab <- lapply(m,erer::maBina)

	# get coefficients
	coefs <- lapply(m,coef)


	if (!is.null(path)){

		save(coefs,file=file.path(path,"housingCoefs.RData"))

		if (marginal){

			texreg(mab[c("buylinear","buyspline")],custom.model.names=c("Pr(buy|rent)","Pr(buy|rent)"),stars=c(0.01,0.05,0.1),digits=4,file=file.path(path,"buy.tex"),caption="marginal effects at sample mean of x",table=FALSE,use.packages=FALSE,dcolumn=TRUE,booktabs=TRUE)
			htmlreg(mab[c("buylinear","buyspline")],custom.model.names=c("Pr(buy|rent)","Pr(buy|rent)"),stars=c(0.01,0.05,0.1),digits=4,file=file.path(path,"buy.html"),caption="marginal effects at sample mean of x")

			texreg(mab[c("selllinear","sellspline")],custom.model.names=c("Pr(sell|own)","Pr(sell|own)"),stars=c(0.01,0.05,0.1),digits=4,file=file.path(path,"sell.tex"),caption="marginal effects at sample mean of x",table=FALSE,use.packages=FALSE,dcolumn=TRUE,booktabs=TRUE)
			htmlreg(mab[c("selllinear","sellspline")],custom.model.names=c("Pr(sell|own)","Pr(sell|own)"),stars=c(0.01,0.05,0.1),digits=4,file=file.path(path,"sell.html"),caption="marginal effects at sample mean of x")

		} else {


			texreg(m[c("buylinear","buyspline")],stars=c(0.01,0.05,0.1),digits=4,file=file.path(path,"buy.tex"),caption="coefficient estimates",table=FALSE,use.packages=FALSE,dcolumn=TRUE,booktabs=TRUE,omit.coef="state")
			htmlreg(m[c("buylinear","buyspline")],custom.model.names=c("linear","spline"),stars=c(0.01,0.05,0.1),digits=4,file=file.path(path,"buy.html"),caption="coefficient estimates")

			texreg(m[c("selllinear","sellspline")],stars=c(0.01,0.05,0.1),digits=4,file=file.path(path,"sell.tex"),caption="coefficient estimates",table=FALSE,use.packages=FALSE,dcolumn=TRUE,booktabs=TRUE,omit.coef="state")
			htmlreg(m[c("selllinear","sellspline")],custom.model.names=c("linear","spline"),stars=c(0.01,0.05,0.1),digits=4,file=file.path(path,"sell.html"),caption="coefficient estimates")
		
		}

	}

	# make predictions
	preds <- lapply(m,function(x) data.frame(predict(x,type="response")))

	m$sims <- lapply(preds, function(x) mean( runif(nrow(x)) < x$response))
	m$true <- data.frame(true.buy = d$variables[!is.na(p2y) & buy<2 & own==FALSE,mean(buy)],
		               true.sell= d$variables[!is.na(p2y) & sell<2 & own==TRUE,mean(sell)]
		)


	screenreg(m[1:4],digits=4,custom.model.names=names(m[1:4]),stars=c(0.01,0.05,0.1))
	screenreg(mab[1:4],digits=4,custom.model.names=paste0("ME of ",names(m[1:4])),stars=c(0.01,0.05,0.1))
	return(m)

}



#' nonh_wealth policy function
#' @param d dataset
#' @param quants at which quantiles of nonh_wealth to run quantile regression (default NULL)
#' @param path where to save results
#' @param plot if to plot mean/median nonh_wealth
#' @family FirstStage
#' @examples
#' load("~/Dropbox/mobility/SIPP/Sipp_aggby_age_svy.RData")
#' s <- nonh_wealthPolicy(svy=des)
nonh_wealthPolicy <- function(svy,path="~/Dropbox/mobility/output/model/BBL",plot=FALSE){
	
	# TODO this should be by calendar year
	# not by every 4 months
	# of course people will save less within 4 months thatn withing 1 year!

	# TODO redefine nonh_wealth variable! nonh_wealth_t = Assets_t+1 - Assets_t, where 
	# Assets_t = wealth_t - equity_t.



	tab         <- svy$variables[,list(mean=weighted.mean(nonh_wealth,HHweight),median=Hmisc::wtd.quantile(nonh_wealth,weights=HHweight,probs=0.5)),by=age][order(age)]
	#tab         <- d[,list(mean=mean(nonh_wealth),median=median(nonh_wealth)),by=age][order(age)]
	mtab        <- melt(tab,"age")
	setnames(mtab,c("age","nonH_wealth","value"))
	p           <- ggplot(mtab,aes(x=age,y=value,color=nonH_wealth)) + geom_line(size=1) + theme_bw() + scale_y_continuous(name="Non-housing wealth. Thousands 96 USD") + ggtitle('Mean and Median of Non-housing wealth in SIPP')
	

	m <- list()

	# should weight that regression
	# TODO

	m$OLS1 <- svyglm(nonh_wealth ~ age + age2 + mortg.rent + numkids + HHincome + wealth ,design=svy)
	m$OLS2 <- svyglm(nonh_wealth ~ age + age2 + mortg.rent + numkids + ns(HHincome,df=3) + ns(wealth,df=3) ,design=svy)
	

	t1 <- proc.time()[3]
	cat("entering quantile regression 1. stay tuned.\n")
	m$quantreg <- quantreg::rq(nonh_wealth ~ age + age2 + mortg.rent + numkids +ns(HHincome,df=3) + ns(wealth,df=3) ,data=svy$variables,weights=svy$variables$HHweight,tau=0.5,method="pfn")
	cat(sprintf("quantile regression 1 took %g seconds\n",proc.time()[3]-t1))

	# summaries
	#s <- list()
	#s$quantreg <- summary(m$quantreg,cov=TRUE)
	
	# coefs
	save.coefs <- list()
	save.coefs$OLS <- coef(m$OLS)
	save.coefs$quantreg <- coef(m$quantreg)
	#save.coefs$quantreg <- s$quantreg$coefficients


	# this is a pseudo R2 for survey objects
	# there is no R2 for such objects, because
	# deviance (below) is valid only for independently sampled
	# maxLik objects
	myrsquared = function(m){return(1- m$deviance / m$null.deviance)}

	# print to tex

	if (!is.null(path)){

		if (plot){
		pdf(file.path(path,"median-nonh_wealth.pdf"),width=8,height=6)
		print(p)
		dev.off()
		}

		save(save.coefs,file=file.path(path,"nonh_wealth.RData"))

		texreg(m,custom.model.names=names(m),stars=c(0.01,0.05,0.1),digits=4,file=file.path(path,"nonh_wealth.tex"),table=FALSE,booktabs=TRUE,dcolumn=TRUE,include.deviance=FALSE,include.dispersion=FALSE,use.packages=FALSE)
		htmlreg(m,custom.model.names=names(m),stars=c(0.01,0.05,0.1),digits=4,file=file.path(path,"nonh_wealth.html"),caption="nonh_wealth policy estimates")

		print(lapply(m[-3],myrsquared))
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
