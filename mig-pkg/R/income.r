


# contains income models by geography
# contains house value models by geography
# contains children arrival process


# method to predict income for logit estimation,
# i.e. only predict in states other than j
# setMethod("buildLogitData", signature = c("IncomeProcess","character"),
# 	definition = function(x,y,...){
# 		load("~/Dropbox/mobility/SIPP/Sipp_aggby_age.RData")
# 		r <- buildLogit(logi=merged,RE.coefs=x@RE.coefs,saveto=y)
# 		return(r)
# 	})




#' predict all states

#' predict all but current state

#' save to disk

#' 


#' Children arrival process
#'
ChildrenArrivalProcess <- function(d,path=NULL){

	r <- svytable(formula=~nkids + nkids2 + age,design=d)

	r2 <- array(0,dim(r))
	for (i in 1:dim(r)[3]){
		r2[ , ,i] <- prop.table(r[ , ,i],margin=1)
	}
	dimnames(r2) <- dimnames(r)
	if (!is.null(path)) saveRDS(r2,file=path)
	return(r2)
}


#' Model to predict house values in other regions
#'
#' estiamtes a linear svyglm model to predict the house price
#' relevant for household with observables x in region j
#' @examples
#' load('~/Dropbox/mobility/SIPP/Sipp_aggby_age_svy.RData')
#' des$variables[,state := Division]
#' h <- HouseValueModel(d=des,path="~/Dropbox/mobility/output/model/BBL/house-values")
HouseValueModel <- function(d,path){

	m = lapply(d$variables[,unique(state)],function(x) svyglm(hvalue ~ -1 + factor(year) + age + age2 + nkids + ns(HHincome,df=2) + ns(nonh_wealth,df=3),design=subset(d,own==TRUE&state==x)))
	names(m) <- d$variables[,unique(state)]

	
	sp <- list()
	sp$coefs <- sapply(m,coef)
	sp$HHincome <- lapply(m,function(x) attributes(x$model[,"ns(HHincome, df = 2)"]))
	sp$nonh_wealth <- lapply(m,function(x) attributes(x$model[,"ns(nonh_wealth, df = 3)"]))
	# save 
	if (!is.null(path)){
		saveRDS(m,file=file.path(path,"hvalue-model.rds"))
		saveRDS(sp,file=file.path(path,"hvalue-coefs.rds"))
	}

	return(m)
}



# TODO 
# get empirical autocrrelation
# merged[HHincome>0,acf(HHincome,lag.max=1,plot=FALSE)$acf[2], by=Division]


#' Income Rank by Region and Age
#'
Rank.HHincome <- function(dat,geo="Division",n=3,plot=FALSE,path="~/Dropbox/mobility/output/model/R2julia"){

	if (geo=="Division") {
		dat[,state := Division]
	} else if (geo=="Div2") {
		dat[,state := Div2]
	} else if (geo=="state"){

	}

	# csv file name
	zname <- paste0("zsupp_n",n,".csv")
	rhoname <- paste0("rho_n",n,".csv")

	# censor income data at 5 and 95 percentile
	kv <- c("HHincome","upid","age","timeid","year","state","HHweight")
	dat <- dat[HHincome>0,kv,with=FALSE]
	q <- dat[,quantile(HHincome,probs=c(0.05,0.95))]
	dat <- dat[HHincome>q[1] & HHincome < q[2]]

	# take out macro time-region effect
	dat[,state.med := Hmisc::wtd.quantile(HHincome,weights=HHweight,probs=0.5,na.rm=T),by=list(year,state)]

	# compute measure of deviation
	# dat[,ytilde := HHincome - state.med]
	# dat[,ytilde := 100*(HHincome - state.med)/state.med, by=list(year,state)]
	dat[,ytilde := 100*HHincome/state.med, by=list(year,state)]

	# throw inds with fewer than 2 ages
	dat <- dat[age>20 & age<65]
	dat[,ncell := .N ,by=list(age,state)]
	dat <- dat[ncell>=n]

	dat[,dage := diff(age), by=list(upid)]
	dat <- dat[dage>0]

	# throw away cells with fewer than n observations
	lb=0;ub=1;pad=0.1;
    prange = seq( lb+(ub-lb)*pad,  lb+(ub-lb)*(1-pad) ,l=n)

	# compute ranks of ytilde by age and state
	dat[,yrank := cut(ytilde,breaks=Hmisc::wtd.quantile(ytilde,weights=HHweight,probs=prange),labels=FALSE,include.lowest=TRUE,right=TRUE),by=list(age,state)]

	# compute lagged rank
	setkey(dat,upid,timeid)
	dat[,Lyrank := dat[list(upid,timeid-1)][["yrank"]] ]


	# get breaks for model:
	z <- ddply(dat,.(state,age), function(x) Hmisc::wtd.quantile(x$ytilde,weights=x$HHweight,probs=prange))
	zy <- ddply(dat,.(state,age), function(x) Hmisc::wtd.quantile(x$HHincome,weights=x$HHweight,probs=prange))

	# compute average over individiual correlation coefficients by region
	rho <- dat[,list(rho=cor(yrank,Lyrank,use="pairwise")),by=state]

	# write to csv
	write.csv(z,file=file.path(path,zname))
	write.csv(rho,file=file.path(path,rhoname))


	# make a plot
	m = melt(z,id.vars=c("state","age"))
	my= melt(zy,id.vars=c("state","age"))
	names(m) <- c("state","age","quantile","value")
	names(my) <- c("state","age","quantile","value")
	pl = ggplot(m,aes(x=age,y=value,color=quantile)) + geom_line(size=1) + facet_wrap(~state) + theme_bw() + scale_y_continuous(name="percent of region median income") + ggtitle("Income Quantiles by Age")
	ply = ggplot(my,aes(x=age,y=value,color=quantile)) + geom_line(size=1) + facet_wrap(~state) + theme_bw() + scale_y_continuous(name="income in 1000 $")

	plmacro <- ggplot(dat[,unique(state.med),by=list(year,state)],aes(x=year,y=V1,color=state)) + geom_line() + ggtitle("macro effects")

	if (plot){

		dr <- "~/Dropbox/mobility/output/data/sipp/"
		ggsave(plot=pl,filename=file.path(dr,"z-quantiles-pct.pdf"),width=23,height=15,units="cm")
		ggsave(plot=ply,filename=file.path(dr,"z-quantiles.pdf"),width=23,height=15,units="cm")
		ggsave(plot=plmacro,filename=file.path(dr,"z-macro.pdf"),width=23,height=15,units="cm")

	}

	r <- list(d=dat,z=z,rho=rho,pl=pl,ply=ply)
	return(r)
}


#' Linear Random Effects Panel Model of Income
#'
#' estimate a linear panel of incoem with AR(1) error term
#' fit a model similar to the one in Baltagi (2005, 5.2.1).
#'
#' model is for income in location j is
#' \itemize{
#' \item log(y_ijt) = beta_j + gamma'X_it + mu_i + v_it, where
#' \itemize{
#' \item y_ijt is log income of HH i in MONTH t in location j
#' \item v_it = rho v_it-1 + eps_it
#' \item eps ~ iid N(0,sigma_eps),
#' \item mu  ~ iid N(0,sigma_mu),
#' \item mu independent of v_it. v_i0 ~ N(0,sigma_eps^2 / (1-rho^2))
#' \item The random effects assumption is incorporated by assuming that
#' \item mu_i is iid (uncorrelated with X).
#' }}
#' @family IncomePrediction
#' @param dat data set of income relevant variables
#' @param geo level of geopgraphy: state, Division, Div2.
#' @return list for each state with coefficients and fixed effects
#' for each individual. saves data.
#' @examples
#' load("~/Dropbox/mobility/SIPP/Sipp_aggby_age.RData")
#' l <- RE.HHincome(dat=merged)
RE.HHincome <- function(dat,geo="Division",
						path="~/Dropbox/mobility/output/model/BBL/inc-process"){

	if (geo=="Division") {
		dat[,state := Division]
		fname <- "Div-REcoefs.rds"

	} else if (geo=="Div2") {
		dat[,state := Div2]
		fname <- "Div2-REcoefs.rds"
	} else if (geo=="state"){
		fname <- "state-REcoefs.rds"

	}

	st <- dat[,unique(state)]
	st <- st[order(st)]

	# keep only those few variables around
	kv <- c("HHincome","upid","age","age2","cohort","timeid","state")
	dat <- dat[HHincome>0,kv,with=FALSE]

	# coh <- model.matrix(~cohort -1 ,data=dat)
	# dat <- cbind(dat,coh)

	# this formulation if you use the predict.lme
	#AR1 <- lapply(st, function(x) {cat(sprintf("estimating model for %s\n",x)); lme(logHHincome ~ age + I(age^2) +cohort , random=~1|upid,correlation=corAR1(0,form=~yrmnid|upid),data=subset(dat,state==x))})

	# problem with using the actual object to make predictions: you will never have individual i (estimated effect in state j) in the object
	# for state k. so you cannot predict state k.

	# AR1 <- lapply(st, function(x) {cat(sprintf("estimating model for %s\n",x)); lme(log(HHincome) ~ age + age2 + cohort1920  + cohort1940 + cohort1960 + cohort1980, random=~1|upid,correlation=corAR1(0,form=~timeid|upid),data=subset(dat,state==x))})
	AR1 <- lapply(st, function(x) {cat(sprintf("estimating model for %s\n",x)); lme(log(HHincome) ~ age + age2 , random=~1|upid,correlation=corAR1(0,form=~timeid|upid),data=subset(dat,state==x))})
	lms <- lapply(st, function(x) {cat(sprintf("estimating model for %s\n",x)); lm(log(HHincome) ~ age + age2 , data=subset(dat,state==x))})
		names(AR1) <- st
	names(lms) <- st

	# print a subset of states to one table
	fi <- "incomeRE-123.tex"
	sst <- sample(st,size=3)
	texreg(AR1[sst],file=file.path(path,fi),include.variance=TRUE,include.bic=FALSE,include.aic=FALSE,include.loglik=FALSE,booktabs=TRUE,dcolumn=TRUE,table=FALSE,omit.coef="cohort",digits=3,custom.model.names=sst,use.packages=FALSE)


	# print results to tex files
	for (i in st){

		fi <- paste0("incomeRE-",i,".tex")
		texreg(list(AR1[[i]]),file=file.path(path,fi),include.variance=TRUE,booktabs=TRUE,dcolumn=TRUE,use.packages=FALSE)

	}
	# print all into one html file
		fi <- "incomeRE-all.html"
		htmlreg(AR1,file=file.path(path,fi),include.variance=TRUE,caption="all models",custom.model.names=paste0("income process in ",st))



	# save coefs into a handy list
	RE.coefs <- lapply(AR1,lme.getCoefs)

	# write to json
	write.csv(t(sapply(RE.coefs,function(x) x$fixed)),file=file.path(path,"Div-Ageprofile.csv"))
	write.csv(t(sapply(RE.coefs,function(x) x[c("rho","sigma","sig.RE")])),file=file.path(path,"Div-IncParams.csv"))

	if (geo=="Division"){
		nd = data.frame(const=1,age=20:65,age2=(20:65)^2)
		md = as.matrix(nd)
		for (i in st){
			nd = cbind(nd,md %*% fixef(AR1[[i]]))
		}
		names(nd)[-(1:3)] <- st
		md = melt(nd[,c("age",st)],id.vars="age")
		names(md) <- c("age","Division","value")
		pl <- ggplot(md,aes(x=age,y=1000*exp(value),color=Division)) + geom_line(size=1) + theme_bw() + ggtitle('Age Profiles') + scale_y_continuous(name="Annual income in 1996 US Dollars")
		pl <- pl + scale_color_manual(values=getcbPalette(n=9))
		pdf(file=file.path(path,"Div-Profiles.pdf"),width=8,height=6)
		print(pl)
		dev.off()
	}

	# out = data.frame(t(sapply(RE.coefs,function(x) x$fixed)))
	# out = cbind(out,as.numeric(lapply(RE.coefs,function(x) x$rho)))
	# out = cbind(out,as.numeric(lapply(RE.coefs,function(x) x$sigma)))
	# out = cbind(out,as.numeric(lapply(RE.coefs,function(x) x$sig.RE)))
	# out$region = rownames(out)
	# names(out) = c(names(RE.coefs[[1]]$fixed), c("rho","sigma","sigRE"))

	# write.csv(out,file=file.path(path,"Div-ageprofile.csv"),row.names=FALSE)



	attr(RE.coefs,"type") <- "RE.coefs"
	saveRDS(RE.coefs,file=file.path(path,fname))
	#RE.models <- AR1
	#save(RE.models,file=file.path(path,"income-REmodels.RData"))

	return(list(RE.coefs,lms))
}


#' Exract coeffs and FE from lme object
#'
lme.getCoefs <- function(obj){

	r <- list()
	r$fixed <- fixef(obj)	# that will return constant parameters
	r$RE    <- data.table(upid=rownames(coef(obj)),intercept=coef(obj)[,1],key="upid")	# that will return constant parameters
	r$rho   <- coef(obj$modelStruct,unconstrained=FALSE)["corStruct.Phi1"]
    r$sigma <- obj$sigma	
	r$sig.RE  <- as.numeric(VarCorr(obj)["(Intercept)","StdDev"])

	attr(r,"estim.type") <- "lme"

	return(r)
}


#' extract Xvars from data.table
#'
#' @family IncomePrediction
getIncomeM <- function(with.FE,tmps){

	if (with.FE){
		m <- as.matrix(tmps[,list(1,intercept,age,age2,cohort1920,cohort1940,cohort1960,cohort1980)])	# (n by [1,alpha_i,age,age2,...] )
	} else {
		m <- as.matrix(tmps[,list(1,age,age2,cohort1920,cohort1940,cohort1960,cohort1980)])	
	}
	return(m)
}


#' income prediciton level 1
#'
#' merge random effects id-specific intercept and
#' adjust as deviation from state s population intercept
#' for each upid,age combination, 
#' take the guys random effect, find difference from 
#' region j's intercept, predict population income in k,
#' and finally add the personal effect.
#' format
#' ======
#' logit model requires long data.
#' strategy: take copy of 'income' by state to get
#' id    age    state    y
#'  1     29       WI    1000
#'  1     30       WI    1030
#'  1     31       WI    1100
#'  ...
#' @family IncomePrediction
makePrediction1 <- function(y,RE.coefs,with.FE,State_dist,prices=NULL) {

	st <- names(RE.coefs)
	st <- st[order(st)]
    setkey(y,upid,age,state)
	
	l <- list()

	# for all current states
	for (s in st){

		# for all guys in s
		tmps <- y[state==s]

		# get id-specific intercept
		# and adjust w.r.t. population intercept

		tmps <- getREintercept(tmps,RE.coefs[[s]])

		# get model matrix with explanatory variables
		m <- getIncomeM(with.FE,tmps)

		ll <- makePrediction2(s,RE.coefs,m,with.FE,tmps,State_dist,prices)

		l[[s]] <- ll
		rm(tmps,ll)
		gc()

	}

	l <- rbindlist(l)

	return(l)
}

		

#' income prediction level 2
#' 
#' predict income for all j given s
#' @family IncomePrediction
makePrediction2 <- function(s,RE.coefs,m,with.FE,tmps,pred.all=FALSE,State_dist,prices){
	
	ll <- list()	
		
	# copy current state 
	# don't need to predict anything here

	# TODO 
	# this is the main problem with the current setup:
	# this here does not predict anything for state s,
	# because state s is known.
	# not useful for prediction if not for initial logit model.

	st <- names(RE.coefs)

	if(!pred.all){

		ll[[s]] <- copy(tmps)
		ll[[s]][,move.to  := state]
		ll[[s]][,distance := 0]

		
		# predict states other than s
		prst <- st[-which(st==s)]

	}	else {

		# predict all states 
		prst <- st

	}

	
	# loop over all states in j
	for (j in prst){

		# get regression coefficients 
		# pertaining to state j

		if (with.FE){

			be <- RE.coefs[[j]]$fixed
			be <- c(be[1],1,be[-1])	# be = [beta0, 1, beta1, ... betak]

		} else {

			be <- RE.coefs[[j]]$fixed
		}

		if (!is.null(prices)){
			# change intercept with 
			# value porportional to current
			# division income index divY
			be[1] <- prices$scale[j] * prices$div[state==j,y]
		}

		# copy origin state data.table
		# into slot j of list
		ll[[j]] <- copy(tmps)

		# change destination
		ll[[j]][,move.to := j ]
		# get distance
		ll[[j]][,distance := State_dist[s,j] ]
		
		# predict income there
		ll[[j]][,logHHincome := myPredict(data=m,beta=be)]

	}

	ll <- rbindlist(ll)
	ll[,c("cohort1920","cohort1940","cohort1960","cohort1980") := NULL]
	attr(ll,"origin") <- s
	attr(ll,"with.FE") <- with.FE
	attr(ll,"predicted.all") <- pred.all

	return(ll)

}


#' get RE intercept and merge into current state
#' 
#' @family IncomePrediction
getREintercept <- function(tmps,RE.coefs){

	# l[[s]] is sorted by upid
	setkey(tmps,upid)

	# adds column "intercept" from RE regression
	tmps <- RE.coefs$RE[ tmps ]

	# adjust "intercept" to be difference to population intercept:
	tmps[, intercept := intercept - RE.coefs$fixed[[1]] ]

	return(tmps)

}