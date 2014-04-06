



#' Incoem Process Class
#' 
IncomeProcess <- setClass("IncomeProcess",
	slots = c(RE.coefs="list",scales="data.table",prices="data.table")
	)

#' show method for Income process
setMethod("show",signature = "IncomeProcess",
	definition = function(object){
		cat("object of class ",class(object),"\n")
		cat("number of states",length(object@RE.coefs),"\n")
		cat("models are of class",attr(object@RE.coefs,"estim.type"),"\n")
		cat("Aggregate prices and incomes: \n")
		print()
		cat("\nintercept scale factors: \n")
		print(scales)
	})



#' estimate linear mixed model income process
setMethod("estimate.lme",signature = "IncomeProcess",
	definition = function(object){
		load("~/Dropbox/mobility/SIPP/Sipp_aggby_age.RData")
		x <- RE.HHincome(dat=merged)
		object@RE.coefs <- x
	})


#' predict all states

#' predict all but current state

#' save to disk

#' 




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
#' @return list for each state with coefficients and fixed effects
#' for each individual. saves data.
#' @examples
#' load("~/Dropbox/mobility/SIPP/Sipp_aggby_age.RData")
#' l <- RE.HHincome(dat=merged)
RE.HHincome <- function(dat,
						path="~/Dropbox/mobility/output/model/BBL/inc-process"){

	st <- dat[,unique(state)]
	st <- st[order(st)]

	# keep only those few variables around
	kv <- c("HHincome","upid","age","age2","cohort","timeid","state")
	dat <- dat[HHincome>0,kv,with=FALSE]

	coh <- model.matrix(~cohort -1 ,data=dat)
	dat <- cbind(dat,coh)

	# this formulation if you use the predict.lme
	#AR1 <- lapply(st, function(x) {cat(sprintf("estimating model for %s\n",x)); lme(logHHincome ~ age + I(age^2) +cohort , random=~1|upid,correlation=corAR1(0,form=~yrmnid|upid),data=subset(dat,state==x))})

	# problem with that: you will never have individual i (estimated effect in state j) in the object
	# for state k. so you cannot predict state k.

	AR1 <- lapply(st, function(x) {cat(sprintf("estimating model for %s\n",x)); lme(log(HHincome) ~ age + age2 + cohort1920  + cohort1940 + cohort1960 + cohort1980, random=~1|upid,correlation=corAR1(0,form=~timeid|upid),data=subset(dat,state==x))})
	names(AR1) <- st


	# print results to tex files
	for (i in st){

		fi <- paste0("incomeRE-",i,".tex")
		texreg(list(AR1[[i]]),file=file.path(path,fi),include.RE=TRUE,booktabs=TRUE,dcolumn=TRUE)

	}
	# print all into one html file
		fi <- "incomeRE-all.html"
		htmlreg(AR1,file=file.path(path,fi),include.RE=TRUE,caption="all models",custom.model.names=paste0("income process in ",st))



	# save coefs into a handy list
	RE.coefs <- lapply(AR1,lme.getCoefs)
	save(RE.coefs,file=file.path(path,"income-REcoefs.RData"))
	#RE.models <- AR1
	#save(RE.models,file=file.path(path,"income-REmodels.RData"))

	return(RE.coefs)
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
makePrediction2 <- function(s,RE.coefs,m,with.FE,tmps,State_dist,prices){
	
	ll <- list()	
		
	# copy current state 
	# don't need to predict anything here

	# TODO 
	# this is the main problem with the current setup:
	# this here does not predict anything for state s,
	# because state s is known.
	# not useful for prediction if not for initial logit model.

	ll[[s]] <- copy(tmps)
	ll[[s]][,move.to  := state]
	ll[[s]][,distance := 0]

	st <- names(RE.coefs)
	
	# predict states
	prst <- st[-which(st==s)]

	# loop over all states except s
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
	attr(ll,"pred.for") <- prst

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