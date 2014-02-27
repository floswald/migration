




#' Summary Statistics from Sipp
#'
#' @param save file to save
#' @examples
#' load("~/Dropbox/mobility/SIPP/SippFull.RData")
#' z = Sipp.SumStats(merged,NULL)
Sipp.SumStats <- function(d,saveto="~/Dropbox/mobility/output/data/sipp/sumstats.RData"){

	l <- list()

	# monthly state-to-state transitions
	l$mS2S <- d[,mean(S2S.mn)]

	kv <- c("S2S.mn","HHincome","numkids","age","sex","wealth","home.equity","thhmortg","own","yr_bought","mortg.rent","college","saving","year","born.here")

	# summary of key vars
	l$sum <- summary(d[,kv,with=FALSE])

	# monthly state-to-state transitions by current state
	l$mS2S_state <- d[,mean(S2S.mn),by=state]

	# get movers
	movtmp <- d[S2S.mn==TRUE,list(upid=unique(upid))]
	setkey(d,upid)
	movers <- d[ movtmp[,upid] ]
	stayers <- d[ !upid %in%  movtmp[,upid] ]

	# get sum stats for movers
	means.mov <- movers[,lapply(.SD,mean,na.rm=T),.SDcols=kv]
	# means stayers
	means <- stayers[,lapply(.SD,mean,na.rm=T),.SDcols=kv]
	l$means <- as.data.frame(rbind(means,means.mov))
	rownames(l$means) <- c("never.moved","moved")

	# number of moves per person
	l$num.moves <- movers[,list(moves=sum(S2S.mn,na.rm=T)),by=upid][,table(moves)]

	# get to and from
	setkey(movers,yrmnid)
    movers[,c("from","to") := list("hi","there")]
	movers[,c("from","to") := get.istate(states=state,imove=S2S.mn),by=upid]

	# moves back to home
	l$moves.home <- movers[S2S.mn==TRUE,list(from.home=sum(from==state.born,na.rm=T)/length(from),
											 to.home  =sum(to==state.born,na.rm=T)/length(from),
											 to.other =sum(to!=state.born,na.rm=T)/length(from))]

	moves <- movers[S2S.mn==TRUE]
	mvtab <- moves[,list(mid=cumsum(S2S.mn),from=from[S2S.mn],to=to[S2S.mn],wave=wave[S2S.mn],yearmon=yearmon[S2S.mn],age=age[S2S.mn]),by=upid]
	l$movers <- movers
	l$mvtab <- mvtab


	# get location transition matrix of movers
	l$trans <- movers[,table(from,to)]

	if (!is.null(saveto)){
	   sumstats=l
		save(sumstats,file=saveto)
	}
	return(l)
}


#' Get Movers from full data
#'
getMovers <- function(d){

	movtmp <- d[S2S.mn==TRUE,list(upid=unique(upid))]
	setkey(d,upid)
	movers <- d[ movtmp[,upid] ]

	return(movers)
}


#' reduced form models
#'
reduced.form <- function(d){

	#m[[1]] <- glm( S2S.mn ~ HHincome + index_sa + numkids + home.equity , data=d)
	#m[[2]] <- glm( S2S.mn ~ HHincome + index_sa + numkids + age + wealth + mortg.rent, data=d)
	#m[[3]] <- glm( S2S.mn ~ HHincome + index_sa + numkids + age + wealth + mortg.rent + college, data=d)
	#m[[4]] <- glm( S2S.mn ~ HHincome + numkids + age + wealth + duration_at_current + college + born.here + dindex, data=d)

	# split models by own/rent

	own <- list()
	rent <- list()

	# TODO S2S.mn should be "move in next period"!
	own$move <- glm( S2S.lead ~ HHincome + ddnumkids + age + I(age^2) + wealth + duration_at_current + college + born.here + home.equity + dindex, data=d[own==TRUE], family=binomial(link="logit"))
	own$sell <- glm( sell ~ HHincome + ddnumkids + age + I(age^2) + wealth + duration_at_current + college + born.here + home.equity + dindex, data=d[own==TRUE], family=binomial(link="logit"))

	#rent$buy <- 
	#rent$buy <- 



	m$saving <- lm(saving ~ age + I(age^2) + wealth + HHincome + mortg.rent + state + ddnumkids,data=merged)



	return(m)

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
#' \item v_it = rho v_it-1 + eps_it
#' \item eps ~ iid N(0,sigma_eps),
#' \item mu  ~ iid N(0,sigma_mu),
#' \item mu independent of v_it. v_i0 ~ N(0,sigma_eps^2 / (1-rho^2))
#' \item The random effects assumption is incorporated by assuming that
#' \item mu_i is iid (uncorrelated with X).
#' }}
#' @param dat data set of income relevant variables
#' @return list for each state with coefficients and fixed effects
#' for each individual. saves data.
#' @examples
#' load("~/Dropbox/mobility/SIPP/SippIncome.RData")
#' l <- RE.HHincome(dat=income[state %in% c("AZ","AL","AR")],path="~/Dropbox/mobility/output/model/BBL",type="html")
RE.HHincome <- function(dat,
						path="~/Dropbox/mobility/output/model/BBL"){

	st <- dat[,unique(state)]
	st <- st[order(st)]



	# this formulation if you use the predict.lme
	#AR1 <- lapply(st, function(x) {cat(sprintf("estimating model for %s\n",x)); lme(logHHincome ~ age + I(age^2) +cohort , random=~1|upid,correlation=corAR1(0,form=~yrmnid|upid),data=subset(dat,state==x))})
	AR1 <- lapply(st, function(x) {cat(sprintf("estimating model for %s\n",x)); lme(logHHincome ~ age + age2 + cohort1920  + cohort1940 + cohort1960 + cohort1980, random=~1|upid,correlation=corAR1(0,form=~yrmnid|upid),data=subset(dat,state==x))})
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

		
#' Make a MN Logit estimation datset
#'
#' takes output of \code{\link{RE.HHincome}} and
#' predicts income in all locations. same for house prices
#'
#' We compute the the linear predictor of the model in
#' \code{\link{RE.HHincome}}, i.e. shocks are irrelevant.
#'
#' @param income data.table from \code{\link{subset.all}}
#' @param RE.coefs regression coefs from \code{\link{RE.HHincome}}
#' @param verbose TRUE/FALSE
#' @param saveto if not NULL, where to save
#' @param with.FE TRUE if you want to make prediction in state k with 
#' individual fixed effect estimated for state j.
#' @return data.table with predicted incomes
#' @examples
#' load("~/Dropbox/mobility/SIPP/prelogit.RData")
#' load("~/Dropbox/mobility/output/model/BBL/income-REcoefs.RData")
#' l <- buildLogit(prelogit,RE.coefs)
buildLogit <- function(logi,RE.coefs,with.FE=FALSE,verbose=TRUE,saveto="~/Dropbox/mobility/output/model/BBL/logit.RData"){

	#load(file.path(modelpath,"income-REmodels.RData"))		# contains RE.models

	# take the first obs by age for time=constant
	# variables and compute mean for numerics.

	# log hh income is on a monthly basis and
	# you compute the mean of that by age.
	setkey(logi,upid,age)
	y <- logi[,list(logHHincome=mean(logHHincome,na.rm=T),
					wealth     = mean(wealth,na.rm     = T),
					mortg.rent = mean(mortg.rent,na.rm = T),
					age2       = age2[1],
					born       = born[1],
					born.here  = born.here[1],
					college    = college[1],
					numkids    = numkids[1],
					state      = state[1],
					cohort     = cohort[1],
					cohort1920 = cohort1920[1],
					cohort1940 = cohort1940[1],
					cohort1960 = cohort1960[1],
					cohort1980 = cohort1980[1]),
               by=list(upid,age)]



	st <- names(RE.coefs)
	st <- st[order(st)]
    setkey(y,upid,age,state)

	# for each upid,age combination, 
	# take the guys random effect, find difference from 
	# region j's intercept, predict population income in k,
    # and finally add the personal effect.

	# format
	# ======

	# logit model requires long data.

	# strategy: take copy of 'income' by state to get
	# id    age    state    y
	#  1     29       WI    1000
	#  1     30       WI    1030
	#  1     31       WI    1100
	#  ...

	# then a list tmp <- list()
	# tmp[[1]] <- copy(income[state=='WI'])
	# tmp[[2]] <- copy(income[state=='WI'])
	# tmp[[2]][, state := newState ]
	# tmp[[2]][,     y := predictYinNewState]

	# list for holding current state groups
	l <- list()

	for (s in st){


		# for all guys in s
		tmps <- y[state==s]


		# l[[s]] is sorted by upid
		setkey(tmps,upid)

		# adds column "intercept" from RE regression
		tmps <- RE.coefs[[s]]$RE[ tmps ]

		# adjust "intercept" to be difference to population intercept:
		tmps[, intercept := intercept - RE.coefs[[s]]$fixed[[1]] ]
		
		if (with.FE){
			m <- as.matrix(tmps[,list(1,intercept,age,age2,cohort1920,cohort1940,cohort1960,cohort1980)])	# (n by [1,alpha_i,age,age2,...] )
		} else {
			m <- as.matrix(tmps[,list(1,age,age2,cohort1920,cohort1940,cohort1960,cohort1980)])	
		}

		#if (verbose) cat("in: \n")

		# rbindlist does not rbind by column name
		# take care here to keep the ordering of columns constant!

		# predict wage in all states k
		ll <- list()
		
		# save current state as copy in the result list
		ll[[s]] <- copy(tmps)

		for (j in st[-which(st==s)]){

				
			# get regression coefficients 
			# pertaining to state j

			if (with.FE){
				be <- RE.coefs[[j]]$fixed
				be <- c(be[1],1,be[-1])	# be = [beta0, 1, beta1, ... betak]

			} else {

				be <- RE.coefs[[j]]$fixed
			}

			# copy origin state data.table
			ll[[j]] <- copy(tmps)

			# change to current state
			ll[[j]][,state := j ]
			# predict income there

			# NOTICE that
			# this predicts MONTHLY log household income
			ll[[j]][,logHHincome := myPredict(data=m,beta=be)]

		}
			
		tmp2 <- rbindlist(ll)
		rm(ll)
		# remove age2, year and cohort dummies
		tmp2[,c("age2","cohort1920","cohort1940","cohort1960","cohort1980") := NULL]


		l[[s]] <- tmp2
		rm(tmps,tmp2)
		gc()

	if (verbose) cat(sprintf("done with income prediction for origin %s\n",s))
	}


	# join all lists
	l <- rbindlist(l)


	# this data needs to be merged by an age-cohort => calendar time
	# mapping

	# TODO!



	# add price/income to that
	data(HomeValues,package="EconData")
	data(CPIHOSSL,package="EconData")	# monthly xts data on inflation
	cpi.h <- xts::to.yearly(CPIHOSSL)[,1]
	# set 1996 as base year
	coredata(cpi.h) <- coredata(cpi.h) / as.numeric(cpi.h['1996'])
	# subset to 1996 -
	#cpi.h <- cpi.h['1996::']
	names(cpi.h) <- "cpiH"

	cpi <- data.table(year=year(index(cpi.h)),cpiH=coredata(cpi.h),key="year")
	setnames(cpi,c("year","cpiH"))

	# divide by 1000$
	HV <- HomeValues[,list(Home.Value=mean(Home.Value)/1000),by=list(State,year(qtr))]
	setkey(HV,year)
	# adjust by inflation 
	HV <- cpi[HV]
	HV[,HValue96 := Home.Value / cpiH ]

	
	# add to l
	# --------
	
	setnames(HV,"State","state")
	setkey(HV,state,year)

	# create calyear variable in income dataset
	l[, year := age + born]
	setkey(l,state,year)

	# merge! huge!
	l <- HV[ l ]


	# bring in the moving data
	load("~/Dropbox/mobility/output/data/sipp/sumstats.RData")
	mvt <- sumstats$mvtab
	mvt[,c("mid","wave","yearmon") := NULL]
	setkey(mvt,upid,age)

	setkey(l,upid,age)

	l <- l[mvt]









	if (!is.null(saveto)){
		if (verbose) cat("all done. saving file.\n")
		save(l,file=saveto)
	}

	return(l)

}






myPredict <- function(data,beta){
	return(data %*% beta)
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
	return(r)
}




#' Get House Value distributions
#'
#' decompose p_ijt = mu_j + mu_t + e_ijt
#' to get ecdf of e_ijt
#' @examples 
#' load("~/git/migration/data/hvalues.RData")
#' p <- Hval.ecdf(d=hvalues)
Hval.ecdf <- function(d){

	des <- svydesign(id=~1,weights=~HHweight,data=d)
	m <- svyglm( formula=log(hvalue) ~ factor(year) + state , design= des)



}




#' Probit of Buy
#'
#' in each location, estimate the probability
#' of buying.
#'
#' Pr[buy_it==TRUE | y_it,wealth_it/y_it,price_jt/income_it,numkids_it, age] or even
#' Pr[buy_it==TRUE |      wealth_it/y_it,price_jt/income_it,numkids_it, age]






