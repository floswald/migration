




#' Summary Statistics from Sipp
#'
#' @param save file to save
#' @examples
#' load("~/Dropbox/mobility/SIPP/SippFull.RData")
#' z = Sipp.SumStats(merged,"~/Dropbox/mobility/output/data/sipp/sumstats.RData")
Sipp.SumStats <- function(d,saveto="~/Dropbox/mobility/output/data/sipp/sumstats.RData"){

	l <- list()

	# monthly state-to-state transitions
	l$mS2S <- d[,mean(S2S)]

	kv <- c("S2S","HHincome","numkids","age","sex","wealth","home.equity","thhmortg","own","yr_bought","mortg.rent","college","saving","year","born.here")

	# summary of key vars
	l$sum <- summary(d[,kv,with=FALSE])

	# monthly state-to-state transitions by current state
	l$mS2S_state <- d[,mean(S2S),by=state]

	# get movers
	movtmp <- d[S2S==TRUE,list(upid=unique(upid))]
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
	l$num.moves <- movers[,list(moves=sum(S2S,na.rm=T)),by=upid][,table(moves)]

	# get to and from
	#setkey(movers,yrmnid)
    #movers[,c("from","to") := list("hi","there")]
	#movers[,c("from","to") := get.istate(states=state,imove=S2S),by=upid]

	#setkey(movers,upid,yrmnid)
	#movers[,c("from","to") := list(c(state[-length(state)],NA), c(state[-1],NA)), by=upid]

	# moves back to home
	l$moves.home <- movers[S2S==TRUE,list(from.home=sum(from==state.born,na.rm=T)/length(from),
											 to.home  =sum(to==state.born,na.rm=T)/length(from),
											 to.other =sum(to!=state.born,na.rm=T)/length(from))]

	moves <- movers[S2S==TRUE]

	# get periods where amove happens and get age, from, to
	mvtab <- moves[,list(mid=cumsum(S2S),from=from[S2S],to=to[S2S],wave=wave[S2S],yearmon=yearmon[S2S],age=age[S2S]),by=upid]
	setkey(mvtab,from,to)

	## get distances data
	#data(State_distTable_agg,package="EconData")
      
	#setkey(State_distTable_agg,from,to)
	#mvtab <- State_distTable_agg[ mvtab ]
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
#' @family IncomePrediction
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
makePrediction1 <- function(y,RE.coefs,with.FE,State_dist) {

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

		ll <- makePrediction2(s,RE.coefs,m,with.FE,tmps,State_dist)

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
makePrediction2 <- function(s,RE.coefs,m,with.FE,tmps,State_dist){
	
	ll <- list()	
		
	# copy current state 
	# don't need to predict anything here
	ll[[s]] <- copy(tmps)
	ll[[s]][,move.to  := state]
	ll[[s]][,distance := 0]

	st <- names(RE.coefs)
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

		# copy origin state data.table
		# into slot j of list
		ll[[j]] <- copy(tmps)

		# change destination
		ll[[j]][,move.to := j ]
		# get distance
		ll[[j]][,distance := State_dist[s,j] ]
		
		# predict income there
		# NOTICE that
		# this predicts MONTHLY log household income
		ll[[j]][,logHHincome := myPredict(data=m,beta=be)]

	}

	ll <- rbindlist(ll)
	ll[,c("age2","cohort1920","cohort1940","cohort1960","cohort1980") := NULL]
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

#' get home values and adjust by inflation
#'
#' @family IncomePrediction
getHomeValues <- function(){

	data(HomeValues,package="EconData")
	data(CPIHOSSL,package="EconData")	# monthly xts data on inflation
	cpi.h <- xts::to.yearly(CPIHOSSL)[,1]

	# set 1996 as base year
	coredata(cpi.h) <- coredata(cpi.h) / as.numeric(cpi.h['1996'])
	names(cpi.h) <- "cpiH"

	cpi <- data.table(year=year(index(cpi.h)),cpiH=coredata(cpi.h),key="year")
	setnames(cpi,c("year","cpiH"))

	# divide by 1000$
	HV <- HomeValues[,list(Home.Value=mean(Home.Value)/1000),by=list(State,year(qtr))]
	setkey(HV,year)

	# adjust by inflation 
	HV <- cpi[HV]
	HV[,HValue96 := Home.Value / cpiH ]
	HV[,c("cpiH","Home.Value") := NULL]

	# aggregate states: c("ME.VT","ND.SD.WY")
	HV.ME.VT <- HV[State %in% c("ME","VT"),list(HValue96 = mean(HValue96)),by=list(year)]
	HV.ME.VT[, State := "ME.VT"]
	HV.ND.SD.WY <- HV[State %in% c("ND","SD","WY"),list(HValue96 = mean(HValue96)),by=list(year)]
	HV.ND.SD.WY[, State := "ND.SD.WY"]

	# delete from table ...
	HV <- HV[!State %in% c("ME","VT","ND","SD","WY")]

	# ... and add new states
	HV <- rbind(HV,HV.ME.VT,HV.ND.SD.WY,use.names=TRUE)

	return(HV)
}



#' multinomial logit model of location choice
#'
#' @examples 
#' if (Sys.info()["user"] == "florianoswald" ){
#' load("~/Dropbox/mobility/output/model/BBL/logit.RData")
#' } else {
#' load("C:/Users/florian_o/Dropbox/mobility/output/model/BBL/logit.RData")
#' }
#' res <- runMNLogit(d=l,fraction=0.3)
#' p1 = l[1:48]
#' pr1=direct.predict(newdata=p1,object=res,probability=TRUE)
#' # plot(pr1[-grep("ID",colnames(pr1))])
runMNLogit <- function(d,fraction=1){


	if (fraction<1) {

		cat(sprintf("taking %d %% random sample from full data\n",fraction*100))

		su <- d[,sample(unique(upid),size=round(fraction*length(unique(upid))))]
		setkey(d,upid)
		d <- d[.(su)]
	}

	d <- d[complete.cases(d[,list(HValue96)])]

	# you cannot have stay:  d[,cor(choice,stay)]
	#fm = formula(choice ~ -1 + distance + logHHincome + HValue96 + stay | 1 | 1 )
	#fm = formula(choice ~ -1 + distance | age | logHHincome + HValue96 )
	#fm = formula(choice ~ -1 + distance + HValue96  | age + I(age^2) + numkids + born.here| logHHincome )
	#fm = formula(choice ~ -1 + distance + HValue96  | age + numkids + born.here| logHHincome )
	fm = formula(choice ~ -1 + distance + HValue96  | age + I(age^2) + numkids | logHHincome )
	#fm = formula(choice ~ -1 + distance + HValue96  | age + I(age^2) + born.here | logHHincome )
	res = mnlogit(fm,d,"move.to",ncores=1,print.level=1,maxiter=100)

	return(res)
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
#' @param movreg data.table with movers register. this is a data.table
#' as produced by \code{\link{Sipp.Sumstats}}, with columns \code{from,to,upid,age} at the minium.
#' @param with.FE TRUE if you want to make prediction in state k with 
#' individual fixed effect estimated for state j.
#' @family IncomePrediction
#' @return data.table with predicted incomes
#' @examples
#' load("~/Dropbox/mobility/SIPP/prelogit.RData")
#' load("~/Dropbox/mobility/output/model/BBL/income-REcoefs.RData")
#' load("~/Dropbox/mobility/output/data/sipp/sumstats.RData")
#' mvt <- sumstats$mvtab
#' mvt[,c("wave","mid","yearmon","from") := NULL]
#' l <- buildLogit(prelogit,RE.coefs,movreg=mvt)
buildLogit <- function(logi,RE.coefs,with.FE=FALSE,verbose=TRUE,saveto="~/Dropbox/mobility/output/model/BBL/logit.RData",movreg){

	#load(file.path(modelpath,"income-REmodels.RData"))		# contains RE.models

	# take the first obs by age for time=constant
	# variables and compute mean for numerics.

	# logi is on a monthly basis 
	# predict income on an annual basis only: aggregate by age

	setkey(logi,upid,age)
	y <- logi[,list(logHHincome=mean(logHHincome,na.rm=T),
					wealth     = mean(wealth,na.rm     = T),
					mortg.rent = mean(mortg.rent,na.rm = T),
					age2       = age2[1],
					year       = year[1],
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

	# initiate choice column as zero choice
	#y[,c("to.tmp","choice","distance") := list(state,FALSE,0)]


	# get distances data
	data(State_distMat_agg,package="EconData")

	# make predictions of income
	l <- makePrediction1(y,RE.coefs,with.FE,State_distMat_agg)
	gc()

	# get homevalues by year and state
	# inflation adjusted
	l <- mergeHomeValues(l)



	# merge back into monthly data


	# bring in the moving data
	# ------------


	# merge with l
	# ============

	# add a choice indicator
	# ======================
	l[,choice := FALSE]
	l[,stay   := FALSE]

	
	# add the moving choices and merge
	l <- mergePredIncomeMovingHist(l,movreg)

	gc()

	if (!is.null(saveto)){
		if (verbose) cat("all done. saving file.\n")
		save(l,file=saveto)
	}

	return(l)

}

mergeHomeValues <- function(d){
	
	# get Home Values by year/state
	HV <- getHomeValues()
	
	# add to l
	# --------
	
	setnames(HV,"State","move.to")
	setkey(HV,move.to,year)

	setkey(d,move.to,year)

	# merge! huge!
	d <-  HV[ d ]

	return(d)
}


mergePredIncomeMovingHist <- function(l,mvt){


	setkey(mvt,upid,age)
	setkey(l,upid,age)

	# data for all states in age where
	# upid moved
	yesmove <-  l[mvt]

	yesmove <- DTSetChoice(yesmove)
	yesmove[,c("to") := NULL]



	# data of all non-movers
	# here you erase ages of movers where they have not moved!
	l <- l[!J(yesmove[,list(upid,age)])]

	# set choice of all stayers
	l[state==move.to , choice := TRUE ]

	# stack back together
	setcolorder(yesmove,names(l))
	l <- rbindlist(list(l,yesmove))
	l[move.to==state, stay := TRUE]

	return(l)
}


DTSetChoice <- function(DT){

	# whereever the move.to column is equal to the 
	# merged value from mvt, "to", a move took place. 
	DT[move.to==to, choice := TRUE,by=list(upid,age)]

	# do by age because there I allow for multiple moves by age (if observed in data)

	return(DT)

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






