


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
getHomeValues <- function(freq="yearly"){

	data(HomeValues,package="EconData")
	data(CPIHOSSL,package="EconData")	# monthly xts data on inflation

	if(freq=="yearly"){
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

	} else if (freq=="quarterly"){

		cpi.h <- xts::to.quarterly(CPIHOSSL)[,1]

		# set 1996 as base year
		coredata(cpi.h) <- coredata(cpi.h) / as.numeric(cpi.h['1996-01-01'])
		names(cpi.h) <- "cpiH"

		cpi <- data.table(qtr=index(cpi.h),cpiH=coredata(cpi.h),key="qtr")
		setnames(cpi,c("qtr","cpiH"))

		# divide by 1000$
		HV <- HomeValues[,list(Home.Value=mean(Home.Value)/1000),by=list(State,qtr)]
		setkey(HV,qtr)

		# adjust by inflation 
		HV <- cpi[HV]
		HV[,HValue96 := Home.Value / cpiH ]
		HV[,c("cpiH","Home.Value") := NULL]

		# aggregate states: c("ME.VT","ND.SD.WY")
		HV.ME.VT <- HV[State %in% c("ME","VT"),list(HValue96 = mean(HValue96)),by=qtr]
		HV.ME.VT[, State := "ME.VT"]
		HV.ND.SD.WY <- HV[State %in% c("ND","SD","WY"),list(HValue96 = mean(HValue96)),by=list(qtr)]
		HV.ND.SD.WY[, State := "ND.SD.WY"]

		# delete from table ...
		HV <- HV[!State %in% c("ME","VT","ND","SD","WY")]

		# ... and add new states
		HV <- rbind(HV,HV.ME.VT,HV.ND.SD.WY,use.names=TRUE)


	}

	return(HV)
}



#' Run multinomial logit model of location choice
#'
#' @family LogitModel FirstStage
#' @examples 
#' if (Sys.info()["user"] == "florianoswald" ){
#' load("~/Dropbox/mobility/output/model/BBL/logit30.RData")
#' } else {
#' load("C:/Users/florian_o/Dropbox/mobility/output/model/BBL/logit30.RData")
#' }
#' res <- runMNLogit(d=l)
#' # plot(pr1[-grep("ID",colnames(pr1))])
runMNLogit <- function(d,saveto="~/Dropbox/mobility/output/model/BBL/logit30Res.RData"){

	d <- d[complete.cases(d[,list(HValue96)])]

	# you cannot have stay:  d[,cor(choice,stay)]
	#fm = formula(choice ~ -1 + distance + logHHincome + HValue96 + stay | 1 | 1 )
	#fm = formula(choice ~ -1 + distance | age | logHHincome + HValue96 )
	#fm = formula(choice ~ -1 + distance + HValue96  | age + I(age^2) + numkids + born.here| logHHincome )
	#fm = formula(choice ~ -1 + distance + HValue96  | age + numkids + born.here| logHHincome )
	#fm = formula(choice ~ -1 + distance + HValue96  | age + I(age^2) + numkids | logHHincome )
	#fm = formula(choice ~ -1 + distance + HValue96  | age + I(age^2) + mortg.rent | logHHincome )
	#fm = formula(choice ~ -1 + distance + HValue96  | age + I(age^2) + own  + numkids | logHHincome )
	fm = formula(choice ~ -1 + distance + HValue96  | age + I(age^2) + own  | logHHincome )
	#fm = formula(choice ~ -1 + distance + HValue96  | age + I(age^2) + born.here | logHHincome )


	# add a case id
	res = mnlogit(fm,d,"move.to",ncores=1,print.level=1,maxiter=100,chid="upid2",linDepTol=0.0001)

	if (!is.null(saveto)){
		save(res,file=saveto)
	}

	return(res)
}






#' @family LogitModel
simulateMoveLogit <- function(m){
	m[,sim.move := move.to[ findInterval(runif(1), cumsum(prediction)) +1], by=upid2]
}

#' @family LogitModel
makeMovingIndicatorsLogit <- function(r,m){

	r[,sim.move                                   := m[,sim.move] ]
	r[,stay.model                                 := FALSE]
	r[move.to==sim.move & distance==0, stay.model := TRUE]
	r[,stay.data                                  := FALSE]
	r[distance==0 & choice==TRUE, stay.data       := TRUE]
	r[,move.to.model := NA_character_ ]
	r[move.to==sim.move & distance > 0, move.to.model := sim.move]
	r[,move.to.data := NA_character_ ]
	r[distance > 0 & choice==TRUE, move.to.data:= move.to]

	return(r)

}

#' @family LogitModel
getFreqsLogit <- function(m,r){

	setkey(m,upid2,move.to)
	setkey(r,upid2,move.to)

	r <- makeMovingIndicatorsLogit(r,m)


	freqs <- r[,list(data.stay=sum(stay.data),model.stay=sum(stay.model)),by=move.to]

	tmp <- r[,as.data.frame(table(move.to.model))]
	freqs[,model.move.to := tmp$Freq]

	tmp <- r[,as.data.frame(table(move.to.data))]
	freqs[,data.move.to := tmp$Freq]

	freqs[,dataN := data.stay + data.move.to]
	freqs[,modelN := model.stay + model.move.to]

	props <- freqs[,list(move.to,dat.stay=data.stay/dataN,dat.mv=data.move.to/dataN,mod.stay=model.stay/modelN,mod.mv=model.move.to/modelN)]
	setcolorder(props,c(1,2,4,3,5))
	props <- props[,lapply(.SD,round,3),.SDcols=2:5,by=move.to]

	return(list(freqs=freqs,props=props))
}



#' simulate number of moves from mnlogit result
#'
#' @family LogitModel
#' @param res mnlogit result
#' @param print.to location to print table
simMovesFromLogit <- function(res,print.to="~/Dropbox/mobility/output/model/BBL/"){

	stopifnot(class(res) == "mnlogit" )

	# get prediciton on estimation data
	p <- data.table(direct.predict(res,probability=TRUE))
	p[,upid2 := res$data[,unique(upid2)] ]

	m <- melt(p,id.vars="upid2",variable.factor=FALSE,verbose=TRUE,variable.name="move.to",value.name="prediction")
	# simulate each case-id: given all 48 choices, which one do you choose?
	m <- simulateMoveLogit(m)


	r <- copy(res$data)
	fp <- getFreqsLogit(m,r)

	
	if (!is.null(print.to)){
	print(xtable(fp$freqs),file=file.path(print.to,"logit_pred_freqs.tex"),include.rownames=FALSE,floating=FALSE)
	print(xtable(fp$props),file=file.path(print.to,"logit_pred_props.tex"),include.rownames=FALSE,floating=FALSE)
	}


	return(fp)
}


#' print Logit Model Results
#'
#' @family LogitModel
#' @param res mnlogit result
#' @param omit regex of coefficients to omit from latex table. html tables don't omit anything.
#' @param path where to save tables
#' @return NULL
printLogitModel <- function(res,omit,path="~/Dropbox/mobility/output/model/BBL"){

	ncof <- names(coef(res))
	ncof <- gsub("I\\(|\\)|\\^","",ncof)

	texreg(list(res), custom.model.names="$Pr(\\text{move to} = k)$",file=file.path(path,"logitRes.tex"),
		   omit.coef=omit,booktabs=TRUE,dcolumn=TRUE,caption="logit model results",use.packages=FALSE,
		   table=FALSE,sanitize.text.function = function(x){x}, custom.coef.names=ncof)
	htmlreg(list(res), custom.model.names="Pr(move to k)",file=file.path(path,"logitRes.html"),
		   omit.coef=NA,booktabs=TRUE,floating=FALSE,dcolumn=TRUE, custom.coef.names=ncof)

	screenreg(list(res), custom.model.names="Pr(move to k)",
		   omit.coef=omit, custom.coef.names=ncof)

	return(NULL)

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
#' @param saveto if not NULL, where to save small data
#' @param movreg data.table with movers register. this is a data.table
#' as produced by \code{\link{Sipp.Sumstats}}, with columns \code{from,to,upid,age} at the minium.
#' @param with.FE TRUE if you want to make prediction in state k with 
#' individual fixed effect estimated for state j.
#' @family LogitModel
#' @return data.table with predicted incomes
#' @examples
#' load("~/Dropbox/mobility/SIPP/prelogit.RData")
#' load("~/Dropbox/mobility/output/model/BBL/income-REcoefs.RData")
#' load("~/Dropbox/mobility/output/data/sipp/sumstats.RData")
#' mvt <- sumstats$mvtab
#' mvt[,c("wave","mid","from") := NULL]
#' l <- buildLogit(prelogit,RE.coefs,movreg=mvt)
buildLogit <- function(logi,RE.coefs,with.FE=FALSE,verbose=TRUE,saveto="~/Dropbox/mobility/output/model/BBL/logit.RData",savetosmall="~/Dropbox/mobility/output/model/BBL/logit30.RData",movreg){

	#load(file.path(modelpath,"income-REmodels.RData"))		# contains RE.models

	# take the first obs by age for time=constant
	# variables and compute mean for numerics.

	# logi is on a monthly basis 
	# predict income on an annual basis only: aggregate by age

	setkey(logi,upid,age)
	y <- logi[,list(logHHincome=mean(logHHincome,na.rm=T),
					wealth     = mean(wealth,na.rm     = T),
					mortg.rent = mean(mortg.rent,na.rm = T),
					own        = own[1],
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
		su <- l[,sample(unique(upid),size=round(0.3*length(unique(upid))))]
		setkey(l,upid)
		l <- l[.(su)]
		save(l,file=savetosmall)

	}

	return(l)

}

#' Merge Home Values with logit data
#' 
#' @family LogitModel
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


#' Merge Moving Histories with Logit data
#' 
#' @family LogitModel
mergePredIncomeMovingHist <- function(l,mvt){


	setkey(mvt,upid,age)
	setkey(l,upid,age)

	# data for all states in age where
	# upid moved
	yesmove <-  l[mvt]

	yesmove <- DTSetChoice(yesmove)

	# add column upid2
	yesmove[,upid2 := paste0(upid,"_",yearmon) ]
	yesmove[,c("to","yearmon") := NULL]



	# data of all non-movers
	# here you erase ages of movers where they have not moved!
	l <- l[!J(yesmove[,list(upid,age)])]

	# set choice of all stayers
	l[state==move.to , choice := TRUE ]

	# stack back together

	# add new col upid2 to l
	l[,upid2 := paste0( upid,"_",age )]

	setcolorder(yesmove,names(l))
	l <- rbindlist(list(l,yesmove))
	l[move.to==state, stay := TRUE]

	return(l)
}


#' Set discrete choice on mover's histories
#' 
#' @family LogitModel
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



#' Savings Policy Function Model
#'
#' estimates the reduced form for liquid savings form data
#' @examples
#' load("~/Dropbox/mobility/SIPP/Sipp4mn.RData")
savingsModel <- function(d,saveto="~/Dropbox/mobility/output/model/BBL/savings.RData"){

	# loaded 4-monthly data

	# throw away renters with positive house value
	d <- d[(own==TRUE) | (own==FALSE & hvalue==0)]

	# throw away negative incomes
	d <- d[HHincome>0]

	# TODO for probit of house choice
	# create current house value for renters from rent paid
	#d[own==FALSE, hvalue := mortg.rent * 12 / 0.075 ] 
	#d[,p2y := hvalue / HHincome]
	#d[,w2y := hvalue / wealth]


	f1 <- formula(saving ~ age + I(age^2) + HHincome +  mortg.rent + numkids + wealth + own)
	f2 <- formula(saving ~ age + I(age^2) + log(HHincome) + mortg.rent + numkids + wealth + own)

	lms <- list()
	lms[[1]] <- lm(formula=f1, data=d)
	lms[[2]] <- lm(formula=f2, data=d)
	
	# if survey design
	# coefs are identical
	svy <- list()
	des <- svydesign(id=~1,weights=~HHweight,data=d)
	svy[[1]] <- svyglm(formula=f1, design=des)
	svy[[2]] <- svyglm(formula=f2, design=des)

	out <- list(lm=lms,svy=svy)

	save(out,file=saveto)

	return(out)

}




#' Housing Status Policy Function Model
#'
#' estimates the reduced form for housing status changes form data
#' @param path
#' @param marginal TRUE if marginal effects 
#' @family FirstStage
#' @examples
#' load("~/Dropbox/mobility/SIPP/Sipp4mn.RData")
#' h <- housingModel(d=merged4mn)
housingModel <- function(d,path="~/Dropbox/mobility/output/model/BBL",marginal=FALSE){

	# loaded 4-monthly data in d

	# throw away renters with positive house value
	d <- d[(own==TRUE) | (own==FALSE & hvalue==0)]

	# throw away negative incomes
	d <- d[HHincome>0]

	# CAUTION
	# remember that HHIncome is MONTHLY INCOME!!
	d[,income := 12 * HHincome]

	# change in ownership
	setkey(d,upid,yrmnid)
	d[,down := c(diff(as.numeric(own)),0),by=upid]
	d[,buy := FALSE]
	d[down==1, buy := TRUE]
	d[,sell := FALSE]
	d[down==-1, sell := TRUE]

	d[,dkids := c(diff(numkids),0),by=upid]

	# get average home values
	hv <- getHomeValues(freq="quarterly")
	HV = hv[,list(state=State,qtr,HValue96)]
	setkey(HV,state,qtr)
	setkey(d,state,qtr)

	d <- HV[d]
	d[,p2y := HValue96 / income ] 
	d[wealth!= 0,p2w := HValue96 / wealth] 
	d[,age2 := age^2 ]

	rent <- d[own==FALSE,list(state,qtr,HValue96,income,numkids,HHweight,educ,age,age2,sex,wealth,mortg.rent,duration_at_current,born.here,p2y,p2w,buy,dkids)]

	# throw out all cases with some NA
	rent = rent[complete.cases(rent)]

	own <- d[own==TRUE,list(state,qtr,hvalue,income,numkids,HHweight,educ,age,age2,sex,mortg.rent,home.equity,duration_at_current,born.here,p2y,p2w,sell,wealth,dkids)]

	own = own[complete.cases(own)]

	# models
	m <- list()
	m$buylinear <- glm(buy ~ age + age2+dkids+ p2y + p2w  + mortg.rent ,data=rent,family=binomial(link="probit"),x=TRUE) 
	m$buyspline <- glm(buy ~ age + age2+dkids+ bs(p2y,knots=c(3,5),degree=1) + ns(p2w)  + mortg.rent ,data=rent,family=binomial(link="probit"),x=TRUE) 

	m$sellspline <- glm(sell ~ age + age2+dkids+income+ ns(home.equity,df=2) + mortg.rent + duration_at_current,data=own,family=binomial(link="probit"),x=TRUE)
	
	m$selllinear <- glm(sell ~ age + age2+dkids+income+ home.equity + mortg.rent + duration_at_current,data=own,family=binomial(link="probit"),x=TRUE)


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


			texreg(m[c("buylinear","buyspline")],custom.model.names=c("Pr(buy|rent)","Pr(buy|rent)"),stars=c(0.01,0.05,0.1),digits=4,file=file.path(path,"buy.tex"),caption="coefficient estimates",table=FALSE)
			htmlreg(m[c("buylinear","buyspline")],custom.model.names=c("Pr(buy|rent)","Pr(buy|rent)"),stars=c(0.01,0.05,0.1),digits=4,file=file.path(path,"buy.html"),caption="coefficient estimates")

			texreg(m[c("selllinear","sellspline")],custom.model.names=c("Pr(sell|own)","Pr(sell|own)"),stars=c(0.01,0.05,0.1),digits=4,file=file.path(path,"sell.tex"),caption="coefficient estimates",table=FALSE)
			htmlreg(m[c("selllinear","sellspline")],custom.model.names=c("Pr(sell|own)","Pr(sell|own)"),stars=c(0.01,0.05,0.1),digits=4,file=file.path(path,"sell.html"),caption="coefficient estimates")
		
		}

	}

	screenreg(m,digits=4,custom.model.names=names(m),stars=c(0.01,0.05,0.1))
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
	
	d[,age2 := age^2 ]
	d[,w2 := HHweight / 10000 ] 
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

