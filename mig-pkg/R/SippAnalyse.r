




#' Summary Statistics from Sipp
#'
#' @param save file to save
Sipp.SumStats <- function(d,saveto){

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
	movers[,c("from","to") := get.istate(states=state,imove=S2S.mn),by=upid]

	# moves back to home
	l$moves.home <- movers[S2S.mn==TRUE,list(from.home=sum(from==state.born,na.rm=T)/length(from),
											 to.home  =sum(to==state.born,na.rm=T)/length(from),
											 to.other =sum(to!=state.born,na.rm=T)/length(from))]

	# moves away from home


	# get location transition matrix of movers
	l$trans <- movers[,table(from,to)]

	if (!is.null(saveto)) save(l,file=saveto)
	
	return(l)
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
#' model is y_it = beta + gamma'X_it + mu_i + v_it, where
#' v_it = rho v_it-1 + eps_it
#' eps ~ iid N(0,sigma_eps),
#' mu  ~ iid N(0,sigma_mu),
#' mu independent of v_it. v_i0 ~ N(0,sigma_eps^2 / (1-rho^2))
#' The random effects assumption is incorporated by assuming that
#' mu_i is iid (uncorrelated with X).
#' @param dat data set of income relevant variables
#' @return list for each state with coefficients and fixed effects
#' for each individual. saves data.
#' @examples
#' load("~/Dropbox/mobility/SIPP/SippIncome.RData")
#' l <- RE.HHincome(dat=income[state %in% c("AZ","AL","AR")],path="~/Dropbox/mobility/output/model/BBL",type="html")
RE.HHincome <- function(dat,
						path="~/Dropbox/mobility/output/model/BBL",
						type="tex"){

	st <- dat[,unique(state)]
	AR1 <- lapply(st, function(x) lme(log(HHincome) ~ age + I(age^2) + cohort , random=~1|upid,correlation=corAR1(0,form=~yrmnid|upid),data=subset(dat,state==x)))
	names(AR1) <- st

	if (type=="tex"){
		# print results to tex files
		for (i in st){

			fi <- paste0("incomeRE-",i,".tex")
			texreg(list(AR1[[i]]),file=file.path(path,fi),include.RE=TRUE,booktabs=TRUE,dcolumn=TRUE)

		}
	} else if (type=="html"){
		# print all into one html file
			fi <- "incomeRE-all.html"
			htmlreg(AR1,file=file.path(path,fi),include.RE=TRUE,caption="all models",custom.model.names=paste0("income process in ",st))

	}


	# save coefs into a handy list
	RE.HHincome.models <- AR1
	RE.coefs <- lapply(AR1,lme.getCoefs)
	save(RE.HHincome.models,RE.coefs,file=file.path(path,"income-RE.RData"))

	return(RE.coefs)
}

		
#' Predict income in all locations
#'
#' takes output of \code{\link{RE.HHincome}} and
#' predicts income in all locations. 
#'
#' We compute the the linear predictor of the model in
#' \code{\link{RE.HHincome}}, i.e. shocks are irrelevant.
#' 
#'
#' @param path location of income-RE.RData and SippIncome.RData
predict.income <- function(datapath="~/Dropbox/mobility/SIPP/",modelpath="~/Dropbox/mobility/output/model/BBL"){

	load(file.path(datapath,"SippIncome.RData"))	# contains income
	load(file.path(modelpath,"income-RE.RData"))		# contains RE.HHincome.model and RE.coefs

	# take the first obs by age
	y <- income[,list(HHincome=HHincome[1],cohort=cohort[1],state=state[1]),by=list(upid,age)]

	st <- y[,unique(state)]
  setkey(y,state,upid,age)

	# for each upid,age combination, 
	# take the guys random effect, add to
	# region j's intercept, and multiply out other variables

	for (s in st){

    # for all guys in s
		tmp <- y[.(s)]
    setkey(tmp,upid)
    tmp <- tmp[ RE.coefs[[s]]$RE ]
    
    # predict wage in all states k
    for (j in st[-which(st==s)]){
      tmp <- cbind(tmp, predict(RE.HHincome.models[[j]],newdata=tmp[,list(upid,age,cohort)],level=0) ) # get population (fixed) prediction in this state
    }
    setnames(tmp,7:ncol(tmp),paste0("predIncome.",st[-which(st==s)]))
    tmp[,7:ncol(tmp) := lapply(.SD[,7:ncol(tmp) + intercept ,with=FALSE])]
	}


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






