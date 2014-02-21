




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
	AR1 <- lapply(st, function(x) {cat(sprintf("estimating model for %s\n",x)); lme(logHHincome ~ age + I(age^2) +cohort , random=~1|upid,correlation=corAR1(0,form=~yrmnid|upid),data=subset(dat,state==x))})
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
	RE.models <- AR1
	save(RE.models,file=file.path(path,"income-REmodels.RData"))

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
#' @param datapath location of SippIncome.RData
#' @param modelpath location of SippIncome.RData
#' @return TRUE
predict.income <- function(datapath="~/Dropbox/mobility/SIPP/",modelpath="~/Dropbox/mobility/output/model/BBL"){

	load(file.path(datapath,"SippIncome.RData"))	# contains income
	load(file.path(modelpath,"income-REmodels.RData"))		# contains RE.models
	load(file.path(modelpath,"income-REcoefs.RData"))		# contains RE.coefs

	# take the first obs by age
	# you will not have predictions vary by month
	y <- income[,list(logHHincome=logHHincome[1],state=state[1],cohort=cohort[1]),by=list(upid,age)]

	st <- names(RE.coefs)
    setkey(y,upid,age,state)

	# for each upid,age combination, 
	# take the guys random effect, find difference from 
	# region j's intercept, predict population income in k,
    # and finally add the personal effect.

	for (s in st){

		# for all guys in s
		tmp <- y[state==s]

		# tmp is sorted by upid
		setkey(tmp,upid)

		# adds column "intercept" from RE regression
		tmp <- tmp[ RE.coefs[[s]]$RE ]

		# adjust "intercept" to be difference to population intercept:
		tmp[, intercept := intercept - RE.coefs[[s]]$fixed[[1]] ]
		
		# predict wage in all states k
		for (j in st[-which(st==s)]){

			# level=0 predicts population effect only
			
			#m <- as.matrix(tmp[,list(1,intercept,age,age2=age^2,cohort1920,cohort1940,cohort1960,cohort1980)])	# (n by [1,alpha_i,age,age2,...] )
			#be <- RE.coefs[[j]]$fixed
			#be <- c(be[1],1,be[-1])	# be = [beta0, 1, beta1, ... betak]
			

			#expr <- paste0("tmp[, ", paste0("HHincome.",j), " := myPredict(data=m,beta=be]")
			expr <- paste0("tmp[, ", paste0("logHHincome.",j), " := predict(RE.models[[j]],newdata=tmp[,list(upid,age,cohort)],level=0) + intercept ]")
			eval(parse(text=expr))
		}

		# merge back into y
		tmp[,c("logHHincome","cohort","state","intercept") := NULL]

		setkey(tmp,upid,age)
		y <- y[tmp]
		rm(tmp)
		gc()

	}

	save(y,file=file.path(modelpath,"predIncome.RData"))

	return(TRUE)

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






