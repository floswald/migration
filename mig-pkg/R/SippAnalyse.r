




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

	save(l,file=saveto)
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
#' @return list for each state with coefficients and fixed effects
#' for each individual. saves data.
#' @examples
#' load("~/Dropbox/mobility/SIPP/SippFull.RData")
#' l <- RE.HHincome(dat=merged[HHincome>0 & state %in% c("AZ","AL","AR"),list(upid,yrmnid,HHincome,age,year,state)],path="~/Dropbox/mobility/output/model/BBL",type="html")
RE.HHincome <- function(dat=merged[HHincome>0,list(upid,yrmnid,HHincome,age,year,state)],
						path="~/Dropbox/mobility/output/model/BBL",
						type="tex"){

	coyrs = seq(1900,1990,by=10)
	dat[,coh := .SD[,year-age][[1]], by=upid ]
	dat[,cohort := coyrs[findInterval(coh,coyrs)]]
	dat[,coh := NULL]
	st <- dat[,unique(state)]
	AR1 <- lapply(st, function(x) lme(log(HHincome) ~ age +I(age^2) + factor(cohort) , random=~1|upid,correlation=corAR1(0,form=~yrmnid|upid),data=subset(dat,state==x)))
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
	out <- lapply(AR1,lme.getCoefs)
	save(out,file=file.path(path,"income-RE.RData"))

	return(out)
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






