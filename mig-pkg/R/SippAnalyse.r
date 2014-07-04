



#' Sipp Summary Stats
#'
#'	 load("~/Dropbox/mobility/SIPP/Sipp_aggby_age.RData")
Sipp.SumStats <- function(d){

	s <- d[D2D<2,list(Children=weighted.mean(numkids,HHweight,na.rm=T),Income=weighted.mean(HHincome,HHweight,na.rm=T),Age=weighted.mean(age,HHweight,na.rm=T),Wealth=weighted.mean(wealth,HHweight,na.rm=T),Equity=weighted.mean(home.equity,HHweight,na.rm=T),College=weighted.mean(college,HHweight,na.rm=T),Own=weighted.mean(own,HHweight,na.rm=T)),by=D2D]

	m <- t(s)
	m <- m[-1,]
	colnames(m) <- c("Stay","Move")

	print(xtable(m,digits=2),file="~/Dropbox/mobility/output/data/sipp/stats.tex")
	return(m)

}

Sipp.moments <- function(d,svy){

	r <- list()

	# Population Proportions and Counts
	# =================================
	
	# number of moves per household as percentage of population
	nmoves = d[,list(moves=sum(D2D,na.rm=T),HHweight),by=upid]
	nmoves[,c("moved0","moved1","moved2") := list(moves==0,moves==1,moves>1)]
    nmoves[,moved1 := moves==1]
    nmoves[,moved2 := moves>1]
	r$moves0 <- nmoves[,list(moment="moved0",value=weighted.mean(moved0,HHweight), sd=sqrt(wtd.var(moved0,HHweight)))]
	r$moves1 <- nmoves[,list(moment="moved1",value=weighted.mean(moved1,HHweight), sd=sqrt(wtd.var(moved1,HHweight)))]
	r$moves2 <- nmoves[,list(moment="moved2",value=weighted.mean(moved2,HHweight), sd=sqrt(wtd.var(moved2,HHweight)))]

	# moving rate 
	r$mv_rate <- d[age>25&age<65,list(moment="move_rate",value=weighted.mean(D2D,HHweight,na.rm=T),sd=sqrt(wtd.var(D2D,HHweight,na.rm=T)))] 

	# moving rate by ownership status
	r$mv_rate_h0 = d[age>25&age<65&own==0,list(moment="move_rate_h0",value=weighted.mean(D2D,HHweight,na.rm=T),sd=sqrt(wtd.var(D2D,HHweight,na.rm=T)))]
	r$mv_rate_h1 = d[age>25&age<65&own==1,list(moment="move_rate_h1",value=weighted.mean(D2D,HHweight,na.rm=T),sd=sqrt(wtd.var(D2D,HHweight,na.rm=T)))]

	# homeownership rate
	r$own_rate = d[age>25&age<65,list(moment="own_rate",value=weighted.mean(own,HHweight,na.rm=T),sd=sqrt(wtd.var(own,HHweight,na.rm=T)))]

	# check whether same ballpark
	c(r$own_rate[,value], 1-r$own_rate[,value]) %*% c(r$mv_rate_h0[,value],r$mv_rate_h1[,value])

	# linear probability model of homeownership
	# =========================================

	dlm = summary(svyglm(own ~ age + age2 + Division + kids,svy))
	nms = paste0("lm_h_",rownames(dlm$coefficients))
	nms = gsub("\\(|\\)","",nms)
	r$own_rate_reg = data.table(moment=nms,value=dlm$coefficients[,"Estimate"],sd=dlm$coefficients[,"Std. Error"])


	# linear probability model of mobility
	# ====================================

	mv_reg = summary(svyglm(D2D ~ age + age2 + km_distance + km_distance2 + own + kids,svy))
	nms = paste0("lm_mv_",rownames(mv_reg$coefficients))
	nms = gsub("\\(|\\)","",nms)
	r$mv_rate_reg = data.table(moment=nms,value=mv_reg$coefficients[,"Estimate"],sd=mv_reg$coefficients[,"Std. Error"])

	# linear regression of total wealth
	# =================================

	wlm = summary(svyglm(w2medinc ~ age + age2 + own + Division, svy))
	nms = paste0("lm_w_",rownames(wlm$coefficients))
	nms = gsub("\\(|\\)","",nms)
	r$wealth_reg = data.table(moment=nms,value=wlm$coefficients[,"Estimate"],sd=wlm$coefficients[,"Std. Error"])


	# l = lapply(d[,unique(Division)],function(x) lm(own ~ ns(age,df=4),data=merged[Division==x]))
	# own_reg_age = d[age>25&age<65,list(value=weighted.mean(own,HHweight),sd = sqrt(wtd.var(own,HHweight,na.rm=T))),by=list(age,Division)]
	# ggplot(own_reg_age,aes(x=age,y=value,color=Division)) + geom_point() + geom_smooth()

	# nonh_wealth and total wealth by ownership
	wealth_h = ddply(d,c("own"),summarise,nonh = wtd.quantile(nonh_wealth,HHweight,0.5),wealth=wtd.quantile(wealth,HHweight,0.5))
	# nonh_wealth and total wealth by ownership and age
	wealth_h_age = ddply(d,c("own","age"),summarise,nonh = wtd.quantile(nonh_wealth,HHweight,0.5),wealth=wtd.quantile(wealth,HHweight,0.5))
	# nonh_wealth and total wealth by ownership and age and region
	wealth_h_age_div = ddply(d,c("own","age","Division"),summarise,nonh = wtd.quantile(nonh_wealth,HHweight,0.5),wealth=wtd.quantile(wealth,HHweight,0.5))

	# include wealth moments?
	# TODO should be wealth relative to region and year median income

	r$wealth_h = data.table(moment=paste0("wealth_h_",wealth_h$own),value=wealth_h$wealth,sd = 0.0)
	# r$wealth_h_age = data.table(moment=paste0("wealth_h_",wealth_h_age$own,"_age_",wealth_h_age$age),value=wealth_h_age$wealth,sd = 0.0)
	# r$wealth_h_age_div = data.table(moment=paste0("wealth_age_div_h_",wealth_h_age_div$own,"_age_",wealth_h_age_div$age,"_div_",wealth_h_age_div$Division),value=wealth_h_age$wealth,sd = 0.0)

	# pp <- ggplot(wealth_h_age_div,aes(x=age,y=nonh,color=Division)) + geom_line() + facet_wrap(~own) + ggtitle('total wealth in 1000s of dollars')

	return(rbindlist(r))

}




#' Summary Statistics from Sipp
#'
#' @param save file to save
#' @examples
#' load("~/Dropbox/mobility/SIPP/SippFull.RData")
#' z = Sipp.SumStats(merged,"~/Dropbox/mobility/output/data/sipp/sumstats.RData")
# Sipp.SumStats <- function(d,saveto="~/Dropbox/mobility/output/data/sipp/sumstats.RData"){

# 	l <- list()

# 	# monthly state-to-state transitions
# 	l$mD2D <- d[,mean(D2D)]

# 	kv <- c("D2D","D2D","HHincome","numkids","age","wealth","nonh_wealth","home.equity","hvalue","own","mortg.rent","college","saving")

# 	# summary of key vars
# 	l$sum <- summary(d[,kv,with=FALSE])

# 	# monthly state-to-state transitions by current state
# 	l$mD2D_state <- d[,mean(D2D),by=state]

# 	# get movers
# 	movtmp <- d[D2D==TRUE,list(upid=unique(upid))]
# 	setkey(d,upid)
# 	movers <- d[ movtmp[,upid] ]
# 	stayers <- d[ !upid %in%  movtmp[,upid] ]

# 	# get sum stats for movers
# 	means.mov <- movers[,lapply(.SD,mean,na.rm=T),.SDcols=kv]
# 	# means stayers
# 	means <- stayers[,lapply(.SD,mean,na.rm=T),.SDcols=kv]
# 	l$means <- as.data.frame(rbind(means,means.mov))
# 	rownames(l$means) <- c("never.moved","moved")

# 	# number of moves per person
# 	l$num.moves <- movers[,list(moves=sum(D2D,na.rm=T)),by=upid][,table(moves)]

# 	# get to and from
# 	#setkey(movers,yrmnid)
#     #movers[,c("from","to") := list("hi","there")]
# 	#movers[,c("from","to") := get.istate(states=state,imove=D2D),by=upid]

# 	#setkey(movers,upid,yrmnid)
# 	#movers[,c("from","to") := list(c(state[-length(state)],NA), c(state[-1],NA)), by=upid]

# 	# moves back to home
# 	l$moves.home <- movers[D2D==TRUE,list(from.home=sum(from==state.born,na.rm=T)/length(from),
# 											 to.home  =sum(to==state.born,na.rm=T)/length(from),
# 											 to.other =sum(to!=state.born,na.rm=T)/length(from))]

# 	moves <- movers[D2D==TRUE]

# 	# get periods where amove happens and get age, from, to
# 	mvtab <- moves[,list(mid=cumsum(D2D),from=from[D2D],to=to[D2D],age=age[D2D]),by=upid]
# 	setkey(mvtab,from,to)

# 	## get distances data
# 	#data(State_distTable_agg,package="EconData")
      
# 	#setkey(State_distTable_agg,from,to)
# 	#mvtab <- State_distTable_agg[ mvtab ]
# 	l$movers <- movers
# 	l$mvtab <- mvtab


# 	# get location transition matrix of movers
# 	l$trans <- movers[,table(from,to)]

# 	if (!is.null(saveto)){
# 	   sumstats=l
# 		save(sumstats,file=saveto)
# 	}
# 	return(l)
# }


#' Get Movers from full data
#'
getMovers <- function(d){

	movtmp <- d[S2S.mn==TRUE,list(upid=unique(upid))]
	setkey(d,upid)
	movers <- d[ movtmp[,upid] ]

	return(movers)
}


































