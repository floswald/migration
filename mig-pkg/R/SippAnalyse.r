



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




#' make SIPP survey Design object
#'
SippSvyDesign <- function(){

 	load("~/Dropbox/mobility/SIPP/Sipp_aggby_age.RData")

 	des <- svydesign(ids=~1,weights=~HHweight,data=merged)
 	save(des,file="~/Dropbox/mobility/SIPP/Sipp_aggby_age_svy.RData")
}






























