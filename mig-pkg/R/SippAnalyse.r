




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


#' SIPP probit of staying vs moving
#'
#' descriptive evidence
#' @examples
#' load("~/Dropbox/mobility/SIPP/Sipp_aggby_age_svy.RData")
#' pr <- SippProbitMove(d=des,"~/Dropbox/mobility/output/data/sipp")
#' 
#' ## goodness of fit
#' 
#' dat <- copy(des$variables)
#' dat[,probmove := predict(pr,type="response")]
#' dat[,pred.move := runif(n=nrow(dat))<probmove]
#' print(dat[,list(actual.moves=sum(S2S),predicted=sum(pred.move))])
SippProbitMove <- function(d,path=NULL){

	stopifnot("survey.design" %in% class(d) )

	m <- svyglm(S2S ~ age + age2 + dkids + own + HHincome + home.equity + duration + college, family=binomial(link="probit"),design=d)

	if (!is.null(path)){
		texreg(list(m),file=file.path(path,"S2S-probit.tex"),digits=3,table=FALSE,dcolumn=TRUE,booktabs=TRUE,use.packages=FALSE)
	}
	return(m)
}

#' make SIPP survey Design object
#'
SippSvyDesign <- function(){

 	load("~/Dropbox/mobility/SIPP/Sipp_aggby_age.RData")

 	des <- svydesign(ids=~1,weights=~HHweight,data=merged[S2S<2 & complete.cases(merged)])
 	save(des,file="~/Dropbox/mobility/SIPP/Sipp_aggby_age_svy.RData")
}






























