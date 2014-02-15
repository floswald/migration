




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

	m <- list()
	#m[[1]] <- glm( S2S.mn ~ HHincome + index_sa + numkids + home.equity , data=d)
	#m[[2]] <- glm( S2S.mn ~ HHincome + index_sa + numkids + age + wealth + mortg.rent, data=d)
	#m[[3]] <- glm( S2S.mn ~ HHincome + index_sa + numkids + age + wealth + mortg.rent + college, data=d)
	#m[[4]] <- glm( S2S.mn ~ HHincome + numkids + age + wealth + duration_at_current + college + born.here + dindex, data=d)

	m$probMove <- glm( S2S.mn ~ ddHHincome + ddnumkids + age + ddwealth + duration_at_current + college + born.here + home.equity + dindex, data=d, family=binomial(link="probit"))

	m$probOwn <- glm( own ~ HHincome + numkids + age + I(age^2) + wealth + duration_at_current + college + born.here + index_sa + state, data=d)

	m$saving <- lm(saving ~ age + I(age^2) + wealth + HHincome + mortg.rent + state + ddnumkids,data=merged)



	return(m)

}





