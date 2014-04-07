



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
	# furthermore, you can't have any of those formulae:
	#fm = formula(choice ~ -1 + distance + logHHincome + HValue96 + stay | 1 | 1 )
	#fm = formula(choice ~ -1 + distance | age | logHHincome + HValue96 )
	#fm = formula(choice ~ -1 + distance + HValue96  | age + I(age^2) + numkids + born.here| logHHincome )
	#fm = formula(choice ~ -1 + distance + HValue96  | age + numkids + born.here| logHHincome )
	#fm = formula(choice ~ -1 + distance + HValue96  | age + I(age^2) + numkids | logHHincome )
	#fm = formula(choice ~ -1 + distance + HValue96  | age + I(age^2) + mortg.rent | logHHincome )
	#fm = formula(choice ~ -1 + distance + HValue96  | age + I(age^2) + own  + numkids | logHHincome )
	

	# those work:
	
	#fm = formula(choice ~ -1 + distance + HValue96  | age + age2 + own + duration | logHHincome )
	fm = formula(choice ~ -1 + distance + HValue96  | age + age2 + own  | logHHincome )
	#fm = formula(choice ~ -1 + distance + HValue96  | age + I(age^2) + born.here | logHHincome )


	# add a case id
	res = mnlogit(fm,d,"move.to",ncores=1,print.level=1,maxiter=100,chid="caseid",linDepTol=0.0001)

	if (!is.null(saveto)){
		save(res,file=saveto)
	}

	return(res)
}






#' @family LogitModel
simulateMoveLogit <- function(m){
	m[,sim.move := move.to[ findInterval(runif(1), cumsum(prediction)) +1], by=caseid]
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

	setkey(m,caseid,move.to)
	setkey(r,caseid,move.to)


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
simMovesFromLogit <- function(res,newdat=NULL,GOF=FALSE,print.to="~/Dropbox/mobility/output/model/BBL/"){

	stopifnot(class(res) == "mnlogit" )

	# get prediciton on estimation data
	nm <- names(res$data)
	if (is.null(newdat)) {
		data <- res$data
	} else {
		data <- newdat
	}

	p <- data.table(direct.predict(res,newdata=data[,nm,with=FALSE],probability=TRUE))
	p[,caseid := data[,unique(caseid)] ]


	m <- melt(p,id.vars="caseid",variable.factor=FALSE,verbose=TRUE,variable.name="move.to",value.name="prediction")
	# simulate each case-id: given all 48 choices, which one do you choose?
	m <- simulateMoveLogit(m)
	setkey(m,caseid,move.to)

	# table with where each guy moves
	# this line probably not necessary
	r <- data[,list(caseid,move.to,distance,choice)]
	setkey(r,caseid,move.to)

	rm(res)
	gc()

	# merge back into data
	r <- r[m]


	r[,simchoice0 := move.to==sim.move & distance==0]
	r[,simchoice1 := move.to==sim.move & distance>0]

	# simulation choice at distance==0
	r[,c("m.move","m.stay") := FALSE]

	# model: stay
	setkey(r,simchoice0)
	r[J(TRUE), m.stay := TRUE]	

	# model:move 
	setkey(r,simchoice1)
	r[J(TRUE), m.move := TRUE]	

	if (GOF){
		r[,c("d.move","d.stay") := FALSE]

		# true choices
		r[,dpos := distance >0]
		setkey(r,choice,dpos)
		r[J(TRUE,TRUE), d.move := TRUE]
		r[J(TRUE,FALSE), d.stay := TRUE]

		fp <- list()
		fp$freqs <- r[,list(model.stay=sum(m.stay),data.stay=sum(d.stay),model.move=sum(m.move),data.move=sum(d.move)),by=move.to]
		fp$freqs[,dataN := data.stay+data.move]
		fp$freqs[,modelN := model.stay+model.move]

		fp$props <- fp$freqs[,list(move.to,data.stay=data.stay/dataN,model.stay=model.stay/modelN,data.move=data.move/dataN,model.move=model.move/modelN)]

		if (!is.null(print.to)){

			print(xtable(fp$freqs),file=file.path(print.to,"logit_pred_freqs.tex"),include.rownames=FALSE,floating=FALSE)
			print(xtable(fp$props),file=file.path(print.to,"logit_pred_props.tex"),include.rownames=FALSE,floating=FALSE)
			return(list(fp=fp,r=r))
		
		} else {
			return(list(fp=fp,r=r))
		}

	} else {

		return( r )

	}


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
#' load("~/Dropbox/mobility/SIPP/Sipp_aggby_age.RData")
#' load("~/Dropbox/mobility/output/model/BBL/inc-process/income-REcoefs.RData")
#' l <- buildLogit(merged,RE.coefs)
buildLogit <- function(logi,RE.coefs,with.FE=TRUE,verbose=TRUE,saveto="~/Dropbox/mobility/output/model/BBL/logit.RData",savetosmall="~/Dropbox/mobility/output/model/BBL/logit30.RData"){

	#load(file.path(modelpath,"income-REmodels.RData"))		# contains RE.models

	# take the first obs by age for time=constant
	# variables and compute mean for numerics.

	# logi is on a monthly basis 
	# predict income on an annual basis only: aggregate by age

	# keep only pos incomes
	logi <- logi[HHincome>0]

	cohorts <- model.matrix(~cohort - 1,data=logi)
	logi <- cbind(logi,cohorts)

	# further subset. keep only vars you are using in the logit model!
	logi <- logi[,list(logHHincome=log(HHincome),
					#wealth ,
					#mortg.rent,
					own,
					upid,
					age,
					age2,
					from,
					to,
					S2S,
					year,
					duration,
					#born.here,
					#college,
					dkids,
					state,
					cohort,
					cohort1920,
					cohort1940,
					cohort1960,
					cohort1980)]

	# initiate choice column as zero choice
	#y[,c("to.tmp","choice","distance") := list(state,FALSE,0)]


	# get distances data
	data(State_distMat_agg,package="EconData")

	# make predictions of income
	# will add column logHHincome

	# TODO
	# IncProcess
	# attr(logi,"type") <- "LogitEstim"
	# l <- inflate(IncProc,logi)


	l <- makePrediction1(logi,RE.coefs,with.FE,State_distMat_agg)
	gc()

	# get homevalues by year and state
	# inflation adjusted

	# need that because house values vary by state/year
	l <- mergeHomeValues(l)
	setkey(l,upid,age,move.to)

	# drop multiple moves
	l <- l[S2S<2]

	# drop final period of each guy: don't know whether stay or move.
	l <- l[complete.cases(l)]
	#     l <- l[!(is.na(from) & is.na(to))]

	# buildLogitDChoice
	l <- buildLogitDchoice(l)

	# drop cases with no choice at all.
	l[,onechoice := sum(choice),by=caseid]
	stopifnot( l[,min(onechoice)==1] | l[,max(onechoice)==1] )
	l[,onechoice := NULL]

	gc()

	attr(l,"shape") <- "choice.set"

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



buildLogitDchoice <- function(d){

	# create variable that's TRUE
	# whenever move.to is equal to "to"
	d[,choice := FALSE]
	d[move.to==to, choice := TRUE ]

	# add a choice and stay indicator
	d[,stay   := FALSE]

	setkey(d,S2S,choice)

	# add a case id
	d[,caseid := paste0(upid,"_",age)]

	# find all cases where move happened
	move.cases <- d[choice==TRUE & distance>0,list(caseid=unique(caseid))]

	setkey(move.cases,caseid)
	setkey(d,caseid)

	# find all cases where a move NOT happened
	d[!list(move.cases),stay := TRUE]

	return(d)
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







