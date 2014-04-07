

#' Initiate a BBL simulation dataset
#'
#' builds the initial BBL dataset. Selects a random sample
#' from SIPP data at it's first observation in the data.
#'
#' @examples
#' load("~/Dropbox/mobility/SIPP/Sipp_aggby_age.RData")
#' BBL <- list(maxAge=60,FE=TRUE,n=0.1)
#' l <- InitBBLData(merged,RE.coefs,BBLpars=BBL)
InitBBLData <- function(logi,BBLpars,saveto="~/Dropbox/mobility/output/model/BBL/BBLSimData.RData"){

	
	stopifnot(BBLpars$n<1)

	# keep only pos incomes
	logi <- logi[HHincome>0]

	# further subset. drop people who are too old
	logi <- logi[age < BBLpars$maxAge]


	# create a consecutive counter by age for each guy
	setkey(logi,upid,age)
	logi[,count := 1:.N,by=upid]

	setkey(logi,upid,count)

	# draw a random sample
	# of individuals
	n <- logi[,list(upid=sample(unique(upid),size=length(wealth)*BBLpars$n)),by=state][,upid]

	# subset data to that sample, at the first obs for each guy
	logi <- logi[list(n,1),list(HHincome,
					logHHincome=log(HHincome),
					wealth ,
					home.equity,
					mortg.rent,
					own,
					age,
					age2,
					year,
					duration,
					numkids,
					state,
					cohort)]

	logi[,count := NULL]

	return(logi)
}




#' Build BBL Logit dataset
#'
#' expand flat data into data with predictions for each location
buildLogitBBL <- function(logi,RE.coefs,with.FE,prices){

	cohorts <- model.matrix(~cohort - 1,data=logi)
	logi <- cbind(logi,cohorts)
	
	# get distances data
	data(State_distMat_agg,package="EconData")

	# make predictions of income
	# this needs prices list!

	l <- makePrediction1(logi,RE.coefs,with.FE,State_distMat_agg,prices)
	gc()

	# get homevalues by year and state
	# inflation adjusted

	# fill in current prices
	setkey(l,move.to)
	l <- l[prices]

	# add choice and caseid
	l[,choice:=TRUE]
	setnames(l,"upid","caseid")

	return(l)
}


#' Create the BBL data set
#'
#' forward simulate lifecycle profiles based 
#' on policy functions and initial conditions
#'
#' @param data full SIPP dataset
#' @param RFmodels list with reduced form models
#' @param BBLpars list with simulation parameters
CreateBBLData <- function(data,RFmodels,BBLpars){

	# check input
	stopifnot( c("savings","housing","location","RE.coefs") %in% names(RFmodels) )

	# setup data.tables
	# take a sample from full data

	init <- InitBBLData(data,BBLpars=BBL,saveto=NULL)

	# get random prices
	# prices <- getPrices()

	# build logit data
	logit <- buildLogitBBL(init,RFmodels$RE.coefs,with.FE=TRUE)


	# predict location
	d1 <- simMovesFromLogit(res=RFmodels$location,newdat=logit,print.to=NULL)

	# keep only caseid, move.to and distance
	d1 <- d1[move.to==sim.move,list(caseid,distance,m.move,m.stay)]
	# predict save/house
	# update d


	# here are the simulated periods
	# in each period, slot in the new house price and income
	# component, and recompute the decision rules
	for (p in 1:10){

	}



	return(d)



}