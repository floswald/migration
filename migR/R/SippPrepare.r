#' Extract data.tables from SIPP database
#'
#' Select variables and build data.tables
#' from the SIPP database, downloaded and 
#' built with anthony damico's usgsd tools.
#' selects ALL waves from coredata, but only
#' selected waves from topical modules. 
#' 
#' Notice that Damico's repo has since evolved. You can replicate what I did by using the code
#' in a fork I created from his code, located at \url{https://github.com/floswald/asdfree}. In particular, you need
#' to run the code in \url{https://github.com/floswald/asdfree/blob/master/SIPP/down1996.R}
#' 
#' To build data, don't use this function but the easier to use
#' \code{\link{Extract.wrap}}
#' @references \url{https://github.com/floswald/asdfree},\url{https://github.com/ajdamico/usgsd}, 
#' \url{http://www.asdfree.com/}
#' @param dbfile location of database
#' @param ck string of variable names from core data to keep
#' @param which.core numeric vector of which core waves to keep
#' @param which.tm numeric vector of which topical modules to keep
#' @param which.wgt character vector of name of weight tables 
#' @param tk list of character vectors of variable names from topical 
#' data to keep, one vector for each topical module
#' @param subset SQL string for selecting from database
#' @param outfile filename of where to save results
#' @param test if TRUE extract only a short test dataset
ExtractorSippDB <- function(dbfile,ck,which.core,which.tm,which.wgt,tk,subset='',outfile,verbose,test=FALSE){

	sql <- dbDriver("SQLite")
	db  <- dbConnect(sql, dbfile)

	# find out all core data tables. 
	dbTabs <- dbListTables(db)
	if (verbose) print(dbTabs)

	cores <- list()
	
	for( icore in 1:length(which.core) ){

		if (verbose) cat(sprintf("processing core wave %d of %d\n",icore,length(which.core)))

		sql.string <- paste0( "SELECT " , paste( ck , collapse = "," ) , " from w" , which.core[icore] , paste0(' ',subset) )
		if (verbose) print(sql.string)
		cores[[icore]] <- data.table(dbGetQuery( db , sql.string ))
		cores[[icore]][,wave := which.core[icore] ]

	}
	names(cores) <- paste0("core_",which.core)

	topics <- list()
	
	for( itop in 1:length(which.tm) ){

		if (verbose) cat(sprintf("processing topical wave %d of %d\n",itop,length(which.tm)))

		sql.string <- paste0( "SELECT " , paste( tk[[itop]] , collapse = "," ) , " from tm" , which.tm[itop] , paste0(' ',subset))
		if (verbose) print(sql.string)
		topics[[itop]] <- data.table(dbGetQuery( db , sql.string ))

	}
	names(topics) <- paste0("TM_",which.tm)
		
	wgts <- list()

	# weights
	if (!is.null(which.wgt)){


		for (iw in 1:length(which.wgt)){

			# selects all

			sql.string <- paste0( "SELECT * from " , which.wgt[iw])
			if (verbose) print(sql.string)

		}
	}

	save(cores,topics,wgts,file=outfile)

	if (verbose) cat("done.\n")
}
			



#' Extractor wrapper
#'
#' Selects variables from SIPP database
#' and does some initial subsetting. This
#' is an interface to \code{\link{ExtractorSippDB}}
#' @param verbose 
#' @param dropbox path to folder where to save this
#' @param which names of datasets to extract. Names are: "1996", "2001", "2004", "2008", "Mig_2008"
#' @param dblocation path to location of SIPP database, obtained as illustrated in \url{https://github.com/floswald/asdfree/blob/master/SIPP/down1996.R}.
#' @return NULL saves subset data.tables into dropbox
Extract.wrap <- function(verbose=TRUE,which=paste0(c(1996,2001,2004,2008)),dblocation = "~/datasets/SIPP/R", dropbox="C:/Users/florian_o/Dropbox/mobility/data/SIPP"){

	# extract 1993
	# ============

	# topic modules varnames
	# ----------------------

	# address ID:		entry
	# person number:	pnum
	# identifier:		id

	# module 2: migration
	# which year moved here:	tm8702
	# which state before :  	tm8706
	# which state born:     	tm8730
	
	# module 4: wealth
	# total equity in property:	tm8666
	# average joint savings :  	sc4314
	# which state born:     	tm8730
	#dbfile <- "~/datasets/SIPP/R/SIPP93.db"
	#which.tm <- c(2,4,7)
	#tk     <- list(c("entry","pnum","id","tm8702","tm8706","tm8730"),
				   #c("entry","pnum","id"),
				   #c("entry","pnum","id"),


	if ("1996" %in% which) {

	# extract 1996
	# ============

		dbfile <- file.path(dblocation,"SIPP96.db")
		ck     <- c("ssuid",		# sample unit id
	                "srefmon",		# reference month (1-4)
	                "rhcalmn",		# cal month
	                "errp",			# HH relationship
	                "rhcalyr",		# cal year
	                "tfipsst",		# state
	                "eoutcome",		# interview outcome
	                "ehhnumpp",		# number of persons in HH
	                "eppintvw",		# person interview outcome
	                "tmetro",		# metropolitan area/residual
	                "etenure",		# housing tenure
	                "thtotinc",		# tot hh income
	                "rfid",			# family id
	                "efrefper",		# person num of fam reference person
	                "rfnkids",		# number of kids in family (of HH ref person!)
	                "rmesr", 		# Employment status recode for month 
	                "whfnwgt",		# final HH weight
	                "epppnum",		# person number
	                "eeducate",		# highest educ degree
	                "eentaid",		# address ID where entered sample
	                "tage",			# age
	                "esex",			# sex
	                "ersnowrk")		# main reason for not working
		which.core <- 1:12
		which.tm <- c(2,3,6,9,12)
		tk     <- list(c("ssuid",		# 
						 "epppnum", 	# 
						 "eprstate", 	# previous state of residence
						 "ebrstate", 	# state where born
						 "eprevres", 	# previous state of residence
						 "toutinyr", 	# year moved into previous state
						 "tmovest",  	# year moved into current state 
						 "tmovyryr",  	# Year moved into the current home 
						 "eprevten"),	# residence status in previous state
		               c("ssuid",
						 "epppnum",
						 "thhtwlth",    # total HH wealth 
						 "thhtheq",     # total HH home equity
						 "thhmortg",    # HH mortgage principal
						 "thhore",      # equity in real estate that is not your home such as rental properties and other real estate
						 "ehbuyyr",     # year bought
						 "thomeamt",    # monthly rent/mortgage payment
						 "thhintbk",    # Interest Earning assets held in banking institutions
						 "thhintot",    # Interest Earning assets held in other institutions
						 "tpropval"),   # how much do you think you could sell your house for today?
		               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhore", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot","tpropval"),
		               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhore","thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot","tpropval"),
		               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhore","thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot","tpropval"))
		which.wgt <- "wgtw12"
		subset = "WHERE eoutcome < 203 AND errp IN (1,2) AND tage > 15"

		ExtractorSippDB(dbfile,ck,which.core,which.tm,which.wgt,tk,subset,outfile=file.path(dropbox,"subset96.RData"),verbose)
		if (verbose) cat("done with 1996 panel.\n")

	} 
	if ("2001" %in% which){

		# extract 2001
		# ============

		dbfile <- file.path(dblocation,"SIPP01.db")
		ck     <- c("ssuid",         # sample unit id
	                "srefmon",       # reference month (1-4)
	                "rhcalmn",       # cal month
	                "errp",          # HH relationship
	                "rhcalyr",       # cal year
	                "tfipsst",       # state
	                "tmovrflg",      # mover flag
	                "eoutcome",      # interview outcome
	                "ehhnumpp",		# number of persons in HH
	                "eppintvw",      # person interview outcome
	                "tmetro",        # metropolitan area/residual
	                "etenure",       # housing tenure
	                "thtotinc",      # tot hh income
	                "rfid",          # family id
	                "efrefper",      # person num of fam reference person
	                "rfnkids",       # number of kids in family (of HH ref person!)
	                "rmesr", 		# Employment status recode for month 
	                "whfnwgt",       # final HH weight
	                "epppnum",       # person number
	                "eeducate",      # highest educ degree
	                "eentaid",       # address ID where entered sample
	                "tage",          # age
	                "esex",          # sex
	                "ersnowrk")      # main reason for not working
	                
		which.core <- 1:9
		which.tm <- c(2,3,6,9)
                                             
		tk     <- list(c("ssuid", "epppnum", "tbrstate","eprevres", "toutinyr", "tmovest","tmovyryr", "eprevten","tprstate"),
		               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhore", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot","tpropval"),
		               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhore", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot","tpropval"),
		               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhore", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot","tpropval"))
		which.wgt <- "wgtw9"

		# subset: correct interview status and only reference persons of age > 15.
		subset = "WHERE eppintvw < 3 AND errp IN (1,2) AND tage > 15"

		ExtractorSippDB(dbfile,ck,which.core,which.tm,which.wgt,tk,subset,outfile=file.path(dropbox,"subset01.RData"),verbose)
		if (verbose) cat("done with 2001 panel.\n")

	} 

	if ("2004" %in% which){

		# extract 2004
		# ============

		dbfile <- file.path(dblocation,"SIPP04.db")
		ck     <- c("ssuid",           # sample unit id
	                "srefmon",         # reference month (1-4)
	                "rhcalmn",         # cal month
	                "errp",            # HH relationship
	                "rhcalyr",         # cal year
	                "tfipsst",         # state
	                "tmovrflg",        # mover flag
	                "eoutcome",        # interview outcome
	                "ehhnumpp",		# number of persons in HH
	                "eppintvw",        # person interview outcome
	                "tmetro",          # metropolitan area/residual
	                "etenure",         # housing tenure
	                "rmesr", 		# Employment status recode for month 
	                "thtotinc",        # tot hh income
	                "rfid",            # family id
	                "efrefper",        # person num of fam reference person
	                "rfnkids",         # number of kids in family (of HH ref person!)
	                "whfnwgt",         # final HH weight
	                "epppnum",         # person number
	                "eeducate",        # highest educ degree
	                "eentaid",         # address ID where entered sample
	                "tage",            # age
	                "esex",            # sex
	                "ersnowrk")        # main reason for not working
		which.core <- 1:12
		which.tm <- c(2,3,6)
		tk     <- list(c("ssuid", "epppnum", "tbrstate", "eprevres", "toutinyr", "tmovest","tmovyryr", "eprevten","tprstate"),
		               c("ssuid", "epppnum", "thhtwlth", "thhore", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot","tpropval"),
		               c("ssuid", "epppnum", "thhtwlth", "thhore", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot","tpropval"))
		which.wgt <- "wgtw12"                                                                                                        
		subset = "WHERE eppintvw < 3 AND errp IN (1,2) AND tage > 15"

		ExtractorSippDB(dbfile,ck,which.core,which.tm,which.wgt,tk,subset,outfile=file.path(dropbox,"subset04.RData"),verbose)
		if (verbose) cat("done with 2004 panel.\n")

	} 
   
    if ("2008" %in% which){

		# extract 2008
		# ============

		# wealth modules

		dbfile <- file.path(dblocation,"SIPP08.db")
		ck     <- c("ssuid",        # sample unit id
	                "srefmon",      # reference month (1-4)
	                "rhcalmn",      # cal month
	                "errp",         # HH relationship
	                "rhcalyr",      # cal year
	                "tfipsst",      # state
	                "tmovrflg",     # mover flag
	                "eoutcome",     # interview outcome
	                "ehhnumpp",		# number of persons in HH
	                "eppintvw",     # person interview outcome
	                "tmetro",       # metropolitan area/residual
	                "etenure",      # housing tenure
	                "thtotinc",     # tot hh income
	                "rmesr", 		# Employment status recode for month 
	                "rfid",         # family id
	                "efrefper",     # person num of fam reference person
	                "rfnkids",      # number of kids in family (of HH ref person!)
	                "whfnwgt",      # final HH weight
	                "epppnum",      # person number
	                "eeducate",     # highest educ degree
	                "eentaid",      # address ID where entered sample
	                "tage",         # age
	                "esex",         # sex
	                "ersnowrk")     # main reason for not working
		which.core <- 1:13
		which.tm <- c(2,4,7,10)
		tk     <- list(c("ssuid", "epppnum", "eprevres", "toutinyr", "tmovest", "eprevten","tmovyryr","tbrstate","tprstate"),
		               c("ssuid", "epppnum", "thhtwlth", "thhore", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot","tpropval"),
		               c("ssuid", "epppnum", "thhtwlth", "thhore", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot","tpropval"),
		               c("ssuid", "epppnum", "thhtwlth", "thhore", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot","tpropval"))
		which.wgt <- "wgtw7"
		subset = "WHERE eppintvw < 3 AND errp IN (1,2) AND tage > 15"

		ExtractorSippDB(dbfile,ck,which.core,which.tm,which.wgt,tk,subset,outfile=file.path(dropbox,"subset08.RData"),verbose)
		if (verbose) cat("done with 2008 panel.\n")

	} 

	if ("Mig_2008" %in% which){	

		# extract 2008 Migration history
		# ============


		dbfile <- file.path(dblocation,"SIPP08.db")
    
		sql <- dbDriver("SQLite")
		db  <- dbConnect(sql, dbfile)

		tk <- c("ssuid", "epppnum", "wpfinwgt", "tage","ems","tfipsst","eprevres", "toutinyr", "tmovest","tmovyryr", "eprevten","tbrstate","tprstate")
		subset = "WHERE eoutcome < 208 AND errp IN (1,2) AND tage > 15"

		sql.string <- paste0( "SELECT " , paste( tk , collapse = "," ) , " from tm" , 2 , paste0(' ',subset))
		mig <- data.table(dbGetQuery( db , sql.string ))
		save(mig,file=file.path(dropbox,"subsetMig_08.RData"))
		if (verbose) cat("done with 2008 Migration history.\n")

	}

	# get migration history as separate dataset


}



#' get home values and adjust by inflation
#'
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




#' Clean Sipp Data
#'
#' take output from \code{\link{Extract.wrap}} 
#' and clean data. apply labels, account for
#' missing vars. merge topical and core data.
#' output two datasets, differing in time
#' resolution (monthly or 4-monthly). 
#'
#' Data is cleaned for inconsistencies across
#' SIPP panels 1996-2008, merged with house price
#' indices by state, and dollar denoted variables
#' are deflated to 2012 as a base year using the 
#' US cpi. All dollar values are denoted in 1000s of
#' US dollars. The SIPP can be cast at different 
#' time resolutions, i.e. you can look at monthly data
#' quarterly data, annual, etc. you chose the level of 
#' aggregation by setting the argument \code{agg.by}
#' @param agg.by list of variable names by which to aggregate. those should be time variables present in the dataset like qtr, year, age etc
#' @param TM.idx list with one index vector
#' of Topic Module (TM) waves to use per panel. Name list
#' elements like "p96" [panel 96]
#' @param inpath to output from \code{\link{Extract.wrap}}. These are called \code{subsetxxxx.RData}.
#' @param outpath to save resulting dataset to disk. Object is called \code{merged}.
#' @return NULL. Saves 2 data.tables to dropbox. 
Clean.Sipp <- function(inpath="~/Dropbox/research/mobility/data/SIPP",outpath="~/git/migration/mig-pkg/data",
					   TM.idx=list(p96=c(3,6,9,12),
								   p01=c(3,6,9),
								   p04=c(3,6),
								   p08=c(4,7,10)),
					   agg.by="age",
					   verbose=TRUE){

	# list to collect all panels
	m <- list()

	# get years
	yrs <- str_extract( names(TM.idx),"\\d+")	
		
	# loop over all years and clean
	# depending on year, there are different tasks

	for (yr in 1:length(TM.idx)){

		if (verbose) cat(sprintf("cleaning %s \n",yrs[yr]))

		load(file.path(inpath,paste0("subset",yrs[yr],".RData")))

		# convert ssuid to a string.
		lapply(topics,function(x) x[,ssuid := as.character(ssuid)])
		lapply(cores,function(x) x[,ssuid := as.character(ssuid)])


		# set keys on data.table
		lapply(topics,function(x) setkey(x, ssuid, epppnum))
		lapply(cores,function(x) setkey(x, ssuid, epppnum, srefmon))

		mergexx <- merge.idx(cores,topics,breaks=TM.idx[[yr]])

		if (verbose) cat(sprintf("merged core-TM of %s \n",yrs[yr]))

		# make one table out of it
		mergexx <- rbindlist(mergexx)
		setkey(mergexx, "ssuid", "epppnum" )

		# merge with migration
		# migration always in module 2
		mergexx <- mergexx[ topics$TM_2 ]
		
		if ("96" == yrs[yr]){
			
			# clean 1996
			# ==========

			# add vars that are missing in 1996 migration
			setnames(mergexx,c("eprstate","ebrstate"),c("tprstate","tbrstate"))
			mergexx[, tmovrflg := -1]

		}

		# free memory
		rm(cores, topics)
		gc()

		# clean 
		# =====


		# educ: high-school degree
		mergexx[,HS := FALSE]
		mergexx[eeducate==39|eeducate==40|eeducate==41,HS := TRUE]	# HS grad, some college, certificate from voc,tech,trade or bus school beyond HS

		# educ: college
		mergexx[,college := FALSE]
		mergexx[eeducate>41,college := TRUE]	

		mergexx[,own := FALSE]
		mergexx[etenure==1,own := TRUE]

		# lagged variables
		# generate 1-month lags

		mergexx[,yearmon := rhcalyr * 100 + rhcalmn]

		# need a sequence of unique year month identifiers
		tmp <- mergexx[,list(yearmon=unique(yearmon))]
		tmp <- tmp[complete.cases(tmp)]
		tmp[,year := round(yearmon/100,0)]
		tmp[,month := yearmon-year*100]
		tmp[,qtr := zoo::as.yearqtr(as.Date(paste0(year,"-",month,"-","01"))) ]
		tmp[,c("year","month") := NULL]
		setkey(tmp,yearmon)
		tmp[,timeid := 1:nrow(tmp)]

		setkey(mergexx,yearmon)

		mergexx <- mergexx[ tmp ]

		# give some nicer names
		nm <- data.table(oldname=c("tfipsst",
								   "tmovrflg",
								   "etenure",
								   "rfnkids", 
				                   "ehhnumpp",		
								   "esex", 
								   "tage",
								   "eeducate",  
								   "thhtwlth",
								   "thhtheq",    
								   "rhcalyr",
								   "rhcalmn",
								   "tprstate",          
								   "eprevres",               
								   "tbrstate",            
								   "toutinyr",    
								   "tmovest",                
								   "eprevten",    
								   "thtotinc",
								   "ehbuyyr",
								   "thomeamt",
	              				   "rmesr", 	
								   "tpropval",
								   "thhore",
								   "whfnwgt"),
						 newname=c("FIPS",  
								   "mover",   
								   "tenure",
								   "numkids",
								   "numpersons",
								   "sex",
								   "age",
								   "educ", 
								   "wealth",
								   "home.equity",
								   "year",  
								   "month",  
								   "prev.state",
								   "prev.home",
								   "state.bornID",
								   "yr.moved.into.previous",
								   "yr.moved.here",
								   "prev.tenure",
								   "HHincome",
								   "yr_bought",
								   "mortg.rent",
								   "empstat",
								   "hvalue",
								   "RE.equity.other",
								   "HHweight"))
		if (verbose) print(nm)

		setnames(mergexx,nm$oldname,nm$newname)


		# make and clean nonhousing wealth (savings)
		# "savings" is not what corresponds to "a" in the model
		# "a" is all other nonhousing wealth
		mergexx[, saving := thhintbk + thhintot]
		mergexx[, c("thhintbk","thhintot") := NULL]

		# drop wealth above 99-th percentile
		mergexx <- mergexx[wealth < quantile(wealth,probs=.99,na.rm=T)]

		mergexx[, nonh_wealth := wealth - home.equity ]

		# Household weight needs to be 
		# divided by 10000. see pdf with email.
		mergexx[,HHweight := HHweight / 10000 ]


		# make a unique person number
		# in theory i should just have 1 person per ssuid
		# but better be safe here.
		mergexx[,upid := paste0(yrs[yr],ssuid,epppnum)]
		
		# create cohort
		coyrs = seq(1900,1990,by=20)
		mergexx[,born := .SD[,year-age][[1]], by=upid ]	# pick the first element and just define year born as that.
		mergexx[,cohort := as.character(coyrs[findInterval(born,coyrs)])]

		# code NAs and some labels
		mergexx[,c("eoutcome","eppintvw","efrefper","eentaid","tenure","ersnowrk") := NULL]
		mergexx[yr_bought==-1, yr_bought := NA ]

		mergexx[yr.moved.here<0, yr.moved.here     := born ] 	# =-5 => always lived here!
		mergexx[yr.moved.here==9999 ,yr.moved.here := NA ] 	# =-5 => always lived here!

		mergexx[state.bornID==-1, state.bornID := NA ] 	# =5 => always lived here!

		mergexx[prev.tenure<0, prev.tenure := NA ] 	
		mergexx[,previous.own := FALSE]
		mergexx[prev.tenure==1, previous.own := TRUE]
		mergexx[,prev.tenure := NULL]

		mergexx[yr.moved.into.previous<0, yr.moved.into.previous     := NA ] 	# =5 => always lived here!
		mergexx[yr.moved.into.previous==9999, yr.moved.into.previous := NA ] 	# =5 => always lived here!
		mergexx[mover==-1, mover := NA ]


		setkey(mergexx,ssuid,epppnum,timeid)

		mergexx[yr.moved.here > 0,duration_at_current := year - yr.moved.here]
		mergexx[yr.moved.here > 0 & yr.moved.into.previous > 0,duration_at_previous := yr.moved.here - yr.moved.into.previous]
		mergexx[duration_at_current < 0 , duration_at_current := NA]


		mergexx[, panel := yrs[yr]]

		# rbindlist (below) merges by 
		# column order, not name!
		if (yr==1){
			colorder <- names(mergexx)
		} else {
			setcolorder(mergexx,colorder)
		}

		m[[yr]] <- copy(mergexx)

		rm(mergexx)

	}
	
	merged <- rbindlist(m)
	merged[,timeid := NULL]

	# need to redefine timeid here!!!!!
	tmp <- merged[,list(yearmon=unique(yearmon))]
	setkey(tmp,yearmon)
	tmp[,timeid := 1:nrow(tmp)]

	setkey(merged,yearmon)
	merged <- merged[ tmp ]

	if (verbose) cat("combined all panels into one data.table\n")



	
	
	# Note: 1996 and 2001 have aggregated states
	# ==========================================

	# 61 is sum of
	# 23 = maine
	# 50 = vermont

	# 62 is sum of
	# 38 = north dakota
	# 46 = south dakota
	# 56 = wyoming

	# adjust 2004 and 2008 for that!
	merged[ (! panel %in% c("96","01")) & (FIPS %in% c(23,50)),    FIPS := 61L]
	merged[ (! panel %in% c("96","01")) & (FIPS %in% c(38,46,56)), FIPS := 62L]

	merged[ state.bornID %in% c(61,62),    state.bornID := NA]	# overwrite foreign countries 61 and 62 with NA
	merged[ state.bornID %in% c(23,50),    state.bornID := 61L] # and update with agg codes
	merged[ state.bornID %in% c(38,46,56), state.bornID := 62L]

	merged[,born.here := FIPS==state.bornID ]

	setkey(merged,FIPS)

	# merge with FIPS codes
	data(US_states,package="EconData",envir=environment())
	US_states[,Division := abbreviate(Division,minlength=3)]
	US_states[,PSID := NULL]
	# add aggregated states to FIPS register
	x         <- data.table(FIPS=c(61,62),STATE=c(NA,NA),state=c("ME.VT","ND.SD.WY"),Reg_ID=c(1,2),Region=c("Northeast","Midwest"),Div_ID=c(1,4),Division=c("NwE","WNC"))
	US_states <- rbind(US_states,x)
	setkey(US_states,FIPS)


	merged <-  US_states[ merged ]
	merged[,metro2 := "urban"]
	merged[tmetro==2,metro2:="rural"]
	merged[,Div2 := paste(Division,metro2,sep=".")]

	# same for state born
	setkey(merged,state.bornID)
	setnames(US_states,c("FIPS","state"),c("state.bornID","state.born"))

	US_states[,c("STATE","Reg_ID","Region","Div_ID","Division") := NULL]
	setkey(US_states,state.bornID)

	merged <- US_states[ merged ]
	merged[,c("state.bornID","STATE") := NULL]

	
	# census estimates of median income in current dollars
	# ====================================================

	data(US_medinc_current,package="EconData")
	d <- data.table(medinc_current$incl)
	# normalizing constant: median income in 1996

	d[,c("State","Year") := list(tolower(as.character(State)),as.numeric(as.character(Year))) ]
	setnames(d,c("STATE","year","CensusMedinc"))
	rm(US_states)
	data(US_states,package="EconData")
	US_states[,STATE := tolower(STATE)]
	US_states <- US_states[,list(STATE,state)]
	setkey(US_states,STATE)
	setkey(d,STATE)

	d <- US_states[d]
	medinc = d[year==1996 & STATE == "united states",CensusMedinc]
	d[,STATE := NULL]
	setkey(d,state,year)
	setkey(merged,state,year)

	merged <- d[ merged ]




	# end state aggregation
	# ======================

	# Inflation
	# =========

	cat("adjusting prices by http://research.stlouisfed.org/fred2/series/CPIAUCSL base year 2012\n")

	# adjust prices by inflation

	# HHincome is monthly. make annual and then average by age
	merged[,HHincome := HHincome * 12]
	# same for rent/mortgage payment
	merged[,mortg.rent := mortg.rent * 12]
	# my estimate of median income
	merged[HHincome>0,MyMedinc := Hmisc::wtd.quantile(HHincome,HHweight,na.rm=T,probs=0.5),by=list(year,Division)]

	setkey(merged,qtr)
	# data(cpi,envir=environment())
	# cpi <- as.xts(window(cpi$qtr.base2010,start=c(1995,4)))
	data(CPIAUCSL,package="EconData",envir=environment())
	cpi <- CPIAUCSL['1995-10/2012-12']   # take 1996 onwards
	cpi <- to.quarterly(cpi)
	cpi <- cpi$cpi.Open
	names(cpi) <- "cpi"
	coredata(cpi) <- coredata(cpi)/as.numeric(cpi[as.yearqtr("2012 Q1")])	# base year 2012
	# cpi <- cpi$cpi.Open
	cpi <- data.table(qtr=as.yearqtr(index(cpi)),cpi12=as.numeric(cpi),key="qtr")

	# data(cpi)
	# cpi <- window(cpi$qtr.base2010,start=c(1995,4))
	# cpi <- cpi/cpi[1]	# base year 1996
	# cpi <- data.table(qtr=as.yearqtr(index(cpi)),cpi96=coredata(cpi),key="qtr")

	merged <- cpi[ merged ]
         
	# adjust by inflation and divide by 1000$
	merged[,c("HHincome","wealth","home.equity","thhmortg","mortg.rent","saving","nonh_wealth","hvalue","RE.equity.other","CensusMedinc","MyMedinc") := lapply(.SD[,list(HHincome,wealth,home.equity,thhmortg,mortg.rent,saving,nonh_wealth,hvalue,RE.equity.other,CensusMedinc,MyMedinc)],function(x) x / (cpi12 * 1000)) ]
	
	merged[,age2 := age^2 ]



	# create moving indicator:
	# ========================

	# Define 
	# from: current period region
	# to  : next period region
	# whenever from != to, you move AT THE END OF THE CURRENT PERIOD
	setkey(merged,upid,timeid)
	merged[,c("from","to") := list(c(state[-length(state)],NA), c(state[-1],NA)), by=upid]
	merged[,c("fromD","toD") := list(c(Division[-length(Division)],NA), c(Division[-1],NA)), by=upid]
	merged[,c("fromD2","toD2") := list(c(Div2[-length(Div2)],NA), c(Div2[-1],NA)), by=upid]

	# indicates that at the end of the current period, you move to location "to"
	merged[,S2S := from != to]	# NA!=0 returns NA.
	merged[,D2D := fromD != toD]	# NA!=0 returns NA.
	merged[,D22D2 := fromD2 != toD2]	# NA!=0 returns NA.
	
	# drop age 
	# ========

	merged <- merged[age %in% 20:65]
			
	
	# change in ownership in time period
	# ==================================

	merged[,down := c(diff(as.numeric(own)),0),by=upid]
	merged[,buy := FALSE]
	merged[down==1, buy := TRUE]
	merged[,sell := FALSE]
	merged[down==-1, sell := TRUE]

	# change in number of kids
	# ========================

	merged[,nkids := numkids]
	merged[nkids > 3, nkids := 4]
	merged[,dkids := c(diff(nkids),0),by=upid]
	merged[,nkids2 := c(nkids[-1],NA),by=upid] # next period kids

	merged[,kids := numkids >0]
	merged[,kids2 := c(kids[-1],NA),by=upid]	# next period kids


	# create empstat
	merged[,employed := FALSE]
	merged[empstat==1|empstat==2|empstat==4, employed := TRUE ] # .With a job entire month, worked all weeks, absent 1 week not due layoff, absent 1+ weeks, with job at least 1 week no layoff no jobsearch


	# get income in next period
	# =========================

	merged[,HHincome_plus := merged[list(upid,timeid+1)][["HHincome"]] ]
	merged[,hvalue_plus   := merged[list(upid,timeid+1)][["hvalue"]] ]

	# imputed renters house value assuming a 
	# effective user cost of 5%	
	merged[own==FALSE,r_hvalue := mortg.rent / 0.05 ]
	merged[,r_hvalue_plus   := merged[list(upid,timeid+1)][["r_hvalue"]] ]



	# do time aggregation
	# ===================

	# needs to to aggregation operations 
	# and needs to assign a new time index.

	# do required time aggregation
	# first order by time
	setkey(merged,upid,yearmon)

	if (!is.null(agg.by)){

		# aggregate by agg.by
		# construct the call as a string
		mcall <- paste0("merged <- merged[,list(state=state[1],HValue96=0,
						   state.born=state.born[1],
						   Region=Region[1],
						   Division=Division[1],
						   Div2=Div2[1],
						   HHincome=mean(HHincome,na.rm=T),
						   HHincome_plus=mean(HHincome_plus,na.rm=T),
						   metro=tmetro[1]	,
						   kids=kids[1],
						   numkids=numkids[1],
						   numpersons=numpersons[1],
						   employed=employed[1],
						   HHweight=mean(HHweight,na.rm=T),
						   CensusMedinc=CensusMedinc[1],
						   MyMedinc=MyMedinc[1],
						   age=min(age,na.rm=T),
						   year=min(year,na.rm=T),
						   wealth=mean(wealth,na.rm=T),
						   nonh_wealth=mean(nonh_wealth,na.rm=T),
						   saving=mean(saving,na.rm=T),
						   hvalue=mean(hvalue,na.rm=T),
						   hvalue_plus=mean(hvalue_plus,na.rm=T),
						   r_hvalue=mean(r_hvalue,na.rm=T),
						   r_hvalue_plus=mean(r_hvalue_plus,na.rm=T),
						   RE.equity.other=mean(RE.equity.other,na.rm=T),
						   home.equity=mean(home.equity,na.rm=T),
						   mortg.rent=mean(mortg.rent,na.rm=T),
						   dkids=sum(dkids,na.rm=T),
						   nkids=nkids[1],
						   nkids2=nkids2[length(nkids2[!is.na(nkids2)])],
						   buy=sum(buy),
						   sell=sum(sell),
						   college=college[1],
						   born=born[1],
						   own=own[1],
						   S2S=sum(S2S),
						   D2D=sum(D2D),
						   D22D2=sum(D22D2),
						   from=from[1],
						   to=to[length(to[!is.na(to)])],
						   fromD=fromD[1],
						   toD=toD[length(toD[!is.na(toD)])],
						   cohort=cohort[1],
						   duration=duration_at_current[1]),by=list(upid,",agg.by,")]")

		# evaluate the call
		eval(parse(text=mcall))

		mcall <- paste0("merged[,timeid := ",agg.by,"]")
		eval(parse(text=mcall))

		# add the distance matrix
		data(Division_distMat,package="EconData")
		divdist = melt(data.table(Division_distMat))
		divdist[,fromD := rownames(Division_distMat)]
		setnames(divdist,c("variable","value"),c("toD","km_distance"))
		setkey(divdist,fromD,toD)
		setkey(merged,fromD,toD)

		merged <- divdist[merged]

			# re-add age2
		merged[,age2 := age^2]
		merged[,km_distance2 := km_distance^2]
	
		fname <- paste0("Sipp_",agg.by,".rda")
		
	} else {

		fname <- "Sipp_aggby_NULL.RData"

	}

	# create wealth to median income
	merged[HHincome > 0, w2medinc := wealth / (medinc/1000)]

	# create price to income ratios
	merged[HHincome>0 & own==TRUE ,p2y := hvalue / (HHincome) ] 
	merged[HHincome>0 & own==FALSE,p2y := r_hvalue / HHincome ] 
	merged[wealth!= 0 & own==TRUE ,p2w := hvalue / wealth] 
	merged[wealth!= 0 & own==FALSE,p2w := r_hvalue / wealth] 

	if (verbose) cat("writing data to disk now.\n")

	save(merged,file=file.path(outpath,fname))

	return(merged)

}
	



#' auxiliary function to get movers
#' origin and destination state in a data.table (AT THE BEGINNING OF CURRENT PERIOD, SAY)
#' @examples 
#' ttab = data.table(pid = rep(c(1,2),each=5),state=c(3,3,4,4,4,6,7,7,8,9),istate=c(FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE))
#' ttab[,c("from","to") := get.istate(states=state,imove=istate),with=FALSE]
get.istate <- function(states,imove) {

	pid <- 1:length(imove)	# how many periods
	mid <- pid[imove]	# gives index of moving periods
	from <- rep(NA_character_,length(imove))
	to   <- rep(NA_character_,length(imove))
	for (j in mid){
		if (j==1) {
		} else {
			to[j] <- states[j]
			from[j] <- states[j-1]
		}
	}
	return(list(from,to))
}



#' Merge SIPP cores and topical modules auxiliary function
#'
#' merges the most recent wealth module onto the corresponding
#' core data.  i.e. if breaks=c(3,6,9), the TM was asked
#' in waves 3,6 and 9. therefore merge TM_3 onto cores
#' 1-3, merge TM_6 onto 4-6, etc
#' @param core list of core datasets
#' @param topic list of topical datasets
#' @param breaks numeric vector of waves where a TM was asked.
#' @param topic.names NULL by default assumes names of 
#' \code{topic} are like "TM_2". if not, supply names here.
#' @examples
#' co <- lapply(1:12, function(x) data.table(ssuid=1:4,covar=rnorm(4),key="ssuid"))
#' br <- c(2,5,9,12)
#' tm <- lapply(1:5, function(x) data.table(ssuid=1:4,tmvar=10*c(1,br)[x] + sample(1:4,size=4),key="ssuid"))
#' names(tm) <- paste0("TM_",c(1,br))	# don't merge first TM
#' merge.idx(core=co,topic=tm,breaks=br)
merge.idx <- function(core,topic,breaks=c(3,6,9,12),topic.names=NULL){

	mergexx <- list()

	if (is.null(topic.names)){
		topic.names <- paste0("TM_",breaks)
	}

	for (i in 1:length(core)) {

		# i is in which interval of breaks?
		# which is the last available topic for core number i?
		xi <- min(apply(outer(i,breaks,">"),1,sum) + 1,length(breaks))

		# mergexx[[i]] <- core[[i]][ topic[[ topic.names[xi] ]] ]
		mergexx[[i]] <- topic[[ topic.names[xi] ]][ core[[i]] ]

	}
	return(mergexx)

}


#' get FHFA state level HPI
#'
#' download FHFA state-level house price
#' index 1990-2013, quarterly data.
#' 
#' source of data is
#' \url{http://www.fhfa.gov/Default.aspx?Page=87}
#' @param from url 
#' @param to location to save data
#' @return TRUE
download.FHFA <- function(from="http://www.fhfa.gov/webfiles/25831/3q13hpists_expandeddata.txt",to="~/git/migration/mig-pkg/data"){

	#h <- data.table(read.table(file=from,header=TRUE,sep="\t"))
	fhfa <- fread(input=from,header=TRUE,sep="\t")	# directly into a data.table
	fhfa[,note := NULL]
	fhfa[,qtr := zoo::as.yearqtr(paste0(yr,"Q ",qtr),"%Y Q%q")]
	fhfa[,yr := NULL]
	warning("removed note field from fhfa data.")
	save(fhfa,file=file.path(to,"fhfa.RData"))

	return(TRUE)
}

						  

						  

getHval.data <- function(data="~/Dropbox/mobility/SIPP/Sipp4mn.RData"){
	load(data)

	hvalues <- merged4mn[hvalue>0,list(hvalue,year,state)]

	save(hvalues,file="~/git/migration/data/hvalues.RData")

}



#' Create SIPP survey Design object
#'
#' @details Uses variable \code{HHweight} to build a \code{\link{svydesign}} object.
#' @export
SippSvyDesign <- function(merged=NULL){
	if (is.null(merged)){
		data(Sipp_age,envir=environment())
	}


 	des <- svydesign(ids=~1,weights=~HHweight,data=merged)
 	save(des,file="~/git/migration/mig-pkg/data/Sipp_age_svy.rda")
 	return(des)
}
