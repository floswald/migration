




#' Extract data.tables from SIPP database
#'
#' Select variables and build a data.tables
#' from the SIPP database, downloaded and 
#' built with anthony damico's usgsd tools.
#' selects ALL waves from coredata, but only
#' selected waves from topical modules. 
#' 
#' don't use this function but the easier to use
#' \code{\link{Extract.wrap}}
#' @references \url{https://github.com/ajdamico/usgsd}, 
#' \url{http://www.asdfree.com/}, 
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
#' @return NULL saves subset data.tables into dropbox
Extract.wrap <- function(verbose=TRUE,dropbox="C:/Users/florian_o/Dropbox/mobility/SIPP"){

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


	# extract 1996
	# ============

	dbfile <- "~/datasets/SIPP/R/SIPP96.db"
	ck     <- c("ssuid",		# sample unit id
                "srefmon",		# reference month (1-4)
                "rhcalmn",		# cal month
                "errp",			# HH relationship
                "rhcalyr",		# cal year
                "tfipsst",		# state
                "eoutcome",		# interview outcome
                "eppintvw",		# person interview outcome
                "tmetro",		# metropolitan area/residual
                "etenure",		# housing tenure
                "thtotinc",		# tot hh income
                "rfid",			# family id
                "efrefper",		# person num of fam reference person
                "rfnkids",		# number of kids in family (of HH ref person!)
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
					 "tmovest",  	# year moved here
					 "eprevten"),	# residence status in previous state
	               c("ssuid",
					 "epppnum",
					 "thhtwlth",    # total HH wealth 
					 "thhtheq",     # total HH home equity
					 "thhmortg",    # HH mortgage principal
					 "ehbuyyr",     # year bought
					 "thomeamt",    # monthly rent/mortgage payment
					 "thhintbk",    # Interest Earning assets held in banking institutions
					 "thhintot",    # Interest Earning assets held in other institutions
					 "tpropval"),   # how much do you think you could sell your house for today?
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot","tpropval"),
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot","tpropval"),
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot","tpropval"))
	which.wgt <- "wgtw12"
	subset = "WHERE eoutcome < 203 AND errp IN (1,2) AND tage > 15"

	ExtractorSippDB(dbfile,ck,which.core,which.tm,which.wgt,tk,subset,outfile=file.path(dropbox,"subset96.RData"),verbose)
	if (verbose) cat("done with 1996 panel.\n")

	# extract 2001
	# ============

	dbfile <- "~/datasets/SIPP/R/SIPP01.db"
	ck     <- c("ssuid",         # sample unit id
                "srefmon",       # reference month (1-4)
                "rhcalmn",       # cal month
                "errp",          # HH relationship
                "rhcalyr",       # cal year
                "tfipsst",       # state
                "tmovrflg",      # mover flag
                "eoutcome",      # interview outcome
                "eppintvw",      # person interview outcome
                "tmetro",        # metropolitan area/residual
                "etenure",       # housing tenure
                "thtotinc",      # tot hh income
                "rfid",          # family id
                "efrefper",      # person num of fam reference person
                "rfnkids",       # number of kids in family (of HH ref person!)
                "whfnwgt",       # final HH weight
                "epppnum",       # person number
                "eeducate",      # highest educ degree
                "eentaid",       # address ID where entered sample
                "tage",          # age
                "esex",          # sex
                "ersnowrk")      # main reason for not working
                
	which.core <- 1:9
	which.tm <- c(2,3,6,9)
	tk     <- list(c("ssuid", "epppnum", "tbrstate","eprevres", "toutinyr", "tmovest", "eprevten","tprstate"),
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot","tpropval"),
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot","tpropval"),
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot","tpropval"))
	which.wgt <- "wgtw9"

	# subset: correct interview status and only reference persons of age > 15.
	subset = "WHERE eppintvw < 3 AND errp IN (1,2) AND tage > 15"

	ExtractorSippDB(dbfile,ck,which.core,which.tm,which.wgt,tk,subset,outfile=file.path(dropbox,"subset01.RData"),verbose)
	if (verbose) cat("done with 2001 panel.\n")


	# extract 2004
	# ============

	dbfile <- "~/datasets/SIPP/R/SIPP04.db"
	ck     <- c("ssuid",           # sample unit id
                "srefmon",         # reference month (1-4)
                "rhcalmn",         # cal month
                "errp",            # HH relationship
                "rhcalyr",         # cal year
                "tfipsst",         # state
                "tmovrflg",        # mover flag
                "eoutcome",        # interview outcome
                "eppintvw",        # person interview outcome
                "tmetro",          # metropolitan area/residual
                "etenure",         # housing tenure
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
	tk     <- list(c("ssuid", "epppnum", "tbrstate", "eprevres", "toutinyr", "tmovest", "eprevten","tprstate"),
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot","tpropval"),
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot","tpropval"))
	which.wgt <- "wgtw12"                                                                                                        
	subset = "WHERE eppintvw < 3 AND errp IN (1,2) AND tage > 15"

	ExtractorSippDB(dbfile,ck,which.core,which.tm,which.wgt,tk,subset,outfile=file.path(dropbox,"subset04.RData"),verbose)
	if (verbose) cat("done with 2004 panel.\n")

	# extract 2008
	# ============

	# wealth modules

	dbfile <- "~/datasets/SIPP/R/SIPP08.db"
	ck     <- c("ssuid",        # sample unit id
                "srefmon",      # reference month (1-4)
                "rhcalmn",      # cal month
                "errp",         # HH relationship
                "rhcalyr",      # cal year
                "tfipsst",      # state
                "tmovrflg",     # mover flag
                "eoutcome",     # interview outcome
                "eppintvw",     # person interview outcome
                "tmetro",       # metropolitan area/residual
                "etenure",      # housing tenure
                "thtotinc",     # tot hh income
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
	tk     <- list(c("ssuid", "epppnum", "eprevres", "toutinyr", "tmovest", "eprevten","tbrstate","tprstate"),
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot","tpropval"),
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot","tpropval"),
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot","tpropval"))
	which.wgt <- "wgtw7"
	subset = "WHERE eppintvw < 3 AND errp IN (1,2) AND tage > 15"

	ExtractorSippDB(dbfile,ck,which.core,which.tm,which.wgt,tk,subset,outfile=file.path(dropbox,"subset08.RData"),verbose)
	if (verbose) cat("done with 2008 panel.\n")

}




#' Clean Sipp Data
#'
#' take output from \code{\link{Extract.wrap}} 
#' and clean data. apply labels, account for
#' missing vars. merge topical and core data.
#' output two datasets, differing in time
#' resolution (monthly of 4-monthly). 
#'
#' Data is cleaned for inconsistencies across
#' SIPP panels 1996-2008, merged with house price
#' indices by state, and dollar denoted variables
#' are deflated to 1996 as a base year using the 
#' US cpi. House price indices are normalized to
#' to the state with lowest index value in 1995:Q4
#' @param TM.idx list with one index vector
#' of TM waves to use per panel. Name list
#' elements like "p96" [panel 96]
#' @param path to output from \code{\link{Extract.wrap}}
#' @param subset.4months TRUE if want to subset monthly data to only the final reference month (srefmon 4). the assumption is
#' events in srefmon 4 are representative of the previous three months. 
#' @return NULL. Saves 2 data.tables to dropbox. 
Clean.Sipp <- function(path="~/Dropbox/mobility/SIPP",
					   TM.idx=list(p96=c(3,6,9,12),
								   p01=c(3,6,9),
								   p04=c(3,6),
								   p08=c(4,7,10)),
					   verbose=TRUE,
					   subset.4months=TRUE){

	# list to collect all panels
	m <- list()

	# get years
	yrs <- str_extract( names(TM.idx),"\\d+")	
		
	# loop over all years and clean
	# depending on year, there are different tasks


	# TODO
	#
	# there will be a distinction between years before and after 
	# the 1996 panel

	# if (before) idvars = c("suid", "entry", "pnum")
	# if (after)  idvars = c("ssuid", "epppnum")

	# before 1996, match is
	#TM		core
	#ID		SUID
	#ENTRY	ENTRY
	#PNUM	PNUM

	# so must rename ID in TM to SUID,"net.wealth"



	for (yr in 1:length(TM.idx)){

		if (verbose) cat(sprintf("cleaning %s \n",yrs[yr]))

		load(file.path(path,paste0("subset",yrs[yr],".RData")))

		# set keys on data.tables
		lapply(topics,function(x) setkey(x, "ssuid", "epppnum"))
		lapply(cores,function(x) setkey(x, "ssuid", "epppnum"))

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
		tmp[,yrmnid := 1:nrow(tmp)]

		setkey(mergexx,yearmon)

		mergexx <- mergexx[ tmp ]

		# give some nicer names
		nm <- data.table(oldname=c("tfipsst",
								   "tmovrflg",
								   "etenure",
								   "rfnkids", 
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
								   "tpropval",
								   "whfnwgt"),
						 newname=c("FIPS",  
								   "mover",   
								   "tenure",
								   "numkids",
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
								   "hvalue",
								   "HHweight"))
		if (verbose) print(nm)

		setnames(mergexx,nm$oldname,nm$newname)


		# make savings
		mergexx[, saving := thhintbk + thhintot]
		mergexx[, c("thhintbk","thhintot") := NULL]



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


		setkey(mergexx,ssuid,epppnum,yrmnid)
		#mergexx[,own.1mn      := mergexx[list(ssuid,epppnum,yrmnid-1)][["own"]]]
		#mergexx[,numkids.1mn  := mergexx[list(ssuid,epppnum,yrmnid-1)][["numkids"]]]
		#mergexx[,wealth.1mn   := mergexx[list(ssuid,epppnum,yrmnid-1)][["wealth"]]]
		#mergexx[,equity.1mn   := mergexx[list(ssuid,epppnum,yrmnid-1)][["home.equity"]]]
		#mergexx[,HHincome.1mn := mergexx[list(ssuid,epppnum,yrmnid-1)][["HHincome"]]]

		## changes
		#mergexx[,dnumkids  := mergexx[list(ssuid,epppnum,yrmnid-1)][["numkids"]]]
		#mergexx[,dwealth   := mergexx[list(ssuid,epppnum,yrmnid-1)][["wealth"]]]
		#mergexx[,dequity   := mergexx[list(ssuid,epppnum,yrmnid-1)][["home.equity"]]]
		#mergexx[,dHHincome := mergexx[list(ssuid,epppnum,yrmnid-1)][["HHincome"]]]

		#mergexx[,ddnumkids  := mergexx[list(ssuid,epppnum,yrmnid-4)][["numkids"]]]
		#mergexx[,ddwealth   := mergexx[list(ssuid,epppnum,yrmnid-4)][["wealth"]]]
		#mergexx[,ddequity   := mergexx[list(ssuid,epppnum,yrmnid-4)][["home.equity"]]]
		#mergexx[,ddHHincome := mergexx[list(ssuid,epppnum,yrmnid-4)][["HHincome"]]]


		mergexx[yr.moved.here > 0,duration_at_current := year - yr.moved.here]
		mergexx[yr.moved.here > 0 & yr.moved.into.previous > 0,duration_at_previous := yr.moved.here - yr.moved.into.previous]
		mergexx[duration_at_current < 0 , duration_at_current := NA]



		# create a monthly state-2-state indicator
		# indicates THAT YOU HAVE JUST MOVED! (AT THE BEGINNING OF CURRENT PERIOD, SAY)

		#mergexx[,S2S.mn := c(FALSE,(diff(FIPS)!=0 )),by=upid]	# NA!=0 returns NA.


		## create a per-wave indicator
		#mergexx[,S2S.wave := max(S2S.mn,na.rm=T),by=list(upid,wave)]	



		# create a lead of that
		#mergexx[,S2S.lead := mergexx[list(ssuid,epppnum,yrmnid+1)][["S2S.mn"]]]
		mergexx[,own.lead := mergexx[list(ssuid,epppnum,yrmnid+1)][["own"]]]
		mergexx[, sell := FALSE]
		mergexx[own==TRUE & own.lead==FALSE, sell := TRUE]

		# whether moved within a wave
		# when counting, choose one reference
		# month, or you'll count 4 times:
		# mergexx[srefmon==4,table(S2S)]
		#mergexx[,S2S := ( mover==4 )]
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
	data(US_states,package="EconData")
	# add aggregated states to FIPS register
	x         <- data.table(FIPS=c(61,62),STATE=c(NA,NA),state=c("ME.VT","ND.SD.WY"),Reg_ID=c(1,2),Region=c("Northeast","Midwest"),Div_ID=c(1,4),Division=c("New England","West North Central"))
	US_states <- rbind(US_states,x)
	setkey(US_states,FIPS)


	merged <-  US_states[ merged ]

	# same for state born
	setkey(merged,state.bornID)
	setnames(US_states,c("FIPS","state"),c("state.bornID","state.born"))

	US_states[,c("STATE","Reg_ID","Region","Div_ID","Division") := NULL]
	setkey(US_states,state.bornID)

	merged <- US_states[ merged ]
	merged[,c("state.bornID","STATE") := NULL]


	# end state aggregation
	# ==========================================


	# create moving indicator:
	# whenever "from" != "to", you moved.
	setkey(merged,upid,yrmnid)
	merged[,c("from","to") := list(c(state[-length(state)],NA), c(state[-1],NA)), by=upid]

	# indicates that at the end of the current period, you move to location "to"
	merged[,S2S := from != to]	# NA!=0 returns NA.

	# create a per-wave indicator
	merged[,S2S.wave := any(S2S),by=list(upid,wave)]	


	setkey(merged,qtr)
	# Inflation
	# =========

	# adjust prices by inflation

	data(cpi)
	cpi <- window(cpi$qtr.base2010,start=c(1995,4))
	cpi <- cpi/cpi[1]	# base year 1996
	cpi <- data.table(qtr=as.yearqtr(index(cpi)),cpi96=coredata(cpi),key="qtr")

	merged <- cpi[ merged ]

	# adjust by inflation and divide by 1000$
	merged[,c("HHincome","wealth","home.equity","thhmortg","mortg.rent","saving","hvalue") := lapply(.SD[,list(HHincome,wealth,home.equity,thhmortg,mortg.rent,saving,hvalue)],function(x) x / (cpi96 * 1000)) ]


	
	# load fhfa house price data
	# why do you need that?
   # data(fhfa)

	#setkey(fhfa,state,qtr)
	#lowest <- fhfa[qtr=="1995 Q4",state[.I[which.min(index_sa)]]]
	#fhfa[,index_sa := index_sa / fhfa[state==lowest & qtr=="1995 Q4"][["index_sa"]] ]	# index relative to lowest index value in 1995:Q4


	## aggregate state prices
	## ----------------------

	## separate state groups
	#fhfa[ (state %in% c("ME","VT")),     state := "ME.VT"]
	#fhfa[ (state %in% c("SD","ND","WY")),state := "ND.SD.WY"]

	#xfhfa <- fhfa[ state %in% c("ND.SD.WY","ME.VT") ]
	#fhfa  <- fhfa[!state %in% c("ND.SD.WY","ME.VT") ]

	## do aggregation
	#xfhfa <- xfhfa[,list(index_sa=mean(index_sa),index_nsa=mean(index_nsa)),by=list(state,qtr)]

	## put back together
	#fhfa  <- rbind(fhfa,xfhfa,use.names=TRUE)

    #setkey(merged,state,qtr)
	#setkey(fhfa,state,qtr)
	#merged <- fhfa[ merged ]

	#if (verbose) cat("merged house prices into data.\n")
	
	# drop FIPS
	merged[,FIPS := NULL]


	# drop inconsistencies
	# --------------------

	# 1) people who have an age difference less than 0 or greater than 1 are 
	# either not the same people, or age is mismeasured.
	# TODO
	#merged <- merged[diff(age) > -1 & diff(age) < 2]



	# subset to a 4-monthly dataset

	merged4mn <- merged[srefmon==4]
	merged4mn[,yrmnid := NULL ]
	# need a sequence of unique year month identifiers
	tmp <- merged4mn[,list(yearmon=unique(yearmon))]
	tmp <- tmp[complete.cases(tmp)]
	tmp[,year := round(yearmon/100,0)]
	tmp[,month := yearmon-year*100]
	tmp[,c("year","month") := NULL]
	setkey(tmp,yearmon)
	tmp[,yrmnid := 1:nrow(tmp)]

	setkey(merged4mn,yearmon)

	merged4mn <- merged4mn[ tmp ]

	# drop reference month
	merged[,srefmon := NULL]
	merged4mn[,srefmon := NULL]


	if (verbose) cat("writing data to disk now.\n")

	save(merged,file="~/Dropbox/mobility/SIPP/SippFull.RData")
	save(merged4mn,file="~/Dropbox/mobility/SIPP/Sipp4mn.RData")

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
		xi <- apply(outer(i,breaks,">"),1,sum) + 1

		mergexx[[i]] <- core[[i]][ topic[[ topic.names[xi] ]] ]

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


#' Re-subset all data
#'
#' wrapper to subset all datasets
#' after full data, i.e. after \code{\link{Clean.Sipp}}
#' has been run.
#'
#' @param path/to/location/of/full/data
subset.all <- function(path="~/Dropbox/mobility/SIPP"){


	# loosing 5 moves here by subsetting to pos income
	# this is the mnlogit dataset
	load(file.path(path,"SippFull.RData"))
	logit <- merged[HHincome>0, list(upid,
									  yrmnid,
									  logHHincome=log(HHincome),
									  age,age2=age^2,
									  year,state,cohort,
									  born,numkids,born.here,
									  college,own,
									  wealth,mortg.rent)]

	# make a model.matrix out of that (i.e. no factors but dummies)
	cohorts <- model.matrix(~cohort - 1,data=logit)
	prelogit <- cbind(logit,cohorts)
	save(prelogit,file=file.path(path,"prelogit.RData"))

	# make a test dataset
	# take an x% random sample by state
	inds <- logit[,list(upid=sample(unique(upid),size=round(length(unique(upid))*0.5))),by=state]
	setkey(logit,upid)
	logitTest <- logit[inds[,upid]]
	save(logitTest,file=file.path(path,"LogitTest.RData"))

	rm(logit,merged)
	gc()


	load(file.path(path,"Sipp4mn.RData"))
	setkey(merged4mn,upid,yrmnid)

	save.buy <- merged4mn[, list(upid,
							age,state,
							wealth,
							mortg.rent,
							yearmon,
							home.equity,
							HHincome,
							HHweight,
							saving, wave,
							own,hvalue,
							numkids)]
	save(save.buy,file=file.path(path,"preSaveBuy.RData"))

	cat('done with subsetting all.\n')
}

