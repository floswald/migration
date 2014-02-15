




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
	tk     <- list(c("ssuid", "epppnum", "eprstate", "ebrstate", "eprevres", "toutinyr", "tmovest", "eprevten"),
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot"),
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot"),
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot"),
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot"))
	which.wgt <- "wgtw12"
	subset = "WHERE eoutcome < 203 AND errp IN (1,2) AND tage > 15"

	ExtractorSippDB(dbfile,ck,which.core,which.tm,which.wgt,tk,subset,outfile=file.path(dropbox,"subset96.RData"),verbose)

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
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot"),
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot"),
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot"))
	which.wgt <- "wgtw9"

	# subset: correct interview status and only reference persons of age > 15.
	subset = "WHERE eppintvw < 3 AND errp IN (1,2) AND tage > 15"

	ExtractorSippDB(dbfile,ck,which.core,which.tm,which.wgt,tk,subset,outfile=file.path(dropbox,"subset01.RData"),verbose)


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
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot"),
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot"))
	which.wgt <- "wgtw12"
	subset = "WHERE eppintvw < 3 AND errp IN (1,2) AND tage > 15"

	ExtractorSippDB(dbfile,ck,which.core,which.tm,which.wgt,tk,subset,outfile=file.path(dropbox,"subset04.RData"),verbose)

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
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot"),
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot"),
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot"))
	which.wgt <- "wgtw7"
	subset = "WHERE eppintvw < 3 AND errp IN (1,2) AND tage > 15"

	ExtractorSippDB(dbfile,ck,which.core,which.tm,which.wgt,tk,subset,outfile=file.path(dropbox,"subset08.RData"),verbose)

}




#' Clean Sipp Data
#'
#' take output from \code{\link{Extract.wrap}} 
#' and clean data. apply labels, account for
#' missing vars. merge topical and core data.
#' output one dataset
#' @param TM.idx list with one index vector
#' of TM waves to use per panel. Name list
#' elements like "p96" [panel 96]
#' @param path to output from \code{\link{Extract.wrap}}
Clean.Sipp <- function(path="~/Dropbox/mobility/SIPP",TM.idx=list(p96=c(3,6,9,12),p01=c(3,6,9),p04=c(3,6),p08=c(4,7,10)),verbose=TRUE){

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
		nm <- data.table(oldname=c("tfipsst","tmovrflg","etenure","rfnkids", "esex", "tage","eeducate",  "thhtwlth","thhtheq",    "rhcalyr","rhcalmn","tprstate",          "eprevres",               "tbrstate",             "toutinyr",                    "tmovest",                "eprevten",    "thtotinc","ehbuyyr","thomeamt"),
						 newname=c("FIPS",  "mover",   "tenure", "numkids","sex","age", "educ",   "wealth", "home.equity","year",   "month",  "prev.state","prev.home","state.born","yr.moved.into.previous","yr.moved.here","prev.tenure","HHincome","yr_bought","mortg.rent"))
		if (verbose) print(nm)

		setnames(mergexx,nm$oldname,nm$newname)


		# make savings
		mergexx[, saving := thhintbk + thhintot]
		mergexx[, c("thhintbk","thhintot") := NULL]

		# code NAs and some labels
		mergexx[,c("eoutcome","eppintvw","efrefper","eentaid","tenure","ersnowrk") := NULL]
		mergexx[yr_bought==-1, yr_bought := NA ]

		mergexx[yr.moved.here<0, yr.moved.here     := NA ] 	# =-5 => always lived here!
		mergexx[yr.moved.here==9999 ,yr.moved.here := NA ] 	# =-5 => always lived here!

		mergexx[state.born==-1, state.born := NA ] 	# =5 => always lived here!

		mergexx[prev.tenure<0, prev.tenure := NA ] 	
		mergexx[,previous.own := FALSE]
		mergexx[prev.tenure==1, previous.own := TRUE]
		mergexx[,prev.tenure := NULL]

		mergexx[yr.moved.into.previous<0, yr.moved.into.previous     := NA ] 	# =5 => always lived here!
		mergexx[yr.moved.into.previous==9999, yr.moved.into.previous := NA ] 	# =5 => always lived here!
		mergexx[mover==-1, mover := NA ]


		setkey(mergexx,ssuid,epppnum,yrmnid)
		mergexx[,own.1mn      := mergexx[list(ssuid,epppnum,yrmnid-1)][["own"]]]
		mergexx[,numkids.1mn  := mergexx[list(ssuid,epppnum,yrmnid-1)][["numkids"]]]
		mergexx[,wealth.1mn   := mergexx[list(ssuid,epppnum,yrmnid-1)][["wealth"]]]
		mergexx[,equity.1mn   := mergexx[list(ssuid,epppnum,yrmnid-1)][["home.equity"]]]
		mergexx[,HHincome.1mn := mergexx[list(ssuid,epppnum,yrmnid-1)][["HHincome"]]]

		# changes
		mergexx[,dnumkids  := mergexx[list(ssuid,epppnum,yrmnid-1)][["numkids"]]]
		mergexx[,dwealth   := mergexx[list(ssuid,epppnum,yrmnid-1)][["wealth"]]]
		mergexx[,dequity   := mergexx[list(ssuid,epppnum,yrmnid-1)][["home.equity"]]]
		mergexx[,dHHincome := mergexx[list(ssuid,epppnum,yrmnid-1)][["HHincome"]]]

		mergexx[,ddnumkids  := mergexx[list(ssuid,epppnum,yrmnid-4)][["numkids"]]]
		mergexx[,ddwealth   := mergexx[list(ssuid,epppnum,yrmnid-4)][["wealth"]]]
		mergexx[,ddequity   := mergexx[list(ssuid,epppnum,yrmnid-4)][["home.equity"]]]
		mergexx[,ddHHincome := mergexx[list(ssuid,epppnum,yrmnid-4)][["HHincome"]]]


		mergexx[yr.moved.here > 0,duration_at_current := year - yr.moved.here]
		mergexx[yr.moved.here > 0 & yr.moved.into.previous > 0,duration_at_previous := yr.moved.here - yr.moved.into.previous]
		mergexx[duration_at_current < 0 , duration_at_current := NA]


		# make a unique person number
		# in theory i should just have 1 person per ssuid
		# but better be safe here.
		mergexx[,upid := paste0(yrs[yr],ssuid,epppnum)]

		# create a monthly state-2-state indicator
		mergexx[,S2S.mn := c(FALSE,(diff(FIPS)!=0 )),by=upid]	# NA!=0 returns NA.

		# whether moved within a wave
		# when counting, choose one reference
		# month, or you'll count 4 times:
		# mergexx[srefmon==4,table(S2S)]
		mergexx[,S2S := ( mover==4 )]
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

	# Note: 1996 and 2001 have aggregates states
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

	merged[ state.born %in% c(61,62),    state.born := NA]	# overwrite foreign countries 61 and 62 with NA
	merged[ state.born %in% c(23,50),    state.born := 61L] # and update with agg codes
	merged[ state.born %in% c(38,46,56), state.born := 62L]

	merged[,born.here := FIPS==state.born ]

	setkey(merged,FIPS)

	# merge with FIPS codes
	data(states)
	abbr[,State := NULL]
	# add aggregated states to FIPS register
	x <- data.table(Abbreviation=c("ME.VT","ND.SD.WY"),FIPS=c(61,62))
	abbr <- rbind(abbr,x)
	setkey(abbr,FIPS)


	merged <-  abbr[ merged ]
	setnames(merged,"Abbreviation","state")

	setkey(merged,state.born)
	setnames(abbr,"FIPS","state.born")

	merged <- abbr [ merged ]
	setnames(merged,c("Abbreviation","state.born"),c("state.born","state.bornID"))
	merged[,state.bornID := NULL]

	# merge prices onto this
	# need a qtrdate column: 
	#merged[,tmp := as.Date(paste0(year,"-",month,"-","01")) ]
	#merged[,qtr := zoo::as.yearqtr(tmp) ]
	#merged[,c("tmp") := NULL]
	setkey(merged,state,qtr)
	
	# load fhfa house price data
	data(fhfa)
	setkey(fhfa,state,qtr)

	# aggregate state prices
	# ----------------------

	# separate state groups
	fhfa[ (state %in% c("ME","VT")),     state := "ME.VT"]
	fhfa[ (state %in% c("SD","ND","WY")),state := "ND.SD.WY"]

	xfhfa <- fhfa[ state %in% c("ND.SD.WY","ME.VT") ]
	fhfa  <- fhfa[!state %in% c("ND.SD.WY","ME.VT") ]

	# do aggregation
	xfhfa <- xfhfa[,list(index_sa=mean(index_sa),index_nsa=mean(index_nsa)),by=list(state,qtr)]

	# put back together
	fhfa  <- rbind(fhfa,xfhfa,use.names=TRUE)

	setkey(fhfa,state,qtr)
	merged <- fhfa[ merged ]

	# create price difference with first year in sample
	merged[, dindex := log(index_sa) - .SD[,log(index_sa)][[1]] , by=state]

	# create price difference for owners with "year bought"

	if (verbose) cat("merge house prices into data.\n")
	
	# drop FIPS
	merged[,FIPS := NULL]

	merged4mn <- merged[srefmon==4]

	# drop reference month
	merged[,srefmon := NULL]
	merged4mn[,srefmon := NULL]


	if (verbose) cat("writing data to disk now.\n")

	save(merged,file="~/Dropbox/mobility/SIPP/SippFull.RData")
	save(merged4mn,file="~/Dropbox/mobility/SIPP/Sipp4mn.RData")

}
	



#' auxiliary function to get movers
#' origin and destination state in a data.table
#' @examples 
#' ttab = data.table(pid = rep(c(1,2),each=5),state=c(3,3,4,4,4,6,7,7,8,9),istate=c(FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,TRUE,FALSE,TRUE,TRUE))
#' ttab[,c("from","to") := get.istate(states=state,imove=istate),with=FALSE]
get.istate <- function(states,imove) {
	mid <- 1:length(imove)
	mid <- mid[imove]	# gives index of moving periods
	from <- rep(NA,length(imove))
	to   <- rep(NA,length(imove))
	for (j in mid){
		to[j] <- states[j]
		from[j] <- states[j-1]
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

						  
						  



#' Estimate Probability of Moving
#'
#' 

