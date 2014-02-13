




#' Extract data.tables from SIPP database
#'
#' Select variables and build a data.tables
#' from the SIPP database, downloaded and 
#' built with ajdamico's usgsd tools.
#' selects ALL waves from coredata, but only
#' selected waves from topical modules. 
#' @references \url{https://github.com/ajdamico/usgsd}
#' @param dbfile location of database
#' @param ck string of variable names from core data to keep
#' @param which.core numeric vector of which core waves to keep
#' @param which.tm numeric vector of which topical modules to keep
#' @param which.wgt character vector of name of weight tables 
#' @param tk list of character vectors of variable names from topical 
#' data to keep, one vector for each topical module
#' @param subset SQL string for selecting from database
#' @param outfile filename of where to save results
ExtractorSippDB <- function(dbfile,ck,which.core,which.tm,which.wgt,tk,subset='',outfile,verbose){

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
#' @param verbose 
#' @param dropbox path to folder where to save this
Extract.wrap <- function(verbose=FALSE,dropbox="C:/Users/florian_o/Dropbox/mobility/SIPP"){

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
	ck     <- c("ssuid",
                "srefmon",
                "rhcalmn",
                "errp",
                "rhcalyr",
                "tfipsst",
                "eoutcome",
                "eppintvw",
                "rhnf",
                "tmetro",
                "etenure",
                "thtotinc",
                "epubhse",
                "rfid",
                "efrefper",
                "rfnkids",
                "wffinwgt",
                "whfnwgt",
                "tftotinc",
                "epppnum",
                "wpfinwgt",
                "eeducate",
                "eentaid",
                "tage",
                "esex",
                "ersnowrk",
                "east3e")
	which.core <- 1:12
	which.tm <- c(2,3,6,9,12)
	tk     <- list(c("ssuid", "epppnum", "eprstate", "ebrstate", "eprevres", "tmovyryr", "toutinyr", "tmovest", "eprevten"),
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
	ck     <- c("ssuid",
                "srefmon",
                "rhcalmn",
                "errp",
                "rhcalyr",
                "tfipsst",
                "tmovrflg",
                "eoutcome",
                "eppintvw",
                "rhnf",
                "tmetro",
                "etenure",
                "thtotinc",
                "epubhse",
                "rfid",
                "efrefper",
                "rfnkids",
                "wffinwgt",
                "whfnwgt",
                "tftotinc",
                "epppnum",
                "wpfinwgt",
                "eeducate",
                "eentaid",
                "tage",
                "esex",
                "ersnowrk",
                "east3e")
	which.core <- 1:9
	which.tm <- c(2,3,6,9)
	tk     <- list(c("ssuid", "epppnum", "tbrstate","eprevres", "tmovyryr", "toutinyr", "tmovest", "eprevten","tbrstate","tprstate"),
	               c("ssuid", "epppnum", "thhtnw", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot"),
	               c("ssuid", "epppnum", "thhtnw", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot"),
	               c("ssuid", "epppnum", "thhtnw", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot"))
	which.wgt <- "wgtw9"

	# subset: correct interview status and only reference persons of age > 15.
	subset = "WHERE eppintvw < 3 AND errp IN (1,2) AND tage > 15"

	ExtractorSippDB(dbfile,ck,which.core,which.tm,which.wgt,tk,subset,outfile=file.path(dropbox,"subset01.RData"),verbose)


	# extract 2004
	# ============

	dbfile <- "~/datasets/SIPP/R/SIPP04.db"
	ck     <- c("ssuid",
                "srefmon",
                "rhcalmn",
                "errp",
                "rhcalyr",
                "tfipsst",
                "tmovrflg",
                "eoutcome",
                "eppintvw",
                "rhnf",
                "tmetro",
                "etenure",
                "thtotinc",
                "epubhse",
                "rfid",
                "efrefper",
                "rfnkids",
                "wffinwgt",
                "whfnwgt",
                "tftotinc",
                "epppnum",
                "wpfinwgt",
                "eeducate",
                "eentaid",
                "tage",
                "esex",
                "ersnowrk",
                "east3e")
	which.core <- 1:12
	which.tm <- c(2,3,6)
	tk     <- list(c("ssuid", "epppnum", "tbrstate", "eprevres", "tmovyryr", "toutinyr", "tmovest", "eprevten","tprstate"),
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot"),
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot"))
	which.wgt <- "wgtw12"
	subset = "WHERE eppintvw < 3 AND errp IN (1,2) AND tage > 15"

	ExtractorSippDB(dbfile,ck,which.core,which.tm,which.wgt,tk,subset,outfile=file.path(dropbox,"subset04.RData"),verbose)

	# extract 2008
	# ============

	# wealth modules

	dbfile <- "~/datasets/SIPP/R/SIPP08.db"
	ck     <- c("ssuid",
                "srefmon",
                "rhcalmn",
                "errp",
                "rhcalyr",
                "tfipsst",
                "tmovrflg",
                "eoutcome",
                "eppintvw",
                "rhnf",
                "tmetro",
                "etenure",
                "thtotinc",
                "epubhse",
                "rfid",
                "efrefper",
                "rfnkids",
                "wffinwgt",
                "whfnwgt",
                "tftotinc",
                "epppnum",
                "wpfinwgt",
                "eeducate",
                "eentaid",
                "tage",
                "esex",
                "ersnowrk",
                "east3e")
	which.core <- 1:13
	which.tm <- c(2,4,7,10)
	tk     <- list(c("ssuid", "epppnum", "eprevres", "tmovyryr", "toutinyr", "tmovest", "eprevten","tbrstate","tprstate"),
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot"),
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot"),
	               c("ssuid", "epppnum", "thhtwlth", "thhtheq", "thhmortg", "ehbuyyr","thomeamt","thhintbk","thhintot"))
	which.wgt <- "wgtw7"
	subset = "WHERE eppintvw < 3 AND errp IN (1,2) AND tage > 15"

	ExtractorSippDB(dbfile,ck,which.core,which.tm,which.wgt,tk,subset,outfile=file.path(dropbox,"subset08.RData"),verbose)

}




#' Clean Sipp Data
#'
#' take output from \link{\code{Extract.wrap}} 
#' and clean data. apply labels, account for
#' missing vars. merge topical and core data.
#' output one dataset
#' @param TM.idx list with one index vector
#' of TM waves to use per panel. Name list
#' elements like "p96" [panel 96]
#' @param path to output from \code{Extract.wrap}
Clean.Sipp <- function(path="~/Dropbox/mobility/SIPP",TM.idx=list(p96=c(3,6,9,12),p01=c(3,6,9),p04=c(3,6),p08=c(4,7,10))){

	# list to collect all panels
	m <- list()

	# get years
	yrs <- str_extract( names(TM.idx),"\\d+")	
		
	# loop over all years and clean
	# depending on year, there are different tasks

	# there will be a distinction between years before and after 
	# the 1996 panel

	# if (before) idvars = c("suid", "entry", "pnum")
	# if (after)  idvars = c("ssuid", "epppnum")

	# before 1996, match is
	#TM		core
	#ID		SUID
	#ENTRY	ENTRY
	#PNUM	PNUM

	# so must rename ID in TM to SUID



	for (yr in 1:length(TM.idx)){

		load(file.path(path,paste0("subset",yrs[yr],".RData")))

		# set keys on data.tables
		lapply(topics,function(x) setkey(x, "ssuid", "epppnum"))
		lapply(cores,function(x) setkey(x, "ssuid", "epppnum"))

		mergexx <- merge.idx(cores,topics,breaks=TM.idx[[yr]])

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
			mergexx[, tbrstate := ebrstate]
			mergexx[, ebrstate := NULL]
			mergexx[, tmovrflg := -1]

		}

		# free memory
		rm(cores, topics)
		gc()

		# clean 
		# =====

		mergexx[,upid := paste0(yrs[yr],ssuid,epppnum)]

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
		setkey(tmp,yearmon)
		tmp[,yrmnid := 1:nrow(tmp)]
		setkey(mergexx,yearmon)

		mergexx <- mergexx[ tmp ]

		# give some nicer names
		nm <- data.table(oldname=c("tfipsst","tmovrflg","etenure","rfnkids","wffinwgt", "esex","wpfinwgt",  "tage","eeducate","east3e",  "thhtnw",   "thhtwlth","thhtheq",    "rhcalyr","rhcalmn","tprstate",          "eprevres",               "tbrstate",      "tmovyryr",           "toutinyr",                    "tmovest",                "eprevten",           "thtotinc","tftotinc"),
						 newname=c("state",  "mover",   "tenure", "numkids","famweight","sex", "persweight","age", "educ",    "mortgage","net.wealth","wealth", "home.equity","year",   "month",  "MIG_previous_state","MIG_where_previous_home","MIG_state_born","MIG_year_moved_here","MIG_year_moved_into_previous","MIG_year_moved_to_state","MIG_previous_tenure","HHincome","faminc"))
		setnames(mergexx,nm$oldname,nm$newname)

		setkey(mergexx,ssuid,epppnum,yrmnid)
		mergexx[,own.1mn      := mergexx[list(ssuid,epppnum,yrmnid-1)][["own"]]]
		mergexx[,numkids.1mn  := mergexx[list(ssuid,epppnum,yrmnid-1)][["numkids"]]]
		mergexx[,wealth.1mn   := mergexx[list(ssuid,epppnum,yrmnid-1)][["wealth"]]]
		mergexx[,equity.1mn   := mergexx[list(ssuid,epppnum,yrmnid-1)][["home.equity"]]]
		mergexx[,faminc.1mn   := mergexx[list(ssuid,epppnum,yrmnid-1)][["faminc"]]]
		mergexx[,state.1mn    := mergexx[list(ssuid,epppnum,yrmnid-1)][["state"]]]
		mergexx[,state.lead   := mergexx[list(ssuid,epppnum,yrmnid+1)][["state"]]]

		mergexx[MIG_year_moved_here > 0,duration_at_current := year - MIG_year_moved_here]
		mergexx[MIG_year_moved_here > 0 & MIG_year_moved_into_previous > 0,duration_at_previous := MIG_year_moved_here - MIG_year_moved_into_previous]

		# create a monthly state-2-state indicator
		mergexx[,S2S.mn := (state != state.1mn & !is.na(state.1mn) )]
		mergexx[,S2S.mnTO := (state != state.lead & !is.na(state.lead) )]
		# relationship between duration and S2S
		mergexx[,mean(S2S.mn,na.rm=T),by=duration_at_current][,plot(duration_at_current,V1)]

		# whether moved within a wave
		# when counting, choose one reference
		# month, or you'll count 4 times:
		# mergexx[srefmon==4,table(S2S)]
		mergexx[,S2S := ( mover==4 )]
		mergexx[, panel := yrs[yr]]

		m[[yr]] <- copy(mergexx)

		rm(mergexx)

	}
	
	merged <- rbindlist(m)

	movtmp <- merged[S2S.mn==TRUE,list(upid=unique(upid))]
	setkey(merged,upid)
	movers <- merged[ movtmp[,upid] ]

	save(merged,file="~/datasets/SIPP/m010408.RData")

	# state names need fixing in 2001
	# 61 is sum of
	# 23 = maine
	# 50 = vermont

	# 62 is sum of
	# 38 = north dakota
	# 46 = south dakota
	# 56 = wyoming
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


Sipp2001States <- function(){

	d <- data.table(fips=c(1 ,	2 ,	4 ,	5 ,	6 ,	8 ,	9 ,	10,	11,	12,	13,	15,	16,	17,	18,	19,	20,	21,	22,	24,	25,	26,	27,	28,	29,	30,	31,	32,	33,	34,	35,	36,	37,	39,	40,	41,	42,	44,	45,	47,	48,	49,	51,	53,	54,	55,	61,	62),
					state=c("Alabama","Alaska"      , "Arizona"     , "Arkansas"    , "California"  , "Colorado"    , "Connecticut" , "Delaware"    , "D.C."        , "Florida"     , "Georgia"     , "Hawaii"      , "Idaho"       , "Illinois"    , "Indiana"     , "Iowa"        , "Kansas"      , "Kentucky"    , "Louisiana"   , "Maryland"    , "Massachusetts" , "Michigan"   , "Minnesota"   , "Mississippi" , "Missouri"   , "Montana"    , "Nebraska"   , "Nevada"       , "New Hampshire"   , "New Jersey"      , "New Mexico"      , "New York"        , "North Carolina"  , "Ohio"            , "Oklahoma"        , "Oregon"          , "Pennsylvania"    , "Rhode Island"    , "South Carolina"  , "Tennessee"       , "Texas"           , "Utah"            , "Virginia"        , "Washington"      , "West Virginia"   , "Wisconsin"       , "Maine, Vermont"  , "North.South.Dak.Wyoming"))
	return(d)
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




