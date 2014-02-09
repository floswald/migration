


#' merge SIPP 1996 core data with topical
#'
MergeSipp96 <- function(path="~/datasets/SIPP/1996/dta/export",outfile,heads=TRUE,test=FALSE){

	# core file names
	fi <- list.files()
	co <- grep("core",fi,value=TRUE)
	tm <- grep("TM",fi,value=TRUE)

	# build the core data for all waves
	#l <- lapply(core[1:4],function(x) data.table(foreign::read.dta(x),key=c("ssuid","epppnum","srefmon")))
	l <- lapply(core,function(x) data.table(read.csv(x),key=c("ssuid","epppnum","srefmon")))
	d1 <- rbindlist(l)

	# merge core waves 1-3 with wealth_t3.dta	(ie wealth variables from topical module T3)
	# merge core waves 4-6 with wealth_t6.dta
	# merge core waves 7-9 with wealth_t9.dta
	# merge core waves 10-12 with wealth_t12.dta

	setkey(d1,ssuid,epppnum)

	# merge with mig hist
	# mig hist always in 2nd topical module

	mig <- data.table(foreign::read.dta("mig.dta"),key=c("ssuid","epppnum"))
	mig[,ssuid2 := as.numeric(ssuid)]
	mig[,epppnum2 := as.integer(epppnum)]
	mig[,c("epppnum","ssuid") := NULL]
	setnames(mig,c("epppnum2","ssuid2"), c("epppnum","ssuid"))
	setkey(mig,ssuid,epppnum)

	m <- mig[d1]
	m <- m[complete.cases(m)]

	# merge with wealth modules

	w1 <- data.table(foreign::read.dta("wealth_t3.dta"),key=c("ssuid","epppnum"))
	w1[,ssuid2 := as.numeric(ssuid)]
	w1[,epppnum2 := as.integer(epppnum)]
	w1[,c("epppnum","ssuid") := NULL]
	setnames(w1,c("epppnum2","ssuid2"), c("epppnum","ssuid"))
	setkey(w1,ssuid,epppnum)

	m <- w1[m]
	m <- m[complete.cases(m)]

}




#' merge SIPP 2001,2004 and 2008 core and topical data
#'
MergeSipp <- function(path="~/datasets/SIPP/2001/dta/export",outfile){


	# wealth variables:
	# 2001: modules 3,6,9
	# 2004: modules 3,6
	# 2008: modules 3,7,10
	
	# mig hist always in 2nd topical module
	# merge migration onto final full file

	fi <- list.files()
	co <- grep("core",fi,value=TRUE)
	tm <- grep("TM",fi,value=TRUE)

	# 2001: modules 3,6,9
	# -1 or 0 = "not in universe"
	if (grep("2001",path)){

		# waves 1-3
		l <- lapply(co[1:3],function(x) fread(x))
		d <- rbindlist(l)
		setkey(d,"ssuid","epppnum")
	
		tm3 <- fread(grep("3",tm,value=TRUE))
		setkey(tm3, "ssuid","epppnum")
		m1 <- tm3[d]

		# waves 4-6
		l <- lapply(co[4:6],function(x) fread(x))
		d <- rbindlist(l)
		setkey(d,"ssuid","epppnum")
	
		tm6 <- fread(grep("6",tm,value=TRUE))
		setkey(tm6, "ssuid","epppnum")
		m2 <- tm6[d]
		
		# waves 7-9
		l <- lapply(co[7:9],function(x) fread(x))
		d <- rbindlist(l)
		setkey(d,"ssuid","epppnum")
	
		tm9 <- fread(grep("9",tm,value=TRUE))
		setkey(tm9, "ssuid","epppnum")
		m3 <- tm9[d]

		m <- rbindlist(list(m1,m2,m3))

		# merge with mig hist
		mig <- fread(grep("2",tm,value=TRUE))
		setkey(mig,"ssuid","epppnum")
		setkey(m,"ssuid","epppnum")

		m <- mig[m]

	} else if (grep("2004",path)){

		# waves 1-3
		l <- lapply(co[1:3],function(x) fread(x))
		d <- rbindlist(l)
		setkey(d,"ssuid","epppnum")
	
		tm3 <- fread(grep("3",tm,value=TRUE))
		setkey(tm3, "ssuid","epppnum")
		m1 <- tm3[d]

		# waves 4-6
		l <- lapply(co[4:6],function(x) fread(x))
		d <- rbindlist(l)
		setkey(d,"ssuid","epppnum")
	
		tm6 <- fread(grep("6",tm,value=TRUE))
		setkey(tm6, "ssuid","epppnum")
		m2 <- tm6[d]

		m <- rbindlist(list(m1,m2))

		# merge with mig hist
		mig <- fread(grep("2",tm,value=TRUE))
		setkey(mig,"ssuid","epppnum")
		setkey(m,"ssuid","epppnum")

		m <- mig[m]

	# 2008: modules 3,7,10
	} else if (grep("2008",path)){

		# waves 1-3
		l <- lapply(co[1:3],function(x) fread(x))
		d <- rbindlist(l)
		setkey(d,"ssuid","epppnum")
	
		tm3 <- fread(grep("3",tm,value=TRUE))
		setkey(tm3, "ssuid","epppnum")
		m1 <- tm3[d]

		# waves 4-7
		l <- lapply(co[4:7],function(x) fread(x))
		d <- rbindlist(l)
		setkey(d,"ssuid","epppnum")
	
		tm7 <- fread(grep("7",tm,value=TRUE))
		setkey(tm7, "ssuid","epppnum")
		m2 <- tm7[d]
		
		# waves 8-19
		l <- lapply(co[8:10],function(x) fread(x))
		d <- rbindlist(l)
		setkey(d,"ssuid","epppnum")
	
		tm10 <- fread(grep("10",tm,value=TRUE))
		setkey(tm10, "ssuid","epppnum")
		m3 <- tm10[d]

		m <- rbindlist(list(m1,m2,m3))

		# merge with mig hist
		mig <- fread(grep("2",tm,value=TRUE))
		setkey(mig,"ssuid","epppnum")
		setkey(m,"ssuid","epppnum")

		m <- mig[m]

	} 

	save(m,file=outfile)
	return(NULL)
}



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
#' @param which.wgt name of weight tables 
#' @param tk string of variable names from topical data to keep
#' @param subset SQL string for selecting from database
#' @param outfile filename of where to save results
ExtractSippDB <- function(dbfile,ck,which.core,which.tm,tk,subset='',outfile){

	sql <- dbDriver("SQLite")
	db  <- dbConnect(sql, dbfile)

	# find out all core data tables. 
	dbTabs <- dbListTables(db)

	cores <- list()
	
	for( icore in 1:lenth(which.core) ){

		sql.string <- paste0( "select " , paste( ck , collapse = "," ) , " from w" , which.core[icore] , subset )
		cores[[icore]] <- data.table(dbGetQuery( db , sql.string ))

	}

	topics <- list()
	
	for( itop in 1:lenth(which.tm) ){

		sql.string <- paste0( "select " , paste( tk , collapse = "," ) , " from tm" , which.tm[itop] , subset )
		topics[[itop]] <- data.table(dbGetQuery( db , sql.string ))

	}
		
	wgts <- list()

	# weights
	if (!is.null(which.wgt)){


		for (iw in 1:length(which.wgt)){

			# selects all
			sql.string <- paste0( "select * from " , which.wgt[iw] )

		}
	}

	save(cores,topics,wgts,file=outfile)
}
			















#' Merge Sipp core data with topical modules
#'
#' takes all available SIPP panels, processes them, and
#' outputs a unitary dataset
#' The list of topical modules by panel and wave is
#' http://www.census.gov/programs-surveys/sipp/tech-documentation/topical-modules.html
#'
#' Notice that the folderstructure needs to look like this:
#' /Users/florianoswald/datasets/SIPP \cr
#' |-1996\cr
#' |---dat\cr
#' |---dct\cr
#' |---do_NBER\cr
#' |---doc\cr
#' |---dta\cr
#' |-----core_and_topical\cr
#' |-----export\cr
#' |-2001\cr
#' |---dat\cr
#' |---dct\cr
#' |---do_NBER\cr
#' |---doc\cr
#' |---dta\cr
#' |-----core_and_topical\cr
#' |-----export\cr
#' |-2004\cr
#' |---dat\cr
#' |---dct\cr
#' |---do_NBER\cr
#' |---doc\cr
#' |---dta\cr
#' |-----core_and_topical\cr
#' |-----export\cr
#' |-2008\cr
#' |---dat\cr
#' |---dct\cr
#' |---do_NBER\cr
#' |---doc\cr
#' |---dta\cr
#' |-----core_and_topical\cr
#' |-----export\cr
#' in particular, the paths yyyy/dta/export must exist
#' @param path path/to/SIPP/folderstructure
#' @param outfile where to save dataset
#' @param heads TRUE if only family reference person is kept
#' @param test TRUE if run on test dataset
MergeSIPP <- function(path="~/datasets/SIPP",outfile="~/git/migration/mig-pkg/data/sipp.RData",heads=TRUE,test=FALSE){

	yrs <- list.files(path)

	l <- list()

	for (y in 1:length(yrs)){

		if(test){
			fi <- file.path(path,yrs[y],paste0("dta/core_and_topical/test_",yrs[y],".dta"))

		} else {
			fi <- file.path(path,yrs[y],paste0("dta/core_and_topical/core_top_",yrs[y],".dta"))

		}

		# 


		d <- data.table(foreign::read.dta(fi))
		
		cat(sprintf('\nloaded year %s \n',yrs[y]))

		# fix family reference number. 
		# currently a numeric 101 101 101
		# but needs to be compatible with epppnum, which is
		# "0101" "0101"
		d[,pid := as.integer( epppnum )]	# person id within each ssuid
		d[,famhead := efrefper==pid]	# indicator of family reference person

		d[,non.work := as.numeric(ersnowrk)]
		d[non.work==3, non.work := 2]
		d[,non.work := factor(non.work,labels=levels(ersnowrk)[-3])]

		# in some waves those vars are missing
		if ( !("tmovrflg" %in% names(d)) ) d[ ,tmovrflg := "missing var"]
		if ( !("tprstate" %in% names(d)) ) d[ ,tprstate := "missing var"]
		if ( !("tbrstate" %in% names(d)) ) d[ ,tbrstate := "missing var"]
			
		nm <- data.table(oldname=c("tfipsst","tmovrflg","etenure","rfnkids","wffinwgt", "esex","wpfinwgt",  "tage","eeducate","east3e",  "thhtnw",   "thhtwlth","thhtheq",    "rhcalyr","rhcalmn","tprstate",          "eprevres",               "tbrstate",      "tmovyryr",           "toutinyr",                    "tmovest",                "eprevten",           "thtotinc","tftotinc"),
						 newname=c("state",  "mover",   "tenure", "numkids","famweight","sex", "persweight","age", "educ",    "mortgage","net.wealth","wealth", "home.equity","year",   "month",  "MIG_previous_state","MIG_where_previous_home","MIG_state_born","MIG_year_moved_here","MIG_year_moved_into_previous","MIG_year_moved_to_state","MIG_previous_tenure","HHincome","faminc"))

		setnames(d,nm$oldname,nm$newname)

		# make a unique observation number
		d[,upid := paste0(yrs[y],ssuid,epppnum)]

		keep <- c("famhead","upid","ssuid","pid",nm$newname)
		nvs <- setdiff(names(d),keep)

		# drop all vars not in that list
		d[,nvs := NULL, with=FALSE]

		d[,own := FALSE]
		d[as.numeric(tenure)==1, own := TRUE ]



		# numeric educ variable
		d[,educx := as.numeric(educ)]
		d[,college := FALSE]
		d[educx>10, college := TRUE]

		
		# make lagged variables
		# =====================

		# want: own(t-1), income(t-1), non.work(t-1), kids(t-1)
		# where t-1 means "a year ago"
		d[,monthid := as.integer(month)]
		d[,yearmon := year * 100 + monthid]

		# need a sequence of unique year month identifiers
		tmp <- d[,list(yearmon=unique(yearmon))]
		tmp <- tmp[complete.cases(tmp)]
		setkey(tmp,yearmon)
		tmp[,yrmnid := 1:nrow(tmp)]
		setkey(d,yearmon)

		d <- d[tmp]

		setkey(d,ssuid,pid,year,monthid)
		d[,own.1yr      := d[list(ssuid,pid,year-1,monthid)][["own"]]]
		#d[,non.work.1yr := d[list(ssuid,pid,year-1,monthid)][["non.work"]]]
		d[,numkids.1yr  := d[list(ssuid,pid,year-1,monthid)][["numkids"]]]
		d[,wealth.1yr   := d[list(ssuid,pid,year-1,monthid)][["wealth"]]]
		d[,equity.1yr   := d[list(ssuid,pid,year-1,monthid)][["home.equity"]]]
		d[,faminc.1yr   := d[list(ssuid,pid,year-1,monthid)][["faminc"]]]
		d[,state.1yr    := d[list(ssuid,pid,year-1,monthid)][["state"]]]
		
		setkey(d,ssuid,pid,yrmnid)
		d[,own.1mn      := d[list(ssuid,pid,yrmnid-1)][["own"]]]
		#d[,non.work.1mn := d[list(ssuid,pid,yrmnid-1)][["non.work"]]]
		d[,numkids.1mn  := d[list(ssuid,pid,yrmnid-1)][["numkids"]]]
		d[,wealth.1mn   := d[list(ssuid,pid,yrmnid-1)][["wealth"]]]
		d[,equity.1mn   := d[list(ssuid,pid,yrmnid-1)][["home.equity"]]]
		d[,faminc.1mn   := d[list(ssuid,pid,yrmnid-1)][["faminc"]]]
		d[,state.1mn    := d[list(ssuid,pid,yrmnid-1)][["state"]]]

		d[MIG_year_moved_here > 0,duration_at_current := as.numeric(yrs[y]) - MIG_year_moved_here]
		d[MIG_year_moved_here > 0 & MIG_year_moved_into_previous > 0,duration_at_previous := MIG_year_moved_here - MIG_year_moved_into_previous]

		# create a monthly state-2-state indicator
		d[,S2S.mn := (state != state.1mn & !is.na(state.1mn) )]
		# relationship between duration an S2S
		d[,mean(S2S.mn,na.rm=T),by=duration_at_current][,plot(duration_at_current,V1)]

		# create annual mover variable (n.a. for 1996)
		d[,S2S := ( mover=="Moved, different state" )]



		# make growth variables
		# =====================

		d[,dfaminc.mn := faminc - faminc.1mn,by=upid]
		d[,dfaminc.yr := faminc - faminc.1yr,by=upid]



		# mark panel
		d[,panel := yrs[y] ]

		# heads only
		# ==========

		if (heads) d <- d[famhead==TRUE]

		l[[y]] <- d

		cat(sprintf('\ndone with year %s \n',yrs[y]))

	}

	#     sipp <- rbindlist(l)

	#save(sipp,file=outfile)

	return(l)
}




#' Build Sipp Panel for all years
#'
#' takes all available SIPP panels, processes them, and
#' outputs a unitary dataset
#'
#' Notice that the folderstructure needs to look like this:
#' /Users/florianoswald/datasets/SIPP \cr
#' |-1996\cr
#' |---dat\cr
#' |---dct\cr
#' |---do_NBER\cr
#' |---doc\cr
#' |---dta\cr
#' |-----core_and_topical\cr
#' |-2001\cr
#' |---dat\cr
#' |---dct\cr
#' |---do_NBER\cr
#' |---doc\cr
#' |---dta\cr
#' |-----core_and_topical\cr
#' |-2004\cr
#' |---dat\cr
#' |---dct\cr
#' |---do_NBER\cr
#' |---doc\cr
#' |---dta\cr
#' |-----core_and_topical\cr
#' |-2008\cr
#' |---dat\cr
#' |---dct\cr
#' |---do_NBER\cr
#' |---doc\cr
#' |---dta\cr
#' |-----core_and_topical\cr
#' in particular, the paths yyyy/dta/core_and_topical must exist
#' @param path path/to/SIPP/folderstructure
#' @param outfile where to save dataset
#' @param heads TRUE if only family reference person is kept
#' @param test TRUE if run on test dataset
MakeSIPP_all <- function(path="~/datasets/SIPP",outfile="~/git/migration/mig-pkg/data/sipp.RData",heads=TRUE,test=FALSE){

	yrs <- list.files(path)

	l <- list()

	for (y in 1:length(yrs)){

		if(test){
			fi <- file.path(path,yrs[y],paste0("dta/core_and_topical/test_",yrs[y],".dta"))

		} else {
			fi <- file.path(path,yrs[y],paste0("dta/core_and_topical/core_top_",yrs[y],".dta"))

		}

		d <- data.table(foreign::read.dta(fi))
		
		cat(sprintf('\nloaded year %s \n',yrs[y]))

		# fix family reference number. 
		# currently a numeric 101 101 101
		# but needs to be compatible with epppnum, which is
		# "0101" "0101"
		d[,pid := as.integer( epppnum )]	# person id within each ssuid
		d[,famhead := efrefper==pid]	# indicator of family reference person

		d[,non.work := as.numeric(ersnowrk)]
		d[non.work==3, non.work := 2]
		d[,non.work := factor(non.work,labels=levels(ersnowrk)[-3])]

		# in some waves those vars are missing
		if ( !("tmovrflg" %in% names(d)) ) d[ ,tmovrflg := "missing var"]
		if ( !("tprstate" %in% names(d)) ) d[ ,tprstate := "missing var"]
		if ( !("tbrstate" %in% names(d)) ) d[ ,tbrstate := "missing var"]
			
		nm <- data.table(oldname=c("tfipsst","tmovrflg","etenure","rfnkids","wffinwgt", "esex","wpfinwgt",  "tage","eeducate","east3e",  "thhtnw",   "thhtwlth","thhtheq",    "rhcalyr","rhcalmn","tprstate",          "eprevres",               "tbrstate",      "tmovyryr",           "toutinyr",                    "tmovest",                "eprevten",           "thtotinc","tftotinc"),
						 newname=c("state",  "mover",   "tenure", "numkids","famweight","sex", "persweight","age", "educ",    "mortgage","net.wealth","wealth", "home.equity","year",   "month",  "MIG_previous_state","MIG_where_previous_home","MIG_state_born","MIG_year_moved_here","MIG_year_moved_into_previous","MIG_year_moved_to_state","MIG_previous_tenure","HHincome","faminc"))

		setnames(d,nm$oldname,nm$newname)

		# make a unique observation number
		d[,upid := paste0(yrs[y],ssuid,epppnum)]

		keep <- c("famhead","upid","ssuid","pid",nm$newname)
		nvs <- setdiff(names(d),keep)

		# drop all vars not in that list
		d[,nvs := NULL, with=FALSE]

		d[,own := FALSE]
		d[as.numeric(tenure)==1, own := TRUE ]



		# numeric educ variable
		d[,educx := as.numeric(educ)]
		d[,college := FALSE]
		d[educx>10, college := TRUE]

		
		# make lagged variables
		# =====================

		# want: own(t-1), income(t-1), non.work(t-1), kids(t-1)
		# where t-1 means "a year ago"
		d[,monthid := as.integer(month)]
		d[,yearmon := year * 100 + monthid]

		# need a sequence of unique year month identifiers
		tmp <- d[,list(yearmon=unique(yearmon))]
		tmp <- tmp[complete.cases(tmp)]
		setkey(tmp,yearmon)
		tmp[,yrmnid := 1:nrow(tmp)]
		setkey(d,yearmon)

		d <- d[tmp]

		setkey(d,ssuid,pid,year,monthid)
		d[,own.1yr      := d[list(ssuid,pid,year-1,monthid)][["own"]]]
		#d[,non.work.1yr := d[list(ssuid,pid,year-1,monthid)][["non.work"]]]
		d[,numkids.1yr  := d[list(ssuid,pid,year-1,monthid)][["numkids"]]]
		d[,wealth.1yr   := d[list(ssuid,pid,year-1,monthid)][["wealth"]]]
		d[,equity.1yr   := d[list(ssuid,pid,year-1,monthid)][["home.equity"]]]
		d[,faminc.1yr   := d[list(ssuid,pid,year-1,monthid)][["faminc"]]]
		d[,state.1yr    := d[list(ssuid,pid,year-1,monthid)][["state"]]]
		
		setkey(d,ssuid,pid,yrmnid)
		d[,own.1mn      := d[list(ssuid,pid,yrmnid-1)][["own"]]]
		#d[,non.work.1mn := d[list(ssuid,pid,yrmnid-1)][["non.work"]]]
		d[,numkids.1mn  := d[list(ssuid,pid,yrmnid-1)][["numkids"]]]
		d[,wealth.1mn   := d[list(ssuid,pid,yrmnid-1)][["wealth"]]]
		d[,equity.1mn   := d[list(ssuid,pid,yrmnid-1)][["home.equity"]]]
		d[,faminc.1mn   := d[list(ssuid,pid,yrmnid-1)][["faminc"]]]
		d[,state.1mn    := d[list(ssuid,pid,yrmnid-1)][["state"]]]

		d[MIG_year_moved_here > 0,duration_at_current := as.numeric(yrs[y]) - MIG_year_moved_here]
		d[MIG_year_moved_here > 0 & MIG_year_moved_into_previous > 0,duration_at_previous := MIG_year_moved_here - MIG_year_moved_into_previous]

		# create a monthly state-2-state indicator
		d[,S2S.mn := (state != state.1mn & !is.na(state.1mn) )]
		# relationship between duration an S2S
		d[,mean(S2S.mn,na.rm=T),by=duration_at_current][,plot(duration_at_current,V1)]

		# create annual mover variable (n.a. for 1996)
		d[,S2S := ( mover=="Moved, different state" )]



		# make growth variables
		# =====================

		d[,dfaminc.mn := faminc - faminc.1mn,by=upid]
		d[,dfaminc.yr := faminc - faminc.1yr,by=upid]



		# mark panel
		d[,panel := yrs[y] ]

		# heads only
		# ==========

		if (heads) d <- d[famhead==TRUE]

		l[[y]] <- d

		cat(sprintf('\ndone with year %s \n',yrs[y]))

	}

	#     sipp <- rbindlist(l)

	#save(sipp,file=outfile)

	return(l)
}


	
