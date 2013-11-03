
rm(list=ls())

library(data.table)
library(RCurl)

# TODO
# how to deal with missing grow up variable?
# use labor income head or labor income head + wife?

# build dataset to explore.
# variable catalogue: make sure we have a consistent set of variables in all years

# look into http://bit.ly/1dF3TkQ for a google doc with all variable definitions

# switches
build.data <- FALSE # set TRUE if want to rebuild from raw data



# Data from Family Files
# ======================

if (build.data){

	# years to take
	first.year <- 1994

	# download variable catalogue from google docs
	# -------------------------------------------

	library(RCurl)
	myVars <- getURL("https://docs.google.com/spreadsheet/pub?key=0AnOrv_MIRexjdFl1cGFwajZLRzhpMks5VUh5ei03MkE&gid=1&output=csv")

	d           <- read.csv(textConnection(myVars),colClasses="character")
	d           <- d[-(27:33),]
	rownames(d) <- d$variable.label
	d           <- d[,-1]
	names(d)    <- substr(names(d),2,5)

	# construct fam.vars data.frame for psidR
	# ---------------------------------------

	famv <- data.frame(year        = as.numeric(names(d)),
					  age          = as.character(d["AGE OF HEAD",]),
					  educ         = as.character(d["COMPLETED ED-HD",]),
					  educOLD      = as.character(d["EDUCATION OLD",]),
					  incomeHEAD   = as.character(d["HEAD LABOR INCOME",]),
					  incomeWIFE   = as.character(d["WIFE LABOR INCOME",]),
					  incomeFAM    = as.character(d["TOTAL FAMILY INCOME",]),
					  Hvalue       = as.character(d["HOUSE VALUE  (A16)",]),
					  mort1        = as.character(d["A24 REM PRINCIPAL MOR 1",]),
					  mort2        = as.character(d["A24 REM PRINCIPAL MOR 2",]),
					  mort1.mnthly = as.character(d["A25 MNTHLY PMTS MOR   1",]),
					  mort2.mnthly = as.character(d["A25 MNTHLY PMTS MOR   2",]),
					  mort.annual  = as.character(d["ANN MOR PMTS",]),
					  rent.annual  = as.character(d["ANN RENT (A27)",]),
					  rent.per     = as.character(d["A31 DOLLARS RENT",]),
					  rent.unit    = as.character(d["A31 DOLLLARS PER WHAT",]),
					  empstat      = as.character(d["B1 EMPLOYMENT STATUS-HD",]),
					  marstat      = as.character(d["HEAD MARITAL STATUS",]),
					  numkids      = as.character(d["# CHILDREN IN FU",]),
					  state        = as.character(d["CURRENT STATE FIPS",]),
					  home         = as.character(d["STATE HD GREW UP",]),
					  newhead      = as.character(d["WTR NEW HEAD",]),
					  moved        = as.character(d["MOVED SINCE LAST YEAR?",]),
					  why.moved    = as.character(d["WHY MOVED",]),
					  likely.move  = as.character(d["LIKELIHOOD OF MOVING",]),
					  wealth       = as.character(d["WEALTH W/O EQUITY",]),
					  weight       = as.character(d["CORE FAMILY WEIGHT",]))
					 
	# the wealth variables need to be merged from a supplementary dataset after we've merged family and individual
	# index file. so set wealth to NA whenever it has an "S" prefix.

	famv$wealth[substr(famv$wealth,1,1)=="S"] <- NA

	famv <- subset(famv,year>=first.year)
	rm(first.year)


	# merge family files to individual index.
	# =======================================

	library(psidR)
	panel <- build.panel(datadir="~/datasets/PSID/fam-files",fam.vars=famv,verbose=TRUE,design=2,core=FALSE)

	# some pid's exit and re-enter the panel: drop them.
	tmp <- copy(panel$data)
	tmp[,yeardiff := max(diff(year))>2,by=pid]
	dat <- copy(tmp[yeardiff==FALSE])
	dat[,yeardiff := NULL]

	rm(panel)
	gc()
	save(dat,file="~/git/migration/data/psidraw.RData")
} else {
	load("~/git/migration/data/psidraw.RData")
}

# housing tenure indicator
dat[Hvalue>9999996, Hvalue := NA]
dat[mort1>9999996, mort1:= NA]
dat[mort2>9999996, mort2:= NA]
dat[,own := Hvalue!=0 & !is.na(Hvalue)]


# state codes of HD GREW UP are not FIPS  before 1994
# ===================================================

myVars <- getURL("https://docs.google.com/spreadsheet/pub?key=0AnOrv_MIRexjdFl1cGFwajZLRzhpMks5VUh5ei03MkE&single=true&gid=4&output=csv")
st.psid <- read.csv(textConnection(myVars),colClasses="character")
st.psid <- st.psid[-c(51,52),]
st.psid <- data.table(st.psid,key="State")

# load state abbreviations and merge onto st.psid
load("~/git/Rdata/out/states-abbrev.RData")
setkey(abbr,State)
st.psid <- st.psid[abbr]
st.psid <- st.psid[complete.cases(st.psid)]
st.psid[,c("FIPS","Abbreviation.1") := NULL]
setnames(st.psid,"FIPS.1","FIPS")
st.psid <- rbind(st.psid,data.table(State=c("ABROAD","DK"),psid_code=c(0,99),Abbreviation=c("ABROAD","DK"),region=c(NA,NA),FIPS=c(0,99)))

# test missing homes
dat[is.na(home),all( unique(year) %in% 1994:1996 )]	# only some years have no home info at all

# quick check: how many people live in the state they were born?
dat[,same:=home==state]
dat[,table(same)]
dat[,same := NULL]

# drop all data earlier than 1994
# -------------------------------

dat <- copy(dat[year>1993])
dat[,c("educOLD","mort.annual","rent.annual","newhead") := NULL]

# what is the panel structure, i.e. how many new obs do we have each year, how many survive from previous year?
dat[,firstyear := min(year),by=pid]
dat[,table(firstyear)]
dat[,firstyear := NULL]


# home 1994-1996 is missing
# =========================

# drop observations who are new heads in 1994-1996 and do not survive until 1997
# update "home" for the rest by carrying backward values after 1996
setkey(dat,pid,year)
dat[,concerned := year[1] %in% 1994:1996, by=pid]	# heads whose first year is either 94,95 or 96 are concerned.
dat[,survive := TRUE]
dat[concerned==TRUE,survive := 1997 %in% year, by=pid]	# if they have year 1997 in their record, they survive the hole.
dat <- copy(dat[survive==TRUE])	# if they are around only during the hole, drop.
dat[,c("concerned","survive") := NULL]	# clean up

# great number of people born abroad
dat[,table(home)]

# for immigrants from abroad, 2 possibilities:
# 1) for those home is abroad, and they are assumed to never move back home.
# 2) for those home is first state they lived in.

dat[,homeless := any(is.na(home)),by=pid]	# if home obs is missing, consider homeless
# there seems to be some initial measurement error in home.
# take latest available answer to the question.
homeless <- dat[homeless==TRUE,list(newhome=tail(home[!is.na(home)],1)),by=pid]
setkey(homeless,pid)
dat <- copy(homeless[dat])

# set home to newhome for homeless guys
dat[homeless==TRUE,home := newhome]
dat[,homeless := NULL]


# drop observations with missing state
dat <- copy(dat[!(state==99 | state==15)])

# drop also if have state==15, which is an inexistent FIPS code
dat <- copy(dat[state!=15])

# merge in state names for home and state, and merge region
setkey(dat,state)
setkey(st.psid,FIPS)
dat[,c("State","region") := st.psid[dat][,list(Abbreviation,region)]]
setkey(dat,home)
dat[,c("Home","Home.region") := st.psid[dat][,list(Abbreviation,region)]]
dat[,newhome := NULL]



# create lagged variables
# ======================

# create lagged state to detect interstate movers
# first create a year index, since year are not equally spaced
setkey(dat,year)
yindex <- data.table(yid=1:dat[,length(unique(year))],year=dat[,list(V1=unique(year))][order(V1),V1],key="year")
dat[,yid := dat[yindex][["yid"]] ]
setkey(dat,pid,yid)
dat[,state.l := dat[list(pid,yid-1)][["state"]] ]
dat[,State.l := dat[list(pid,yid-1)][["State"]] ]

# create interstate moving variable
dat[,inter := FALSE]
# when state is not equal to lagged state and lagged state is not NA, we have a state move
dat[state != state.l & !(is.na(state.l)), inter := TRUE,by=pid]	

# create general moving indicator
dat[,moveYES := moved==1]

# create lag and lead value of region to detect interregional moves
dat[,region.from := dat[list(pid,yid-1)][["region"]] ]
dat[,region.to   := dat[list(pid,yid+1)][["region"]] ]

# create interregion moving ind
dat[,interreg := FALSE]
dat[region != region.from & !is.na(region.from), interreg := TRUE,by=pid]

# create lagged and leaded income
dat[,incomeFAM.l := dat[list(pid,yid-1)][["incomeFAM"]] ]
dat[,incomeFAM.next := dat[list(pid,yid+1)][["incomeFAM"]] ]


# more lagged variables
# ---------------------

setkey(dat,pid,yid)
dat[,marstat.l := dat[list(pid,yid-1)][["marstat"]] ]
dat[,why.move.l := dat[list(pid,yid-1)][["why.moved"]] ]	# this tests consistency of estimate
dat[,dnumkids := diff(numkids),by=pid]
dat[,divorce := marstat==4&marstat.l==1]


# merge in asset information
# ==========================

# TODO impute for 1995, 1996, 1997
 
library(foreign)
setkey(dat,year,interview)
assvars <- c("S316","S416","S516","S616","S716","S816")
idvars  <- c("S301","S401","S501","S601","S701","S801")
assyrs <- c(1994,1999,seq(2001,2007,by=2))
for (iy in 1:length(assyrs)){
	tmp <- data.table(read.dta(file=paste0("~/datasets/PSID/fam-files/wlth",assyrs[iy],".dta")))
	tmp[,year := assyrs[iy]]
	setnames(tmp,idvars[iy],"interview")
	setkey(tmp,year,interview)
	dat[.(assyrs[iy]), wealth := tmp[dat][year==assyrs[iy]][[assvars[iy]]] ]
}

rm(assyrs,idvars,assvars)

# merge in consumption
# ====================

idvars  <- paste0(c("CON99",paste0("CON0",c(1,3,5,7,9))),"_ID")
consyrs <- c(1999,seq(2001,2009,by=2))
for (iy in 1:length(consyrs)){
	tmp <- data.table(read.dta(file=paste0("~/datasets/PSID/fam-files/cons",consyrs[iy],".dta")))
	tmp[,year := consyrs[iy]]
	setnames(tmp,idvars[iy],"interview")
	setkey(tmp,year,interview)
	dat[.(consyrs[iy]), cons := tmp[dat][year==consyrs[iy]][["cons"]] ]
}
dat[cons>9999999, cons := NA]	# they coded NA as 9,999,999 in cons data.





# Factor Setup
# ============

# get uniform numerical codes across waves
# see again http://bit.ly/1dF3TkQ

# why.moved
dat[year==2011&why.moved>9, why.moved := NA]
dat[year!=2011&why.moved>8, why.moved := NA]
dat[,why.moved := factor(why.moved,labels=c("not moved","new job","more housing","less housing","want to own","better area","forced","ambiguous","homeless","DK"))]

# employment status
dat[empstat<1|empstat>8,empstat := NA]
dat[,empstat   := factor(empstat,labels=c("working","temp laid off","unemployed","retired","disabled","house keeping","student","other"))]

# marital status
dat[marstat<1|marstat>5,marstat := NA]
dat[,marstat   := factor(marstat,labels=c("married","never married","widowed","divorced","separated"))]


# make multinomial dataset
# =======================

# WNC <- dat[region=="West North Central"]
# wnc=WNC[,c(1,3,4,7,8,15,17,28,32,39),with=FALSE]
# wnc = wnc[!(is.na(region.to) | is.na(Home.region))]
# 
# setkey(wnc,pid,age)

# assign a dummy distance
# wnc[,region.to := factor(as.numeric(factor(region.to)),labels=paste0("R",1:8))]
# wnc[,dist_R1 := (1 - 7)^2]	# 7 is numeric code of current loc
# wnc[,dist_R2 := (2 - 7)^2]	# 7 is numeric code of current loc
# wnc[,dist_R3 := (3 - 7)^2]	# 7 is numeric code of current loc
# wnc[,dist_R4 := (4 - 7)^2]	# 7 is numeric code of current loc
# wnc[,dist_R5 := (5 - 7)^2]	# 7 is numeric code of current loc
# wnc[,dist_R6 := (6 - 7)^2]	# 7 is numeric code of current loc
# wnc[,dist_R7 := (7 - 7)^2]	# 7 is numeric code of current loc
# wnc[,dist_R8 := (8 - 7)^2]	# 7 is numeric code of current loc

# mldata <- mlogit.data(data=wnc,varying=11:18,id.var="pid",choice="region.to",shape="wide",sep="_")

# clogit <- mlogit(data=mldata,formula = region.to ~ 1 | incomeFAM + own + age,reflevel="R7")
# clogit <- mlogit(data=mldata,formula = region.to ~ 1 | incomeFAM + Hvalue + empstat + factor(numkids) + own + Home.region,reflevel="R7")


# make a second dataset containing movers location history
ss <- copy(dat[,list(inter,pid,state,yid)])
setkey(ss,pid,yid)
ss[,state.l2 := ss[list(pid,yid-2)][["state"]] ]
ss[,state.l3 := ss[list(pid,yid-3)][["state"]] ]
ss[,state.l4 := ss[list(pid,yid-4)][["state"]] ]
ss[,state.l5 := ss[list(pid,yid-5)][["state"]] ]
ss[is.na(state.l2),state.l2 := 999 ]
ss[is.na(state.l3),state.l3 := 999 ]
ss[is.na(state.l4),state.l4 := 999 ]
ss[is.na(state.l5),state.l5 := 999 ]
ss[inter==TRUE,state.back := ((state==state.l2)|(state==state.l3)|(state==state.l4)|(state==state.l5))]

rr <- copy(dat[,list(interreg,pid,region,Home.region,yid)])
rr[is.na(Home.region),Home.region := "999"]
setkey(rr,pid,yid)

rr[,region.l2 := rr[list(pid,yid-2)][["region"]] ]
rr[,region.l3 := rr[list(pid,yid-3)][["region"]] ]
rr[,region.l4 := rr[list(pid,yid-4)][["region"]] ]
rr[,region.l5 := rr[list(pid,yid-5)][["region"]] ]
rr[,region.l6 := rr[list(pid,yid-6)][["region"]] ]
rr[,region.l7 := rr[list(pid,yid-7)][["region"]] ]
rr[,region.l8 := rr[list(pid,yid-8)][["region"]] ]
rr[,region.l9 := rr[list(pid,yid-9)][["region"]] ]
rr[,region.l10 := rr[list(pid,yid-10)][["region"]] ]
rr[,region.l11 := rr[list(pid,yid-11)][["region"]] ]
rr[,region.l12 := rr[list(pid,yid-12)][["region"]] ]
rr[,region.l13 := rr[list(pid,yid-13)][["region"]] ]
rr[,region.l14 := rr[list(pid,yid-14)][["region"]] ]
rr[,region.l15 := rr[list(pid,yid-15)][["region"]] ]
rr[,region.l16 := rr[list(pid,yid-16)][["region"]] ]
rr[,region.l17 := rr[list(pid,yid-17)][["region"]] ]

rr[is.na(region.l2),region.l2 := "999" ]
rr[is.na(region.l3),region.l3 := "999" ]
rr[is.na(region.l4),region.l4 := "999" ]
rr[is.na(region.l5),region.l5 := "999" ]
rr[is.na(region.l6 ),region.l6  := "999" ]
rr[is.na(region.l7 ),region.l7  := "999" ]
rr[is.na(region.l8 ),region.l8  := "999" ]
rr[is.na(region.l9 ),region.l9  := "999" ]
rr[is.na(region.l10),region.l10 := "999" ]
rr[is.na(region.l11),region.l11 := "999" ]
rr[is.na(region.l12),region.l12 := "999" ]
rr[is.na(region.l13),region.l13 := "999" ]
rr[is.na(region.l14),region.l14 := "999" ]
rr[is.na(region.l15),region.l15 := "999" ]
rr[is.na(region.l16),region.l16 := "999" ]
rr[is.na(region.l17),region.l17 := "999" ]
rr[interreg==TRUE,region.back := ((region==region.l2)|(region==region.l3)|(region==region.l4)|(region==region.l5)|(region==region.l6)|(region==region.l7)|(region==region.l8)|(region==region.l9)|(region==region.l10)|(region==region.l11)|(region==region.l12)|(region==region.l13)|(region==region.l14)|(region==region.l15)|(region==region.l16)|(region==region.l17))]
rr[interreg==TRUE,region.home := (region==Home.region)]



# save dataset
save(dat,ss,rr,file="~/git/migration/data/psid.RData")



# house price process
vals = dat[,weighted.mean(Hvalue,weight,na.rm=T),by=list(region,year)]
vals[,rank := rank(V1),by=year]
setkey(vals,year,region)
