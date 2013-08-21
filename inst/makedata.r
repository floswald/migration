
rm(list=ls())
# TODO
# how to deal with missing grow up variable?
# use labor income head or labor income head + wife?

# build dataset to explore.
# variable catalogue: make sure we have a consistent set of variables in all years

# look into http://tinyurl.com/n33ojvy for a catalogue of variables

# Data from Family Files
# ======================

# download library catalogue from google docs
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
				  Hvalue       = as.character(d["HOUSE VALUE (A16)",]),
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


# merge family files to individual index.
# =======================================

library(psidR)
panel <- build.panel(datadir="~/datasets/PSID/fam-files",fam.vars=famv,verbose=TRUE,design=2)

# some pid's exit and re-enter the panel: drop them.
tmp <- copy(panel$data)
tmp[,yeardiff := max(diff(year))>2,by=pid]
dat <- copy(tmp[yeardiff==FALSE])
dat[,yeardiff := NULL]

rm(panel)
gc()


# state codes of HD GREW UP are not FIPS 
# ======================================

myVars <- getURL("https://docs.google.com/spreadsheet/pub?key=0AnOrv_MIRexjdFl1cGFwajZLRzhpMks5VUh5ei03MkE&single=true&gid=4&output=csv")
st.psid <- read.csv(textConnection(myVars),colClasses="character")
st.psid <- st.psid[-c(51,52),]
st.psid <- data.table(st.psid,key="State")

# load state abbreviations and merge onto st.psid
load("~/git/Rdata/out/states-abbrev.RData")
abbr[,FIPS := as.character(FIPS)]
setkey(abbr,State)
st.psid <- st.psid[abbr]
st.psid <- st.psid[complete.cases(st.psid)]
st.psid[,c("FIPS","Abbreviation.1") := NULL]
setnames(st.psid,"FIPS.1","FIPS")
st.psid[,psid_code:= as.character(psid_code)]
st.psid <- rbind(st.psid,data.table(State=c("ABROAD","DK"),psid_code=c(0,99),Abbreviation=c(NA,NA),FIPS=c(0,99)))

# for all years < 1994, setkey to psid_code and merge
setnames(st.psid,"psid_code","home")
setkey(st.psid,home)

# state codes should be characters, not numbers
dat[,state := as.character(state)]
dat[,home  := as.character(home)]

# merge FIPS code on home where home is the old psid_code
setkey(dat,home)
dat[year < 1994,homenew := st.psid[dat[year<1994]][,FIPS] ]
dat[year < 1994,home := homenew]

# merge in state names for home and state
setkey(dat,home)
setkey(st.psid,FIPS)
dat[,Home := st.psid[dat][,Abbreviation]]
setkey(dat,state)
dat[,State := st.psid[dat][,Abbreviation]]
dat[,homenew := NULL]

dat[,same:=Home==State]
dat[,table(same)]

