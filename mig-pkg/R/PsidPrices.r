

#' creates median house value and income
#' by desired geography, by year from 1968-1996
#'
makePSID <- function(path="~/datasets/psid2",geo="Division"){

	# variables:
	#    * own yes/no
	#    * house value
	#    * current state
	#    * year
	#    * family income
	#    * family weight

	# set up psidR

	# TODO: use ONLY PSID state indicator, don't switch in 1985 when FIPS becomes available!
	# TODO: put together full PSID to get lifetime moving rates: how many people move never, 1 time, ...

	# state codes are not FIPS up to 1984: http://psidonline.isr.umich.edu/data/Documentation/PSIDStateCodes.pdf
	# afterwards FIPS.
	famvars <- data.frame(year = 1968, own = NA,hvalue="V5",state="V93",faminc="V81",famweight="V439") 
	famvars <- rbind(famvars,data.frame(year = 1969, own = NA,hvalue="V449",state="V537",faminc="V529",famweight="V1014"))
	famvars <- rbind(famvars,data.frame(year = 1970, own = NA,hvalue="V1122",state="V1103",faminc="V1514",famweight="V1609"))# Total 1969 family Money Income
	famvars <- rbind(famvars,data.frame(year = 1971, own = NA,hvalue="V1823",state="V1803",faminc="V2226",famweight="V2321")) # Total 1970 Family Money Income
	famvars <- rbind(famvars,data.frame(year = 1972, own = NA,hvalue="V2423",state="V2403",faminc="V2852",famweight="V2968")) # Total 1971 Family money income
	famvars <- rbind(famvars,data.frame(year = 1973, own = NA,hvalue="V3021",state="V3003",faminc="V3256",famweight="V3301")) # Total 1972 Family Money Income
	famvars <- rbind(famvars,data.frame(year = 1974, own = NA,hvalue="V3417",state="V3403",faminc="V3676",famweight="V3721")) # Total 1973 Family Money Income
	famvars <- rbind(famvars,data.frame(year = 1975, own = NA,hvalue="V3817",state="V3803",faminc="V4154",famweight="V4224")) # Total 1974 Family Money Income
	famvars <- rbind(famvars,data.frame(year = 1976, own = NA,hvalue="V4318",state="V4303",faminc="V5029",famweight="V5099")) # Total 1975 Family Money Income
	famvars <- rbind(famvars,data.frame(year = 1977, own = NA,hvalue="V5217",state="V5203",faminc="V5626",famweight="V5665")) # Total 1976 Family Money Income
	famvars <- rbind(famvars,data.frame(year = 1978, own = NA,hvalue="V5717",state="V5703",faminc="V6173",famweight="V6212")) # Total 1977 Family Money Income
	famvars <- rbind(famvars,data.frame(year = 1979, own = NA,hvalue="V6319",state="V6303",faminc="V6766",famweight="V6805")) # Total 1978 Family Money Income
	famvars <- rbind(famvars,data.frame(year = 1980, own = NA,hvalue="V6917",state="V6903",faminc="V7412",famweight="V7451")) # Total 1979 Family Money Income
	famvars <- rbind(famvars,data.frame(year = 1981, own = NA,hvalue="V7517",state="V7503",faminc="V8065",famweight="V8103")) # Total 1980 Family Money Income
	famvars <- rbind(famvars,data.frame(year = 1982, own = NA,hvalue="V8217",state="V8203",faminc="V8689",famweight="V8727")) # Total 1981 Family Money Income
	famvars <- rbind(famvars,data.frame(year = 1983, own = NA,hvalue="V8817",state="V8803",faminc="V9375",famweight="V9433")) # Total 1982 Family Money Income
	famvars <- rbind(famvars,data.frame(year = 1984, own = NA,hvalue="V10018",state="V10003",faminc="V11022",famweight="V11079")) # Total 1983 Family Money Income

	# now FIPS state codes
	famvars <- rbind(famvars,data.frame(year = 1985, own = NA,hvalue="V11125",state="V12380",faminc="V12371",famweight="V12446")) # Total 1984 Family Money Income
	famvars <- rbind(famvars,data.frame(year = 1986, own = "V13023",hvalue="V12524",state="V13632",faminc="V13623",famweight="V13687")) # Total 1985 Family Money Income
	famvars <- rbind(famvars,data.frame(year = 1987, own = "V14126",hvalue="V13724",state="V14679",faminc="V14670",famweight="V14737")) # Total 1986 Family Money Income
	famvars <- rbind(famvars,data.frame(year = 1988, own = "V15140",hvalue="V14824",state="V16153",faminc="V16144",famweight="V16208")) # Total 1987 Family Money Income
	famvars <- rbind(famvars,data.frame(year = 1989, own = "V16641",hvalue="V16324",state="V17539",faminc="V17533",famweight="V17612")) # Total 1988 Family Money Income
	famvars <- rbind(famvars,data.frame(year = 1990, own = "V18072",hvalue="V17724",state="V18890",faminc="V18875",famweight="V18943")) # Total 1989 Family Money Income, famweight is 1990 core fam weight
	famvars <- rbind(famvars,data.frame(year = 1991, own = "V19372",hvalue="V19024",state="V20190",faminc="V20175",famweight="V20243")) # Total 1990 Family Money Income, famweight is 1990 core fam weight
	famvars <- rbind(famvars,data.frame(year = 1992, own = "V20672",hvalue="V20324",state="V20303",faminc="V21481",famweight="V21547")) # Total 1991 Family Money Income
	famvars <- rbind(famvars,data.frame(year = 1993, own = "V22427",hvalue="V21610",state="V23328",faminc="V23322",famweight="V23361")) # otal 1992 Family Money Income, famweight is 1990 core fam weight
	famvars <- rbind(famvars,data.frame(year = 1994, own = "ER2032",hvalue="ER2033",state="ER4157",faminc="ER4153",famweight="ER4160")) # Total 1993 Family Money Income, famweight is 1990 core fam weight
	famvars <- rbind(famvars,data.frame(year = 1995, own = "ER5031",hvalue="ER5032",state="ER6997",faminc="ER6993",famweight="ER7000")) # Total 1994 Family Money Income, famweight is 1990 core fam weight
	famvars <- rbind(famvars,data.frame(year = 1996, own = "ER7031",hvalue="ER7032",state="ER9248",faminc="ER9244",famweight="ER9251")) # Total 1995 Family Money Income
	famvars <- rbind(famvars,data.frame(year = 1997, own = "ER10035",hvalue="ER10036",state="ER10004",faminc="ER12079",famweight="ER12084")) # Total 1996 Family Money Income
	# famvars <- rbind(famvars,data.frame(year = 1999, own = "ER10035",hvalue="ER10036",state="ER10004",faminc="ER12079",famweight="ER12084")) # Total 1996 Family Money Income

	p <- build.panel(datadir="~/datasets/psid2",fam.vars=famvars,design="all",verbose=TRUE,core=TRUE,heads.only=TRUE)
	d <- p$data
	saveRDS(d,file="~/git/migration/mig-pkg/data/psid_unclean.rds")
	return(p)
}



CleanPSID <- function(psidfile="~/git/migration/mig-pkg/data/psid_unclean.rds"){

	psid = readRDS(psidfile)


	# adjust psid state codes to FIPS
	data(US_states,package="EconData",envir=environment())
	US_states[,Division := abbreviate(Division,minlength=3)]

	psid_fips <- US_states[,list(FIPS,PSID,Division)]
	setkey(psid_fips,PSID)
	psid1984 <- psid[year<1985]
	psid1997 <- psid[year>1984]

	# merge state variable in 1984 (ie PSID codes)
	setkey(psid1984,state)
	psid1984 <- psid1984[psid_fips]
	psid1984[,FIPS := NULL]

	# merge state variable in 1997 (ie FIPS codes)
	setkey(psid1997,state)
	setkey(psid_fips,FIPS)
	psid1997 <- psid1997[psid_fips]
	psid1997[,PSID:= NULL]
	psid <- rbind(psid1984,psid1997)
	psid <- psid[!is.na(Division)]

	pl <- list()
	pl$nobs_div <- ggplot(psid[,list(nobs=.N),by=list(Division,year)],aes(x=year,y=nobs)) + geom_line()+facet_wrap(~Division)
	pl$nobs     <- ggplot(psid[,list(nobs=.N),by=list(year)],aes(x=year,y=nobs)) + geom_line()

	# censor variables
	psid <- psid[hvalue<999999 & faminc > 0]
	# psid <- psid[acc_hvalue==0|is.na(acc_hvalue)]

	# Inflation: get cpi based in 2011
	# ================================

	# data(cpi,envir=environment())
	cpi <- getCPI("yearly",base=2011)

	setkey(psid,year)
	psid <- cpi[psid]
	# adjust by inflation and divide by 1000 dollars
	psid[,rincome := faminc / (cpi2011 * 1000)]
	psid[,rhvalue := hvalue / (cpi2011 * 1000)]

	pl$densp <- ggplot(psid[(Division=="Pcf"|Division=="NwE")&hvalue>0&year>1990&year<1995],aes(x=rhvalue)) +geom_density(aes(color=factor(year))) + ggtitle("problems: real house value") + facet_wrap(~Division)

	pl$p_all <- ggplot(psid[hvalue>0,wtd.quantile(rhvalue,famweight,0.5),by=list(year)][order(year)],aes(year,V1)) + geom_line() +ggtitle("real 2011 house value")
	pl$p_div <- ggplot(psid[hvalue>0,wtd.quantile(rhvalue,famweight,0.5),by=list(Division,year)][order(year)],aes(year,V1)) + geom_line() + facet_wrap(~Division) +ggtitle("real 2011 house value")

	# "correct" 1992 in Pcf, NwE
	# I assume that this is measurement error.
	pp = psid[,list(y=.SD[rincome>0,wtd.quantile(rincome,famweight,0.5)],p=.SD[rhvalue>0,wtd.quantile(rhvalue,famweight,0.5)]),by=list(year,Division)]
	pp[year==1992 & Division=="Pcf",p := pp[(year==1991|year==1993) & Division=="Pcf",mean(p)]]
	pp[year==1992 & Division=="NwE",p := pp[(year==1991|year==1993) & Division=="NwE",mean(p)]]

	# moving avergae on income
	setkey(pp,year,Division)
	# compute 3 year moving averages
	pp[,yroll := c(y[1],rollapply(y,3,mean),y[.N]),by=Division]
	pp[,proll := c(p[1],rollapply(p,3,mean),p[.N]),by=Division]
	m = melt(pp[,list(year,Division,y,yroll,p,proll)],id.vars=c("year","Division"))
	pl$y_ma <- ggplot(subset(m,!variable %in% c("proll","p")),aes(year,value,color=variable)) + geom_line() + facet_wrap(~Division) + ggtitle('median income and rolling average')
	pl$p_ma <- ggplot(subset(m,!variable %in% c("yroll","y")),aes(year,value,color=variable)) + geom_line() + facet_wrap(~Division) + ggtitle('median price and rolling average')

	pl$p_fixed <- ggplot(pp,aes(x=year,y=p)) + geom_line() + facet_wrap(~Division)
	pl$y <- ggplot(pp,aes(x=year,y=y)) + geom_line() +ggtitle("Real 2011 median income")
	pl$y_div <- ggplot(pp,aes(x=year,y=y)) + geom_line() + facet_wrap(~Division) +ggtitle("Real 2011 median income")

	out = list()
	out$psid = psid
	out$pp = pp
	out$plots = pl

	message("produced psid median house value and median income 1968-1997 inflated to 2011 dollars.")

	return(out)

}