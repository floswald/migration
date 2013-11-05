

# Analyse the 2008 SIPP panel.
library(data.table)
library(ggplot2)
library(survey)
library(erer)
library(texreg)



setwd("~/git/migration/data/")

if( !file.exists( "SIPP2008.RData" ) ){

	library(foreign)
	d <- data.table(read.dta("~/datasets/SIPP/2008/dta/core_and_topical/core_top.dta"))

	# fix family reference number. 
	# currently a numeric 101 101 101
	# but needs to be compatible with epppnum, which is
	# "0101" "0101"
	d[,pid := as.integer( epppnum )]	# person id within each ssuid
	d[,famhead := efrefper==pid]	# indicator of family reference person

	d[,non.work := as.numeric(ersnowrk)]
	d[non.work==3, non.work := 2]
	d[,non.work := factor(non.work,labels=levels(ersnowrk)[-3])]

	setnames(d,c("tfipsst","tmovrflg","etenure","rfnkids","wffinwgt","esex","wpfinwgt","tage","eeducate","east3e","thhtnw","thhtwlth","thhtheq","rhcalyr","rhcalmn"),
			   c("state","mover","tenure","numkids","famweight","sex","persweight","age","educ","mortgage","net.wealth","wealth","home.equity","year","month"))

	# rename the migration variables to distinguish them better
	setnames(d,c("tprstate","eprevres","tbrstate","tmovyryr","toutinyr","tmovest","eprevten"),
			   c("MIG_previous_state","MIG_where_previous_home","MIG_state_born","MIG_year_moved_here","MIG_year_moved_into_previous","MIG_year_moved_to_state","MIG_previous_tenure"))

	d[,own := FALSE]
	d[as.numeric(tenure)==1, own := TRUE ]

	d[,S2S := ( mover=="Moved, different state" )]
	d[,duration_at_current := 2009 - MIG_year_moved_here]
	d[duration_at_current>2000, duration_at_current := NA]
	d[,duration_at_previous := MIG_year_moved_here - MIG_year_moved_into_previous]

	# make a unique observation number
	d[,upid := paste0(ssuid,epppnum)]

	
	# make lagged variables
	# =====================

	# want: own(t-1), income(t-1), non.work(t-1), kids(t-1)
	# where t-1 means "a year ago"
	d[,monthid := as.integer(month)]
	d[,yearmon := year * 100 + monthid]

	setkey(d,ssuid,pid,year,monthid)
	d[,own.1yr      := d[list(ssuid,pid,year-1,monthid)][["own"]]]
	d[,non.work.1yr := d[list(ssuid,pid,year-1,monthid)][["non.work"]]]
	d[,numkids.1yr  := d[list(ssuid,pid,year-1,monthid)][["numkids"]]]
	d[,wealth.1yr   := d[list(ssuid,pid,year-1,monthid)][["wealth"]]]
	d[,equity.1yr   := d[list(ssuid,pid,year-1,monthid)][["home.equity"]]]
	
	setkey(d,ssuid,pid,yearmon)
	d[,own.1mn      := d[list(ssuid,pid,yearmon-1)][["own"]]]
	d[,non.work.1mn := d[list(ssuid,pid,yearmon-1)][["non.work"]]]
	d[,numkids.1mn  := d[list(ssuid,pid,yearmon-1)][["numkids"]]]
	d[,wealth.1mn   := d[list(ssuid,pid,yearmon-1)][["wealth"]]]
	d[,equity.1mn   := d[list(ssuid,pid,yearmon-1)][["home.equity"]]]


	# heads only
	# ==========

	f <- d[famhead==TRUE]

	save(d,f, file="SIPP2008.RData")

} 

# if you want to plot, 
	#load("SIPP2008.RData")

# plot of family heads moving or not
p <- list()
p$all <- ggplot(f[age>25&age<65,list(moved.X.state=weighted.mean(S2S,famweight)),by=list(age)],aes(x=age,y=moved.X.state)) + geom_point() + geom_smooth(size=1.1) + scale_y_continuous(limit=c(0,0.012)) + theme_bw()

# plot of family heads moving or not by tenure
p$own <- ggplot(f[age>25&age<65,list(moved.X.state=weighted.mean(S2S,famweight),nobs=length(S2S)),by=list(age,own)],aes(x=age,y=moved.X.state,color=own)) + geom_point(aes(size=nobs),alpha=0.7) + geom_smooth(size=1.1)  + scale_y_continuous(limit=c(0,0.012)) + theme_bw()


p$dur <- ggplot(f[age>25&age<65,list(moved.X.state=weighted.mean(S2S,famweight),nobs=length(S2S)),by=list(duration_at_previous)],aes(x=duration_at_previous,y=moved.X.state)) + geom_point(aes(size=nobs),alpha=0.7) + geom_smooth(size=1.1)   + theme_bw()

pdf("output/sipp/all.pdf")
print(p$all)
dev.off()

pdf("output/sipp/own.pdf")
print(p$own)
dev.off()

pdf("output/sipp/dur.pdf")
print(p$dur)
dev.off()

# look at yearly lags
# ===================

sur <- list()
sur$fam <- svydesign(id=~1,weights=~famweight,data=f)

# remove data
# ===========

rm(d,f)
gc()

m <- list()
#m$lin <- lm(S2S ~ age + age + I(age^2) + own.1yr + wealth.1yr + equity.1yr + numkids.1yr + non.work.1yr,data=f)
#m$svylin <- svyglm(S2S ~ age + I(age^2) + own.1yr + wealth.1yr + equity.1yr + numkids.1yr + non.work.1yr,design=sur$fam)
#m$svyprobit <- svyglm(S2S ~ age + I(age^2) + own.1yr + wealth.1yr + equity.1yr + numkids.1yr + non.work.1yr,design=sur$fam, family=quasibinomial(link="probit"),x=TRUE)
m$svylogit <- svyglm(S2S ~ age + I(age^2) + own.1yr + wealth.1yr + equity.1yr + numkids.1yr + non.work.1yr,design=sur$fam, family=quasibinomial(link="logit"),x=TRUE)

#m$svyprob_marginal <- maBina(m$svyprobit,digits=5)	
#m$svylogit_marginal <- maBina(m$svylogit,digits=5)	

# look at monthly lags
# ===================

m2 <- list()
#m2$lin <- lm(S2S ~ age + age + I(age^2) + own.1mn + wealth.1mn + equity.1mn + numkids.1mn + non.work.1mn,data=f)
#m2$svylin <- svyglm(S2S ~ age + I(age^2) + own.1mn + wealth.1mn + equity.1mn + numkids.1mn + non.work.1mn,design=sur$fam)
#m2$svyprobit <- svyglm(S2S ~ age + I(age^2) + own.1mn + wealth.1mn + equity.1mn + numkids.1mn + non.work.1mn,design=sur$fam, family=quasibinomial(link="probit"),x=TRUE)
m2$svylogit <- svyglm(S2S ~ age + I(age^2) + own.1mn + wealth.1mn + equity.1mn + numkids.1mn + non.work.1mn,design=sur$fam, family=quasibinomial(link="logit"),x=TRUE)

#m2$svyprob_marginal <- maBina(m2$svyprobit,digits=5)	
#m2$svylogit_marginal <- maBina(m2$svylogit,digits=5)	


texreg(l=list(m$svylogit),custom.names=c("year lag"),file="output/SIPP_yearly.tex",table=FALSE)
texreg(l=list(m2$svylogit),custom.names=c("monthly lag"),file="output/SIPP_monthly.tex",table=FALSE)
