

# documentation of data at http://www.nber.org/cps/cpsmar13.pdf

library(data.table)
library(foreign)
library(ggplot2)
library(reshape2)
library(xtable)

d <- data.table(read.dta("~/datasets/CPS/outdata/selected.dta"))

# adjust data for shift in coding of NXTRES
d[h_year<2011& nxtres>12, nxtres := nxtres + 1L]
d[,nxtres.2 := nxtres]
d[nxtres %in% c(2L,9:14) ,nxtres.2 := 100L]	# housing
d[nxtres %in% c(1L,3L)    ,nxtres.2 := 200L]	# family 
d[nxtres %in% 4:8       ,nxtres.2 := 300L]	# work 
d[nxtres %in% 15:19     ,nxtres.2 := 400L]	# other

d[, nxtresf := factor(nxtres, labels=c("NIU","change marstat","estab. own household","other fam reason","new job/job transfer","look for job","closer to work","retired","other job reason","want to own","better house","better neighborhood","cheaper housing","foreclosure","other housing","attend/leave college","climate change","health","natural disaster","other"))]

levels(d$migsame) <- c("NIU","yes (nonmover)","no, different US","no, different foreign")

d[, main.reason := factor(nxtres.2, labels=c("NIU","housing","family","work","other"))]

library(survey)
des <- svydesign(ids=~1,weights=~fsup_wgt,data=d)
desmv <- subset(des,as.numeric(migsame)==3)		# mover sample
desmv.st <- subset(des,as.numeric(mig_mtr3) > 2 & as.numeric(mig_mtr3) < 7)		# mover sample across state


# 1) did move at all?
# notice that tenure is CURRENT tenure. 
# those stats tell you how many migrants ended up as owners, not how many owners migrated.

tabs <- list()
tabs$all <- prop.table(round(svytable(~main.reason , des),3))

tabs$mv1 <- data.frame(svytable(formula=~migsame, des,N=100,exclude="NIU"))
names(tabs$mv1) <- c("same address last year?","Percent")

# 2) did move to another state?
tabs$mv2 <- data.frame(svytable(formula=~mig_mtr3, des,N=100,exclude="Not in universe (children under"))
names(tabs$mv2) <- c("where did you move?","Percent")

# 3)for all X state movers, what was main reason for moving ?
tabs$mv3 <- data.frame(round(svytable(~nxtresf, desmv.st, N=100),1))
names(tabs$mv3) <- c("main reason for moving","Percent")
tabs$mv3 <- tabs$mv3[order(tabs$mv3$Percent,decreasing=TRUE),]

# 4) aggregated up of 3)
tabs$mv4 <- data.frame(round(svytable(~main.reason, desmv.st, N=100),1))
names(tabs$mv4) <- c("main reason for moving","Percent")
tabs$mv4 <- tabs$mv4[order(tabs$mv4$Percent,decreasing=TRUE),]

# print to tables
print(xtable(tabs$mv1,align=c("rr|r")),file="~/git/migration/data/output/CPS/mv1.tex",floating=FALSE,include.rownames=FALSE)
print(xtable(tabs$mv2,align=c("rr|r")),file="~/git/migration/data/output/CPS/mv2.tex",floating=FALSE,include.rownames=FALSE)
print(xtable(tabs$mv3,align=c("rr|r")),file="~/git/migration/data/output/CPS/mv3.tex",floating=FALSE,include.rownames=FALSE)
print(xtable(tabs$mv4,align=c("rr|r")),file="~/git/migration/data/output/CPS/mv4.tex",floating=FALSE,include.rownames=FALSE)

# 4) trend in moving across state lines
tabs$mv5 <- data.frame(round(prop.table(svytable(~main.reason + h_year, des),2),3))
tabs$mv6 <- dcast(tabs$mv5,main.reason~ h_year)

p <- list()
p$move.trend <- ggplot(subset(tabs$mv5,main.reason!="NIU"),aes(x=h_year,y=Freq,color=main.reason,group=main.reason)) + geom_line(size=1.1) + ggtitle('main reason for moving across states') + scale_y_continuous(name="percent of population") + theme_bw()
ggsave(p$move.trend, file="~/git/migration/data/output/CPS/trend.pdf",width=8,height=7)



