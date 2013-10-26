

library(data.table)
library(foreign)
setwd("~/datasets/CPS/outdata")

d <- data.table(read.dta("selected.dta"))

# adjust data for shift in coding of NXTRES
d[h_year<2013& nxtres>12, nxtres := nxtres + 1L]
d[,nxtres.2 := nxtres]
d[nxtres %in% c(2L,9:14) ,nxtres.2 := 100L]	# housing
d[nxtres %in% c(1L,3L)    ,nxtres.2 := 200L]	# family 
d[nxtres %in% 4:8       ,nxtres.2 := 300L]	# work 
d[nxtres %in% 15:19     ,nxtres.2 := 400L]	# other

d[, nxtresf := factor(nxtres, labels=c("NIU","change marstat","estab. own household","other fam reason","new job/job transfer","look for job","closer to work","retired","other job reason","want to own","better house","better neighborhood","cheaper housing","foreclosure","other housing","attend/leave college","climate change","health","natural disaster","other"))]

d[, nxtres2f := factor(nxtres.2, labels=c("NIU","housing","family","work","other"))]

library(survey)
des <- svydesign(ids=~1,weights=~fsup_wgt,data=d)
desmv <- subset(des,as.numeric(migsame)==3)		# mover sample
desmv.st <- subset(des,as.numeric(mig_mtr3) > 2 & as.numeric(mig_mtr3) < 7)		# mover sample across state


# 1) did move at all?
# notice that tenure is CURRENT tenure. 
# those stats tell you how many migrants ended up as owners, not how many owners migrated.
mv1 <- svytable(formula=~migsame+h_tenure, des,N=100)

# 2) did move to another state?
mv2 <- svytable(formula=~mig_mtr3+h_tenure, des,N=100)

# 3) what was main reason for moving across state line?
mv3 <- data.frame(round(svytable(~nxtresf, desmv.st, N=100,exclude="NIU"),1))
mv4 <- data.frame(round(svytable(~nxtres2f, desmv.st, N=100,exclude="NIU"),1))
names(mv4) <- c("main reason for moving","Percent")
mv4 <- mv4[order(mv4$Percent,decreasing=TRUE),]


