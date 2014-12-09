



# ACS 2007-2011 statistics

getACS <- function(){


	us = geo.make(state ="*")

	# total population
	pop = acs.fetch(geography=us,table.number="B01003")

	dat = data.frame(estimate(pop))
	dat$State = rownames(dat)
	names(dat)[1] = "population"

	# median house value B25077, 2007-2011 American Community Survey
	med.val = acs.fetch(geography=us,table.number="B25077")
	d2 = as.data.frame(estimate(med.val))
	d2$State = rownames(d2)
	dat = merge(dat,d2)
	names(dat)[3] = "med.value"

	# median gross rent 2007-2011 American Community Survey
	med.rent = acs.fetch(geography=us,table.number="B25064")
	d2 = as.data.frame(estimate(med.rent))
	d2$State = rownames(d2)
	dat = merge(dat,d2)
	names(dat)[4] = "med.rent"
	dat$med.rent = 12*dat$med.rent

	# median household income 2011 dollars
	median.income = acs.fetch(geography=us,table.number="B19013")
	d2 = as.data.frame(estimate(median.income))
	d2$State = rownames(d2)
	dat = merge(dat,d2)
	names(dat)[5] = "med.income"

	# ownership rate by state
	ownership = acs.fetch(geography=us,table.number="B25008",col.names="pretty")
	d2 = as.data.frame(estimate(ownership))
	d2$State = rownames(d2)
	d2$own = d2[,2] / d2[,1]
	d2 = subset(d2,select=c(State,own))
	dat = merge(dat,d2)
	names(dat)[6] = "ownership"

	dat$r2p = dat$med.rent / dat$med.value


	data(US_states,package="EconData")
	US = US_states[,list(STATE,state,Division)]
	dat$STATE = toupper(dat$State)

	dat = merge(dat,US,by="STATE")
	dat = as.data.table(dat[complete.cases(dat),])
	dat[,w := population / .SD[,sum(population)],by=Division]

	save(dat,file="~/git/migration/mig-pkg/data/acs.rda")

	y =dat[,list(med.value=prettyNum(round(weighted.mean(med.value,w)),big.mark=","),med.rent=prettyNum(round(weighted.mean(med.rent,w)),big.mark=","),med.income=prettyNum(round(weighted.mean(med.income,w)),big.mark=","),ownership=round(100*weighted.mean(ownership,w)),r2p=round(100*weighted.mean(r2p,w),1),dpop = sum(population)),by=Division]
	y[,pop.share := round(100*dpop / sum(dpop),1)]
	y[,dpop := NULL]

	return(y)


}


merge.ACS.price = function(acs){

	x = Export.VAR()
	xx = x$pyagg[,list(sdy=round(sd(y),2),sdp=round(sd(p))),by=Division]
	setnames(xx,"Division","Div")
	setkey(xx,Div)


	acs[,Div := abbreviate(Division,minlength=3)]
	setkey(acs,Div)
	acs.p = merge(acs,xx)

	data(cps)
	d_moves = cps[h_year==2013,list(D2D=round(100*mean(D2D),2)),by=list(Division=currdiv)]
	setkey(d_moves,Division)
	setkey(acs.p,Division)
	acs.p.move = merge(acs.p,d_moves)
	acs.p.move[,Div:=NULL]
	write.csv(acs.p.move,file="~/Dropbox/giulia/usa-maps/table.csv")
	return(acs.p.move)
}

