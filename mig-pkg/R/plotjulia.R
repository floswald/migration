

# plot julia data

plot.experiments <- function(){

	# load data
	d1 = read.csv("~/Dropbox/mobility/output/model/Julia2R/MC1.csv")
	d2 = read.csv("~/Dropbox/mobility/output/model/Julia2R/MC2.csv")
	d3 = read.csv("~/Dropbox/mobility/output/model/Julia2R/MC3.csv")

	p <- list()

	p[[1]] <- ggplot(d1,aes(x=moveto,y=prob,color=factor(MC))) + geom_line(size=1) + facet_wrap(~own) + ggtitle('Changes in owner cost') + theme_bw() + scale_x_discrete()
	p[[2]] <- ggplot(d2,aes(x=moveto,y=prob,color=factor(MC))) + geom_line(size=1) + facet_wrap(~own) + ggtitle('Changes in distance cost') + theme_bw() + scale_x_discrete()

	ggsave(plot=p[[1]],file="~/Dropbox/mobility/output/model/full/expMC/alpha1.pdf",width=13,height=9,scale=0.6)

	pdf("~/Dropbox/mobility/output/model/full/expMC/alpha2.pdf",width=12,height=9)
	print(p[[2]])
	dev.off()
	return(p)
}


df2hdf5 <- function(ff,df,path){
	nm = names(df)
	for (i in nm){
		tmpnm = file.path(path,i)
		h5write(df[,i],ff,tmpnm)
	}
}



plot.simulation <- function(){

	fi <- "~/Dropbox/mobility/output/model/data_repo/out_graphs_jl"
	d <- read.csv("~/Dropbox/mobility/output/model/data_repo/out_data_jl/simdata.csv")

	d <- as.data.table(d)

	# get housing spells
	d[,down := cumsum(c(0,diff(hh))),by=id]
	setkey(d,id,age)
	d[,down_plus := c(0,down[-29]),by=id]
	d[,down_change := down != down_plus]
	d[,spell := factor(cumsum(down_change)),by=id]
	d[,status := "Renter"]
	d[hh==1,status:="Owner"]

	# continuous variables
	contdat <- melt(d[,list(id,age,c,income,save,wealth,v,move,moveto)],id.vars=c("id","age"))
	contdat[variable=="move" & value==0.0,value := NA]

	# housing <- d[,list(h_choice=as.logical(hh[1]),duration=.N),by=list(id,spell)]
	housing <- d[,list(h_choice=status[1],duration=.N),by=list(id,spell)]
	housing[,to:=cumsum(duration)+1,by=id]
	housing[,from:=as.integer(to-duration),by=id]

	pl <- ggplot() + geom_rect(data=housing,aes(xmin=from,xmax=to,ymin=-Inf,ymax=Inf,fill=h_choice),alpha=0.8) 
	pl <- pl + geom_line(data=subset(contdat, variable %in% c("save","c","income")),aes(x=age,y=value,linetype=variable)) + facet_wrap(~id)

	# add move indicator
	pl <- pl + geom_point(data=subset(contdat, variable == "move"),aes(x=age,y=value))

	# fix legends
	gg_color_hue <- function(n) {
 	hues = seq(15, 375, length=n+1)
    hcl(h=hues, l=65, c=100)[1:n]
    }
	pl <- pl + scale_fill_manual(guide=guide_legend(title="Housing\nstatus"),values=gg_color_hue(2)) + ggtitle("Individual simulation histories") + theme_bw()

	pdf(file.path(fi,"sim-inds.pdf"),width=8,height=5)
	print(pl)
	dev.off()

	return(pl)
}



# export data to julia
export.Julia <- function(print.tabs=NULL,print.plots=NULL){
	data(Sipp_age,envir=environment())
	data(Sipp_age_svy,envir=environment())
	path <- "~/Dropbox/mobility/output/model/data_repo/in_data_jl"

	if (!is.null(print.tabs)){
		cat(sprintf("printing tables to %s",print.tabs))
	}
	if (!is.null(print.plots)){
		cat(sprintf("printing tables to %s",print.plots))
	}

	# subset age
	sub = merged[,age>=20 & age <= 50]

	# regional processes for p and y
	reg <- Export.VAR(merged)

	# individual income processes by region
	ind <- Export.IncomeProcess(merged)
	
	# stayer's copula
	# cops <- Sipp.wage_residual_copulas()

	# mover's copula
	# mcoppars <- coef(cops$movers)
	# scoppars <- t(sapply(cops$stayers,coef))

	# pop proportion in each state
	prop = merged[,list(N = length(upid)),by=Division]
	prop[,proportion:= N / .SD[,sum(N)]]

	# add rent 2 price ratio
	rents = merged[sub & own==FALSE,list(rent=mean(mortg.rent)),by=Division]
	values = merged[sub & own==TRUE,list(value=mean(hvalue)),by=Division]
	setkey(rents,Division)
	setkey(values,Division)
	prop[,r2p := values[rents][,rent / value] ]
	prop=as.data.frame(prop)
	stopifnot(sum(prop$proportion)==1)

	# distance matrix
	data(Division_distMat,package="EconData")
	dist=data.frame(Division_distMat)

	# data moments
	moms = Sipp.moments(merged,des)
	m <- as.data.frame(moms)

	# transition matrices for kids by age
	merged[,kids2 := c(kids[-1],NA),by=upid]	# next period kids
	kids_trans=merged[,xtabs(~kids+kids2+age)]
	for (i in 1:dim(kids_trans)[3]){
		kids_trans[,,i] = kids_trans[,,i] / rowSums(kids_trans[,,i])
	}

	kids_trans = data.frame(kids_trans)
	kids_trans$kids = as.integer(kids_trans$kids) -1L
	kids_trans$kids2 = as.integer(kids_trans$kids2) -1L 
	kids_trans$age = as.numeric(as.character(kids_trans$age))

	# initial wealth distribution
	wealth0 = merged[age==20 & wealth>0 & wealth < 100,wealth]
	fit.b = MASS:::fitdistr(wealth0,"lognormal")

	# a dataframe for scalar parameters
	par_df = data.frame(param = names(fit.b$estimate), value=fit.b$estimate)





	rm(merged,des)
	gc()

	# write to disk
    # save(mcoppars,file=file.path(path,"mcopula.rda"))	
	save(par_df,file=file.path(path,"par_df.rda"))
	save(dist,file=file.path(path,"distance.rda"))
	save(m,file=file.path(path,"moments.rda"))
	save(kids_trans,file=file.path(path,"kidstrans.rda"))
	save(prop,file=file.path(path,"prop.rda"))

	rcoefs <- reg$coefs
	sig    <- reg$sigmas
	z      <- ind$ztab
	rcoefs <- rcoefs[order(rcoefs$Division), ]
	sig    <- sig   [order(sig   $Division), ]
	z      <- z     [order(z     $Division), ]
	save(rcoefs,file=file.path(path,"region-coefs.rda"))
	save(sig,file=file.path(path,"region-sig.rda"))
	save(z,file=file.path(path,"ztable.rda"))
}


# TODO exporting for region level processes
# ytable: div, ylow, yhigh, beta0, betay, betap
# ptable: div, plow, phigh, beta0, betay, betap
# sigmas: array(J,2,2)



getCPI <- function(freq="yearly",base="2011"){

	base = as.character(base)

	data(CPIAUCSL,package="EconData",envir=environment())
	cpi <- CPIAUCSL   
	if (freq=="yearly"){
		cpi <- to.yearly(cpi)
		cpi <- cpi$cpi.Open
		names(cpi) <- "cpi"
		coredata(cpi) <- coredata(cpi)/as.numeric(cpi[base])	# base year 2012
		str <- paste0("cpi <- data.table(year=year(index(cpi)),cpi",base,"=as.numeric(cpi),key=\"year\")")
		eval(parse(text=str))
		return(cpi)

	} else {
		cpi <- to.quarterly(cpi)
		cpi <- cpi$cpi.Open
		names(cpi) <- "cpi"
		coredata(cpi) <- coredata(cpi)/as.numeric(cpi[as.yearqtr(base)])	
		str <- paste0("cpi <- data.table(qtr=as.yearqtr(index(cpi)),cpi",base,"=as.numeric(cpi),key=\"qtr\")")
		eval(parse(text=str))
		return(cpi)
	}
}

combine_sipp_psid <- function(){

	data(Sipp_age,envir=environment())

	# get 1995 sipp median income (in 2012 dollars)
	sipp =  merged[,list(y = .SD[HHincome>0,Hmisc::wtd.quantile(HHincome,HHweight,probs=0.5,na.rm=T)],p=.SD[hvalue>0,Hmisc::wtd.quantile(hvalue,HHweight,probs=0.5,na.rm=T)]),by=list(year,Division)]
	sipp95 = sipp[year==1995,list(Division,y,p)]
	setkey(sipp95,Division)

	#get psid data
	psid <- CleanPSID()
	pp <- psid$pp[,list(year,Division,y,p)]
	setkey(pp,Division,year)

	# sipp and psid don't agree on 1995 median estimtes. adjust by force.
	diffs = sipp95[,list(Division,dy=y-pp[year==1995,y],dp=p-pp[year==1995,p])]
	pp[,newy := 0]
	pp[,newp := 0]
	for (d in pp[,unique(Division)]) {
		for (yr in 1968:1997) {
			pp[Division==d & year==yr,newy := y + diffs[Division==d,dy]]
			pp[Division==d & year==yr,newp := p + diffs[Division==d,dp]]
		}
	}
	pp[sipp95]

	pp <- rbind(sipp,pp[year<1995,list(year,Division,y=newy,p=newp)])

	return(pp)
}

combine_sipp_y_census_regions <- function(){

	data(Sipp_age,envir=environment())

	# get 1995 sipp median income (in 2012 dollars)
	pp =  merged[HHincome>0,list(y = Hmisc::wtd.quantile(HHincome,HHweight,probs=0.5,na.rm=T)),by=list(year,Division)]
	sipp95 = pp[year==1995,list(Division,y)]
	setkey(sipp95,Division)

	# get census region level data and scale back all divisions accordingly
	# there is no census division level data available!
	# TODO
	# use sipp for as long as possible
	# SIPP itself goes back to only 1984

	# get census region median income
	data(US_medinc_reg,package="EconData",envir=environment())
	setkey(reg_current,year)

	# adjust by cpi
	cpi <- getCPI("yearly",base=2011)
	reg_current <- cpi[reg_current]
	reg_current[,y:= medinc / (1000 * cpi2011)]
	reg_current[,index95 := y / .SD[year==1995,y],by=region]
	regs <- reg_current[year<1995,list(year,region,index95)]

	# get mapping division -> regions
	data(US_states,package="EconData",envir=environment())
	US_states = US_states[,list(Division=abbreviate(Division,minlength=3),Region)]
	US_states = US_states[,list(Region=Region[1]),by=Division]
	setkey(US_states,Division)

	sipp95 <- US_states[sipp95]
	setkey(sipp95,Division)
	sipp95[,year := 1995]

	sipp2 = copy(sipp95)


	for (yr in 1975:1994){
		for (d in 1:nrow(sipp2)){
			sipp95 <- rbind(sipp95,sipp2[d,list(Division,Region,y = y*regs[year==yr&region==Region,index95],year=yr)],use.names=TRUE)
		}
	}
	# income now goes until 1975

	# add to that consumer price index before 1975
	sipp75 = copy(sipp95[year==1975,list(Division,year,y)])
	setkey(sipp75,Division)
	sipp2 = copy(sipp75)
	cpi = getCPI("yearly",base="1975")
	for (yr in 1967:1974){
		sipp75 <- rbind(sipp75,sipp2[,list(Division,year=yr,y=y*cpi[year==yr,cpi1975])])
	}



	pp <- rbind(pp,sipp95[year<1995,list(year,Division,y)],sipp75[year<1975,list(year,Division,y)],use.names=TRUE)
	return(pp)
}


# takes first useable SIPP measure of 
# median house price and extends it backwards
# using fhfa division level indices (until 1975)
# and then the housing inflation index until 1966
CombineHousePrices <- function(){
	data(Sipp_age,envir=environment())

	pp = merged[hvalue>0,list(p = Hmisc::wtd.quantile(hvalue,HHweight,probs=0.5,na.rm=T)),by=list(year,Division)]

	# work out house prices before 1995
	# ---------------------------------

	sipp95 = pp[year==1995,list(Division,p)]
	setkey(sipp95,Division)

	data(FHFA_Div,package="EconData",envir=environment())
	fhfa <- FHFA_Div$yr
	setnames(fhfa,"yr","year")

	fhfa[,Division := as.character(Division)]
	fhfa[Division=="DV_ENC",Division := "ENC"]
	fhfa[Division=="DV_ESC",Division := "ESC"]
	fhfa[Division=="DV_MA",Division := "MdA"]
	fhfa[Division=="DV_MT",Division := "Mnt"]
	fhfa[Division=="DV_NE",Division := "NwE"]
	fhfa[Division=="DV_PAC",Division := "Pcf"]
	fhfa[Division=="DV_PAC",Division := "Pcf"]
	fhfa[Division=="DV_SA",Division := "StA"]
	fhfa[Division=="DV_WNC",Division := "WNC"]
	fhfa[Division=="DV_WSC",Division := "WSC"]

	# fhfa goes back until 1975; from there on use housing CPI to scale back.
	cpi = getCPI("yearly","2011")
	fhfa = cpi[fhfa]
	fhfa[,index2011 := index_nsa / cpi2011]	# index in real 2011 terms
	fhfa[,index1995 := index2011 / .SD[year==1995,index2011],by=Division]  # real terms relative to year 1995

	fhfa[,c("cpi2011","index_nsa","index2011"):=NULL]

	# extend fhfa index back to 1967
	# using mean house price estimate from psid
	p = CleanPSID()
	x = p$psid[hvalue>0 & faminc>0,list(p=weighted.mean(rhvalue,famweight),y=weighted.mean(rincome,famweight)),by=list(year)]
	psid75 = x[,list(year,y75 = y/.SD[year==1975,y],p75=p/.SD[year==1975,p])]
	psid75 = psid75[year<1975]
	for (yr in 1968:1974){
		fhfa <- rbind(fhfa,fhfa[year==1975,list(year=yr,Division,index1995=index1995*psid75[year==yr,p75])])
	}

	setkey(fhfa,Division,year)


	data(CPIHOSSL,package="EconData",envir=environment())
	cpi <- to.yearly(CPIHOSSL['1966/1975'])
	cpi <- cpi[,1]
	names(cpi) <- "cpi"
	coredata(cpi) <- coredata(cpi)/as.numeric(cpi['1975'])	# base year 1975
	# cpi <- cpi$cpi.Open
	names(cpi) <- "cpi"
	cpi <- data.table(year=year(index(cpi)),cpi75=as.numeric(cpi),key="year")

	fhfa75 = fhfa[year==1975]

	fhfa2 = copy(fhfa75)

	for (yr in 1967:1974){
		fhfa75 <- rbind(fhfa75,fhfa2[,list(Division,year=yr,index_nsa=index_nsa*cpi[year==yr,cpi75])])
	}
	setkey(fhfa75,Division,year)
	fhfa<-rbind(fhfa,fhfa75[year!=1975])
	fhfa[,index1995 := index_nsa / .SD[year==1995,index_nsa],by=Division]
	fhfa <- fhfa[year<1995]
	setkey(fhfa,Division,year)

	# extend fhfa index back to 1967
	# using mean house price estimate from psid
	p = CleanPSID()
	x = p$psid[hvalue>0 & faminc>0,list(p=weighted.mean(rhvalue,famweight),y=weighted.mean(rincome,famweight)),by=list(year)]
	psid75 = x[,list(y75 = y/.SD[year==1975,y],p75=p/.SD[year==1975,p])]

	# # will use to grow backwards first SIPP datapoint in 1996
	# # check this
	# ggplot(fhfa,aes(x=year,y=index1995)) + geom_line() + facet_wrap(~Division)


	s=sipp95[fhfa]
	s=s[complete.cases(s)]

	ggplot(s,aes(year,p*index1995)) + geom_line() + facet_wrap(~Division)


	py = rbind(pp,s[,list(year,Division,p=p*index1995)],use.names=TRUE)

	return(py)
} 



# want a model p_pacific = f(P,Y)




Export.VAR <- function(plotpath="~/Dropbox/mobility/output/data/sipp"){
	# py = merged[,list(p = Hmisc::wtd.quantile(hvalue,HHweight,probs=0.5,na.rm=T),y = Hmisc::wtd.quantile(HHincome,HHweight,probs=0.5,na.rm=T)),by=list(year,Division)]
	py = combine_sipp_psid()

	# aggregates as means over regions
	agg <- py[,list(Y=mean(y),P=mean(p)),by=year][order(year)]
	setkey(agg,year)

	setkey(py,year,Division)
	agg[,LY := agg[list(year-1)][["Y"]]]
	agg[,LP := agg[list(year-1)][["P"]]]

	ageqp = P ~ LP + LY
	ageqy = Y ~ LP + LY

	aggmod = systemfit:::systemfit(list(P=ageqp,Y=ageqy),data=agg)

	# export coefficients as table
	aggcoefs <- as.data.frame(t(sapply(aggmod,coef)))
	aggcoefs$Division = rownames(aggcoefs)
	aggcoefs <- aggcoefs[order(aggcoefs$Division), ]
	setkey(py,Division)
	coefs <- cbind(coefs,py[,list(mean_y = mean(y),lb_y=min(y),ub_y=max(y),mean_p=mean(p),lb_p=min(p),ub_p=max(p)),by=Division])
	coefs <- coefs[, !names(coefs) %in% "Division_1"]

	# merge aggregate into regional data
	pyagg = py[agg]

	# plot region and agg overlaid
	my = melt(pyagg[,list(year,Division,y,Y)],c("year","Division"))
	pl = list()
	pl$y <- ggplot(my,aes(x=year,y=value,color=variable)) + geom_line() + facet_wrap(~Division) + ggtitle("Regional and Aggregate Income")
	mp = melt(pyagg[,list(year,Division,p,P)],c("year","Division"))
	pl = list()
	pl$p <- ggplot(mp,aes(x=year,y=value,color=variable)) + geom_line() + facet_wrap(~Division) + ggtitle("Regional and Aggregate house price")

	# estimate regional models: what is relationship y ~ P + Y
	ep <- p ~ P + Y
	ey <- y ~ P + Y
	divs = py[,unique(Division)]
	mods <- lapply(divs,function(x) systemfit:::systemfit(list(y=ey,p=ep),data=pyagg[Division==x]))
	names(mods) = divs

	# pritn models
	texreg(mods[1:4],custom.model.names=paste(rep(divs[1:4],each=2),rep(c("Y","P"),4)),file=file.path(plotpath,"VAR1.tex"),table=FALSE,booktabs=TRUE,dcolumn=TRUE,use.packages=FALSE)
	texreg(mods[5:9],custom.model.names=paste(rep(divs[5:9],each=2),rep(c("Y","P"),5)),file=file.path(plotpath,"VAR2.tex"),table=FALSE,booktabs=TRUE,dcolumn=TRUE,use.packages=FALSE)

	# predict 
	pyagg[,yhat := 0]
	pyagg[,phat := 0]


	for (d in divs){
		pyagg[Division==d,c("yhat","phat") := predict(mods[[d]])]
		# dy[Division==d,phat := predict(pmods[[d]])]
	}

	# visualize fit


	mdy = melt(pyagg[,list(year,Division,y,Y,yhat)],c("year","Division"))

	pl$pred_y <- ggplot(mdy,aes(x=year,y=value,linetype=variable,color=variable)) + geom_line() + facet_wrap(~Division) + theme_bw() + ggtitle("VAR fit to regional income data") + scale_y_continuous(name="1000s of Dollars") + scale_color_manual(values=c("y"="red","Y"="blue","yhat"="red")) + scale_linetype_manual(values=c("solid","solid","dotdash"))

	mdp = melt(pyagg[,list(year,Division,p,P,phat)],c("year","Division"))

	pl$pred_p <- ggplot(mdp,aes(x=year,y=value,linetype=variable,color=variable)) + geom_line() + facet_wrap(~Division) + theme_bw() + ggtitle("VAR fit to regional price data") + scale_y_continuous(name="1000s of Dollars") + scale_color_manual(values=c("p"="red","P"="blue","phat"="red")) + scale_linetype_manual(values=c("solid","solid","dotdash"))

	# export coefficients as table
	coefs <- as.data.frame(t(sapply(mods,coef)))
	coefs$Division = rownames(coefs)
	coefs <- coefs[order(coefs$Division), ]
	setkey(py,Division)
	coefs <- cbind(coefs,py[,list(mean_y = mean(y),lb_y=min(y),ub_y=max(y),mean_p=mean(p),lb_p=min(p),ub_p=max(p)),by=Division])
	coefs <- coefs[, !names(coefs) %in% "Division_1"]

	# TODO export actual data series.



	n <- names(coefs)
	n <- gsub("\\(|\\)","",n)
	names(coefs) <- n

	# covariance matrices as an array
	sigmas <- as.data.frame(t(sapply(mods,function(x) as.numeric(x$residCov))))
	sigmas$Division = rownames(sigmas)
	names(sigmas)[1:4] <- c("var_y","cov_yp","cov_py","var_p")

	return(list(mods=mods,coefs=coefs,sigmas=sigmas,simp=simp,predp=pred))

}







# for individual spec procs
# div, beta0,beta_medinc,beta_age,beta_age2,beta_age3,sigma



#' Produce estimates of individual income process
#' by census division. 
#'
#' exports coefficients to julia
Export.IncomeProcess <- function(dat){

	
	cd <- dat[HHincome>0,list(upid,timeid,CensusMedinc,MyMedinc,HHincome,age,Division)]
	cd <- cd[complete.cases(cd)]

	# get models of individual income
	setkey(cd,upid,Division)
	divs = cd[,unique(Division)]
	lmods = lapply(divs,function(x) lm(log(HHincome) ~ log(CensusMedinc) + age + I(age^2) + I(age^3),cd[Division==x]))
	names(lmods) = divs

	# add residuals indiv data
	cd [ ,resid := 0]
	for (d in divs){
		cd[Division==d,resid := resid(lmods[[d]])]
	}

	# get lagged resids
	setkey(cd,upid,timeid)
	cd[, Lresid := cd[list(upid,timeid-1)][["resid"]] ]

	# get autocorrelation coefficient of residuals
	rhos = lapply(divs,function(x) lm(resid ~ -1 + Lresid,cd[Division==x]))
	names(rhos) = divs

	# why are those rhos so low?????
	# use 0.97 as in french 2005 for now.

	# add the 0.2 and 0.95 percentiles of income in each region 
	# to scale the shocks
	bounds = ddply(subset(dat,HHincome>0),"Division", function(x) quantile(x$HHincome,probs=c(0.2,0.95),na.rm=T)) 
	names(bounds)[-1] <- c("q20","q95")


	# make a table with those
	ztab <- as.data.frame(t(sapply(lmods,coef)))
	ztab$Division <- rownames(ztab)
	ztab$sigma <- unlist(lapply(lmods,function(x) summary(x)$sigma))
	ztab <- merge(ztab,bounds,by="Division")
	rtab <- as.data.frame(unlist(lapply(rhos,coef)))
	rtab$Division <- gsub(".Lresid","",rownames(rtab))
	names(rtab)[1] <- "Lresid"
	rtab$sigma_resid <- unlist(lapply(rhos,function(x) summary(x)$sigma))
	rtab <- rtab[order(rtab$Division),]
	rownames(rtab) <- NULL

	ztab <- merge(ztab,rtab,by="Division")

	# fix names
	nz <- names(ztab)
	nz <- gsub("\\(|\\)|I\\(|\\^","",nz)
	names(ztab) <- nz
	nr <- names(rtab)
	nr <- gsub("\\(|\\)|I\\(|\\^","",nr)
	names(rtab) <- nr


	return(list(incmods=lmods,rhomods=rhos,ztab=ztab))
}



#' Income Rank by Region and Age
#'
Rank.HHincome <- function(dat,geo="Division",n=3,plot=FALSE,path="~/Dropbox/mobility/output/model/data_repo/in_data_jl"){

	if (geo=="Division") {
		dat[,state := Division]
	} else if (geo=="Div2") {
		dat[,state := Div2]
	} else if (geo=="state"){

	}

	# rda file names
	zname <- paste0("zsupp_n",n,".rda")
	rhoname <- paste0("rho_n",n,".rda")
	rhoMovename <- paste0("rhoMove_n",n,".rda")
	transname <- paste0("trans_n",n,".rda")
	transMovename <- paste0("transMove_n",n,".rda")

	# censor income data at 5 and 95 percentile
	kv <- c("HHincome","upid","age","year","state","HHweight","D2D","CensusMedinc")
	dat <- dat[HHincome>0,kv,with=FALSE]
	q <- dat[,quantile(HHincome,probs=c(0.05,0.95))]	# trim data
	dat <- dat[HHincome>q[1] & HHincome < q[2]]

	# take out macro time-region effect
	# dat[,state.med := Hmisc::wtd.quantile(HHincome,weights=HHweight,probs=0.5,na.rm=T),by=list(year,state)]

	# TODO take census income estimates

	# compute measure of deviation
	# dat[,ytilde := HHincome - state.med]
	# dat[,ytilde := 100*(HHincome - state.med)/state.med, by=list(year,state)]
	dat[,ytilde := 100*HHincome/CensusMedinc, by=list(year,state)]

	# throw inds with fewer than 2 ages
	dat <- dat[age>20 & age<65]
	dat[,ncell := .N ,by=list(age,state)]
	dat <- dat[ncell>=n]

	dat[,dage := diff(age), by=list(upid)]
	dat <- dat[dage>0]

	# bin delimiters
	# prange: midpoints of intervals
	lb=0;ub=1;pad=0.1;
    prange = seq( lb+(ub-lb)*pad,  lb+(ub-lb)*(1-pad) ,l=n)
    # prange2 cutoffs of intervals
    prange2 = rep(0,n+1)
    for (i in 1:(length(prange)-1)){
    	prange2[i+1] = mean(c(prange[i],prange[i+1]))
    }
    prange2[n+1] = 1


	# compute ranks of ytilde by age and state
	# this constructs BINs of incomes ie 
	dat[,yrank := cut(ytilde,breaks=Hmisc::wtd.quantile(ytilde,weights=HHweight,probs=prange2),labels=FALSE,include.lowest=TRUE,right=TRUE),by=list(age,state)]
	# dat[D,yrank := cut(ytilde,breaks=Hmisc::wtd.quantile(ytilde,weights=HHweight,probs=prange),labels=FALSE,include.lowest=TRUE,right=TRUE),by=list(age,state)]


	# cd[,resid:= residuals(lm(HHincome ~ age + I(age^2)+ LHHincome))]


	# # estimate this linear model:
	# cd = dat[complete.cases(dat)]
	# setkey(cd,upid,timeid)
	# lmod = cd[,lm(log(HHincome) ~ Division:log(CensusMedinc) + age + age2)]
	# # get residuals of this regression
	# cd[,resid := resid(lm(log(HHincome) ~ Division:log(CensusMedinc) + age + age2))]

	# # investigate residuals for unit root
	# cd[,Lresid := cd[list(upid,timeid-1)][["resid"]] ]
	# lres = cd[,lm(resid ~ Lresid)]
	# cd[,adf.test(resid)]	# reject unit root!

	# # solution 1) use a copula with param coef(lres)["Lresid"]

	# # normalize to [0,1]
	# cd[,nresid := linear.map(resid,1)]
	# # trim hi and lo
	# cd[nresid==0]
	# cd[nresid==0,nresid := 0.0001]
	# cd[nresid==1]
	# cd[nresid==1,nresid:=0.99999]
	# cd[,Lnresid := cd[list(upid,timeid-1)][["nresid"]] ]
	# ccd = cd[,list(nresid,Lnresid)]
	# # fit a beta distribution
	# fit.b = fitdistr(cd[,nresid],"beta",start=list(shape1=2,shape2=2))
	# x.b=rbeta(1e5,fit.b$estimate[[1]],fit.b$estimate[[2]])
	# plot(density(cd[,nresid]))
	# lines(density(x.b),col="red")
	# # ggplot(cd,aes(x=resid)) + geom_density() + facet_wrap(~Division)
	# G = getNormCop(Qn=seq(0.1,0.9,le=4),n=4,cond=TRUE,rho=coef(lres)["Lresid"])

	# # solution 2) use rouwenhorst discretization by division
	# # =======================================================

	# lmod = cd[,lm(log(HHincome) ~ Division:log(CensusMedinc) + Division:age + Division:I(age^2) + Division:I(age^3))]
	# newd = data.frame(expand.grid(age = 20:50,Division=factor(cd[,unique(Division)])))
	# x = merge(newd,cd[,median(CensusMedinc),by=Division],by="Division")
	# names(x)[3] = "CensusMedinc"
	# x = cbind(x,predict(lmod,x))
	# names(x)[3] = "Medinc"
	# x = merge(x,cd[,median(CensusMedinc)*0.95,by=Division],by="Division")
	# names(x)[5] = "CensusMedinc"
	# x = cbind(x,predict(lmod,x))
	# names(x)[5] = "MedincLow"
	# x = merge(x,cd[,median(CensusMedinc)*1.05,by=Division],by="Division")
	# names(x)[ncol(x)] = "CensusMedinc"
	# x = cbind(x,predict(lmod,x))
	# names(x)[c(4,6,8)] = c("ymed","ylow","yhigh")

	# ggplot(x,aes(x=age,y=ymed,color=Division)) + geom_line() + geom_ribbon(aes(ymin=ylow,ymax=yhigh),alpha=0.2)





	# x$regime = "baseline"
	# x = cbind(x,predict(lmod,newd))

	# lmod = lapply(cd[,unique(Division)],function(x) lm(log(HHincome) ~ log(CensusMedinc) + age + age2,cd[Division==x]))
	# names(lmod) = cd[,unique(Division)]

	# rhos = lapply(lmod, function(x) dynlm(resid(x) ~ L(resid(x))))
	# names(rhos) = cd[,unique(Division)]

	# rou = lapply(rhos, function(x) rouwenhorst(mu=coef(x)["(Intercept)"],rho=coef(x)["Lresid"],n=n,sigma=summary(x)$sigma))





	# compute lagged rank
	setkey(dat,upid,age)
	dat[,timeid := 1:.N ,by=upid]

	setkey(dat,upid,timeid)
	dat[,Lyrank := dat[list(upid,timeid-1)][["yrank"]] ]
	dat[,yrank_plus := dat[list(upid,timeid+1)][["yrank"]] ]
	dat[,LHHincome := dat[list(upid,timeid-1)][["HHincome"]] ]


	# get the empricial transition matrices
	setkey(dat,state)
	sts = dat[,unique(state)]
	longtrans = dat[,prop.table(table(yrank,yrank_plus),margin=1),by=state]
	longtrans = cbind(longtrans,expand.grid(from=1:n,to=1:n))
	names(longtrans) <- c("Division","prob","from","to")

	# for movers: we're interested in y(t,k) vs y(t-1,j)
	# i.e need to condition period after D2D==TRUE
	mvid = dat[D2D==TRUE,list(upid=unique(upid))]
	setkey(mvid,upid)
	setkey(dat,upid,timeid)
	movers <- dat[mvid]
	setkey(movers,upid,timeid)
	longtransMove <- movers[movers[D2D==TRUE,list(upid,timeid=timeid+1)],prop.table(table(Lyrank,yrank),margin=1),by=state]
	longtransMove = dat[D2D==TRUE,prop.table(table(yrank,yrank_plus),margin=1),by=state]
	longtransMove = cbind(longtransMove,expand.grid(from=1:n,to=1:n))
	names(longtransMove) <- c("Division","prob","from","to")

	trmats <- lapply(sts, function(x) dat[state==x,prop.table(table(yrank,yrank_plus),margin=1)])
	setkey(movers,upid,timeid,state)
	trmatsMove <- lapply(sts, function(x) movers[movers[D2D==TRUE,list(upid,timeid=timeid+1,x)]][,prop.table(table(Lyrank,yrank),margin=1)])
	names(trmats) <- sts
	names(trmatsMove) <- sts

	stopifnot( all( unlist(lapply(trmats,function(x) rowSums(x) == rep(1,n)))))
	stopifnot( all( unlist(lapply(trmatsMove,function(x) rowSums(x) == rep(1,n)))))

	# get breaks for model:
	# center of bins of percentage deviations from state median income
	z <- ddply(dat,.(state,age), function(x) Hmisc::wtd.quantile(x$ytilde,weights=x$HHweight,probs=prange))
	# bins of actual income
	zy <- ddply(dat,.(state,age), function(x) Hmisc::wtd.quantile(x$HHincome,weights=x$HHweight,probs=prange))

	# compute average over individiual correlation coefficients by region
	rho <- dat[,list(rho=cor(yrank,Lyrank,use="pairwise")),by=state]

	# same for movers
	rhoMove <- dat[D2D==TRUE,list(rho=cor(yrank,yrank_plus,use="pairwise")),by=state]


	longtrans <- as.data.frame(longtrans)
	longtransMove <- as.data.frame(longtransMove)

	# write to dataframe
	save(z,file=file.path(path,zname))
	save(rho,file=file.path(path,rhoname))
	save(rhoMove,file=file.path(path,rhoMovename))
	save(longtrans,file=file.path(path,transname))
	save(longtransMove,file=file.path(path,transMovename))




	# make some plots
	m = melt(z,id.vars=c("state","age"))
	my= melt(zy,id.vars=c("state","age"))
	names(m) <- c("state","age","quantile","value")
	names(my) <- c("state","age","quantile","value")
	pl = ggplot(m,aes(x=age,y=value,color=quantile)) + geom_line(size=1) + facet_wrap(~state) + theme_bw() + scale_y_continuous(name="percent of region median income") + ggtitle("Income Quantiles by Age")
	ply = ggplot(my,aes(x=age,y=value,color=quantile)) + geom_line(size=1) + facet_wrap(~state) + theme_bw() + scale_y_continuous(name="income in 1000 $")

	plmacro <- ggplot(dat[,mean(CensusMedinc),by=list(year,state)],aes(x=year,y=V1,color=state)) + geom_line() + ggtitle("macro effects")

	pltrans <- ggplot(longtrans,aes(x=from,y=to,z=prob,fill=prob)) + geom_tile() + facet_wrap(~Division) + ggtitle("Stayer' Income Rank Transition Matrix")
	pltransMove <- ggplot(longtransMove,aes(x=from,y=to,z=prob,fill=prob)) + geom_tile() + facet_wrap(~Division) + ggtitle("Movers' Income Rank Transition Matrix")

	if (plot){

		dr <- paste0("~/Dropbox/mobility/output/data/sipp/zpoints_",n)
		ggsave(plot=pl,filename=file.path(dr,"z-quantiles-pct.pdf"),width=23,height=15,units="cm")
		ggsave(plot=ply,filename=file.path(dr,"z-quantiles.pdf"),width=23,height=15,units="cm")
		ggsave(plot=plmacro,filename=file.path(dr,"z-macro.pdf"),width=23,height=15,units="cm")
		ggsave(plot=pltrans,filename=file.path(dr,"z-trans.pdf"),width=23,height=15,units="cm")
		ggsave(plot=pltransMove,filename=file.path(dr,"z-trans-move.pdf"),width=23,height=15,units="cm")

	}

	r <- list(d=dat,z=z,rho=rho,rhoMove=rhoMove,pl=pl,ply=ply,pltrans=pltrans,pltransMove=pltransMove,trmats=trmats,trmatsMove=trmatsMove,longtrans=longtrans,longtransMove=longtransMove)
	return(r)
}



# plot model slices
plotModelSlices <- function(who="mac",path="~/Dropbox/mobility/output/model/data_repo",outpath="~/Dropbox/mobility/output/model/data_repo"){
	# load data
	if (who=="iridis"){
		cmd <- paste0("scp -r iridis:~/data_repo/mig/out_data_jl/ ",path)
		system(cmd)
	} else if (who=="sherlock"){
		cmd <- paste0("scp -r sherlock:~/data_repo/mig/out_data_jl/ ",path)
		system(cmd)
	} 

	vals = read.csv(file.path(path,"out_data_jl","migslice1.csv"))
	moms = read.csv(file.path(path,"out_data_jl","migslice2.csv"))


	p1 = ggplot(vals,aes(x=p_val,y=f_val)) + geom_line() + facet_wrap(~p_name,scales="free") + ggtitle('Value of Objective Function vs Parameters') + theme_bw() + scale_y_continuous("Mean Squared Distance") + scale_x_continuous("Parameter value")
	ggsave(plot=p1,filename=file.path(outpath,"out_graphs_jl","objfun.pdf"),width=297,height=210,units="mm")

	m = list()
	for (pp in unique(moms$p_name)){
		tstring = paste0("Moments_vs_",pp)
		m[[pp]] = ggplot(subset(moms,p_name == pp),aes(x=p_val,y=m_val)) + geom_line() + facet_wrap(~m_name,scales="free") + ggtitle(tstring) + scale_x_continuous(name=pp) + scale_y_continuous(name="value of Moment") + theme_bw()
		ggsave(plot=m[[pp]], filename=file.path(outpath,"out_graphs_jl",paste0(tstring,".pdf")),width=297,height=210,units="mm")

	}

	return(list(fvals = p1,moms = m))

}


# look at estimation output
getEstimData <- function(who="mac"){
	if (who=="iridis"){
		cmd <- paste0("scp iridis:~/git/migration/mig/src/cluster/MA.h5"," ~/git/migration/mig/src/cluster/MA.h5")
		system(cmd)
	} else if (who=="sherlock"){
		cmd <- paste0("scp sherlock:~/git/migration/mig/src/cluster/MA.h5"," ~/git/migration/mig/src/cluster/MA.h5")
		system(cmd)
	} 
	chains = h5read("/Users/florianoswald/git/migration/mig/src/cluster/MA.h5","chain")
	opts   = data.frame(h5read("/Users/florianoswald/git/migration/mig/src/cluster/MA.h5","algo/opts"))
	
	params = as.data.table(data.frame(chains[[1]]$parameters))
	for (i in 2:length(chains)) {
		params = rbind(params,as.data.table(data.frame(chains[[i]]$parameters)))
	}
	setcolorder(params,c("chain_id","iter",grep("iter|chain_id",names(params),invert=T,value=T)))

	moments = as.data.table(data.frame(chains[[1]]$moments))
	for (i in 2:length(chains)) {
		moments = rbind(moments,as.data.table(data.frame(chains[[i]]$moments)))
	}
	setcolorder(moments,c("chain_id","iter",grep("iter|chain_id",names(moments),invert=T,value=T)))

	infos = as.data.table(data.frame(chains[[1]]$infos))
	for (i in 2:length(chains)) {
		infos = rbind(infos,as.data.table(data.frame(chains[[i]]$infos)))
	}
	setcolorder(infos,c("chain_id","iter",grep("iter|chain_id",names(infos),invert=T,value=T)))

	algo = list()
	algo$opts = opts
	algo$pars = params
	algo$moms = moments
	algo$infos= infos

	save(algo,file="~/git/migration/mig/src/cluster/MA.rda")
	return(algo)
}

# time series plot for each param with overlaid mean estimate and dots indicating a jump
# histogram of each param
# 
plotEstimData <- function(d){
	pl = list()

	# parameter data
	m = melt(d$pars,id.vars=c("chain_id","iter"))
	N = as.numeric(subset(d$opts,keys=="maxiter")$vals)
	m = subset(m,iter<N)
	setkey(m,chain_id,iter)
	means = m[,list(mean=mean(value)),by=variable]

	pl$paths = ggplot(m,aes(x=iter,y=value,color=factor(chain_id))) + geom_line() + facet_wrap(~variable,scales="free_y") + geom_hline(data=means,aes(yintercept=mean))
	pl$hists = ggplot(subset(m,iter<30),aes(x=iter,y=value,color=chain_id)) + geom_line() + facet_wrap(~variable,scales="free_y")

	# chain data
	ch = melt(d$infos,id.vars=c("chain_id","iter"))
	ch = subset(ch,iter<N)
	setkey(ch,chain_id,iter)
	ch[, jumpval := NA]
	ch[exchanged_with!= 0, jumpval := .SD[,evals ]]
	pl$jumps = ggplot(subset(d$infos,iter<30),aes(iter,y=evals,color=factor(chain_id))) + geom_line() + geom_point(aes(x=iter,y=jumpval))

	return(pl)
}










