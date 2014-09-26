

# plot julia data

plot.simReports <- function(){
	d <- data.table(read.csv("~/Dropbox/mobility/output/model/data_repo/out_data_jl/sim_year.csv"))
	setkey(d,year)
	data(Sipp_age,envir=environment())
	d0=merged[age>19&age<51,list(data_mig=weighted.mean(D2D,HHweight,na.rm=T),data_own=weighted.mean(own,HHweight,na.rm=T)),by=year]
	setkey(d0,year)
	d<- d[d0]
	setnames(d,c("x1","D2D"),c("model","data"))
	m <- melt(d,"year")

	pl <- list()
	
	# migration rates over time

	pl$mig <- ggplot(subset(m,variable %in% c("model_mig","data_mig")),aes(x=year,y=value,color=variable)) + geom_point() + geom_smooth(method="lm",size=1) + theme_bw() + scale_y_continuous("annual moving rate") + ggtitle("Cross Division mobility in SIPP and model")
	ggsave(pl$mig,file="~/Dropbox/mobility/output/model/fit/mig_year.pdf")

	# ownership rates over time
	pl$own <- ggplot(subset(m,variable %in% c("data_own","model_own")),aes(x=year,y=value,linetype=variable)) + geom_line() + theme_bw() + scale_y_continuous("Home Ownership rate") + ggtitle("Ownership rates in SIPP and model")
	ggsave(pl$own,file="~/Dropbox/mobility/output/model/fit/own_year.pdf")

	# ownership by region over time
	d_reg <- data.table(read.csv("~/Dropbox/mobility/output/model/data_repo/out_data_jl/sim_year_reg.csv"))
	d1=merged[age>19&age<51,list(data_own=weighted.mean(own,HHweight,na.rm=T)),by=list(year,Division)]
	setkey(d_reg,year,Division)
	setkey(d1,year,Division)
	d1 = d1[d_reg]
	m1 = melt(d1,c("year","Division"))
	pl$own_reg <- ggplot(m1,aes(year,y=value,linetype=variable)) + geom_line() + facet_wrap(~Division) + ggtitle("Home Ownership rates by Division") + theme_bw()
	ggsave(pl$own_reg,file="~/Dropbox/mobility/output/model/fit/own_year_reg.pdf")

}


# 1. why do owners move less? -> because of MC1.
# 2. is this model able to fit 2005-2011 ownership choices? -> no. need a time varying downpayment contraint or other features that make buying at always higher prices possible. not clear that this is the focus of this paper
# 3. what is the focus of this paper? -> model decreasing cross division migration and ownership over the lifecycle.

analyze.sim <- function(newdata=FALSE){
	fitpath <- "~/Dropbox/mobility/output/model/fit"
	if (newdata){
		s = fread("~/Dropbox/mobility/output/model/data_repo/out_data_jl/sim.csv")
		save(s,file="~/git/migration/mig-pkg/data/simdata.rda")
	} else {
		load("~/git/migration/mig-pkg/data/simdata.rda")
	}

	# throw away incomplete cohorts
	s = s[year>1997 & !(is.na(cohort))]

# 	> s[,own := as.logical(own)]
# > df = s[,list(move=mean(move,na.rm=T),p2y=mean(p2y,na.rm=T),own=mean(own,na.rm=T)),by=list(year,Division)]
# > mdf = melt(df,c("year","Division"))
# > ggplot(mdf, aes(year,y=value,color=Division)) + geom_line() + facet_grid(variable~., scales="free_y")

	setkey(s,id,age)
	s[,move := as.logical(move)]
	s[,own := as.logical(own)]
	s[,p2income := p/income]
	s[,Division_plus := s[list(id,age+1)][["Division"]]]
	s[,h_plus := s[list(id,age+1)][["h"]]]
	s[,p_plus := s[list(id,age+1)][["p"]]]
	s[,y_plus := s[list(id,age+1)][["y"]]]
	s[,z_plus := s[list(id,age+1)][["z"]]]
	s[,income_plus := s[list(id,age+1)][["income"]]]
	s[,wealth_plus := s[list(id,age+1)][["wealth"]]]
	s[,c("dwealth","da","dincome","dp","dy","dz") := list(diff(wealth),diff(a),diff(income),diff(p),diff(y),diff(z)),by=id]

	# determinants of moving in the model
	# ===================================

	mvs = s[move==TRUE,list(id=unique(id))]
	setkey(mvs,id)
	mv = s[mvs]
	setkey(mv,id,age)

	# characteristics of mover transition
	tabs <- list()
	tabs$move_hh <-	mv[move==TRUE,round(prop.table(table(h,h_plus),margin=2),2)]
	colnames(tabs$move_hh) <- c("Rent tomorrow","Owner tomorrow")
	rownames(tabs$move_hh) <- c("Rent today","Owner today")
	print(xtable(tabs$move_hh),floating=FALSE,booktabs=TRUE,dcolumn=TRUE,file=file.path(fitpath,"movers_h.tex"))
	tabs$move_a <- dcast(s[,median(da,na.rm=T),by=list(move,own)],own ~ move)
	tabs$move_a <- as.matrix(tabs$move_a[,2:3])
	colnames(tabs$move_a) <- c("Stay","Move")
	rownames(tabs$move_a) <- c("Renter","Owner")
	print(xtable(tabs$move_a),floating=FALSE,booktabs=TRUE,dcolumn=TRUE,file=file.path(fitpath,"movers_assets.tex"))

	# analyze all
	# -----------
	setkey(s,id,age)
	
	# age-weighted regression for moves
	svy <- svydesign(data=s,weights=~density,id =~1)
	svyl <- list()

	svyl$move        <- svyglm(move ~ Division + p2y,family=quasibinomial("probit"),x=TRUE,design=svy)
	svyl$move_age    <- svyglm(move ~ p2y + age+income,family=quasibinomial("probit"),x=TRUE,design=svy)
	svyl$move_age2   <- svyglm(move ~ p2y + age + income + Division,family=quasibinomial("probit"),x=TRUE,design=svy)
	svyl$move_age_OR <- svyglm(move ~ p2y*own + age+income + Division,family=quasibinomial("probit"),x=TRUE,design=svy)

	svyl$ME <- maBina(svyl$move)
	svyl$ME_age <- maBina(svyl$move_age)
	svyl$ME_age2 <- maBina(svyl$move_age2)
	svyl$ME_age_OR <- maBina(svyl$move_age_OR)

	# interact owners/renters 

	texreg(svyl[c("ME_age","ME_age2","ME_age_OR")],digits=4,table=FALSE,use.packages=FALSE,dcolumn=TRUE,booktabs=TRUE,file="~/Dropbox/mobility/output/model/fit/move_in_sim.tex",custom.model.names=c("Pr(move)","Pr(move|Division)","Pr(move|Division,Own)"),omit.coef="Division")


	# determinants of owning
	# ======================

	svyl$own   <- svyglm(h ~ p2y + Division + p2w  + income + realage,x=TRUE,design=svy)

	data(Sipp_age,envir=environment())
	setnames(merged,c("p2y","HHincome","age"),c("p2y_ind","income","realage"))
	merged[,p2y := hvalue / CensusMedinc]
	svyl$own_sipp <- merged[year>1997 & realage>19 & realage<51,lm(own ~ p2y + Division + p2w + income +realage)]

	texreg(svyl[c("own","own_sipp")],digits=3,table=FALSE,use.packages=FALSE,dcolumn=TRUE,booktabs=TRUE,file="~/Dropbox/mobility/output/model/fit/p2y_data_model.tex",custom.model.names=c("Model","Data"),omit.coef="Division")

	return(tabs)



}


mom.table <- function(){
	d <- data.table(read.csv("~/Dropbox/mobility/output/model/fit/moms.csv"))
	d[,moment := as.character(moment)]
	neword = c(10,11:15,1,2,7,8,9,46,47,48,16:27,4,5,6,3,42,28:41,43,44)
	dd = d[neword,list(moment,data=data_value,model=model_value)]
	digmat = matrix(c(rep(4,11),rep(2,15),rep(4,4),rep(2,17)),nrow=47,ncol=4)
	print(xtable(dd,digits=digmat),include.rownames=FALSE,floating=FALSE,booktabs=TRUE,use.packages=FALSE,file="~/Dropbox/mobility/output/model/fit/moms.tex")

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
	d[,down_plus := c(0,down[-30]),by=id]
	d[,down_change := down != down_plus]
	d[,spell := factor(cumsum(down_change)),by=id]
	d[,status := "Renter"]
	d[hh==1,status:="Owner"]
	d[,age:= age+19]
	d[,newid := paste0("id:",id,",cohort:",unique(year)[cohort],",",Division)]

	# continuous variables
	contdat <- melt(d[,list(newid,age,cons,price=p,income,assets=a,wealth,v,move,moveto)],id.vars=c("newid","age"))
	contdat[variable=="move" & value==0.0,value := NA]
	contdat[,value := as.numeric(value)]

	# housing <- d[,list(h_choice=as.logical(hh[1]),duration=.N),by=list(id,spell)]
	housing <- d[,list(h_status=status[1],duration=.N),by=list(newid,spell)]
	housing[,to:=as.integer(cumsum(duration)+1),by=newid]
	housing[,from:=as.integer(to-duration),by=newid]
	housing[, to := to + 19]
	housing[, from := from + 19]

	# tmp = housing[,table(id) >1 ] 
	# tmp = names(tmp[tmp])
	# housing <- housing[id %in% tmp]

	pl <- ggplot() + geom_rect(data=housing,aes(xmin=from,xmax=to,ymin=-Inf,ymax=Inf,fill=h_status),alpha=0.8) 
	pl1 <- pl + geom_line(data=subset(contdat, variable %in% c("assets","cons","income")),aes(x=age,y=value,linetype=variable)) + facet_wrap(~newid)
	# add move indicator
	pl1 <- pl1 + geom_point(data=subset(contdat, variable == "move"),aes(x=age,y=value)) + theme_bw()

	pl2 <- pl + geom_line(data=subset(contdat, variable %in% c("assets","price","income")),aes(x=age,y=value,linetype=variable)) + facet_wrap(~newid)
	# add move indicator
	pl2 <- pl2 + geom_point(data=subset(contdat, variable == "move"),aes(x=age,y=value)) + theme_bw()


	# fix legends
	gg_color_hue <- function(n) {
 	hues = seq(15, 375, length=n+1)
    hcl(h=hues, l=65, c=100)[1:n]
    }
	pl1 <- pl1 + scale_fill_manual(guide=guide_legend(title="Housing\nstatus"),values=gg_color_hue(2)) + ggtitle("Individual simulation histories") + theme_bw()
	pl2 <- pl2 + scale_fill_manual(guide=guide_legend(title="Housing\nstatus"),values=gg_color_hue(2)) + ggtitle("Individual simulation histories") + theme_bw()

	pdf(file.path(fi,"sim-inds.pdf"),width=8,height=5)
	print(pl1)
	dev.off()
	pdf(file.path(fi,"sim-inds-price.pdf"),width=8,height=5)
	print(pl2)
	dev.off()

	return(list(pl1,pl2))
}



# export data to julia
Export.Julia <- function(print.tabs=NULL,print.plots=NULL){
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
	sub = merged[,age>=20 & age <= 53]

	# compute age distribution
	hi=merged[sub,hist(age,prob=TRUE,breaks=20:53)]
	agedist = data.frame(realage=20:52,density=hi$density)

	# regional processes for p and y
	reg <- Export.VAR()

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
	setkey(prop,Division)

	# add rent 2 price ratio
	r2p = merged[age>19&age<51, list(p = .SD[hvalue>0,median(hvalue,na.rm=T)], y = .SD[HHincome>0,median(HHincome,na.rm=T)],rent=.SD[own==FALSE,median(mortg.rent,na.rm=T)]),by=list(year,Division)]
	r2p[ , r2p := rent / p]
	r2p = r2p[year>1997,list(r2p=mean(r2p)),by=Division]
	setkey(r2p,Division)
	prop <- prop[r2p]

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
	save(agedist,file=file.path(path,"agedist.rda"))
	save(dist,file=file.path(path,"distance.rda"))
	save(m,file=file.path(path,"moments.rda"))
	save(kids_trans,file=file.path(path,"kidstrans.rda"))
	save(prop,file=file.path(path,"prop.rda"))

	VAR_agg   <- as.data.frame(reg$Agg_coefs)
	VAR_reg   <- reg$Agg2Region_coefs
	z         <- ind$ztab
	PYdata    <- reg$PYdata
	pred_y    <- reg$pred_y
	pred_p    <- reg$pred_p
	sigma_agg <- reg$agg_sigma
	sigma_agg$row <- rownames(sigma_agg)
	VAR_reg   <- VAR_reg[order(VAR_reg$Division), ]
	z         <- z[order(z$Division), ]
	save(sigma_agg,file=file.path(path,"sigma_agg.rda"))
	save(VAR_agg,file=file.path(path,"VAR_agg.rda"))
	save(PYdata,file=file.path(path,"PYdata.rda"))
	save(pred_y,file=file.path(path,"pred_y.rda"))
	save(pred_p,file=file.path(path,"pred_p.rda"))
	save(VAR_reg,file=file.path(path,"VAR_reg.rda"))
	save(z,file=file.path(path,"ztable.rda"))

	return(list(par_df=par_df,dist=dist,m=m,kids_trans=kids_trans,prop=prop,VAR_agg=VAR_agg,VAR_reg=VAR_reg,z=z,aggmod=reg$Agg_mod,sigma_agg=sigma_agg,pred_y=pred_y,pred_p=pred_p))

}


Export.VAR <- function(plotpath="~/Dropbox/mobility/output/data/sipp"){
	# py = merged[,list(p = Hmisc::wtd.quantile(hvalue,HHweight,probs=0.5,na.rm=T),y = Hmisc::wtd.quantile(HHincome,HHweight,probs=0.5,na.rm=T)),by=list(year,Division)]

	# data(sipp_psid,envir=environment())
	data(BEA_fhfa,envir=environment())
	# py = combine_sipp_psid()
	setkey(py,year,Division)

	# aggregates house price is mean over regions
	# agg <- py[,list(P=mean(p,na.rm=T),Y=mean(y,na.rm=T)),by=year]
	# setkey(agg,year)

	# taking gdp instead of mean over regions:
	agg <- py[,list(P=mean(p,na.rm=T)),by=year]
	setkey(agg,year)
	gdp = getFRED_gdp()
	agg = gdp[agg]
	setnames(agg,"gdp","Y")
	agg[,LY := agg[list(year-1)][["Y"]]]
	agg[,LP := agg[list(year-1)][["P"]]]

	# aggregate income is GDP per capita


	aggmod = systemfit:::systemfit(list(Y=Y~LY+LP,P= P~LY+LP),data=agg)

	# print model
	texreg(aggmod,file=file.path(plotpath,"VAR_agg.tex"),table=FALSE,booktabs=TRUE,dcolumn=TRUE,use.packages=FALSE)

	# export coefficients as table
	aggcoefs <- as.data.frame(coef(aggmod))
	aggcoefs$param <- names(coef(aggmod))
	names(aggcoefs)[1] <- "value"
	aggcoefs$param <- gsub("\\(|\\)","",aggcoefs$param)

	# add bounds on P and Y
	aggcoefs <- rbind(aggcoefs,agg[,list(value=min(Y),param="min_Y")],agg[,list(value=max(Y),param="max_Y")],agg[,list(value=min(P),param="min_P")],agg[,list(value=max(P),param="max_P")])

	# covariance matrix 
	sigma <- data.frame(aggmod$residCov)

	# setkey(py,Division)
	# coefs <- cbind(coefs,py[,list(mean_y = mean(y),lb_y=min(y),ub_y=max(y),mean_p=mean(p),lb_p=min(p),ub_p=max(p)),by=Division])
	# coefs <- coefs[, !names(coefs) %in% "Division_1"]

	# merge aggregate into regional data
	pyagg = py[agg]

	# plot region and agg overlaid
	# ----------------------------

	my = melt(pyagg[,list(year,Division,y,Y)],c("year","Division"))
	pl = list()
	pl$y <- ggplot(my,aes(x=year,y=value,color=variable)) + geom_line() + facet_wrap(~Division) + ggtitle("Regional and Aggregate Income")
	mp = melt(pyagg[,list(year,Division,p,P)],c("year","Division"))

	pl$p <- ggplot(mp,aes(x=year,y=value,color=variable)) + geom_line() + facet_wrap(~Division) + ggtitle("Regional and Aggregate house price")

	# estimate regional models: what is relationship y ~ P + Y
	ep <- p ~ Y + P
	ey <- y ~ Y + P
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
	}

	pred_y_out = dcast(year ~ Division, value.var="yhat",data=pyagg )
	pred_p_out = dcast(year ~ Division, value.var="phat",data=pyagg )

	# visualize fit


	mdy = melt(pyagg[,list(year,Division,y,yhat)],c("year","Division"))

	pl$pred_y <- ggplot(mdy,aes(x=year,y=value,linetype=variable)) + geom_line() + facet_wrap(~Division) + theme_bw() + ggtitle("VAR fit to regional income data") + scale_y_continuous(name="1000s of Dollars") 

	mdp = melt(pyagg[,list(year,Division,p,phat)],c("year","Division"))

	pl$pred_p <- ggplot(mdp,aes(x=year,y=value,linetype=variable)) + geom_line() + facet_wrap(~Division) + theme_bw() + ggtitle("VAR fit to regional price data") + scale_y_continuous(name="1000s of Dollars") 
	# plot with aggregate as well
	# mdp = melt(pyagg[,list(year,Division,p,P,phat)],c("year","Division"))
	# pl$pred_p <- ggplot(mdp,aes(x=year,y=value,linetype=variable,color=variable)) + geom_line() + facet_wrap(~Division) + theme_bw() + ggtitle("VAR fit to regional price data") + scale_y_continuous(name="1000s of Dollars") + scale_color_manual(values=c("p"="red","P"="blue","phat"="red")) + scale_linetype_manual(values=c("solid","solid","dotdash"))

	# export coefficients as table
	coefs <- as.data.frame(t(sapply(mods,coef)))
	coefs <- coefs[order(rownames(coefs)), ]
	coefs <- cbind(coefs,py[,list(mean_y = mean(y)),by=Division])
	PYseries = as.data.frame(agg[,list(year,Y,P)])

	n <- names(coefs)
	n <- gsub("\\(|\\)","",n)
	names(coefs) <- n

	# covariance matrices as an array
	# sigmas <- as.data.frame(t(sapply(mods,function(x) as.numeric(x$residCov))))
	# sigmas$Division = rownames(sigmas)
	# names(sigmas)[1:4] <- c("var_y","cov_yp","cov_py","var_p")

	return(list(Agg_mod=aggmod,Agg2Region_mods=mods,agg_sigma=sigma,Agg_coefs=aggcoefs,Agg2Region_coefs=coefs,plots=pl,PYdata=PYseries,pred_y=pred_y_out,pred_p=pred_p_out))

}







# for individual spec procs
# div, beta0,beta_medinc,beta_age,beta_age2,beta_age3,sigma



#' Produce estimates of individual income process
#' by census division. 
#'
#' exports coefficients to julia
Export.IncomeProcess <- function(dat){

	
	# cd <- dat[HHincome>0,list(upid,timeid,CensusMedinc,MyMedinc,HHincome,age,Division)]
	cd <- dat[HHincome>0,list(upid,timeid,year,MyMedinc,HHincome,age,Division)]
	cd <- cd[complete.cases(cd)]
	setkey(cd,year,Division)

	x = get_BEA_persincome()
	setkey(x,year,Division)
	cd = x[cd]
	setnames(cd,"y","CensusMedinc")

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
	# TODO
	# this is not what you want!
	rhos = lapply(divs,function(x) lm(resid ~ -1 + Lresid,cd[Division==x]))
	names(rhos) = divs

	# why are those rhos so low?????
	# use 0.97 as in french 2005 for now.

	# add the 0.2 and 0.95 percentiles of income in each region 
	# to scale the shocks
	bounds = ddply(subset(dat,HHincome>0),"Division", function(x) quantile(x$HHincome,probs=c(0.05,0.95),na.rm=T)) 
	names(bounds)[-1] <- c("q05","q95")


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

# TODO exporting for region level processes
# ytable: div, ylow, yhigh, beta0, betay, betap
# ptable: div, plow, phigh, beta0, betay, betap
# sigmas: array(J,2,2)



getCPI <- function(freq="yearly",base="2012"){

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
		str <- paste0("cpi <- data.table(qtr=as.yearqtr(index(cpi)),cpi",gsub(" Q\\d","",base),"=as.numeric(cpi),key=\"qtr\")")
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

	py <- rbind(sipp,pp[year<1995,list(year,Division,y=newy,p=newp)])
	save(py,file="~/git/migration/mig-pkg/data/sipp_psid.rda")

	return(py)
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

combine_BEA_fhfa <-function(){
	BEA= get_BEA_persincome()
	fhfa = getFHFA_realPrices()

	setkey(BEA,year,Division)
	setkey(fhfa,year,Division)

	py <- BEA[fhfa]
	py = py[,list(year,Division,y,p)]
	py = py[complete.cases(py)]



	save(py,file="~/git/migration/mig-pkg/data/BEA_fhfa.rda")
}

get_BEA_persincome <- function(){

	data(PersonalIncome,package="EconData",envir=environment())
	data(Population,package="EconData",envir=environment())

	setkey(pers_income_current,state,year)
	setkey(population,state,year)
	population[pers_income_current]
	pers_income_current[population]
	py = pers_income_current[population]
	py[,pcy := income / population]


	# real per capita income
	cpi = getCPI(base="2012",freq="yearly")
	setkey(py,year)
	py = py[cpi]
	py[,rpcy := pcy / cpi2012]
	py
	py = py[complete.cases(py)]

	# merge divisoin
	data(US_states,package="EconData",envir=environment())
	US_states = US_states[,list(state,Division)]
	setkey(US_states,state)

	setkey(py,state)
	py = US_states[py]
	py[,Division := abbreviate(Division,minlength=3)]
	py[,wgt := population / .SD[,sum(population)],by=list(Division,year)]
	py[,state := NULL]

	py = py[,list(y = weighted.mean(rpcy,wgt)),by=list(Division,year)]
	return(py)

}

getFRED_gdp <- function(){

	gdp = quantmod:::getSymbols("A939RX0Q048SBEA",src="FRED",auto.assign=FALSE)
	gdp = xts:::to.yearly(gdp)
	gdp = gdp[,1]
	names(gdp) <- "gdp"
	gdp = data.table(year=year(index(gdp)),gdp = as.numeric(gdp)/1000,key="year")	# is in 2009 dollars

	cpi09 = getCPI(base="2009") 
	cpi12 = getCPI(base="2012") 
	cpi=cpi09[cpi12]
	cpi[,cpi0912 := cpi2009/cpi2012]
	gdp = gdp[cpi]
	gdp[,c("cpi2009","cpi2012") := NULL]
	gdp[,gdp := gdp / cpi0912]
	gdp[,cpi0912 := NULL]
	return(gdp)

}


getFHFA_realPrices <- function(){

	# regional house prices

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



	fhfa[,index2012 := index_nsa / .SD[year==2012,index_nsa],by=Division]

	setkey(fhfa,Division,year)
	
	# get 2012 mean house value in sipp
	data(Sipp_age,envir=environment())
	v2012 = merged[hvalue>0 & year==2012,list(hvalue=weighted.mean(hvalue,na.rm=T)),by=Division]


	v2012 <- rbind(v2012,data.table(Division="USA",hvalue=merged[hvalue>0 & year==2012,weighted.mean(hvalue,na.rm=T)]))
	setkey(v2012,Division)

	# get nominal house value in all years
	fhfa = v2012[fhfa]
	fhfa[,price := hvalue * index2012]

	# get real house value
	data(CPIHOSSL,package="EconData",envir=environment())
	cpi = to.yearly(CPIHOSSL)
	cpi <- cpi[,1]
	names(cpi) <- "cpi"
	coredata(cpi) = coredata(cpi) / as.numeric(cpi['2012'])
	cpi <- data.table(year=year(index(cpi)),cpi12=as.numeric(cpi),key="year")
	setkey(fhfa,year)
	fhfa = cpi[fhfa]

	# p is real house price in 2012 terms
	fhfa[,p := price ]
	fhfa[,p := price / cpi12]

	# extend to 1967 with cpi
	# -----------------------

	data(CPIHOSSL,package="EconData",envir=environment())
	cpi2 <- to.yearly(CPIHOSSL['1966/1975'])
	cpi2 <- cpi2[,1]
	names(cpi2) <- "cpi"
	coredata(cpi2) <- coredata(cpi2)/as.numeric(cpi2['1975'])	# base year 1975
	names(cpi2) <- "cpi"
	cpi2 <- data.table(year=year(index(cpi2)),cpi75=as.numeric(cpi2),key="year")

	fhfa75 = fhfa[year==1975]

	fhfa2 = copy(fhfa75)

	for (yr in 1967:1974){
		fhfa75 <- rbind(fhfa75,fhfa2[,list(year=yr,cpi12,Division,hvalue,index_nsa,index2012,price,p)])
	}
	for (yr in 1967:1974){
		fhfa75[year==yr,p:= p * cpi2[year==yr,cpi75]]
	}
	fhfa<-rbind(fhfa,fhfa75[year!=1975])
	# ggplot(fhfa,aes(year,y=p,color=Division)) + geom_line()
	return(fhfa)



}

# want a model p_pacific = f(P,Y)







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










