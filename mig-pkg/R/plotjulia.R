

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
# 

Export.VAR <- function(merged,plotpath="~/Dropbox/mobility/output/data/sipp"){
	py = merged[,list(p = Hmisc::wtd.quantile(hvalue,HHweight,probs=0.5,na.rm=T),y = Hmisc::wtd.quantile(HHincome,HHweight,probs=0.5,na.rm=T)),by=list(year,Division)]
	m = melt(py,id.vars=c("year","Division"))

	pl = ggplot(m,aes(x=year,y=value,color = variable)) + geom_line()+geom_point() + facet_wrap(~Division) 
	# make lagged vars
	setkey(py,year,Division)
	py[,Lp := py[list(year-1,Division)][["p"]] ]
	py
	py[,Ly := py[list(year-1,Division)][["y"]] ]
	dy = py[complete.cases(py)]
	divs = py[,unique(Division)]

	# SUR
	ep <- p ~ Lp + Ly
	ey <- y ~ Lp + Ly
	mods <- lapply(divs,function(x) systemfit:::systemfit(list(y=ey,p=ep),data=dy[Division==x]))
	names(mods) = divs
	texreg(mods[1:4],custom.model.names=paste(rep(divs[1:4],each=2),rep(c("Y","P"),4)),file=file.path(plotpath,"VAR1.tex"),table=FALSE,booktabs=TRUE,dcolumn=TRUE,use.packages=FALSE)
	texreg(mods[5:9],custom.model.names=paste(rep(divs[5:9],each=2),rep(c("Y","P"),5)),file=file.path(plotpath,"VAR2.tex"),table=FALSE,booktabs=TRUE,dcolumn=TRUE,use.packages=FALSE)



	dy[,yhat := 0]
	dy[,phat := 0]

	for (d in divs){
		dy[Division==d,c("yhat","phat") := predict(mods[[d]])]
		# dy[Division==d,phat := predict(pmods[[d]])]
	}

	# visualize fit

	mdy = melt(dy,c("year","Division"))
	mdy[,type := "data"]
	mdy[variable %in% c("phat","yhat"), type := "prediction"]
	mdy[,var := "p"]
	mdy[variable %in% c("y","yhat"), var := "y"]

	pred <- ggplot(subset(mdy,variable %in% c("p","y","phat","yhat")),aes(x=year,y=value,linetype=type,color=var)) + geom_line(size=1) + facet_wrap(~Division) + theme_bw() + ggtitle("VAR fit to data") + scale_y_continuous(name="1000s of Dollars") + scale_color_manual(values=c("p"="red","y"="blue"))

	# visualize simulation

	sim0 <- copy(dy[year==1996,list(year,Division,y,p)])
	sim <- copy(dy[year==1996,list(year,Division,y,p)])

	for (yr in 1997:2030) {
		sim0[,year := yr]
		sim <- rbind(sim,sim0)
		for (d in divs){
			sig = mods[[d]]$residCov
			eps = mvtnorm:::rmvnorm(n=1,mean=c(0,0),sigma=sig)
			# sy = summary(ymods[[d]])
			# epsy = rnorm(mean=0,sd=sy$sigma,n=1)
			# sp = summary(pmods[[d]])
			# epsp = rnorm(mean=0,sd=sp$sigma,n=1)
			cy = coef(mods[[d]])[1:3]
			cp = coef(mods[[d]])[4:6]
			sim[year==yr & Division==d, y := cy %*% sim[year==yr-1 & Division==d,c(1,p,y)] + eps[1] ]
			sim[year==yr & Division==d, p := cp %*% sim[year==yr-1 & Division==d,c(1,p,y)] + eps[2]]

			if (sim[year==yr & Division==d, p<30] ){
				sim[year==yr & Division==d, p := 30]
			}
		}
	}
	msim <- melt(sim,id.vars=c("year","Division"))
	simp <- ggplot(msim,aes(x=year,y=value,color=variable)) + geom_line() + facet_wrap(~Division) + geom_line(size=1) + facet_wrap(~Division) + theme_bw() + ggtitle("VAR Simulation Path") + scale_y_continuous(name="1000s of Dollars") + scale_color_manual(values=c("p"="red","y"="blue"))

	# export coefficients as table
	coefs <- as.data.frame(t(sapply(mods,coef)))
	coefs$Division = rownames(coefs)
	coefs <- coefs[order(coefs$Division), ]
	setkey(py,Division)
	coefs <- cbind(coefs,py[,list(mean_y = mean(y),lb_y=min(y),ub_y=max(y),mean_p=mean(p),lb_p=min(p),ub_p=max(p)),by=Division])
	coefs <- coefs[, !names(coefs) %in% "Division_1"]



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



