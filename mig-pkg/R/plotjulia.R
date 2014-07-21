

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




# export data to julia
export.Julia <- function(){
	data(Sipp_age,envir=environment())
	data(Sipp_age_svy,envir=environment())
	out <- "~/Dropbox/mobility/output/model/data_repo/in_data_jl"

	r = makeDivDifferences()
	e <- estimateDivDeviations(r)
	rhoincome <- as.data.frame(e$inc)	# AR1 coef of lagged income deviation
	rhoprice <- as.data.frame(e$price)	# AR1 coef of lagged price deviation
	save(rhoprice,file=file.path(out,"rho-price.rda"))
	save(rhoincome,file=file.path(out,"rho-income.rda"))

	# pop proportion in each state
	prop = merged[,list(N = length(upid)),by=Division]
	prop[,proportion:= N / .SD[,sum(N)]]
	prop=as.data.frame(prop)

	stopifnot(sum(prop$proportion)==1)

	divincome = as.data.frame(r$income$d2)
	divprice  = as.data.frame(r$price$meandiv)
	p2y       = as.data.frame(r$price$p2y)	# average price to income ratio
	normalize = as.data.frame(r$normalize)

	# distance matrix
	data(Division_distMat,package="EconData")
	df=data.frame(Division_distMat)

	# data moments
	m <- as.data.frame(Sipp.moments(merged,des))

	# income ranks by region
	ranks <- Rank.HHincome(merged,n=3,path=out)
	ranks <- Rank.HHincome(merged,n=4,path=out)
	ranks <- Rank.HHincome(merged,n=5,path=out)

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

	rm(merged,des)
	gc()

	# write to disk
	save(df,file=file.path(out,"distance.rda"))
	save(m,file=file.path(out,"moments.rda"))
	save(kids_trans,file=file.path(out,"kidstrans.rda"))


	save(prop,file=file.path(out,"prop.rda"))
	save(divincome,file=file.path(out,"divincome.rda"))
	save(divprice ,file=file.path(out,"divprice.rda"))
	save(p2y      ,file=file.path(out,"p2y.rda"))
	save(normalize,file=file.path(out,"normalize.rda"))
}



Export.IncomeProcess <- function(dat,nz=3,path="~/Dropbox/mobility/output/model/data_repo/in_data_jl"){

	q <- dat[,quantile(HHincome,probs=c(0.05,0.95))]	# trim data
	cd <- dat[HHincome>q[1] & HHincome < q[2],list(upid,timeid,CensusMedinc,HHincome,age,Division)]
	cd <- dat[HHincome>0,list(upid,timeid,CensusMedinc,MyMedinc,HHincome,age,Division)]
	cd <- cd[complete.cases(cd)]

	# get models of individual income
	divs = cd[,unique(Division)]
	lmod = lm(log(HHincome) ~ Division:log(CensusMedinc) + Division:age + Division:I(age^2) + Division:I(age^3),cd)

	# slightly more general?
	lmods = lapply(divs, function(x) lm(log(HHincome) ~ log(CensusMedinc) + age + I(age^2) + I(age^3),cd[Division==x]))
	names(lmods) = divs

	# add residuals indiv data
	cd[,resid := resid(lm(log(HHincome) ~ Division:log(CensusMedinc) + Division:age + Division:I(age^2) + Division:I(age^3)))]

	# get lagged resids
	setkey(cd,upid,timeid)
	cd[, Lresid := cd[list(upid,timeid-1)][["resid"]] ]

	# get autocorrelation coefficient of residuals
	rhos = lapply(divs,function(x) lm(resid ~ Lresid,cd[Division==x]))
	names(rhos) = divs

	# why are those rhos so low?????
	# use 0.97 as in french 2005 for now. shit.

	# get rouwenhorst approximation for each division
	rou = lapply(rhos, function(x) rutils:::rouwenhorst(mu=0.0,rho=0.97,n=nz,sigma=0.11))

	# rou = lapply(rhos, function(x) rutils:::rouwenhorst(mu=coef(x)["(Intercept)"],rho=coef(x)["Lresid"],n=nz,sigma=summary(x)$sigma))


	# make plot of potential profiles by division
	# ===========================================

	# predict age profiles for each division without shocks!
	newd = data.frame(expand.grid(age = 20:50,Division=factor(cd[,unique(Division)])))
	x = merge(newd,cd[,median(CensusMedinc),by=Division],by="Division")
	names(x)[3] = "CensusMedinc"
	x = cbind(x,predict(lmod,x))
	names(x)[4] = "log_y_z0"

	# matrix with shock values for each division
	mx = data.table(x)
	setnames(mx,"CensusMedinc","censmed")
	for (id in divs){
		for (iz in 1:nz){
			str <- paste0("mx[ ,CensusMedinc := censmed + rou$",id,"$zgrid[",iz,"] ]")
			eval(parse(text=str))
			str <- paste0("mx[ ,log_y_z",iz," := predict(lmod,mx) ]")
			eval(parse(text=str))

		}

	}

	mmx = melt(mx[,c(1,2,4,6:ncol(mx)),with=FALSE],id.vars=c("Division","age"))
	pl <- ggplot(mmx, aes(x=age,y=exp(value),color=variable)) + geom_line() + facet_wrap(~Division)

	# simulate 5 guys per division.
	# ============================

	# TODO


	# make an example plot of ranges where that
	# profile could be shifted if state median income goes
	# up or down 5%
	# MACRO effect of shifing ybar around
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


	# export supports
	zname <- paste0("zsupp_n",nz,".rda")
	tname <- paste0("ztrans_n",nz,".rda")
	zsupp <- as.data.frame(mx[,c("Division","age",paste0("log_y_z",1:nz)),with=FALSE])
	save(zsupp,file=file.path(path,zname))

	# export transition matrices
	ztrans <- data.frame(Division=names(rou)[1],rou[[1]]$Pmat)
	for (id in 2:length(divs)){
		ztrans = rbind(ztrans,data.frame(Division=names(rou)[id],rou[[id]]$Pmat))
	}
	save(ztrans,file=file.path(path,tname))
	return(list(supp=zsupp,trans=ztrans))

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
