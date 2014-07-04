

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
	prop=as.data.frame(merged[,list(Division=unique(Division),proportion=.SD[,length(unique(upid)),by=Division][["V1"]] / length(unique(upid)))])

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
	kids_trans=merged[,xtabs(~kids+kids2+age)]
	for (i in 1:dims(kids_trans)[3]){
		kids_trans[,,i] = kids_trans[,,i] / rowSums(kids_trans[,,i])
	}

	kids_trans = data.frame(kids_trans)

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

	# compute lagged rank
	setkey(dat,upid,age)
	dat[,timeid := 1:.N ,by=upid]

	setkey(dat,upid,timeid)
	dat[,Lyrank := dat[list(upid,timeid-1)][["yrank"]] ]
	dat[,yrank_plus := dat[list(upid,timeid+1)][["yrank"]] ]

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

	pltrans <- ggplot(longtrans,aes(x=from,y=to,z=V1,fill=V1)) + geom_tile() + facet_wrap(~state) + ggtitle("Stayer' Income Rank Transition Matrix")
	pltransMove <- ggplot(longtransMove,aes(x=from,y=to,z=V1,fill=V1)) + geom_tile() + facet_wrap(~state) + ggtitle("Movers' Income Rank Transition Matrix")

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
