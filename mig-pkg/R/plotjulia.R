

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




# export data to julia
export.Julia <- function(){
	data(Sipp_age,envir=environment())
	data(Sipp_age_svy,envir=environment())
	out <- "~/Dropbox/mobility/output/model/data_repo/in_data_jl"

	Rlist = list()
	r = makeDivDifferences()

	# pop proportion in each state
	prop=as.data.frame(merged[,list(Division=unique(Division),proportion=.SD[,length(unique(upid)),by=Division][["V1"]] / length(unique(upid)))])

	divincome = as.data.frame(r$income$d2)
	divprice  = as.data.frame(r$price$meandiv)
	p2y       = as.data.frame(r$price$p2y)
	normalize = as.data.frame(r$normalize)

	# distance matrix
	data(Division_distMat,package="EconData")
	df=data.frame(Division_distMat)

	# data moments
	m <- as.data.frame(Sipp.moments(merged,des))

	ranks <- Rank.HHincome(merged,path=out)

	rm(merged,des)
	gc()

	# write to disk
	save(df,file=file.path(out,"distance.rda"))
	save(m,file=file.path(out,"moments.rda"))

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

	# csv file name
	zname <- paste0("zsupp_n",n,".csv")
	rhoname <- paste0("rho_n",n,".csv")

	# censor income data at 5 and 95 percentile
	kv <- c("HHincome","upid","age","timeid","year","state","HHweight")
	dat <- dat[HHincome>0,kv,with=FALSE]
	q <- dat[,quantile(HHincome,probs=c(0.05,0.95))]
	dat <- dat[HHincome>q[1] & HHincome < q[2]]

	# take out macro time-region effect
	dat[,state.med := Hmisc::wtd.quantile(HHincome,weights=HHweight,probs=0.5,na.rm=T),by=list(year,state)]

	# compute measure of deviation
	# dat[,ytilde := HHincome - state.med]
	# dat[,ytilde := 100*(HHincome - state.med)/state.med, by=list(year,state)]
	dat[,ytilde := 100*HHincome/state.med, by=list(year,state)]

	# throw inds with fewer than 2 ages
	dat <- dat[age>20 & age<65]
	dat[,ncell := .N ,by=list(age,state)]
	dat <- dat[ncell>=n]

	dat[,dage := diff(age), by=list(upid)]
	dat <- dat[dage>0]

	# throw away cells with fewer than n observations
	lb=0;ub=1;pad=0.1;
    prange = seq( lb+(ub-lb)*pad,  lb+(ub-lb)*(1-pad) ,l=n)

	# compute ranks of ytilde by age and state
	dat[,yrank := cut(ytilde,breaks=Hmisc::wtd.quantile(ytilde,weights=HHweight,probs=prange),labels=FALSE,include.lowest=TRUE,right=TRUE),by=list(age,state)]

	# compute lagged rank
	setkey(dat,upid,timeid)
	dat[,Lyrank := dat[list(upid,timeid-1)][["yrank"]] ]


	# get breaks for model:
	z <- ddply(dat,.(state,age), function(x) Hmisc::wtd.quantile(x$ytilde,weights=x$HHweight,probs=prange))
	zy <- ddply(dat,.(state,age), function(x) Hmisc::wtd.quantile(x$HHincome,weights=x$HHweight,probs=prange))

	# compute average over individiual correlation coefficients by region
	rho <- dat[,list(rho=cor(yrank,Lyrank,use="pairwise")),by=state]

	# write to csv
	write.csv(z,file=file.path(path,zname))
	write.csv(rho,file=file.path(path,rhoname))


	# make a plot
	m = melt(z,id.vars=c("state","age"))
	my= melt(zy,id.vars=c("state","age"))
	names(m) <- c("state","age","quantile","value")
	names(my) <- c("state","age","quantile","value")
	pl = ggplot(m,aes(x=age,y=value,color=quantile)) + geom_line(size=1) + facet_wrap(~state) + theme_bw() + scale_y_continuous(name="percent of region median income") + ggtitle("Income Quantiles by Age")
	ply = ggplot(my,aes(x=age,y=value,color=quantile)) + geom_line(size=1) + facet_wrap(~state) + theme_bw() + scale_y_continuous(name="income in 1000 $")

	plmacro <- ggplot(dat[,unique(state.med),by=list(year,state)],aes(x=year,y=V1,color=state)) + geom_line() + ggtitle("macro effects")

	if (plot){

		dr <- "~/Dropbox/mobility/output/data/sipp/"
		ggsave(plot=pl,filename=file.path(dr,"z-quantiles-pct.pdf"),width=23,height=15,units="cm")
		ggsave(plot=ply,filename=file.path(dr,"z-quantiles.pdf"),width=23,height=15,units="cm")
		ggsave(plot=plmacro,filename=file.path(dr,"z-macro.pdf"),width=23,height=15,units="cm")

	}

	r <- list(d=dat,z=z,rho=rho,pl=pl,ply=ply)
	return(r)
}
