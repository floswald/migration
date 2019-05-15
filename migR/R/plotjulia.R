


#' Plot Data vs Model Moments
#' 
#' @param path location where julia objective function saves moments in file \code{moms.json}
#' @details takes structural model-generated moments and compares them to data moments in a scatter plot.
#' @return produces \enumerate{
#' \item Figure 4 in main text \emph{Graphical device to show model fit}
#' \item Figure D.1 in appendix \emph{Auxiliary Models} and \emph{Wealth}
#' }
plot_moment_fit <- function(path="~/Dropbox/research/mobility/output/model/fit"){
	j=fromJSON(file=file.path(path,"moms.json"))
	d = data.frame(moment=names(j))
	d$m = 0.0
	d$mod = 0.0
	for (i in 1:nrow(d)){
		d[i,2] = j[[i]]$data
		d[i,3] = j[[i]]$model
	}

	mytheme = theme(plot.title=element_text(vjust=1.5,size=18))

	d$type = "NA"
	d0 = data.table(d)
	d = d0[(!(moment %like% "lm_") & !(moment %like% "move_distance") & !(moment %like% "neg_equity"))]
	d[m>-0.5 & m<0.3, type := "Mobility Rates"]
	d[m>0.3 & m<0.7, type := "Ownership Rates"]
	d[m>1.0 & m < 300 , type := "Wealth Moments"]

	lms = d0[moment %like% "lm_"]
	plm = ggplot(lms,aes(x=m,y=mod)) + geom_point() + scale_y_continuous(name="model",limits=c(-1.2,0.08))+scale_x_continuous(name="data",limits=c(-1.2,0.08)) + geom_abline(intercept=0,slope=1) + ggtitle("Auxiliary Models") + theme_bw() +  mytheme #+ annotate("text",x=-1.0,y=-0.02,label="                 age < 35    age > 35",size=3)  + annotate("text",x=-1.0,y=-0.03,label="_________________________",size=3)  + annotate("text",x=-1.0,y=-0.08,label="data        44%          71%",size=3)+ annotate("text",x=-1.0,y=-0.13,label="model     31%          84%",size=3)
	plm2 = ggplot(lms,aes(x=m,y=mod)) + geom_point() + scale_y_continuous(name="model",limits=c(-0.025,0.08))+scale_x_continuous(name="data",limits=c(-0.025,0.08)) + geom_abline(intercept=0,slope=1) + ggtitle("Auxiliary Models") + theme_bw() + annotate("text",x=-0.01,y=0.06,label="                 age < 35    age > 35",size=3)  + annotate("text",x=-0.01,y=0.059,label="_________________________",size=3)  + annotate("text",x=-0.01,y=0.055,label="data        44%          71%",size=3)+ annotate("text",x=-0.01,y=0.052,label="model     31%          84%",size=3)+ mytheme

	s = split(d,d$type)

	s$`Mobility Rates`$lab = s$`Mobility Rates`$moment
	# s$`Mobility Rates`[s$`Mobility Rates`$moment == "moved2plus"]$lab = "moved twice+"
	s$`Mobility Rates`[s$`Mobility Rates`$moment == "moved1"]$lab = "moved once"
	s$`Mobility Rates`[s$`Mobility Rates`$moment == "moved2plus"]$lab = "moved 2+"
	s$`Mobility Rates`[s$`Mobility Rates`$moment == "cov_own_kids"]$lab = "Cov(own,s)"
	s$`Mobility Rates`[s$`Mobility Rates`$moment == "flow_move_to_Mnt"]$lab = "move to Mountain"

	p1 = ggplot(s$`Mobility Rates`,aes(x=m,y=mod)) + geom_point() + scale_y_continuous(name="model",limits=c(-0.01,0.17))+ scale_x_continuous(name="data",limits=c(-0.01,0.17)) + theme_bw() + geom_abline(intercept=0,slope=1)  + ggtitle("Mobility and Covariances") + mytheme + geom_text(data=subset(s$`Mobility Rates` ,lab %in% c("moved once","moved 2+","Cov(own,s)","move to Mountain")),aes(x=m,y=mod,label=lab),hjust=0.5,vjust=-0.5,size=3)


	s$`Ownership Rates`$lab = s$`Ownership Rates`$moment
	# s$`Ownership Rates`[s$`Ownership Rates`$moment == "moved0"]$lab = "moved never"
	s$`Ownership Rates`[s$`Ownership Rates`$moment == "mean_own_NwE"]$lab = "E[own|NwE]"
	s$`Ownership Rates`[s$`Ownership Rates`$moment == "mean_own_WSC"]$lab = "E[own|WSC]"

	p2 = ggplot(s$`Ownership Rates`,aes(x=m,y=mod)) + geom_point() + scale_y_continuous(name="model",limits=c(0.4,0.65))+ scale_x_continuous(name="data",limits=c(0.4,0.65)) + theme_bw() + geom_abline(intercept=0,slope=1) + ggtitle("Homeownership Rates")+ mytheme + geom_text(data=subset(s$`Ownership Rates` ,lab %in% c("E[own|NwE]","E[own|WSC]")),aes(x=m,y=mod,label=lab),hjust=0.5,vjust=-0.5,size=3)


	s$`Wealth Moments`$lab = s$`Wealth Moments`$moment
	s$`Wealth Moments`[s$`Wealth Moments`$moment == "mean_wealth_NwE"]$lab = "E[wealth | NwE]"
	s$`Wealth Moments`[s$`Wealth Moments`$moment == "mean_wealth_30_40"]$lab = "E[wealth | (30,40]]"
	p3 = ggplot(s$`Wealth Moments`,aes(x=m,y=mod)) + geom_point() + scale_y_continuous(name="model",limits=c(40,235))+ scale_x_continuous(name="data",limits=c(40,235)) + geom_abline(intercept=0,slope=1)+ geom_text(data=subset(s$`Wealth Moments` ,lab %in% c("E[wealth | NwE]","E[wealth | (30,40]]")),aes(x=m,y=mod,label=lab),hjust=0.3,vjust=1.5,size=3) + ggtitle("Wealth") + mytheme + theme_bw()

	ggsave(plm,width  = 5,height = 5,file = file.path(path,"fit_auxmods.pdf"))
	ggsave(plm2,width = 5,height = 5,file = file.path(path,"fit_auxmods2.pdf"))
	ggsave(p3,width   = 5,height = 5,file = file.path(path,"fit_wealth.pdf"))
	ggsave(p2,width   = 5,height = 5,file = file.path(path,"fit_ownership.pdf"))
	ggsave(p1,width   = 5,height = 5,file = file.path(path,"fit_mobility.pdf"))


	return(TRUE)

}




# look at max/min deviation in house prices
getFHFA_max_peak2trough <- function(){
	data(FHFA_msa50,package="EconData")
	ls()
	FHFA_msa50
	FHFA_msa50$yr
	ff = FHFA_msa50$yr
	ff[,maxp := .SD[yr>2005&yr<2011,max(index_nsa)],by=Metropolitan_Area_Name]
	ff[,dmax := 100*(index_nsa - maxp) / maxp]
	ff6 = ff[yr>2006 & yr<2012]
	ff6[,list(max=Metropolitan_Area_Name[which.max(dmax)],min=Metropolitan_Area_Name[which.min(dmax)])]
	ff6[Metropolitan_Area_Name=="Detroit-Dearborn-Livonia, MI  (MSAD)"]
	ff6[,list(max=Metropolitan_Area_Name[which.max(dmax)],min=Metropolitan_Area_Name[which.min(dmax)])]
	ff6[,list(max=Metropolitan_Area_Name[which.max(dmax)],maxv=max(dmax),min=Metropolitan_Area_Name[which.min(dmax)],minv=min(dmax))]
	ff6[,list(min_idx = min(dmax)),by=Metropolitan_Area_Name]
	dmin=ff6[,list(min_idx = min(dmax)),by=Metropolitan_Area_Name]
	dmin[,range(min_idx)]
	dmin[,list(max=Metropolitan_Area_Name[which.max(min_idx)],maxv=max(min_idx),min=Metropolitan_Area_Name[which.min(min_idx)],minv=min(min_idx))]

	# by division
	data(FHFA_Div,package="EconData")
	setkey(FHFA_Div,Division,yr)
	div = FHFA_Div$yr[yr>2005 & yr<2012,list(peak_trough=100*(index_sa[.N]-index_sa[1])/index_sa[1]),by=Division][order(peak_trough)]


	return(list(msa=dmin,div=div))
}



#' Export Data to Julia Package
#' 
#' @details Takes all output data from this R package (mainly: moments)
#' and stores them on disk such that they can be used to run the structural
#' julia model in \code{migration/mig/}.
#' @param writedisk boolean TRUE whether to save to disk
#' @param noCollege boolean TRUE whether to subset data to individuals without college degree.
#' @export
Export.Julia <- function(writedisk=TRUE,noCollege=TRUE){
	data(Sipp_age,envir=environment())
	data(Sipp_age_svy,envir=environment())
	# path <- "~/Dropbox/research/mobility/output/model/data_repo/in_data_jl"
	path <- "~/git/migration/mig/in"

	# individual income processes by region
	ind <- Export.IncomeProcess(merged,writedisk,noCollege)

	if (noCollege) {
		merged <- merged[college==FALSE]
		des <- subset(des,college == FALSE)
	}

	# subset age
	sub = merged[,age>=20 & age <= 53]

	# compute age distribution
	hi=merged[sub,hist(age,prob=TRUE,breaks=20:53)]
	agedist = data.frame(realage=20:52,density=hi$density)

	# regional processes for p and y
	reg <- Export.VAR(writedisk=FALSE)   # dont' write because tex output is in one column... fixed by hand.

	
	# stayer's copula
	# commented out because takes some time
	# cops <- Sipp.wage_residual_copulas()

	# mover's copula
	# mcoppars <- coef(cops$movers)
	# scoppars <- t(sapply(cops$stayers,coef))

	# pop proportion in each state
	prop = merged[,list(N = length(upid)),by=Division]
	prop[,proportion:= N / .SD[,sum(N)]]
	setkey(prop,Division)

	# add rent 2 price ratio
	r2p = merged[age>19&age<51, list(p = .SD[hvalue>0,median(hvalue,na.rm=T)], y = .SD[HHincome>0,median(HHincome,na.rm=T)]),by=list(year,Division)]
	setkey(r2p,year,Division)
	rent = merged[age>19&age<51&own==FALSE, list(rent=median(mortg.rent,na.rm=T)),by=list(year,Division)]
	setkey(rent,year,Division)
	r2p = rent[r2p]
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

	VAR_agg   <- as.data.frame(reg$Agg_coefs)
	VAR_reg   <- reg$Agg2Region_coefs
	z         <- ind$ztab
	PYdata    <- reg$PYdata
	pred_y    <- reg$pred_y
	pred_p    <- reg$pred_p
	sigma_agg <- reg$agg_sigma
	sigma_reg <- reg$sigma_region
	sigma_agg$row <- rownames(sigma_agg)
	VAR_reg   <- VAR_reg[order(VAR_reg$Division), ]
	z         <- z[order(z$Division), ]

	if (writedisk) {
		# write to disk
	    # save(mcoppars,file=file.path(path,"mcopula.rda"))	
		save(par_df,file=file.path(path,"par_df.rda"))
		save(agedist,file=file.path(path,"agedist.rda"))
		save(dist,file=file.path(path,"distance.rda"))
		save(m,file=file.path(path,"moments.rda"))
		save(kids_trans,file=file.path(path,"kidstrans.rda"))
		save(prop,file=file.path(path,"prop.rda"))

		save(sigma_agg,file=file.path(path,"sigma_agg.rda"))
		save(sigma_reg,file=file.path(path,"sigma_reg.rda"))
		save(VAR_agg,file=file.path(path,"VAR_agg.rda"))
		save(PYdata,file=file.path(path,"PYdata.rda"))
		save(pred_y,file=file.path(path,"pred_y.rda"))
		save(pred_p,file=file.path(path,"pred_p.rda"))
		save(VAR_reg,file=file.path(path,"VAR_reg.rda"))
		save(z,file=file.path(path,"ztable.rda"))

	}

	return(list(par_df=par_df,dist=dist,m=m,kids_trans=kids_trans,prop=prop,VAR_agg=VAR_agg,VAR_reg=VAR_reg,z=z,aggmod=reg$Agg_mod,sigma_agg=sigma_agg,sigma_reg=sigma_reg,pred_y=pred_y,pred_p=pred_p))

}




#' Illustrate translation of Aggregate to Regional Shocks
#' 
#' @details This produces figure B.4 in the online appendix titled "10 percent shock to Y"
#' @param plotpath location to output figure.
#' @name Var.impulse
VAR.impulse <- function(plotpath="~/Dropbox/research/mobility/output/data/sipp"){

	data(BEA_fhfa,envir=environment())
	py = BEA_fhfa_agg$py
	setkey(py,year,Division)

	agg = BEA_fhfa_agg$agg
	setkey(agg,year)
	aggmod = systemfit:::systemfit(list(Y=Y~LY+LP,P= P~LY+LP),data=agg)

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
	pyagg = agg[py]

	# plot region and agg overlaid
	# ----------------------------

	# get full division name back
	data(US_states,package="EconData",envir=environment())
	US = US_states[,list(Div=unique(Division),Division=abbreviate(unique(Division),minlength=3))]
	US = US[complete.cases(US)]
	setkey(US,Division)
	setkey(pyagg,Division)
	pyagg = US[pyagg]


	pp=dcast(pyagg[,list(Division,year,p)],year ~ Division)
	yy=dcast(pyagg[,list(Division,year,y)],year ~ Division)
	y_cor = cor(yy[,-1])
	p_cor = cor(pp[,-1])

	price_correlation =  mean(p_cor[p_cor!=1])
	income_correlation =  mean(y_cor[y_cor!=1])


	my = melt(pyagg[,list(year,Division=Div,Regional=y,National=Y)],c("year","Division"))


	pl = list()
	
	# estimate regional models: what is relationship y ~ P + Y
	ep <- p ~ Y + P
	ey <- y ~ Y + P
	divs = py[,unique(Division)]
	mods <- lapply(divs,function(x) systemfit:::systemfit(list(y=ey,p=ep),data=pyagg[Division==x]))
	names(mods) = divs

	# predict 
	pyagg[,yhat := 0]
	pyagg[,phat := 0]

	# 10% shock to income 
	# 6% shock to price
	pyagg[,P := mean(P)]
	pyagg[,Y := mean(Y)]
	pyagg[year==2000,Y := 0.9*Y]


	for (d in divs){
		pyagg[Division==d,c("yhat","phat") := predict(mods[[d]],newdata=.SD),.SDcols=c("Y","P")]
		pyagg[Division==d,yhat := 100*(yhat-yhat[1]) / yhat[1]]
	}


	mdy = pyagg[,list(year,Division=Div,prediction=yhat)]
	
	mytheme <- theme_bw() + theme(plot.title=element_text(hjust=0.5,size=21),legend.text=element_text(size=12),legend.key.size=unit(0.6, "cm"))

	pl$pred_y <- ggplot(mdy,aes(x=year,y=prediction)) + geom_line()  + facet_wrap(~Division)+ ggtitle("10% shock to Y",subtitle="P and Y at their means otherwise.") + scale_y_continuous(name="percent deviation of regional y")  + mytheme


	ggsave(pl$pred_y,file=file.path(plotpath,"impulse_y.pdf"),width=9,height=7)

	return(pl)


}


#' Export Aggregate/Regional VAR Processes
#' 
#' @param plotpath file path to output location
#' @param writedisk boolean whether to save to disk
#' @return produces \enumerate{
#' \item Table 5 in main text \emph{Estimates for Aggregate VAR process}
#' \item Table B.4 in online appendix \emph{Aggregate to Regional price mappings}
#' \item Figure 3 in main text \emph{VAR fit to regional price data (p)}
#' \item Figure B.3 \emph{VAR fit to regional productivity data (q)}
#' \item Figures B.1 and B.2 showing the raw data series, titled \emph{Regional (p) and National (P) house price index} and \emph{Regional (q) and National (Q) Labor Productivity index}
#' \item returns all parameters of the estimated models
#' }
Export.VAR <- function(plotpath="~/Dropbox/research/mobility/output/data/sipp",writedisk){

	# data(sipp_psid,envir=environment())
	data(BEA_fhfa,envir=environment())
	py = BEA_fhfa_agg$py
	setkey(py,year,Division)

	agg = BEA_fhfa_agg$agg
	setkey(agg,year)

	# aggregate income is GDP per capita

	aggmod = systemfit:::systemfit(list(Y=Y~LY+LP,P= P~LY+LP),data=agg)

	# print model
	if (writedisk){
		texreg(aggmod,file=file.path(plotpath,"VAR_agg.tex"),table=FALSE,booktabs=TRUE,dcolumn=TRUE,use.packages=FALSE,custom.model.names=c("$Y_t$","$P_t$"),custom.coef.names=c("Intercept","$Y_{t-1}$","$P_{t-1}$"))
	}

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
	pyagg = agg[py]

	# plot region and agg overlaid
	# ----------------------------

	# get full division name back
	data(US_states,package="EconData",envir=environment())
	US = US_states[,list(Div=unique(Division),Division=abbreviate(unique(Division),minlength=3))]
	US = US[complete.cases(US)]
	setkey(US,Division)
	setkey(pyagg,Division)
	pyagg = US[pyagg]


	pp=dcast(pyagg[,list(Division,year,p)],year ~ Division)
	yy=dcast(pyagg[,list(Division,year,y)],year ~ Division)
	y_cor = cor(yy[,-1])
	p_cor = cor(pp[,-1])

	price_correlation =  mean(p_cor[p_cor!=1])
	income_correlation =  mean(y_cor[y_cor!=1])


	my = melt(pyagg[,list(year,Division=Div,Regional=y,National=Y)],c("year","Division"))

	mytheme <- theme_bw() + theme(plot.title=element_text(vjust=1.9,size=21),legend.text=element_text(size=12),legend.key.size=unit(0.6, "cm"))

	pl = list()
	pl$y <- ggplot(my,aes(x=year,y=value,linetype=variable)) + geom_line() + facet_wrap(~Division) + ggtitle("Regional (q) and National (Q) Labor Productivity Index") + scale_y_continuous(name="1000s of Dollars") + mytheme

	mp = melt(pyagg[,list(year,Division=Div,Regional=p,National=P)],c("year","Division"))
	pl$p <- ggplot(mp,aes(x=year,y=value,linetype=variable)) + geom_line() + facet_wrap(~Division) + ggtitle("Regional (p) and National (P) house price index") + scale_y_continuous(name="1000s of Dollars")+ mytheme

	# estimate regional models: what is relationship y ~ P + Y
	ep <- p ~ Y + P
	ey <- y ~ Y + P
	divs = py[,unique(Division)]
	mods <- lapply(divs,function(x) systemfit:::systemfit(list(y=ey,p=ep),data=pyagg[Division==x]))
	names(mods) = divs

	# pritn models
	if (writedisk){
		texreg(mods[1:4],custom.model.names=paste(rep(divs[1:4],each=2),rep(c("Y","P"),4)),file=file.path(plotpath,"VAR1.tex"),table=FALSE,booktabs=TRUE,dcolumn=TRUE,use.packages=FALSE)
		texreg(mods[5:9],custom.model.names=paste(rep(divs[5:9],each=2),rep(c("Y","P"),5)),file=file.path(plotpath,"VAR2.tex"),table=FALSE,booktabs=TRUE,dcolumn=TRUE,use.packages=FALSE)
	}

	# get var-cov matrix for each regional process
	sigma_region <- data.frame(lapply(mods, function(x) as.vector(x$residCov)))

	# predict 
	pyagg[,yhat := 0]
	pyagg[,phat := 0]


	for (d in divs){
		pyagg[Division==d,c("yhat","phat") := predict(mods[[d]])]
	}

	pred_y_out = dcast(year ~ Division, value.var="yhat",data=pyagg )
	pred_p_out = dcast(year ~ Division, value.var="phat",data=pyagg )

	# visualize fit


	mdy = melt(pyagg[,list(year,Division=Div,data=y,prediction=yhat)],c("year","Division"))

	pl$pred_y <- ggplot(mdy,aes(x=year,y=value,linetype=variable)) + geom_line() + facet_wrap(~Division) + ggtitle("VAR fit to regional productivity data (q)") + scale_y_continuous(name="1000s of Dollars")  + mytheme


	mdp = melt(pyagg[,list(year,Division=Div,data=p,prediction=phat)],c("year","Division"))

	pl$pred_p <- ggplot(mdp,aes(x=year,y=value,linetype=variable)) + geom_line() + facet_wrap(~Division) + mytheme + ggtitle("VAR fit to regional price data (p)") + scale_y_continuous(name="1000s of Dollars") 
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


	ggsave(pl$y,file=file.path(plotpath,"agg_reg_y.pdf"),width=9,height=7)
	ggsave(pl$p,file=file.path(plotpath,"agg_reg_p.pdf"),width=9,height=7)
	ggsave(pl$pred_y,file=file.path(plotpath,"VAR_reg_y.pdf"),width=9,height=7)
	ggsave(pl$pred_p,file=file.path(plotpath,"VAR_reg_p.pdf"),width=9,height=7)


	return(list(Agg_mod=aggmod,Agg2Region_mods=mods,agg_sigma=sigma,Agg_coefs=aggcoefs,Agg2Region_coefs=coefs,plots=pl,PYdata=PYseries,pred_y=pred_y_out,pred_p=pred_p_out,agg_price=agg,pyagg=pyagg,sigma_region=sigma_region))

}



#' Regional Price Correlograms
#' 
#' @param path file path to output figure
#' @details Uses regional time series on q and p to illustrate
#' the structure of the joint process (q,p) across regions. 
#' @return Produces figure 2 in main text.
#' @name correlograms
correlograms <- function(path = "~/Dropbox/research/mobility/output/data/FHFA"){
    data(BEA_fhfa,envir=environment())
    py = BEA_fhfa_agg$py
    setkey(py,year,Division)
    agg = BEA_fhfa_agg$agg
    setkey(agg,year)
    pyagg = agg[py]
    pp=dcast(pyagg[,list(Division,year,p)],year ~ Division)
    yy=dcast(pyagg[,list(Division,year,y)],year ~ Division)

	pts = ts(pp[,-1],start=1967,frequency=1)
	yts = ts(yy[,-1],start=1967,frequency=1)
	ptrends = apply(pts,function(x) ma(x,order=4,centre=4),MARGIN=2)
	ytrends = apply(yts,function(x) ma(x,order=4,centre=4),MARGIN=2)

	dtrendp = pts - ptrends
	dtrendy = yts - ytrends

	pdf(file.path(path,"detrended-y.pdf"),width=14,height=8)
	plot(as.ts(dtrendy),main="Detrended q Series",nc=3)
	dev.off()

	pdf(file.path(path,"detrended-p.pdf"),width=14,height=8)
	plot(as.ts(dtrendp),main="Detrended p Series",nc=3)
	dev.off()

	# all detrended states in one plot
	ty = data.table(dtrendy,index(dtrendy))
	tp = data.table(dtrendp,index(dtrendp))
	setnames(ty,ncol(ty),"year")
	setnames(tp,ncol(tp),"year")
	mdty = melt(ty,id.vars=c("year"))
	mdtp = melt(tp,id.vars=c("year"))
	setnames(mdty,"variable","Division")
	setnames(mdtp,"variable","Division")
	mdtp[,group := "regional price p"]
	mdty[,group := "regional income q"]

	mdt = rbind(mdty,mdtp)
	plots = list()
	plots$detrended = ggplot(mdt,aes(x=year,y=value,linetype=Division)) + geom_line() + facet_wrap(~group,scales="free_y") + theme_bw() + theme(legend.position="none") + ggtitle("Detrended Time Series 1967-2012 for All Census Divisions")
	ggsave(plot=plots$detrended,file.path(path,"detrended.pdf"),width= 8,height=4,device="pdf")



	# correlograms of detrended data
	pcor = cor(dtrendp[3:44,])
	ycor = cor(dtrendy[3:44,])

	print(xtable(pcor,digits=2),floating=FALSE,booktabs=TRUE,dcolumn=TRUE,file=file.path(path,"p_corrs.tex"))
	print(xtable(ycor,digits=2),floating=FALSE,booktabs=TRUE,dcolumn=TRUE,file=file.path(path,"y_corrs.tex"))

	# autocorrelations in raw data
	pacor = apply(X=pts,function(x) pacf(x,plot=FALSE)$acf[1],MARGIN=2)
	yacor = apply(X=yts,function(x) pacf(x,plot=FALSE)$acf[1],MARGIN=2)
	acordf = data.frame(Division=names(pacor),p=pacor,q = yacor)
	print(xtable(acordf),include.rownames=FALSE,floating=FALSE,booktabs=TRUE,dcolumn=TRUE,file=file.path(path,"auto_corrs.tex"))

	# plot
	mp = melt(pcor)
	mp$type = "regional price p"
	my = melt(ycor)
	my$type = "regional income q"
	names(mp) <- names(my) <- c("Division1","Division2","correlation","type" )
	m = rbind(mp,my)
	plots$corrs = ggplot(m,aes(x=Division1,y=Division2,fill=correlation)) + geom_tile() + scale_fill_gradient(low = "white", high = "black") + facet_wrap(.~ type) + scale_y_discrete("Division 2") + scale_x_discrete("Division 1") + ggtitle("Cross Correlations between Time Series") + theme_bw()
	ggsave(plot=plots$corrs,file.path(path,"correlogram.pdf"),width= 8,height=4,device="pdf")

	pdf(file.path(path,"correlogram-detrended.pdf"),width=8,height=8)
		multiplot(plots$detrended, plots$corrs,cols=1)
	dev.off()
	return(plots)
}


plot.PriceVAR <- function(){
	x = Export.VAR()
	predy = melt(x$pred_p,c("year"))
	predy=data.table(predy)
	setnames(predy,c("year","Division","predicted_p"))
	setkey(predy,year,Division)
	setkey(py,year,Division)
	py_pred = py[predy]
	py_pred = melt(py_pred[,list(year,Division,p,predicted_p)],c("year","Division"))
	cbPalette <- c("#000000", "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
	pl = ggplot(py_pred,aes(x=year,y=value,color=Division)) + geom_line(size=0.9) + facet_wrap(~variable) + theme_bw() + scale_y_continuous("2012 dollars (1000s)") + ggtitle("Regional Price Time Series: Model vs Data") + scale_color_manual(values=cbPalette)
	ggsave(pl,file="~/Dropbox/research/mobility/output/model/fit/VAR_data_model.pdf",width=8,height=6)

	# same with income
	predy = melt(x$pred_y,c("year"))
	predy=data.table(predy)
	setnames(predy,c("year","Division","predicted_y"))
	setkey(predy,year,Division)
	setkey(py,year,Division)
	py_pred = py[predy]
	py_pred = melt(py_pred[,list(year,Division,y,predicted_y)],c("year","Division"))
	pl = ggplot(py_pred,aes(x=year,y=value,color=Division)) + geom_line(size=0.9) + facet_wrap(~variable) + theme_bw() + scale_y_continuous("2012 dollars (1000s)") + ggtitle("Regional Per Capita GDP Time Series: Model vs Data") + scale_color_manual(values=cbPalette)
	ggsave(pl,file="~/Dropbox/research/mobility/output/model/fit/VAR_data_model_y.pdf",width=8,height=6)

}




#' Estimate and Export Individual Income Process
#' 
#' @details Uses SIPP micro data to estimate an income process, which is used in the structural model to predict and simulate individual income. This is the implementation of equation (21) in the main text, further illustrated in online appendix C.1. 
#' This function is called from \code{\link{Export.Julia}}, so please refer for input arguments to that function.
#' The procedure subsets sipp income data to leave out year 2007, which I found to be full of inconsistencies.
#' @param dat A data.table of SIPP micro data
#' @param writedisk boolean whether to save to disk 
#' @param nocollege boolean whether subsetting to no college degree or not.
#' @param path string for graph output location
#' @export
#' @return Implements equation (21) in main text, writes table C.1 in online appendix to disk and produces figure C.1 titled \emph{Labor Income profiles for different q levels} also in online appendix.
#' @name Export.IncomeProcess
Export.IncomeProcess <- function(dat,writedisk,nocollege=FALSE,path = "~/Dropbox/research/mobility/output/model/fit"){

	cd <- dat[HHincome>0 & (year != 2007) & (year != 2012),list(upid,timeid,D2D,year,MyMedinc,HHincome,age,Division,college)]
	cd <- cd[complete.cases(cd)]
	setkey(cd,year,Division)

	x = get_BEA_persincome()
	setkey(x,year,Division)
	cd = x[cd]
	setnames(cd,"y","q")

	# get models of individual income
	setkey(cd,upid,Division)
	divs = cd[,unique(Division)]
	lmods = lapply(divs,function(x) lm(log(HHincome) ~ log(q) + college + age + I(age^2) + I(age^3),cd[Division==x]))
	names(lmods) = divs

	if (writedisk){
		texreg(lmods,custom.model.names=names(lmods), digits=3, custom.coef.names=c("Intercept","$q_d$","college","age","$\\text{age}^2$","$\\text{age}^3$"),booktabs=TRUE,dcolumn=TRUE,table=FALSE,sanitize.text.function=function(x){x},file=file.path(path,"region_2_indi_y.tex"),use.packages=FALSE)
	}

	nd = expand.grid(age=20:50,college=!nocollege,q=c(30,45,60))
	x <- lapply(lmods, function(x) cbind(nd,exp(predict(x,nd))))
	x <- lapply(names(x),function(z){cbind(x[[z]],Division=z)})

	predicted_y <- rbindlist(x)
	setnames(predicted_y,c(3,4),c("$q_d$","predict"))

	tikz(file.path(path,"income_profiles.tex"),width=6,height=4)
	p = ggplot(predicted_y,aes(age,y=predict,color=Division)) + geom_line(size=0.9) + facet_wrap(~`$q_d$`) + ggtitle("Labor Income profiles for different $q_{d}$ levels \n    ") + scale_y_continuous("Dollars (1000s)") + theme_bw()
	plot(p)
	dev.off()

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

	# add the 0.2 and 0.95 percentiles of income in each region 
	# to scale the shocks
	if (nocollege){
		bounds = ddply(subset(dat,(college==FALSE) & (HHincome>0)),"Division", function(x) quantile(x$HHincome,probs=c(0.05,0.95),na.rm=T)) 

	} else {
		bounds = ddply(subset(dat,HHincome>0),"Division", function(x) quantile(x$HHincome,probs=c(0.05,0.95),na.rm=T)) 

	}
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




#' Get Macro Price Series
#' 
#' @details This function writes out a dataset which contains
#' the series q and p used in the paper for each Division.
#' This function uses output from \code{\link{get_BEA_persincome}} and
#' \code{\link{getFHFA_realPrices}}.
#' @export
#' @name combine_BEA_fhfa
combine_BEA_fhfa <-function(){
	BEA= get_BEA_persincome()
	fhfa = getFHFA_realPrices()

	setkey(BEA,year,Division)
	setkey(fhfa,year,Division)

	py <- BEA[fhfa]
	py = 
	py = py[,list(year,Division,y,p)]
	py = py[complete.cases(py)]

	setkey(fhfa,year)
	agg <- fhfa[Division=="USA",list(year,P=p)]
	setkey(agg,year)
	gdp = getFRED_gdp()
	agg = gdp[agg]
	setnames(agg,"gdp","Y")
	agg[,LY := agg[list(year-1)][["Y"]]]
	agg[,LP := agg[list(year-1)][["P"]]]

	BEA_fhfa_agg = list(py=py,agg=agg)


	save(BEA_fhfa_agg,file="~/git/migration/mig-pkg/data/BEA_fhfa.rda")

	return(BEA_fhfa_agg)
}



#' Get Personal Income Data from BEA
#' 
#' @details this function depends on R package EconData available at
#' \url{https://github.com/floswald/EconData}
#' @name get_BEA_persincome
get_BEA_persincome <- function(){

	data(PersonalIncome,package="EconData",envir=environment())
	data(Population,package="EconData",envir=environment())

	# this is personal income for an entire state!
	setkey(pers_income,state,year)
	setkey(population,state,year)
	population[pers_income]
	py = pers_income[population]
	py[,pcy := income / population]


	# real per capita income
	cpi = getCPI(base="2012",freq="yearly")
	setkey(py,year)
	py = py[cpi]
	py[,rpcy := pcy / cpi2012]
	py
	py = py[complete.cases(py)]

	# merge divisoin
	data(US_states,package="EconData",envir=environment())
	US_states = US_states[,list(state,Division)]
	setkey(US_states,state)

	setkey(py,state)
	py = US_states[py]
	py[,Division := abbreviate(Division,minlength=3)]

	# average by division
	py[,wgt := population / .SD[,sum(population)],by=list(Division,year)]
	py = py[,list(y = weighted.mean(rpcy,wgt)),by=list(Division,year)]

	return(py)

}

# gdp per capita
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


#' Extend SIPP/FHFA data back to 1967
#' 
#' @details This takes the mean house value by division in base year 2012
#' and uses several price indices to project this value backwards in time.
#' \enumerate{
#' \item FHFA Division Index goes back until 1975
#' \item Use CPI to cover 1967-1975
#' }
#' @export
#' @name getFHFA_realPrices
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


	fhfa[,index2012 := index_sa / .SD[year==2012,index_sa],by=Division]
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


