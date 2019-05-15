


var.sd <- function(name,x,y=c()) {
  x = x - mean(x,na.rm=T)
  if (length(y)==0) {
    y = x
  } else {
    y = y - mean(y,na.rm=T)
  }
  sfit1 = summary(lm( x*y ~1))
  att <- list()
  att$momemt = name
  att$data_value  = sfit1$coef[1,1]
  att$data_sd     = sfit1$coef[1,2]
  # att$data_value  = cov(x,y,use="complete.obs")
  # att$data_sd     = 0.0
  return(att)
}

#' Calculate Moments from SIPP data
#' 
#' @details Computes moments form SIPP data to be used in
#' structural model SMM estimation routine
#' @param d a SIPP data.table
#' @param svy a SIPP dataset in survey format. Can be obtained by calling \code{\link{SippSvyDesign}}
#' @return a datatable with moment names, values and standard deviations
#' @export
Sipp.moments <- function(d,svy,ages=c(20,50)){

	r <- list()

	# subset data do age range
	d <- copy(d[age>=ages[1] & age <= ages[2]])
	svy = subset(svy, (age <= ages[2]) & (age >= ages[1]))

	# moments relating to homeownership
	# =================================

	# ownership ~ age

	dlm = summary(svyglm(own ~ age + age2,svy))
	nms = paste0("lm_h_",rownames(dlm$coefficients))
	nms = gsub("\\(|\\)","",nms)
	r$own_rate_reg = data.table(moment=nms,data_value=dlm$coefficients[,"Estimate"],data_sd=dlm$coefficients[,"Std. Error"])
	r$mean_sell_50 = d[age==50,list(moment="mean_sell_50",data_value=weighted.mean(sell,HHweight,na.rm=T),data_sd=sqrt(wtd.var(sell,HHweight,na.rm=T) / nrow(.SD)))]
	r$mean_move_50 = d[age==50,list(moment="mean_move_50",data_value=weighted.mean(D2D,HHweight,na.rm=T),data_sd=sqrt(wtd.var(D2D,HHweight,na.rm=T) / nrow(.SD)))]

	# own ~ 
	r$mean_own = d[,list(moment="mean_own",data_value=weighted.mean(own,HHweight,na.rm=T),data_sd=sqrt(wtd.var(own,HHweight,na.rm=T) / nrow(.SD)))]

	# own ~ Division
	r$mean_own_div = d[,list(moment="mean_own",data_value=weighted.mean(own,HHweight,na.rm=T),data_sd=sqrt(wtd.var(own,HHweight,na.rm=T) / nrow(.SD))),by=Division][order(Division)]
	r$mean_own_div[,moment := paste0(moment,"_",Division)]
	r$mean_own_div[,Division := NULL]

	# own ~ kids
	r$mean_own_kids = d[,list(moment="mean_own",data_value=weighted.mean(own,HHweight,na.rm=T),data_sd=sqrt(wtd.var(own,HHweight,na.rm=T) / nrow(.SD))),by=kids][order(kids)]
	r$mean_own_kids[,moment := paste0(moment,"_kids",kids)]
	r$mean_own_kids[,kids:= NULL]
	r$cov_own_kids = d[,var.sd("cov_own_kids",own,kids)]


	# moments relating to mobility
	# ============================

	# transition matrix
	# unconditional disritubiont point estimate of proportions
	tra = d[D2D==TRUE,table(fromD,toD)]
	tra_cond = prop.table(tra) 
	tra_alpha = 0.05
	tra_ci = multinomialCI(tra,tra_alpha)
	tra_sd = matrix(0,nrow=nrow(tra),ncol=ncol(tra))
	for (i in 1:length(tra)){
	  tra_sd[i] =  (tra_cond[i] - tra_ci[i,1]) / qnorm(1-tra_alpha/2)
	}
	
	# get fraction of all movers that go to each region on average
	tracond = rowSums(prop.table(tra))
	tracond_sd = rowSums(tra_sd)
	stopifnot(sum(tracond)==1)
	r$proportion_movers <- data.table(moment = paste0("flow_move_to_",names(tracond)), data_value = tracond,data_sd=tracond_sd)


	# linear prob model move ~ age
	# ----------------------------

	mv_reg = summary(svyglm(D2D ~ age + age2 ,svy))
	nms = paste0("lm_mv_",rownames(mv_reg$coefficients))
	nms = gsub("\\(|\\)","",nms)

	r$mv_rate_reg = data.table(moment=nms,data_value=mv_reg$coefficients[,"Estimate"],data_sd=mv_reg$coefficients[,"Std. Error"])

	# number of moves per household as percentage of population
	nmoves = d[,list(moves=sum(D2D,na.rm=T),HHweight),by=upid]
	nmoves[,c("moved0","moved1","moved2") := list(moves==0,moves==1,moves>1)]
    nmoves[,moved1 := moves==1]
    nmoves[,moved2 := moves>1]
    r$move_rate <- d[,list(move=weighted.mean(D2D,HHweight,na.rm=T),sdmove=sqrt(wtd.var(D2D,HHweight,na.rm=T) / nrow(.SD))),by=age][,list(moment="mean_move",data_value=mean(move),data_sd=mean(sdmove))]

    # CAUTIONN!
    # moving zero times in SIPP data means not moving over 4 years. this is not what you expect and also not 
    # what you compute in the model.
    # you want lifetime moves.
	# r$moves0 <- nmoves[,list(moment="moved0",data_value=weighted.mean(moved0,HHweight), data_sd=sqrt(wtd.var(moved0,HHweight)))]
	# r$moves1 <- nmoves[,list(moment="moved1",data_value=weighted.mean(moved1,HHweight), data_sd=sqrt(wtd.var(moved1,HHweight)))]
	# r$moves2 <- nmoves[,list(moment="moved2",data_value=weighted.mean(moved2,HHweight), data_sd=sqrt(wtd.var(moved2,HHweight)))]
	# those numbers from kennan and walker page 39
	r$moves0 <- nmoves[,list(moment="moved0",data_value=0.83, data_sd=1)]
	r$moves1 <- nmoves[,list(moment="moved1",data_value=0.071, data_sd=1)]
	r$moves2 <- nmoves[,list(moment="moved2plus",data_value=0.09, data_sd=1)]


	# move ~ own
	# -----------

	r$mean_move_h0 = d[own==0,list(moment="mean_move_ownFALSE",data_value=weighted.mean(D2D,HHweight,na.rm=T),data_sd=sqrt(wtd.var(D2D,HHweight,na.rm=T) / length(own)))]
	r$mean_move_h1 = d[own==1,list(moment="mean_move_ownTRUE",data_value=weighted.mean(D2D,HHweight,na.rm=T),data_sd=sqrt(wtd.var(D2D,HHweight,na.rm=T) / length(own)))]
	r$cov_move_h = d[,var.sd("cov_move_h",D2D,own)]

	# moving and kids
	# ---------------

	r$mean_move_kids = d[,list(moment="mean_move",data_value=weighted.mean(D2D,HHweight,na.rm=T),data_sd=sqrt(wtd.var(D2D,HHweight,na.rm=T) / nrow(.SD))),by=kids][order(kids)]
	r$mean_move_kids[,moment := paste0(moment,"_kids",kids)]
	r$mean_move_kids[,kids:= NULL]
	r$cov_move_kids = d[,var.sd("cov_move_kids",D2D,kids)]

	# moving and distance
	# -------------------

	# for lack of a better measure, compute quartiles of distance for movers here
	qts <- d[D2D==TRUE,quantile(km_distance)]
	r$q25_move_distance <- data.table(moment="q25_move_distance",data_value=qts["25%"],data_sd = 1)
	r$q50_move_distance <- data.table(moment="q50_move_distance",data_value=qts["50%"],data_sd = 1)
	r$q75_move_distance <- data.table(moment="q75_move_distance",data_value=qts["75%"],data_sd = 1)

	# moving and negative equity
	# --------------------------

	negeq <- d[own==TRUE & D2D==TRUE, prop.table(table(home.equity<0))]["TRUE"]
	r$move_negeq <- data.table(moment="move_neg_equity",data_value=negeq,data_sd = 1)

	# linear regression of total wealth
	# =================================

	# wlm = summary(svyglm(wealth ~ age + age2 ,svy ))
	# nms = paste0("lm_w_",rownames(wlm$coefficients))
	# nms = gsub("\\(|\\)","",nms)
	# r$wealth_reg = data.table(moment=nms,data_value=wlm$coefficients[,"Estimate"],data_sd=wlm$coefficients[,"Std. Error"])

	# mean wealth by age group
	# ------------------------
	r$w_age <- d[,list(data_value=weighted.mean(wealth,HHweight,na.rm=T),data_sd=sqrt(wtd.var(wealth,HHweight,na.rm=T) / nrow(.SD))),by=cut_interval(age,3)][order(cut_interval)]
	r$w_age[,cut_interval := gsub(",","_",gsub("\\[|\\(|\\]","",cut_interval))]
	r$w_age[,moment := paste0("mean_wealth_",cut_interval)]
	r$w_age[,cut_interval := NULL]
	setcolorder(r$w_age,c("moment","data_value","data_sd"))

	# mean wealth conditional on Division
	# -----------------------------------
	r$mean_wealth_div <- d[,list(moment="mean_wealth",data_value=weighted.mean(wealth,HHweight,na.rm=T),data_sd=sqrt(wtd.var(wealth,HHweight,na.rm=T) / nrow(.SD))),by=list(Division)][order(Division)] 
	r$mean_wealth_div[,moment := paste0(moment,"_",Division)]
	r$mean_wealth_div[,c("Division") := NULL]

	# wealth ~ own
	r$mean_wealth_h0 = d[own==0,list(moment="mean_wealth_ownFALSE",data_value=weighted.mean(wealth,HHweight,na.rm=T),data_sd=sqrt(wtd.var(wealth,HHweight,na.rm=T) / length(own)))]
	r$mean_wealth_h1 = d[own==1,list(moment="mean_wealth_ownTRUE",data_value=weighted.mean(wealth,HHweight,na.rm=T),data_sd=sqrt(wtd.var(wealth,HHweight,na.rm=T) / length(own)))]


	# wlm = summary(svyglm(w2medinc ~ age + age2 + own + Division, svy))


	# l = lapply(d[,unique(Division)],function(x) lm(own ~ ns(age,df=4),data=merged[Division==x]))
	# own_reg_age = d[age>25&age<65,list(value=weighted.mean(own,HHweight),data_sd = sqrt(wtd.var(own,HHweight,na.rm=T))),by=list(age,Division)]
	# ggplot(own_reg_age,aes(x=age,y=value,color=Division)) + geom_point() + geom_smooth()

	# nonh_wealth and total wealth by ownership
	wealth_h = ddply(d,c("own"),summarise,nonh = wtd.quantile(nonh_wealth,HHweight,0.5),wealth=wtd.quantile(wealth,HHweight,0.5))
	# nonh_wealth and total wealth by ownership and age
	wealth_h_age = ddply(d,c("own","age"),summarise,nonh = wtd.quantile(nonh_wealth,HHweight,0.5),wealth=wtd.quantile(wealth,HHweight,0.5))
	# nonh_wealth and total wealth by ownership and age and region
	wealth_h_age_div = ddply(d,c("own","age","Division"),summarise,nonh = wtd.quantile(nonh_wealth,HHweight,0.5),wealth=wtd.quantile(wealth,HHweight,0.5))


	# pp <- ggplot(wealth_h_age_div,aes(x=age,y=nonh,color=Division)) + geom_line() + facet_wrap(~own) + ggtitle('total wealth in 1000s of dollars')

	return(rbindlist(r))

}


#' Fraction of non-resident landlords
#' 
#' How many owners live in region k while renting out
#' a flat in region d?
#' @param path to save graphs
Sipp.own_in_j_rent_in_k <- function(path = "~/Dropbox/mobility/output/data/sipp"){
	data(Sipp_aggby_NULL,envir=environment())
	mv <- merged[D2D==TRUE,list(upid=unique(upid))]
	setkey(mv,upid)
	mvs <- merged[mv]
	setkey(mvs,upid,timeid)

	# get "other equity" in period after move
	mvs[,other_eq_lead := mvs[list(upid,timeid+1)][["RE.equity.other"]] ]

	# how many movers have more "real estate equity other than your home"
	# after move than before?
	# answers the question of "how many movers keep their house"

	# get correlation between other equity today and tomorrow for movers
	eqcor <- mvs[D2D==TRUE,cor(RE.equity.other,other_eq_lead,use="complete.obs")]

	# get count of people with other equity before and after move. 
	# if movers keep house, that count should go up.
	eq_before_after <- mvs[D2D==TRUE,list(other.eq.before=sum(RE.equity.other!=0.0),other.eq.after=sum(other_eq_lead!=0.0,na.rm=T))]

	# whats the percentage of movers who had 0 other equity before move, that now do have positive other RE equity?
	num.movers <- nrow(mv)

	# how robust is this to definition of "after"?
	mvs[,other_eq_lead2 := mvs[list(upid,timeid+2)][["RE.equity.other"]] ]
	mvs[,other_eq_lead3 := mvs[list(upid,timeid+3)][["RE.equity.other"]] ]
	mvs[,other_eq_lead4 := mvs[list(upid,timeid+4)][["RE.equity.other"]] ]
	mvs[,other_eq_lead5 := mvs[list(upid,timeid+5)][["RE.equity.other"]] ]
	mvs[,other_eq_lead6 := mvs[list(upid,timeid+6)][["RE.equity.other"]] ]
	mvs[,other_eq_lead7 := mvs[list(upid,timeid+7)][["RE.equity.other"]] ]
	mvs[,other_eq_lead8 := mvs[list(upid,timeid+8)][["RE.equity.other"]] ]
	mvs[,other_eq_lead9 := mvs[list(upid,timeid+9)][["RE.equity.other"]] ]
	mvs[,other_eq_lead10 := mvs[list(upid,timeid+10)][["RE.equity.other"]] ]
	mvs[,other_eq_lead11 := mvs[list(upid,timeid+11)][["RE.equity.other"]] ]
	mvs[,other_eq_lead12 := mvs[list(upid,timeid+12)][["RE.equity.other"]] ]

	eq_robust <- mvs[D2D==TRUE,list(other.eq.before=sum(RE.equity.other!=0.0),other.eq.after1=sum(other_eq_lead!=0.0,na.rm=T),other.eq.after2=sum(other_eq_lead2!=0.0,na.rm=T),other.eq.after3=sum(other_eq_lead3!=0.0,na.rm=T),other.eq.after4=sum(other_eq_lead4!=0.0,na.rm=T),other.eq.after5=sum(other_eq_lead5!=0.0,na.rm=T),other.eq.after6=sum(other_eq_lead6!=0.0,na.rm=T),other.eq.after7=sum(other_eq_lead7!=0.0,na.rm=T),other.eq.after8=sum(other_eq_lead8!=0.0,na.rm=T),other.eq.after9=sum(other_eq_lead9!=0.0,na.rm=T),other.eq.after10=sum(other_eq_lead10!=0.0,na.rm=T),other.eq.after11=sum(other_eq_lead11!=0.0,na.rm=T),other.eq.after12=sum(other_eq_lead12!=0.0,na.rm=T))]

	# conditioning on the ones who had other equity before

	eq_robust2 <- mvs[D2D==TRUE & RE.equity.other!=0,list(other.eq.before=sum(RE.equity.other!=0.0),other.eq.after1=sum(other_eq_lead!=0.0,na.rm=T),other.eq.after2=sum(other_eq_lead2!=0.0,na.rm=T),other.eq.after3=sum(other_eq_lead3!=0.0,na.rm=T),other.eq.after4=sum(other_eq_lead4!=0.0,na.rm=T),other.eq.after5=sum(other_eq_lead5!=0.0,na.rm=T),other.eq.after6=sum(other_eq_lead6!=0.0,na.rm=T),other.eq.after7=sum(other_eq_lead7!=0.0,na.rm=T),other.eq.after8=sum(other_eq_lead8!=0.0,na.rm=T),other.eq.after9=sum(other_eq_lead9!=0.0,na.rm=T),other.eq.after10=sum(other_eq_lead10!=0.0,na.rm=T),other.eq.after11=sum(other_eq_lead11!=0.0,na.rm=T),other.eq.after12=sum(other_eq_lead12!=0.0,na.rm=T))]

	# conditioning on the ones who did not have other equity before

	eq_robust3 <- mvs[D2D==TRUE & RE.equity.other==0,list(other.eq.before=sum(RE.equity.other!=0.0),other.eq.after1=sum(other_eq_lead!=0.0,na.rm=T),other.eq.after2=sum(other_eq_lead2!=0.0,na.rm=T),other.eq.after3=sum(other_eq_lead3!=0.0,na.rm=T),other.eq.after4=sum(other_eq_lead4!=0.0,na.rm=T),other.eq.after5=sum(other_eq_lead5!=0.0,na.rm=T),other.eq.after6=sum(other_eq_lead6!=0.0,na.rm=T),other.eq.after7=sum(other_eq_lead7!=0.0,na.rm=T),other.eq.after8=sum(other_eq_lead8!=0.0,na.rm=T),other.eq.after9=sum(other_eq_lead9!=0.0,na.rm=T),other.eq.after10=sum(other_eq_lead10!=0.0,na.rm=T),other.eq.after11=sum(other_eq_lead11!=0.0,na.rm=T),other.eq.after12=sum(other_eq_lead12!=0.0,na.rm=T))]

	eq_robust3_pos <- mvs[D2D==TRUE & RE.equity.other==0,list(other.eq.before=sum(RE.equity.other>0.0),other.eq.after1=sum(other_eq_lead>0.0,na.rm=T),other.eq.after2=sum(other_eq_lead2>0.0,na.rm=T),other.eq.after3=sum(other_eq_lead3>0.0,na.rm=T),other.eq.after4=sum(other_eq_lead4>0.0,na.rm=T),other.eq.after5=sum(other_eq_lead5>0.0,na.rm=T),other.eq.after6=sum(other_eq_lead6>0.0,na.rm=T),other.eq.after7=sum(other_eq_lead7>0.0,na.rm=T),other.eq.after8=sum(other_eq_lead8>0.0,na.rm=T),other.eq.after9=sum(other_eq_lead9>0.0,na.rm=T),other.eq.after10=sum(other_eq_lead10>0.0,na.rm=T),other.eq.after11=sum(other_eq_lead11>0.0,na.rm=T),other.eq.after12=sum(other_eq_lead12>0.0,na.rm=T))]



	eq_cor_robust <- mvs[D2D==TRUE&RE.equity.other!=0.0,list(cor_0_1=cor(RE.equity.other,other_eq_lead,use="complete.obs"),cor_1_2=cor(other_eq_lead,other_eq_lead2,use="complete.obs"),
		cor_2_3=cor(other_eq_lead2,  other_eq_lead3,use="complete.obs"),
		cor_3_4=cor(other_eq_lead3,  other_eq_lead4,use="complete.obs"),
		cor_4_5=cor(other_eq_lead4,  other_eq_lead5,use="complete.obs"),
		cor_5_6=cor(other_eq_lead5,  other_eq_lead6,use="complete.obs"),
		cor_6_7=cor(other_eq_lead6,  other_eq_lead7,use="complete.obs"),
		cor_7_8=cor(other_eq_lead7,  other_eq_lead8,use="complete.obs"),
		cor_8_9=cor(other_eq_lead8,  other_eq_lead9,use="complete.obs"),
		cor_9_10=cor(other_eq_lead9, other_eq_lead10,use="complete.obs"),
		cor_10_11=cor(other_eq_lead10,other_eq_lead11,use="complete.obs"),
		cor_11_12=cor(other_eq_lead11,other_eq_lead12,use="complete.obs"))]


	pl <- list()

	r = data.frame(t(eq_robust))
	r$months_after <- seq(0,nrow(r)-1,le=nrow(r))
	names(r)[1] = "otherRE"
	rownames(r) = NULL
	r$percent <- 100*r$otherRE / num.movers
	pl$mv1 <- ggplot(r,aes(x=months_after,y=percent)) + geom_point(size=3) + geom_line() + theme_bw() + ggtitle("Movers: Do you have equity in other real estate? (not home)") + scale_y_continuous(name="% of Movers") + scale_x_continuous(name="months after move",breaks=seq(0,nrow(r)-1,le=nrow(r))) 


	# conditional on having had before
	r2 = data.frame(t(eq_robust2))
	names(r2) <- "otherRE"
	r2$months_after <- seq(0,nrow(r)-1,le=nrow(r))
	rownames(r2) = NULL
	r2$percent <- 100*r2$otherRE / num.movers
	r2$type <- "had other\nequity"

	r3 = data.frame(t(eq_robust3))
	names(r3) <- "otherRE"
	rownames(r3) = NULL
	r3$months_after <- seq(0,nrow(r)-1,le=nrow(r))
	r3$percent <- 100*r3$otherRE / num.movers
	r3$type <- "had none"

	r2 <- rbind(r2,r3)
	names(r2)[4] <- "when_moving"

	pl$mv2 <- ggplot(r2,aes(x=months_after,y=percent,color=when_moving)) + geom_point(size=3) + geom_line() + theme_bw() + ggtitle("Movers: Do you have equity in other real estate? (not home)") + scale_y_continuous(name="% of movers saying yes") + scale_x_continuous(name="months after move",breaks=seq(0,nrow(r)-1,le=nrow(r))) 

	pdf(file=file.path(path,"move_2_landlord_2.pdf"),width=10,h=7)
	print(pl$mv2)
	dev.off()


	# whats the percentage of movers who had 0 other equity before move, that now do have positive other RE equity?
	num.movers <- nrow(mv)
	r3p = data.frame(t(eq_robust3))
	names(r3p) <- "otherRE"
	rownames(r3p) = NULL
	r3p$months_after <- seq(0,nrow(r)-1,le=nrow(r))
	r3p$percent <- 100*r3p$otherRE / num.movers

	pl$mv3 <- ggplot(r3p,aes(x=months_after,y=percent)) + geom_point(size=3) + geom_line() + theme_bw() + ggtitle("Percentage of movers with any equity in other real estate") + scale_y_continuous(name="%") + scale_x_continuous(name="months after move",breaks=seq(0,nrow(r)-1,le=nrow(r))) 
	pdf(file=file.path(path,"move_2_landlord.pdf"),width=10,h=7)
	print(pl$mv3)
	dev.off()
	return(pl)

}


#' Estimation of Movers' z Copula
#' 
#' @details Implements the estimation of movers' z transition via a normal copula 
#' with SIPP micro data. This is the implementation described in section C.1 of the online appendix.
#' @param path to save graphs
#' @export
#' @return \enumerate{
#' \item Table C.2 in online appendix reporting estimated Copula parameters.
#' \item Figure C.2 illustrating the marginal distributions of z in periods before and after move
#' \item Figure C.3 is produced in the julia package, not here.
#' }
Sipp.wage_residual_copulas <- function(path="~/Dropbox/research/mobility/output/data/sipp"){

	data(Sipp_age,envir=environment())

	dat <- merged[HHincome > 0]
	dat[,u := resid(lm(log(HHincome) ~ factor(Division) + poly(age,degree=3,raw=T) + college))]
	setkey(dat,upid,timeid)
	dat[,u_plus1 := dat[list(upid,timeid+1)][["u"]]]
	dat[,u_minus1 := dat[list(upid,timeid-1)][["u"]]]

	par(mfrow=(c(1,2)))
	dat[D2D==TRUE,hist(u,breaks=20,freq=FALSE,main="Histrogram of u(t)")]
	lines(density(dat[D2D==TRUE,u]),col="red",lw=2)
	dat[D2D==TRUE,hist(u_plus1,breaks=20,freq=FALSE,main="Histrogram of u(t+1)")]
	lines(density(dat[D2D==TRUE & (!is.na(u_plus1)),u_plus1]),col="red",lw=2)
	par(mfrow=c(1,1))

	# same plot with ggplot
	pl <- list()
	md = melt(dat[D2D==TRUE,list(z=u,z_1=u_plus1)])
	pl$margins <- ggplot(subset(md,value>-3),aes(x=value)) + geom_density() + facet_wrap(~variable) + theme_bw() + ggtitle("Kernel Density Estimate of Movers' z Distribution") + labs(subtitle = "z is before, z_1 is after move")
	ggsave(plot=pl$margins,filename=file.path(path,"z_margins.pdf"))

	margins=dat[D2D==TRUE,list(m=mean(u),s=sd(u),m1=mean(u_plus1,na.rm=T),s1=sd(u_plus1,na.rm=T))]

	# standardize into ranks
	mmat = dat[,list(p_u=u,p_u_plus1=u_plus1,p_u_minus1=u_minus1)]
	pmat = pobs(mmat)

	data = cbind(dat[,list(upid,timeid,D2D,u,u_plus1,u_minus1)],pmat)

	# movers only
	d1 = data[D2D==1,list(p_u,p_u_plus1)]
	d1 = d1[complete.cases(d1)]
	normal.cop = normalCopula(0.7,2,"ar1")
	f1 = fitCopula(normal.cop,as.matrix(d1))
	cop <- mvdc(normalCopula(coef(f1)), c("norm", "norm"), list(list(mean = 0, sd =margins[,s]), list(mean = 0, sd =margins[,s1])))

	# save this object to disk
	save(cop,file="~/git/migration/mig/in/copula.RData")


	d2 = data[D2D==1,list(p_u_minus1,p_u)]
	d2 = d2[complete.cases(d2)]
	f2 = fitCopula(normal.cop,as.matrix(d2))
	cop_before_move <- mvdc(normalCopula(coef(f2)), c("norm", "norm"), list(list(mean = 0, sd =margins[,s]), list(mean = 0, sd =margins[,s1])))

	c1 = as.data.frame(summary(f1)$coefficients)
	c2 = as.data.frame(summary(f2)$coefficients)
	cat(toJSON(list(movers=c1,before_move=c2,margins=cop@paramMargins)),file="~/Dropbox/research/mobility/output/model/fit/copulas.json")

	return(list(movers=f1,before_move=f2))
}


ownership_rates_macro_data <- function(){
	data(Sipp_age,envir=environment())
	d <- merged[age>19 & age<51,list(own=mean(own,na.rm=T),p =.SD[own==TRUE,weighted.mean(hvalue,na.rm=T)], y = mean(CensusMedinc,na.rm=T)),by=list(year,Division)]
	d[,p2y := p / y]
	

}

# joint distribution of bilateral moves
# unconditional
Sipp.TransitionMatrix <- function(path = "~/Dropbox/research/mobility/output/model/data_repo/in_data_jl"){
	data(Sipp_age,envir=environment())
	tra = merged[D2D==TRUE,prop.table(table(fromD,toD))]
	write.table(tra,file.path(path,"transmat.csv"))
}



#' SIPP Summary Statistics
#' 
#' @details Produces summary statistics from SIPP micro dataset
#' @param path to save output
#' @param nocollege boolean whether to subset stats to no college degree.
#' @return writes table 3 in main text to disk (\emph{Annual moving rate in percent of the population}).
Sipp.SumStats <- function(path="~/Dropbox/research/mobility/output/data/sipp",nocollege=FALSE){


	ta <- list()

	# annual moving rate
	# ==================

	data(Sipp_age,envir=environment())

	if (nocollege){
		merged <- merged[college==FALSE]
	}

	S2S_percent <- merged[age>=20 & age<=50,list(S2S=weighted.mean(S2S,HHweight,na.rm=T)*100,D2D=weighted.mean(D2D,HHweight,na.rm=T)*100),by=list(age)][,list(S2S=round(mean(S2S),2),D2D=round(mean(D2D),2))]
	ta$S2S_percent <- merged[age>=20 & age<=50,list(S2S=weighted.mean(S2S,HHweight,na.rm=T)*100,D2D=weighted.mean(D2D,HHweight,na.rm=T)*100),by=list(own,year)][,list(S2S=round(mean(S2S),2),D2D=round(mean(D2D),2)),by=own][order(own)]
	ta$S2S_percent <- as.matrix(ta$S2S_percent[,list(S2S,D2D)])
	ta$S2S_percent <- rbind(as.matrix(S2S_percent),ta$S2S_percent)
	colnames(ta$S2S_percent) <- c("Cross State","Cross Division")
	rownames(ta$S2S_percent) <- c("Overall","Renter","Owner")


	# average ownership rate and p2y by division
	# ==================================

	ta$own_p2y <- merged[age>=20 & age<=50,list(own=weighted.mean(own,HHweight,na.rm=T),p2y=median(p2y,na.rm=T)),by=Division]



	# number of moves per mover
	# =========================

	
	mvs = merged[D2D==TRUE,list(upid=unique(upid))]
	setkey(mvs,upid)
	setkey(merged,upid,timeid)

	d = merged[mvs]
	sm = d[,list(nmoves=sum(D2D,na.rm=T),own=own[1]),by=upid]
	ta$nmv <- as.matrix(sm[,table(own,nmoves)])
	rownames(ta$nmv) <- c("Renter","Owner")

	# fraction of moves that go back home
	merged[,toHome := state.born == to]
	ta$home <- merged[S2S==TRUE,t(as.matrix(round(prop.table(table(toHome))*100,2)))]
	colnames(ta$home) <- c("Other State","State of Birth")

	rm(merged)
	gc()

	# monthly data
	data(Sipp_aggby_NULL,envir=environment())

	if (nocollege){
		merged <- merged[college==FALSE]
	}

	d = merged
	setkey(d,upid,timeid)
	# get empstat before after
	d[,employed_minus := d[list(upid,timeid-1)][["employed"]]]
	d[,employed_plus  := d[list(upid,timeid+1)][["employed"]]]
	ta$emptab <- d[D2D==TRUE,as.matrix(round(prop.table(table(employed,employed_plus),margin=1),3))]
	rownames(ta$emptab) <- c("unemployed $t$","employed $t$")
	colnames(ta$emptab) <- c("unemployed $t+1$","employed $t+1$")

	# get nowage before after
	d[,nowage := FALSE]
	d[HHincome<=0, nowage := TRUE]
	d[,nowage_plus := d[list(upid,timeid+1)][["nowage"]] ]
	ta$wagetab <- d[D2D==TRUE,as.matrix(round(prop.table(table(nowage,nowage_plus),margin=1),3))]
	rownames(ta$wagetab) <- c("$w_{t} > 0$","$w_{t} < 0$")
	colnames(ta$wagetab) <- c("$w_{t+1} > 0$","$w_{t+1} < 0$")

	# get ownership before after
	d[,own_plus := d[list(upid,timeid+1)][["own"]]]
	ta$owntab <- d[D2D==TRUE & age>19 & age<51, round(prop.table(table(own,own_plus),margin=1),2)]
	ta$owntab_nomove <- d[D2D==FALSE& age>19 & age<51, round(prop.table(table(own,own_plus),margin=1),4)]
	rownames(ta$owntab) <- c("Rent today","Own today")
	colnames(ta$owntab) <- c("Rent tomorrow","Own tomorrow")
	rownames(ta$owntab_nomove) <- c("Rent today","Own today")
	colnames(ta$owntab_nomove) <- c("Rent tomorrow","Own tomorrow")


	# print tables
	print(xtable(ta$own_p2y),floating=FALSE,booktabs=TRUE,dcolumn=TRUE,file=file.path(path,"own_p2y.tex"))
	print(xtable(ta$S2S_percent),floating=FALSE,booktabs=TRUE,dcolumn=TRUE,file=file.path(path,"move_rates.tex"))
	print(xtable(ta$nmv),floating=FALSE,booktabs=TRUE,dcolumn=TRUE,file=file.path(path,"num_moves.tex"))
	print(xtable(ta$home),floating=FALSE,booktabs=TRUE,dcolumn=TRUE,file=file.path(path,"mv_home.tex"))

	print(xtable(ta$emptab),floating=FALSE,booktabs=TRUE,dcolumn=TRUE,sanitize.colnames.function=function(x){x},sanitize.rownames.function=function(x){x},file=file.path(path,"emptab.tex"))
	print(xtable(ta$wagetab),floating=FALSE,booktabs=TRUE,dcolumn=TRUE,sanitize.colnames.function=function(x){x},sanitize.rownames.function=function(x){x},file=file.path(path,"wagetab.tex"))
	print(xtable(ta$owntab),floating=FALSE,booktabs=TRUE,dcolumn=TRUE,sanitize.colnames.function=function(x){x},sanitize.rownames.function=function(x){x},file=file.path(path,"owntab.tex"))
	print(xtable(ta$owntab_nomove,digits=4),floating=FALSE,booktabs=TRUE,dcolumn=TRUE,sanitize.colnames.function=function(x){x},sanitize.rownames.function=function(x){x},file=file.path(path,"owntab_nomove.tex"))

	return(ta)

}

#' Probit: Determinants of Cross Division Moves
#'
#' @details Uses SIPP micro data to estimate a probit model 
#' relating observables to a indicator of whether a move took place in period t.
#' @param d a svy object. Can be obtained by calling \code{\link{SippSvyDesign}}.
#' @param path to save output
#' @return produces table 4 in main text: \emph{Determinants of cross census division moves in SIPP data}.
SippProbitMove <- function(d,path="~/Dropbox/mobility/output/data/sipp"){

	stopifnot("survey.design" %in% class(d) )
	d$variables[,HHincome := HHincome /100]  # income in 100,000 dollars
	d$variables[,wealth := wealth /100]  # same for wealth

	mD <- svyglm(D2D ~ age + age2 + kids + own + HHincome + wealth + college + p2y,x=TRUE, family=binomial(link="probit"),design=subset(d,D2D<2 & p2y<1000))
	me <- erer:::maBina(mD,digits=4)

	texreg(list(me),file=file.path(path,"D2D-probit.tex"),table=FALSE,dcolumn=TRUE,booktabs=TRUE,use.packages=FALSE,stars=c(0.1,0.05,0.01),custom.model.names="Marginal Effects", custom.coef.names=c("Intercept","Age","Age Squared","Children in HH","Homeowner","Household income","Total wealth","College","Price/Income"),digits = 4)

	# predict for different covariates

	prd <- data.frame(age=rep(30:60,4),age2=rep((30:60)^2,4),own=rep(rep(c(0,1),each=31),2),kids=rep(c(TRUE,FALSE),each=62), HHincome=40,college=TRUE,wealth=50)
	prd <- cbind(prd,as.data.frame(predict(mD,newdata=prd,type="response")))
	pl=ggplot(prd,aes(x=age)) + geom_line(aes(y=response,color=factor(own))) + geom_ribbon(aes(ymin=response-SE,ymax=response+SE,color=factor(own)),alpha=0.3) + facet_grid(~kids)
	
	# if (!is.null(path)){
	# 	texreg(list(m),file=file.path(path,"S2S-probit.tex"),digits=3,table=FALSE,dcolumn=TRUE,booktabs=TRUE,use.packages=FALSE)
	# 	texreg(list(mD),file=file.path(path,"D2D-probit.tex"),digits=3,table=FALSE,dcolumn=TRUE,booktabs=TRUE,use.packages=FALSE)
	# }
	return(list(S2S=m,D2D=mD))
}


#' Get Movers from full data
#'
getMovers <- function(d){

	movtmp <- d[S2S.mn==TRUE,list(upid=unique(upid))]
	setkey(d,upid)
	movers <- d[ movtmp[,upid] ]

	return(movers)
}


































