



#' Sipp Summary Stats
#'
#'	 load("~/Dropbox/mobility/SIPP/Sipp_aggby_age.RData")
# Sipp.SumStats <- function(d){

# 	s <- d[D2D<2,list(Children=weighted.mean(numkids,HHweight,na.rm=T),Income=weighted.mean(HHincome,HHweight,na.rm=T),Age=weighted.mean(age,HHweight,na.rm=T),Wealth=weighted.mean(wealth,HHweight,na.rm=T),Equity=weighted.mean(home.equity,HHweight,na.rm=T),College=weighted.mean(college,HHweight,na.rm=T),Own=weighted.mean(own,HHweight,na.rm=T)),by=D2D]

# 	m <- t(s)
# 	m <- m[-1,]
# 	colnames(m) <- c("Stay","Move")

# 	print(xtable(m,digits=2),file="~/Dropbox/mobility/output/data/sipp/stats.tex")
# 	return(m)

# }

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

	# linear prob model move ~ age
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

    # CAUTIONN!
    # moving zero times in SIPP data means not moving over 4 years. this is not what you expect and also not 
    # what you compute in the model.
    # you want lifetime moves.
	# r$moves0 <- nmoves[,list(moment="moved0",data_value=weighted.mean(moved0,HHweight), data_sd=sqrt(wtd.var(moved0,HHweight)))]
	# r$moves1 <- nmoves[,list(moment="moved1",data_value=weighted.mean(moved1,HHweight), data_sd=sqrt(wtd.var(moved1,HHweight)))]
	# r$moves2 <- nmoves[,list(moment="moved2",data_value=weighted.mean(moved2,HHweight), data_sd=sqrt(wtd.var(moved2,HHweight)))]
	# those numbers from kennan and walker page 39
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



Sipp.own_in_j_rent_in_k <- function(){
	data(Sipp_aggby_NULL,envir=environment())
	mv <- merged[D2D==TRUE,list(upid=unique(upid))]
	setkey(mv,upid)
	mvs <- merged[mv]
	setkey(mvs,upid,timeid)

	# get "other equity" in period after move
	mvs[,other_eq_lead := mvs[list(upid,timeid+1)][["RE.equity.other"]] ]

	# how many movers have more "real estate equity other than your home"
	# after move than before?
	# answers the question of "how many movers keep their house"

	# get correlation between other equity today and tomorrow for movers
	eqcor <- mvs[D2D==TRUE,cor(RE.equity.other,other_eq_lead,use="complete.obs")]

	# get count of people with other equity before and after move. 
	# if movers keep house, that count should go up.
	eq_before_after <- mvs[D2D==TRUE,list(other.eq.before=sum(RE.equity.other!=0.0),other.eq.after=sum(other_eq_lead!=0.0,na.rm=T))]

	# whats the percentage of movers who had 0 other equity before move, that now do have positive other RE equity?
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

	pdf(file="~/Dropbox/mobility/output/data/sipp/move_2_landlord_2.pdf",width=10,h=7)
	print(pl$mv2)
	dev.off()


	# whats the percentage of movers who had 0 other equity before move, that now do have positive other RE equity?
	num.movers <- nrow(mv)
	r3p = data.frame(t(eq_robust3))
	names(r3p) <- "otherRE"
	rownames(r3p) = NULL
	r3p$months_after <- seq(0,nrow(r)-1,le=nrow(r))
	r3p$percent <- 100*r3p$otherRE / num.movers

	pl$mv3 <- ggplot(r3p,aes(x=months_after,y=percent)) + geom_point(size=3) + geom_line() + theme_bw() + ggtitle("Percentage of movers with any equity in other real estate") + scale_y_continuous(name="%") + scale_x_continuous(name="months after move",breaks=seq(0,nrow(r)-1,le=nrow(r))) 
	pdf(file="~/Dropbox/mobility/output/data/sipp/move_2_landlord.pdf",width=10,h=7)
	print(pl$mv3)
	dev.off()
	return(pl)

}



Sipp.movers_wage_residual_plots <- function(path="~/Dropbox/mobility/output/data/sipp"){

	# you call the residual u in the code
	# but z in the paper.

	# get monthly sipp data
	data(Sipp_aggby_NULL,envir=environment())

	# get movers
	mv <- merged[D2D==TRUE,list(upid=unique(upid))]
	setkey(mv,upid)
	setkey(merged,upid,timeid)
	mvs <- merged[mv]

	mvs <- copy(mvs[HHincome > 0])

	# log wage = beta0 + state +  beta1  *age + beta2*college + u

	# get wage residual
	mvs[,u := resid(lm(log(HHincome) ~ factor(Division) + poly(age,degree=3,raw=T) + college + numkids + sex + tmetro))]

	# aim: get cor( u(t), u(t+1) ) when move happened in t
	mvs[,u_plus1 := mvs[list(upid,timeid+1)][["u"]] ]
	mvs[,u_minus1 := mvs[list(upid,timeid-1)][["u"]] ]

	cor0_1minus = mvs[D2D==TRUE,cor(u,u_minus1,use="complete.obs")]
	cor0_1plus  = mvs[D2D==TRUE,cor(u,u_plus1,use="complete.obs")]

	# get rank correlation
	# absolute rank
	mvs[,R_u := rank(u)]
	# percentage rank
	mvs[,pR_u := rank(u) / nrow(mvs)]
	mvs[,R_u_plus1 := mvs[list(upid,timeid+1)][["R_u"]] ]
	mvs[,R_u_minus1 := mvs[list(upid,timeid-1)][["R_u"]] ]
	mvs[,pR_u_plus1 := mvs[list(upid,timeid+1)][["pR_u"]] ]
	mvs[,pR_u_minus1 := mvs[list(upid,timeid-1)][["pR_u"]] ]

	# rank difference
	mvs[,p_rankdiff := pR_u_plus1 - pR_u]
	mvs[,rankdiff := R_u_plus1 - R_u]
	mvs[,p_rankdiff_0 := pR_u - pR_u_minus1]	# in period before move
	mvs[,rankdiff_0 := R_u - R_u_minus1]		# in period before move

	# conditional on moving, what is correlation of those two variables?
	Rcor0_1minus = mvs[D2D==TRUE,cor(R_u,R_u_minus1,use="complete.obs")]
	Rcor0_1plus  = mvs[D2D==TRUE,cor(R_u,R_u_plus1,use="complete.obs")]

	# stats table
	stab <- matrix(c(cor0_1minus,cor0_1plus,Rcor0_1minus,Rcor0_1plus),byrow=T,c(2,2))
	rownames(stab) <- c("Correlation","Rank Correlation")
	colnames(stab) <- c("$cor(z_t,z_{t-1})$","$cor(z_{t+1},z_t)$")

	print(xtable(stab),sanitize.text.function=function(x){x},file=file.path(path,"z_corrtab.tex"),floating=FALSE,booktabs=TRUE)

	# plot quantiles and density

	m2D <- data.frame(mvs[D2D==TRUE,quantile(pR_u,seq(0.05,0.95,le=100),na.rm=T)])
	names(m2D)[1] <- "rank(t)"
	m2D$quantile <- seq(0.05,0.95,le=100)
	m2D$rankt1 <- mvs[D2D==TRUE,quantile(pR_u,seq(0.05,0.95,le=100),na.rm=T)]
	names(m2D)[3] <- "rank(t+1)"

	m <- data.frame(mvs[D2D==TRUE,quantile(p_rankdiff_0,seq(0.05,0.95,le=100),na.rm=T)])
	names(m)[1] <- "before: rank(z(t)) - rank(z(t-1))"
	m$quantile <- seq(0.05,0.95,le=100)
	m$rankdiff <- mvs[D2D==TRUE,quantile(p_rankdiff,seq(0.05,0.95,le=100),na.rm=T)]
	names(m)[3] <- "after: rank(z(t+1)) - rank(z(t))"

	mm <- melt(m,id.vars="quantile")

	p <- list()

	p$quantiles <- ggplot(mm,aes(x=quantile,y=value)) + facet_wrap(~variable) + geom_point() + theme_bw() + scale_x_continuous(breaks=c(0.1,0.25,0.5,0.75,0.9)) + scale_y_continuous(name="rank difference (rank in [0,1])") + ggtitle("Quantiles of Mover residual rank difference before and after move in t")

	mm2 <- melt(mvs[D2D==TRUE,list(p_rankdiff_0,p_rankdiff)])
	mm2[,var := variable]
	mm2[variable=="p_rankdiff",var := "after: rank(z(t+1)) - rank(z(t))"]
	mm2[variable=="p_rankdiff_0",var := "before: rank(z(t)) - rank(z(t-1))"]
	mm2[,variable := NULL]
	setnames(mm2,"var","variable")
	p$densities <- ggplot(subset(mm2,abs(value) > 1e-3),aes(x=value)) + facet_wrap(~variable) + geom_density() + theme_bw() + ggtitle("Densities of Mover residual rank difference before and after move in t")

	# fit a beta dist 
	# normalize data to [0,1]
	mbeta <- mvs[!is.na(p_rankdiff),list(p_rankdiff)]
	mbeta <- mbeta[order(p_rankdiff)]
	mbeta[,nrank := rutils:::linear.map(p_rankdiff,1)]
	 # trim hi and lo
	mbeta[nrank==0,nrank := 0.0001]
	mbeta[nrank==1,nrank := 0.9999]
	# fit a beta distribution
	fit.b = MASS:::fitdistr(mbeta[,nrank],"beta",start=list(shape1=2,shape2=2))
	x.b=rbeta(1e5,fit.b$estimate[[1]],fit.b$estimate[[2]])
	plot(density(x.b))
	# lines(density(x.b),col="red")

	p$beta <- ggplot(subset(mm2,abs(value) > 1e-3 & variable=="p_rankdiff"),aes(x=value)) + geom_density() + theme_bw() 	
	bdf <- data.frame(x=seq(-1,1,le=100),xn = seq(0,1,le=100))
	bdf$dbeta <- dbeta(bdf$xn,shape1=fit.b$estimate[[1]],shape2=fit.b$estimate[[2]])
	p$beta <- p$beta + geom_line(data=bdf,aes(x=x,y=dbeta),color="red")

	# robust
	# ======

	# this is robust to observables, i.e. the distribution is remarkably stable
	mvs[D2D==TRUE,summary(lm(p_rankdiff ~ age + I(age^2) + college+factor(Division) ))]

	# even unconditional on moving or not
	mvs[,summary(lm(p_rankdiff ~ age + I(age^2) + college+factor(Division) ))]


	# save plots
	ggsave(plot=p$quantiles,file=file.path(path,"z_quantiles.pdf"))
	ggsave(plot=p$densities,file=file.path(path,"z_densities.pdf"))


	# but you want the CONDITIONAL distbirution of z(t+1)!!!!!!



	return(p)
}

Sipp.wage_residual_copulas <- function(){

	library(copula)

	data(Sipp_aggby_NULL,envir=environment())

	# stayers
	# =======

	mv <- merged[D2D==TRUE,list(upid=unique(upid))]
	nmv <- merged[,unique(upid)[!unique(upid) %in% mv[,upid]]]
	dat <- merged[upid %in% nmv & HHincome >0]

	setkey(dat,upid,timeid,Division)

	# log wage = beta0 + state +  beta1  *age + beta2*college + u

	divs <- dat[,unique(Division)]

	# get wage residual for each region
	mods <- lapply(divs, function(x) lm(log(HHincome) ~ poly(age,degree=3,raw=T) + college + numkids , data=dat[Division==x]))
	names(mods) <- divs

	resids <- lapply(mods,resid)

	dat[, u := 0.0]
	for (d in divs){
		dat[Division==d,u := resids[[d]]]
	}

	dat[,u_plus1 := dat[list(upid,timeid+1)][["u"]]]
	d <- dat[!is.na(u_plus1),list(upid,timeid,Division,u,u_plus1)]


	cops <- lapply(divs, function(x) mvdc(copula=ellipCopula(family="normal",param=0.1),margins=c("norm","norm"),paramMargins=list(list(mean=0,sd=1.12),list(mean=0,sd=1.12))))
	names(cops) <- divs

	subs <- lapply(divs,function(x) as.matrix(d[Division==x][upid %in% sample(unique(upid),round(0.5*length(unique(upid)))),list(u,u_plus1)]))
	names(subs) <- divs

	fits <- lapply(divs, function(x) fitMvdc(subs[[x]],cops[[x]],start=c(2, 1, 3, 2, 0.5),optim.control=list(trace=10)))
	names(fits) <- divs


	# movers
	# ======

	setkey(mv,upid)
	setkey(merged,upid,timeid)
	mvs <- merged[mv]

	mvs <- copy(mvs[HHincome > 0])

	# log wage = beta0 + state +  beta1  *age + beta2*college + u

	# get wage residual
	mvs[,u := resid(lm(log(HHincome) ~ factor(Division) + poly(age,degree=3,raw=T) + college + numkids + sex + tmetro))]

	# aim: get cor( u(t), u(t+1) ) when move happened in t
	mvs[,u_plus1 := mvs[list(upid,timeid+1)][["u"]] ]
	mvs[,u_minus1 := mvs[list(upid,timeid-1)][["u"]] ]
	dat = mvs[D2D==TRUE,list(u,u_plus1)]
	dat = dat[complete.cases(dat)]

	myMvd = mvdc(copula=ellipCopula(family="normal",param=0.5),margins=c("norm","norm"),paramMargins=list(list(mean=0,sd=1.12),list(mean=0,sd=1.12)))
	mat = as.matrix(dat)
	m_fit=fitMvdc(mat,myMvd,start=c(2, 1, 3, 2, 0.5))

	return(list(stayers=fits,movers=m_fit))
}


Sipp.movers_empstat <- function()


Sipp.movers_wage_residual_copula_and_empstat <- function(path="~/Dropbox/mobility/output/data/sipp"){

	library(copula)

	data(Sipp_aggby_NULL,envir=environment())

	# get movers
	mv <- merged[D2D==TRUE,list(upid=unique(upid))]
	setkey(mv,upid)
	setkey(merged,upid,timeid)
	mvs <- merged[mv]


	mvs <- copy(mvs[HHincome > 0])

	# log wage = beta0 + state +  beta1  *age + beta2*college + u

	# get wage residual
	mvs[,u := resid(lm(log(HHincome) ~ factor(Division) + poly(age,degree=3,raw=T) + college + numkids + sex + tmetro))]

	# aim: get cor( u(t), u(t+1) ) when move happened in t
	mvs[,u_plus1 := mvs[list(upid,timeid+1)][["u"]] ]
	mvs[,u_minus1 := mvs[list(upid,timeid-1)][["u"]] ]
	dat = mvs[D2D==TRUE,list(u,u_plus1)]
	dat = dat[complete.cases(dat)]

	# look at densities
	pl <- list()
	md = melt(dat)
	pl$margins <- ggplot(subset(md,value>-3),aes(x=value)) + geom_density() + facet_wrap(~variable) + theme_bw() + ggtitle("Cross section distribution of residuals before and after move")

	ggsave(plot=pl$margins,filename=file.path(path,"z_margins.pdf"))


	myMvd = mvdc(copula=ellipCopula(family="normal",param=0.5),margins=c("norm","norm"),paramMargins=list(list(mean=0,sd=1.12),list(mean=0,sd=1.12)))
	mat = as.matrix(dat)
	fit=fitMvdc(mat,myMvd,start=c(2, 1, 3, 2, 0.5))
	coefs = coef(fit)
	myc = mvdc(copula=ellipCopula(family="normal",param=coefs["rho.1"]),margins=c("norm","norm"),paramMargins=list(list(mean=coefs["m1.mean"],sd=coefs["m1.sd"]),list(mean=coefs["m2.mean"],sd=coefs["m2.sd"])))

	pdf(file=file.path(path,"z_cop_contour.pdf"))
	contour(myc,dMvdc,xlim=c(-3,3),ylim=c(-3,3));
	dev.off()

	n = 40
	mat <- matrix(0,n,n)
	uvec = seq(-3,3,le=n)
	for (i in 1:n){
		for (j in 1:n){
			mat[i,j] <- dMvdc(c(uvec[i],uvec[j]),myc)
		}
	}

	surf <- lattice:::wireframe(mat,drape=TRUE,scales=list(arrows=FALSE),xlab="z(t)",ylab="z(t+1)",zlab="density")


	pdf(file=file.path(path,"z_cop_surf.pdf"))
	print(surf)
	dev.off()

	return(list(plots=surf,copula=fit))
}

ownership_rates_macro_data <- function(){
	data(Sipp_age,envir=environment())
	d <- merged[age>19 & age<51,list(own=mean(own,na.rm=T),p =.SD[own==TRUE,weighted.mean(hvalue,na.rm=T)], y = mean(CensusMedinc,na.rm=T)),by=list(year,Division)]
	d[,p2y := p / y]
	

}


Sipp.SumStats <- function(path="~/Dropbox/mobility/output/data/sipp"){


	ta <- list()

	# annual moving rate
	# ==================

	data(Sipp_age,envir=environment())

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
	# monthly data
	data(Sipp_aggby_NULL,envir=environment())
	# get empstat before after
	d[,employed_minus := d[list(upid,timeid-1)][["employed"]]]
	d[,employed_plus  := d[list(upid,timeid+1)][["employed"]]]
	ta$emptab <- d[D2D==TRUE,as.matrix(table(employed,employed_plus))]
	ta$p_emptab <- round(ta$emptab / sum(ta$emptab),2)
	rownames(ta$p_emptab) <- c("unemployed $t$","employed $t$")
	colnames(ta$p_emptab) <- c("unemployed $t+1$","employed $t+1$")

	# get nowage before after
	d[,nowage := FALSE]
	d[HHincome<=0, nowage := TRUE]
	d[,nowage_plus := d[list(upid,timeid+1)][["nowage"]] ]
	ta$wagetab <- d[D2D==TRUE,as.matrix(table(nowage,nowage_plus))]
	ta$p_wagetab <- round(ta$wagetab / sum(ta$wagetab),2)
	rownames(ta$p_wagetab) <- c("$w_{t} > 0$","$w_{t} < 0$")
	colnames(ta$p_wagetab) <- c("$w_{t+1} > 0$","$w_{t+1} < 0$")

	# get ownership before after
	d[,own_plus := d[list(upid,timeid+1)][["own"]]]
	ta$owntab <- d[D2D==TRUE & age>19 & age<51, round(prop.table(table(own,own_plus),margin=2),2)]
	rownames(ta$owntab) <- c("Rent today","Own today")
	colnames(ta$owntab) <- c("Rent tomorrow","Own tomorrow")


	# print tables
	print(xtable(ta$own_p2y),floating=FALSE,booktabs=TRUE,dcolumn=TRUE,file=file.path(path,"own_p2y.tex"))
	print(xtable(ta$S2S_percent),floating=FALSE,booktabs=TRUE,dcolumn=TRUE,file=file.path(path,"move_rates.tex"))
	print(xtable(ta$nmv),floating=FALSE,booktabs=TRUE,dcolumn=TRUE,file=file.path(path,"num_moves.tex"))
	print(xtable(ta$home),floating=FALSE,booktabs=TRUE,dcolumn=TRUE,file=file.path(path,"mv_home.tex"))

	print(xtable(ta$p_emptab),floating=FALSE,booktabs=TRUE,dcolumn=TRUE,sanitize.colnames.function=function(x){x},sanitize.rownames.function=function(x){x},file=file.path(path,"emptab.tex"))
	print(xtable(ta$p_wagetab),floating=FALSE,booktabs=TRUE,dcolumn=TRUE,sanitize.colnames.function=function(x){x},sanitize.rownames.function=function(x){x},file=file.path(path,"wagetab.tex"))

	print(xtable(ta$owntab),floating=FALSE,booktabs=TRUE,dcolumn=TRUE,sanitize.colnames.function=function(x){x},sanitize.rownames.function=function(x){x},file=file.path(path,"owntab.tex"))

	return(ta)

}

#' SIPP probit of staying vs moving
#'
#' descriptive evidence
#' @examples
#' load("~/Dropbox/mobility/SIPP/Sipp_aggby_age_svy.RData")
#' pr <- SippProbitMove(d=des,"~/Dropbox/mobility/output/data/sipp")
#' 
#' ## goodness of fit
#' 
#' dat <- copy(des$variables[S2S<2 & complete.cases(des$variables)])
#' dat[,probmove := predict(pr$S2S,type="response")]
#' dat[,pred.move := runif(n=nrow(dat))<probmove]
#' print(dat[,list(actual.moves=sum(S2S),predicted=sum(pred.move))])
#' 
#' dat <- copy(des$variables[D2D<2 & complete.cases(des$variables)])
#' dat[,probmove := predict(pr$D2D,type="response")]
#' dat[,pred.move := runif(n=nrow(dat))<probmove]
#' print(dat[,list(actual.moves=sum(D2D),predicted=sum(pred.move))])
#'
#' ## plot
#' ## predict model
#' prd <- data.frame(age=rep(30:60,2),age2=rep((30:60)^2,2),own=rep(c(0,1),each=31), HHincome=40,home.equity=rep(c(0,61),each=31),duration=3,dkids=0,college=TRUE)
#' prd <- cbind(prd,as.data.frame(predict(pr,newdata=prd,type="response")))
#' pl=ggplot(prd,aes(x=age)) + geom_line(aes(y=response,color=factor(own))) + geom_ribbon(aes(ymin=response-SE,ymax=response+SE,color=factor(own)),alpha=0.3)
#' load("~/Dropbox/mobility/SIPP/Sipp_aggby_age.RData")
#' m=merged[age>29&age<61&college==TRUE,list(proportion.moved = weighted.mean(S2S,HHweight,na.rm=T)),by=list(age,h=factor(own))][order(age)]
#' p <- ggplot(m,aes(age,y=proportion.moved,color=h)) + geom_point(size=2.5) + geom_smooth(formula=y~ns(x,3),method="rlm",size=1) + theme_bw() + ggtitle('Sipp Raw Data: Proportion of Cross-State movers by age') + scale_color_manual(values=c("blue","red"))
#' ggsave(plot=p,file="~/Dropbox/mobility/output/data/sipp/raw-movers.pdf",width=13,height=9,scale=0.6)
SippProbitMove <- function(d,path="~/Dropbox/mobility/output/data/sipp"){

	stopifnot("survey.design" %in% class(d) )
	d$variables[,HHincome := HHincome /100]  # income in 100,000 dollars
	d$variables[,wealth := wealth /100]  # same for wealth

	mD <- svyglm(D2D ~ age + age2 + kids + own + HHincome + wealth + college,x=TRUE, family=binomial(link="probit"),design=subset(d,D2D<2))
	me <- erer:::maBina(mD,digits=4)

	texreg(list(me),file=file.path(path,"D2D-probit.tex"),table=FALSE,dcolumn=TRUE,booktabs=TRUE,use.packages=FALSE,stars=c(0.1,0.05,0.01),custom.model.names="Marginal Effects", custom.coef.names=c("Intercept","Age","Age Squared","Children in HH","Homeowner","Household income","Total wealth","College"),digits = 4)

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


#' Summary Statistics from Sipp
#'
#' @param save file to save
#' @examples
#' load("~/Dropbox/mobility/SIPP/SippFull.RData")
#' z = Sipp.SumStats(merged,"~/Dropbox/mobility/output/data/sipp/sumstats.RData")
# Sipp.SumStats <- function(d,saveto="~/Dropbox/mobility/output/data/sipp/sumstats.RData"){

# 	l <- list()

# 	# monthly state-to-state transitions
# 	l$mD2D <- d[,mean(D2D)]

# 	kv <- c("D2D","D2D","HHincome","numkids","age","wealth","nonh_wealth","home.equity","hvalue","own","mortg.rent","college","saving")

# 	# summary of key vars
# 	l$sum <- summary(d[,kv,with=FALSE])

# 	# monthly state-to-state transitions by current state
# 	l$mD2D_state <- d[,mean(D2D),by=state]

# 	# get movers
# 	movtmp <- d[D2D==TRUE,list(upid=unique(upid))]
# 	setkey(d,upid)
# 	movers <- d[ movtmp[,upid] ]
# 	stayers <- d[ !upid %in%  movtmp[,upid] ]

# 	# get sum stats for movers
# 	means.mov <- movers[,lapply(.SD,mean,na.rm=T),.SDcols=kv]
# 	# means stayers
# 	means <- stayers[,lapply(.SD,mean,na.rm=T),.SDcols=kv]
# 	l$means <- as.data.frame(rbind(means,means.mov))
# 	rownames(l$means) <- c("never.moved","moved")

# 	# number of moves per person
# 	l$num.moves <- movers[,list(moves=sum(D2D,na.rm=T)),by=upid][,table(moves)]

# 	# get to and from
# 	#setkey(movers,yrmnid)
#     #movers[,c("from","to") := list("hi","there")]
# 	#movers[,c("from","to") := get.istate(states=state,imove=D2D),by=upid]

# 	#setkey(movers,upid,yrmnid)
# 	#movers[,c("from","to") := list(c(state[-length(state)],NA), c(state[-1],NA)), by=upid]

# 	# moves back to home
# 	l$moves.home <- movers[D2D==TRUE,list(from.home=sum(from==state.born,na.rm=T)/length(from),
# 											 to.home  =sum(to==state.born,na.rm=T)/length(from),
# 											 to.other =sum(to!=state.born,na.rm=T)/length(from))]

# 	moves <- movers[D2D==TRUE]

# 	# get periods where amove happens and get age, from, to
# 	mvtab <- moves[,list(mid=cumsum(D2D),from=from[D2D],to=to[D2D],age=age[D2D]),by=upid]
# 	setkey(mvtab,from,to)

# 	## get distances data
# 	#data(State_distTable_agg,package="EconData")
      
# 	#setkey(State_distTable_agg,from,to)
# 	#mvtab <- State_distTable_agg[ mvtab ]
# 	l$movers <- movers
# 	l$mvtab <- mvtab


# 	# get location transition matrix of movers
# 	l$trans <- movers[,table(from,to)]

# 	if (!is.null(saveto)){
# 	   sumstats=l
# 		save(sumstats,file=saveto)
# 	}
# 	return(l)
# }


#' Get Movers from full data
#'
getMovers <- function(d){

	movtmp <- d[S2S.mn==TRUE,list(upid=unique(upid))]
	setkey(d,upid)
	movers <- d[ movtmp[,upid] ]

	return(movers)
}


































