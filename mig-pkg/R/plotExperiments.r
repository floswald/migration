




plot.MortgExper <- function(){
	d <- data.table(read.csv("~/Dropbox/mobility/output/model/data_repo/out_data_jl/expMort_plot.csv"))
	d[,type := "Renter"]
	d[own=="true",type := "Owner"]
	mv = subset(d,realage<41,select=c(type,realage,move,policy))

	pl <- list()
	pl$move = ggplot(mv[policy!="burning_money"],aes(x=realage,y=move,color=policy)) + facet_grid(~type) + geom_line(size=0.8) + theme_bw() + scale_x_continuous("age") + scale_y_continuous("proportion moved") + ggtitle("Removing Mortgage Interest Rate Deduction: Mobility")

	inc <- melt(d[,list(age=realage,type,income,assets,policy)],id.vars=c("type","age","policy"))

	pl$income = ggplot(inc[policy!="burning_money"],aes(x=age,y=value,color=policy,linetype=variable)) + facet_grid(~type) + geom_line(size=0.8) + theme_bw() + scale_x_continuous("age") + scale_y_continuous("proportion moved") + ggtitle("Removing Mortgage Interest Rate Deduction: Income and Assets")

	ggsave(pl$move,file="/Users/florianoswald/Dropbox/mobility/output/model/experiments/MortgageSubsidy/moving.pdf",width=8,height=6)
	ggsave(pl$income,file="/Users/florianoswald/Dropbox/mobility/output/model/experiments/MortgageSubsidy/inc-assets.pdf",width=8,height=6)

	return(pl)
}

MorgExper.table <- function(){
	d <- data.table(read.csv("~/Dropbox/mobility/output/model/experiments/MortgageSubsidy/receipts.csv"))
	d2 <- d[,list(age=realage,per_owner_subsidy=per_owner_subsidy*1000,owner_npv_at_age=npv_at_age*1000,pc_lumpsum=pc_lumpsum*1000)]

	print(xtable(d2,align=c("ll|rr|r")),include.rownames=FALSE,floating=FALSE,booktabs=TRUE,dcolumn=TRUE,file="~/Dropbox/mobility/output/model/experiments/MortgageSubsidy/receipts.tex")


}


# TODO
# got to distinguish by region

plot.shockyp <- function(which,j){

	l=list()

	str = paste0("~/git/migration/data/exp_",which)
	outp = "~/Dropbox/mobility/output/model/experiments/exp_yp"

	if (which=="y" | which=="y3"){
		expr_out = expression(paste("Outflows after shocking ",y[j], " with -30% in 1997"))
		expr_in  = expression(paste("Inflows after shocking ",y[j], " with -30% in 1997"))
	}else{
		expr_out = expression(paste("Outflows after shocking ",p[j], " with -30% in 1997"))
		expr_in  = expression(paste("Inflows after shocking ",p[j], " with -30% in 1997"))
	}

	l$from5 = data.table(read.csv(file.path(str,"from5.csv")))
	from5_own = read.csv(file.path(str,"from5_own.csv"))
	from5_own$type = "Outflow Owners"
	tmp = read.csv(file.path(str,"from5_rent.csv"))
	tmp$type = "Outflow Renters"
	from5_own = data.table(rbind(from5_own,tmp))
	from5_own[,year_1 := NULL]
	l$from5_own = melt(from5_own,c("year","type"))

	# add p2y in each case
	l$from5[,p2y := baseline_p / baseline_y]
	l$from5[,p2y_shock:= shock_p / shock_y]

	# plot outmig
	l$outflow = ggplot(subset(l$from5_own,year>1996),aes(year,y=value,linetype=variable)) + geom_line() + facet_wrap(~type) + ggtitle(expr_out) + theme_bw() + scale_y_continuous(expression(paste("proportion within ",d[5])))

	# make table
	agg = copy(l$from5[year>1996,list(Migration="Outward",base=mean(baseline_move),shock=mean(shock_move))])
	l$to5 = data.table(read.csv(file.path(str,"to5.csv")))
	agg = rbind(agg,l$to5[year>1996,list(Migration="Inward",base=mean(baseline_move),shock=mean(shock_move))])
	l$agg = copy(agg[,percent_change := 100*(shock-base)/base])

	print(xtable(l$agg,digits=4),file=file.path(outp,paste0(which,"_agg.tex")),floating=FALSE,booktabs=TRUE,dcolumn=TRUE,include.rownames=FALSE)


	to5_own = read.csv(file.path(str,"to5_own.csv"))
	to5_own$type = "Inflow Owners"
	tmp = read.csv(file.path(str,"to5_rent.csv"))
	tmp$type = "Inflow Renters"
	to5_own = data.table(rbind(to5_own,tmp))
	to5_own[,year_1 := NULL]
	l$to5_own = melt(to5_own,c("year","type"))
	l$to5_own = rbind(l$to5_own,l$from5[,list(year,type="p2y",variable="normal",value=p2y)])
	l$to5_own = rbind(l$to5_own,l$from5[,list(year,type="p2y",variable="shock",value=p2y_shock)])
	l$to5_own[,type := factor(type,levels=c("Inflow Owners","Inflow Renters","p2y"))]
	l$inflow = ggplot(subset(l$to5_own,year>1996),aes(year,y=value,linetype=variable)) + geom_line() + facet_grid(type~.,scales="free_y")+ggtitle(	expr_in ) + theme_bw()+ scale_y_continuous(expression(paste("proportion outside ",d[5])))

	ggsave(l$outflow,file=file.path(outp,paste0(which,"_out.pdf")))
	ggsave(l$inflow,file=file.path(outp,paste0(which,"_in.pdf")))

	return(l)

}


tabs.MC <- function(){

# half
	h = list()
	h$mv = read.csv("~/git/migration/data/exp_halfMC/mv.csv")
	h$own = read.csv("~/git/migration/data/exp_halfMC/own.csv")
	h$own_mv = read.csv("~/git/migration/data/exp_halfMC/own_mv.csv")

	htab = as.matrix(subset(h$mv,select=c(move_baseline,move_policy)))
	htab = rbind(htab,as.matrix(h$own))
	htab = rbind(htab,c(NA,0.88))
	rownames(htab) <- c("Renter Moving Rate","Owner Moving Rate","Ownership Rate","$\\delta$")
	colnames(htab) <- c("Baseline","half MC")
	h$tab <- htab

	d = list()
	d$mv = read.csv("~/git/migration/data/exp_doubleMC/mv.csv")
	d$own = read.csv("~/git/migration/data/exp_doubleMC/own.csv")
	d$own_mv = read.csv("~/git/migration/data/exp_doubleMC/own_mv.csv")

	htab = as.matrix(subset(d$mv,select=c(move_baseline,move_policy)))
	htab = rbind(htab,as.matrix(d$own))
	htab = rbind(htab,c(NA,1.012))
	rownames(htab) <- c("Renter Moving Rate","Owner Moving Rate","Ownership Rate","$\\delta$")
	colnames(htab) <- c("Baseline","double MC")
	d$tab <- htab


	print(xtable(h$tab,digits=3),file="~/Dropbox/mobility/output/model/experiments/exp_MC/halfMC.tex",floating=FALSE,booktabs=TRUE,dcolumn=TRUE,include.rownames=TRUE,sanitize.rownames.function=function(x){x})
	print(xtable(d$tab,digits=3),file="~/Dropbox/mobility/output/model/experiments/exp_MC/doubleMC.tex",floating=FALSE,booktabs=TRUE,dcolumn=TRUE,include.rownames=TRUE,sanitize.rownames.function=function(x){x})

	out = list(half=h,double=d)
	return(out)
}
