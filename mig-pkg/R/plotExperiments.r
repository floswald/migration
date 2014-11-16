




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
