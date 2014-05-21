

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