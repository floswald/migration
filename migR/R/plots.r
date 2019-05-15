#' Plot Sipp Transition matrix
#'
#' @examples
#' \dontrun{
#' load('~/Dropbox/mobility/SIPP/Sipp_aggby_age.RData')
#' tt <- merged[from!=to,table(from,to)]
#' PlotSippTransitionMatrix(tt)
#' }
PlotSippTransitionMatrix <- function(ttable,path="~/Dropbox/mobility/output/data/sipp"){

	bins <- c(0,1,3,7,10,15,20,25)
	cats <- cut(as.numeric(ttable),breaks=bins,labels=FALSE,right=FALSE)

	pal = getcbPalette(7)

	p <- ggplot(data.frame(expand.grid(from=1:nrow(ttable),to=1:nrow(ttable)),z=factor(cats,labels=c(paste(c(0,1,3,7,10,15)),"> 20"))),aes(x=from,y=to,fill=z)) + geom_tile() + scale_fill_manual(values=pal,name="number\nof movers")

	# add scales
	p <- p + scale_x_continuous(name="from",breaks=1:nrow(ttable),labels=rownames(ttable)) + scale_y_continuous(name="to",breaks=1:nrow(ttable),labels=rownames(ttable)) 

	# add 45deg angle on x names
	p <- p + theme(axis.text.x = element_text(angle=45,vjust=0.5), panel.grid.minor=element_blank()) + ggtitle('SIPP transition matrix')

	if (!is.null(path)){
		pdf(file=file.path(path,"SIPP-transition.pdf"),width=11,height=9)
		print(p)
		dev.off()
	}

	return(p)

}

#' Plot Sipp Migration Rates by Age and Ownership Status
#'
#' @name PlotSippMigrationRates
#' @details Generates a scatter plot of mobility vs age and a plot showing ownership by age.
#' @return Figure 1 in the main text.
#' @param nocollege Boolean whether to subset to no college population.
PlotSippMigrationRates <- function(nocollege=FALSE){

	data(Sipp_age)

	if (nocollege){
		merged <- merged[college==FALSE]
	}

	own = merged[age>20&age<51,list(own=mean(own)*100),by=age][order(age)]
	p0 = ggplot(own,aes(age,y=own)) + geom_ribbon(aes(ymin=0,ymax=own),alpha=0.3,color="blue",fill="blue",size=1) + theme_bw() + scale_y_continuous(name="% own")

	 m=merged[age>20&age<51,list(moved.S2S = weighted.mean(S2S,HHweight,na.rm=T),moved.D2D = weighted.mean(D2D,HHweight,na.rm=T)),by=list(age,h=factor(own))][order(age)]
	 m[,type := "Renter"]
	 m[h==TRUE,type := "Owner"]

	 p1 <- ggplot(m,aes(age,y=moved.S2S*100,color=type,linetype=type)) + geom_smooth(formula=y~splines::ns(x,3),method="rlm",size=1) + geom_point(size=2.5,aes(shape=type),fill="white") + theme_bw() + ggtitle('Sipp Raw Data: Proportion of Cross-State movers by age') + scale_color_manual(values=c("blue","red")) + scale_shape_manual(values=c(21,24))
	 p2 <- ggplot(m,aes(age,y=moved.D2D*100,color=type,linetype=type)) + geom_smooth(formula=y~splines::ns(x,3),method="rlm",size=1) + geom_point(size=2.5,aes(shape=type),fill="white") + theme_bw() + ggtitle('Proportion of Cross-Division movers by age') + scale_color_manual(values=c("blue","red")) + scale_y_continuous(name="% of sample moved") + scale_shape_manual(values=c(21,24)) + theme(plot.title=element_text(vjust=1.0,size=21),legend.position="top",legend.key.width=unit(1.0, "cm"))

	 # add density of owners on top
	vplayout <- function(x, y){
		viewport(layout.pos.row = x, layout.pos.col = y)
	}
	pdf("~/Dropbox/research/mobility/output/data/sipp/raw-moversD2D_v2.pdf",width=7.5,height=6.2)
	 # pdf("~/Dropbox/mobility/output/data/sipp/raw-moversD2D.pdf",width=13,height=9)
	grid.newpage()
	pushViewport(viewport(layout = grid.layout(4, 4))) # a 5 by 5 grid
	print(p2, vp=vplayout(1:3,1:4)) # the main x/y plot will instead spread across most of the grid
	print(p0, vp=vplayout(4,1:4)) # the first density plot will occupy the top of the grid
	dev.off()

	 ggsave(plot=p1,file="~/Dropbox/research/mobility/output/data/sipp/raw-moversS2S_v2.pdf",width=13,height=9,scale=0.6)
	 # ggsave(plot=p2,file="~/Dropbox/mobility/output/data/sipp/raw-moversD2D.pdf",width=13,height=9,scale=0.6)

	 return(list(S2S=p1,D2D=p2))


}



#' Multiple plot function
#
#' ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#' 
#' @param plotlist list of ggplots
#' @param cols Number of columns in layout
#' @param layout matrix specifying the layout. If present, 'cols' is ignored.
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#' @author http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#' @name multiplot
multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL) {

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}