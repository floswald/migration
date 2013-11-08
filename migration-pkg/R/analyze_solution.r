


#' plot value functions
#' 
#' plot value functions at
#' two sets of indices for 
#' owner and renter state
#' @param m list with model description
#' @param b list of results from \code{\link{dev8}}
plot.vfuns <- function( m, b , path="~/git/migration/output/mgraphs"){

	# plot asset vs V(asset) at different slices location
	# keeping fixed asset and location dimension
	slice <- c(1,1,1)
	#     slice <- sapply(m$dimshere[-c(1,4)],function(x) sample(1:x,size=1))



	d <- expand.grid(assets=m$agrid,where=1:m$nL,type=c("Vown","Vrent","EVown","EVrent"))
	d$value <- c(as.numeric(b$Values$Vown[ ,slice[1],slice[2], ,slice[3] ]), 
				 as.numeric(b$Values$Vrent[ ,slice[1],slice[2], ,slice[3] ]),
				 as.numeric(b$Values$EVown[ ,slice[1],slice[2], ,slice[3]]), 
				 as.numeric(b$Values$EVrent[ ,slice[1],slice[2], ,slice[3]]))

	d[d$assets<0 & (d$type=="Vrent"|d$type=="EVrent"), ]$value <- NA
	
	ggplot(d,aes(x=assets,y=value,color=factor(where),group=factor(where))) + geom_line() + facet_wrap(~type,scales="free_y") + theme_bw() + ggtitle(sprintf('V(here) = max_{h=0,1} v(here,h)\nat y=%g, p=%g, age=%g',slice[1],slice[2],slice[3])) 
	
	ggsave(file.path(path,"vfuns1.pdf"))
	
	slice <- c(m$nY,m$nP,m$nT-1)
	
	d <- expand.grid(assets=m$agrid,where=1:m$nL,type=c("Vown","Vrent","EVown","EVrent"))
	d$value <- c(as.numeric(b$Values$Vown[ ,slice[1],slice[2], ,slice[3] ]), 
				 as.numeric(b$Values$Vrent[ ,slice[1],slice[2], ,slice[3] ]),
				 as.numeric(b$Values$EVown[ ,slice[1],slice[2], ,slice[3]]), 
				 as.numeric(b$Values$EVrent[ ,slice[1],slice[2], ,slice[3]]))

	d[d$assets<0 & (d$type=="Vrent"|d$type=="EVrent"), ]$value <- NA
	
	ggplot(d,aes(x=assets,y=value,color=factor(where),group=factor(where))) + geom_line() + facet_wrap(~type,scales="free_y") + theme_bw() + ggtitle(sprintf('V(here) = max_{h=0,1} v(here,h)\nat y=%g, p=%g, age=%g',slice[1],slice[2],slice[3])) 
 
	
	ggsave(file.path(path,"vfuns2.pdf"))

}


#' plot income, price and amenity grids
#'
#' @param m list with model description
plot.ypgrids <- function( m, path="~/git/migration/output/mgraphs" ){

	trellis.device(device=pdf,file=file.path(path,"income.pdf"))
	print(wireframe(x=m$grids$y,row.values=1:m$nY,column.values=1:m$nL,drape=TRUE,scales=list(arrows=FALSE),aspect=c(1,1),main="Joint distribution of Income and Location",xlab="income",ylab="location",zlab="money"))
	dev.off()

	trellis.device(device=pdf,file=file.path(path,"prices.pdf"))
	print(wireframe(x=m$grids$p,row.values=1:m$nP,column.values=1:m$nL,drape=TRUE,scales=list(arrows=FALSE),aspect=c(1,1),main="Joint distribution of Price and Location",xlab="price",ylab="location",zlab="money"))
	dev.off()

	trellis.device(device=pdf,file=file.path(path,"movecost.pdf"))
	print(wireframe(x=m$R_move.cost,row.values=1:m$nL,column.values=1:m$nL,drape=TRUE,scales=list(arrows=FALSE),aspect=c(1,1),main="Cost of Moving from Here to There",xlab="Here",ylab="There",zlab="utils"))
	dev.off()

}



#' Plot probability of moving
#' 
#' Plot the conditional choice probabilities of moving from locatino
#' j to k. this is the function rho.
#' @param m model list
#' @param b blitz solution list
#' @param at at which slice to plotpath="~/git/migration/output/mgraphs/"
#' @param path="~/git/migration/output/mgraphs/"





