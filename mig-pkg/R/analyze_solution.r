



# Plot Tenure Choice Function
# 
# @param m list with model description
# @param b list of results from \code{\link{dev8}}
plot.TenureChoice <- function( m, b , path="~/git/migration/output/mgraphs"){

	X <- cbind(expand.grid(assets=1:m$nA,price=m$grids$p[,3]),as.numeric(b$policies$TenureOwn[ ,1, ,10,10,10]))
	names(X)[3] <- c("Tenure")
	X$Tenure <- factor(X$Tenure,labels=c("Stay","Sell"))
	ggplot(X,aes(x=assets,y=price,z=Tenure,fill=Tenure)) + geom_tile() + scale_fill_manual(values=c("red","blue")) + theme_bw() + ggtitle('Owner Tenure policy in Location 3, age 10') + scale_x_discrete(breaks=c(1,10,20,30,40,50),labels=paste(round(m$grids$a[c(1,10,20,30,40,50)],1)))

	ggsave(file=file.path(path,"TenureOwn.pdf"))

	# Renters who stay
	# ======

	apos <- m$grids$a[m$grids$a>0]
	X <- cbind(expand.grid(assets=1:length(apos),
						   price=m$grids$p[,3],
						   location=factor(c(1,10))),
			  c(as.numeric(b$policies$TenureRent[ which(m$grids$a>0),1, ,1,1,1]),as.numeric(b$policies$TenureRent[ which(m$grids$a>0),1, ,10,10,1])))
	names(X)[4] <- c("Tenure")
	X$Tenure <- factor(X$Tenure,labels=c("Rent","Buy"))
	ggplot(X,aes(x=assets,y=price,z=Tenure,fill=Tenure)) + geom_tile() + scale_fill_manual(values=c("red","blue"))  + ggtitle('Renter Tenure policy when staying in Locations 1 and 10, age 1') + scale_x_discrete(breaks=c(1,5,10,15,20,25),labels=paste(round(apos[c(1,5,10,15,20,25)],1))) + theme_bw() + facet_wrap(~location)

	ggsave(file=file.path(path,"TenureRent1_10.pdf"))
	
	# movers
	X <- cbind(expand.grid(assets=1:length(apos),
						   price=m$grids$p[,3],
						   moving.to=factor(c(1,10),labels=paste0("moving to ",c(1,10)))),
			  c(as.numeric(b$policies$TenureRent[ which(m$grids$a>0),1, ,1,1,1]),as.numeric(b$policies$TenureRent[ which(m$grids$a>0),1, ,1,10,1])))
	names(X)[4] <- c("Tenure")
	X$Tenure <- factor(X$Tenure,labels=c("Rent","Buy"))
	ggplot(X,aes(x=assets,y=price,z=Tenure,fill=Tenure)) + geom_tile() + scale_fill_manual(values=c("red","blue"))  + ggtitle('Renter Tenure policy in loc 1 when moving, age 1') + scale_x_discrete(breaks=c(1,5,10,15,20,25),labels=paste(round(apos[c(1,5,10,15,20,25)],1))) + theme_bw() + facet_wrap(~moving.to)
	ggsave(file=file.path(path,"TenureRentMove1_10.pdf"))

}




# plot value functions
# 
# plot value functions at
# two sets of indices for 
# owner and renter state. Plots at 3 locations.
# @param m list with model description
# @param b list of results from \code{\link{dev8}}
plot.vfuns <- function( m, b , path="~/git/migration/output/mgraphs"){

	# plot asset vs V(asset) at different slices location
	# keeping fixed asset and location dimension
	slice <- c(1,1,1)
	#     slice <- sapply(m$dimshere[-c(1,4)],function(x) sample(1:x,size=1))


	locs <- c(1,5,10)
	d <- expand.grid(assets=m$agrid,where=factor(locs),type=c("Vown","Vrent","EVown","EVrent"))
	d$value <- c(as.numeric(b$Values$Vown[ ,slice[1],slice[2],locs,slice[3] ]), 
				 as.numeric(b$Values$Vrent[ ,slice[1],slice[2],locs ,slice[3] ]),
				 as.numeric(b$Values$EVown[ ,slice[1],slice[2],locs ,slice[3]]), 
				 as.numeric(b$Values$EVrent[ ,slice[1],slice[2],locs ,slice[3]]))

	d[d$assets<0 & (d$type=="Vrent"|d$type=="EVrent"), ]$value <- NA
	
	ggplot(d,aes(x=assets,y=value,color=where,group=where)) + geom_line(size=1.1) + facet_wrap(~type,scales="free_y") + theme_bw() + ggtitle(sprintf('V(here) = max_{h=0,1} v(here,h)\nat y=%g, p=%g, age=%g',slice[1],slice[2],slice[3])) 
	
	ggsave(file.path(path,"vfuns1.pdf"))
	
	slice <- c(m$nY,m$nP,m$nT-1)
	
	d <- expand.grid(assets=m$agrid,where=factor(locs),type=c("Vown","Vrent","EVown","EVrent"))
	d$value <- c(as.numeric(b$Values$Vown[ ,slice[1],slice[2],locs ,slice[3] ]), 
				 as.numeric(b$Values$Vrent[ ,slice[1],slice[2],locs ,slice[3] ]),
				 as.numeric(b$Values$EVown[ ,slice[1],slice[2],locs ,slice[3]]), 
				 as.numeric(b$Values$EVrent[ ,slice[1],slice[2],locs ,slice[3]]))

	d[d$assets<0 & (d$type=="Vrent"|d$type=="EVrent"), ]$value <- NA
	
	ggplot(d,aes(x=assets,y=value,color=where,group=where)) + geom_line(size=1.1) + facet_wrap(~type,scales="free_y") + theme_bw() + ggtitle(sprintf('V(here) = max_{h=0,1} v(here,h)\nat y=%g, p=%g, age=%g',slice[1],slice[2],slice[3])) 
 
	
	ggsave(file.path(path,"vfuns2.pdf"))

}


# plot income, price and amenity grids
#
# @param m list with model description
plot.ypgrids <- function( m, path="~/git/migration/output/mgraphs" ){

	trellis.device(device=pdf,file=file.path(path,"movecost.pdf"))
	print(wireframe(x=m$R_move.cost,row.values=1:m$nL,column.values=1:m$nL,drape=TRUE,scales=list(arrows=FALSE),aspect=c(1,1),main="Cost of Moving from Here to There",xlab="Here",ylab="There",zlab="utils"))
	dev.off()

	df = data.frame(location=factor(1:m$nL))
	lims = apply(m$grids$p,2,range)
	limits=aes(ymax=lims[2,],ymin=lims[1,])
	ggplot(df,aes(x=location)) + geom_errorbar(limits,size=1)  + scale_y_continuous(name="house prices") + ggtitle("House price setup")	+ theme_bw()
	ggsave(file.path(path,"prices.pdf"))
	
	df = data.frame(location=factor(1:m$nL))
	lims = apply(m$grids$y,2,range)
	limits=aes(ymax=lims[2,],ymin=lims[1,])
	ggplot(df,aes(x=location)) + geom_errorbar(limits,size=1)  + scale_y_continuous(name="income") + ggtitle("Income setup")	+ theme_bw()
	ggsave(file.path(path,"income.pdf"))

}



#' Plot probability of moving
#' 
#' Plot the conditional choice probabilities of moving from locatino
#' j to k. this is the function rho.
#' @param m model list
#' @param b blitz solution list
#' @param at at which slice to plotpath="~/git/migration/output/mgraphs/"
#' @param path="~/git/migration/output/mgraphs/"
plot.CCPmoving <- function( m, b, path="~/Dropbox/mobility/output/model/solution" ){


	tmp <- apply(b$policies$rho_own[ , , , , ,1],c(4,5),mean)	# calculate mean prob over all states given here,there
	trellis.device(device=pdf,file=file.path(path,"CCP_owner_wire.pdf"))
	print(	wireframe(tmp,scales=list(arrows=FALSE),xlab="here",ylab="there",zlab="Probability",screen=list(z=230,x=-60),main="Aggregate Moving Probability Owners at age 1"))
	#print(wireframe(b$policies$rho_own[ ,3, ,1,2,1],scales=list(arrows=FALSE),aspect=c(1,1),drape=TRUE,screen=list(z=30,x=-60),main="Owners Probability of moving from 1 to 2",row.values=m$grids$a,column.values=1:m$nP,xlab="assets",ylab="prices",zlab="Probability"))
	dev.off()

	# print image of that
	pdf(file=file.path(path,"CCP_owner_image.pdf"))
	image(z=tmp,x=1:10,y=1:10,xlab="here",ylab="there",main="moving tendencies",col=heat.colors(30))
	dev.off()

	pdf(file=file.path(path,"CCP_owner_image2.pdf"))
	image(z=tmp,x=1:10,y=1:10,xlab="here",ylab="there",main="moving tendencies",col=heat.colors(30))
	abline(v=6,lwd=2)
	abline(h=6,lwd=2)
	dev.off()

	# print probs over age
	tmp <- apply(b$policies$rho_own,c(4,5,6),mean)	# calculate mean prob over all states given here,there,age
	t1 <- wireframe(tmp[ , ,1],scales=list(arrows=FALSE),xlab="here",ylab="there",zlab="",screen=list(z=230,x=-60),zlim=c(0,0.5),main="age = 1")
	t2 <- wireframe(tmp[ , ,5],scales=list(arrows=FALSE),xlab="here",ylab="there",zlab="",screen=list(z=230,x=-60),zlim=c(0,0.5),main="age = 5")
	t3 <- wireframe(tmp[ , ,15],scales=list(arrows=FALSE),xlab="here",ylab="there",zlab="",screen=list(z=230,x=-60),zlim=c(0,0.5),main="age = 15")
	t4 <- wireframe(tmp[ , ,29],scales=list(arrows=FALSE),xlab="here",ylab="there",zlab="",screen=list(z=230,x=-60),zlim=c(0,0.5),main="age = 29")
	trellis.device(device=pdf,file=file.path(path,"CCP_owner_wire_age.pdf"))
	print(t1,split=c(1,1,2,2), more=TRUE)
	print(t2,split=c(2,1,2,2), more=TRUE)
	print(t3,split=c(1,2,2,2), more=TRUE)
	print(t4,split=c(2,2,2,2))
	dev.off()

	# in another way
	df <- data.frame(expand.grid(here=factor(1:m$nL),there=factor(1:m$nL),age=paste0("age:",1:m$nT)),prob=as.numeric(tmp))
	ggplot(subset(df,age %in% c("age:1","age:10","age:29")),aes(x=there,y=prob,color=here,group=here)) + geom_line(size=1) + facet_wrap(~age) + theme_bw()
	ggsave(file.path(path,"CCP_owner_ggplot_age.pdf"),width=10,height=6)




	X  <- data.frame(expand.grid(assets=m$grids$apos,location=factor(1:m$nL)),probability=as.numeric(b$policies$rho_rent[m$idx$apos,1,4 ,1, ,1]))
	ggplot(X,aes(x=assets,color=location,y=probability)) + geom_line(size=1) + ggtitle('Renter probability of moving from 1 to x, age 1')  + theme_bw()
	ggsave(file.path(path,"CCP_renter_moving.pdf"))

	if (!m$Zero$amenity & !m$Zero$movecost){
		X <- data.frame(move.to=1:m$nL,probability=apply(b$policies$rho_own[ ,1, ,1, ,10],3,mean))
		ggplot(X,aes(x=move.to,y=probability)) + geom_point(size=3) + geom_path(size=1) + scale_x_discrete(breaks=1:10) + theme_bw() + ggtitle('Mean Probability of moving to location x when at location 1\nmean taken over all asset and price values')
		ggsave(file.path(path,"CCPmoveing.pdf"))
	}

	fname <- paste0("CCPmoving_ZeroMovecost_",paste(m$Zero$movecost),"_ZeroAmenity_",paste(m$Zero$amenity),".pdf")

	if (!m$Zero$amenity & m$Zero$movecost){

		X <- data.frame(move.to=1:m$nL,probability=apply(b$policies$rho_own[ ,1, ,1, ,10],3,mean))
		ggplot(X,aes(x=move.to,y=probability)) + geom_point(size=3) + geom_path(size=1) + scale_x_discrete(breaks=1:10) + theme_bw() + ggtitle(sprintf('Owner Prob of Moving with\nZero.MoveCost %s and Zero.Amenity %s',Zero.MoveCost,Zero.Amenity))
		ggsave(file.path(path,fname))

	}

	if (m$Zero$amenity & m$Zero$movecost){
		X <- data.frame(move.to=1:m$nL,probability=apply(b$policies$rho_own[ ,1, ,1, ,10],3,mean))
		ggplot(X,aes(x=move.to,y=probability)) + geom_point(size=3) + geom_path(size=1) + scale_x_discrete(breaks=1:10) + theme_bw() + ggtitle(sprintf('Owner Prob of Moving with\nZero.MoveCost %s and Zero.Amenity %s',Zero.MoveCost,Zero.Amenity))

		ggsave(file.path(path,fname))
	}
	

	if (m$Zero$amenity & !m$Zero$movecost){
		X <- data.frame(move.to=1:m$nL,probability=apply(b$policies$rho_own[ ,1, ,1, ,10],3,mean))
		ggplot(X,aes(x=move.to,y=probability)) + geom_point(size=3) + geom_path(size=1) + scale_x_discrete(breaks=1:10) + theme_bw() + ggtitle(sprintf('Owner Prob of Moving with\nZero.MoveCost %s and Zero.Amenity %s',Zero.MoveCost,Zero.Amenity))
		ggsave(file.path(path,fname))
	}


	#X  <- data.frame(expand.grid(assets=m$grids$a,location=factor(1:m$nL)),probability=as.numeric(b$policies$rho_own[,1,4 ,1, ,1]))
	#ggplot(X,aes(x=assets,color=location,y=probability)) + geom_line(size=1) + ggtitle('Renter probability of moving from 1 to x, age 1')  + theme_bw()

}



