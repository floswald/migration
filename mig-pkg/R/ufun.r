

#' CRRA Utility Function
#'
#' this is a devel function. only used to 
#' check accuracy of C++ production code.
#' the function is defined by 
#' u(c,h) = (1/(1-gamma)) * c^(1-gamma) + theta*h
#' @param cons scalar consumption. must be non-negative.
#' @param gamma scalar CRRA parameter
#' @param theta scalar utility weight of housing. theta = 0 for renters.
R_ufun <- function(cons,gamma,theta) {

	if (cons<0){
		warning("negative consumption. returning NULL")
		return( NULL );
	} else if (gamma!=1) {

		# CRRA utility
		ret <- (1/(1-gamma)) * cons^(1-gamma) + theta

	} else if (gamma==1){

		# log utility
		ret <- log( cons ) + theta

	}
	return( ret )
}


#' CRRA Utility Function with neg Cons penalty
#'
#' this is a devel function. only used to 
#' check accuracy of C++ production code.
#' the function is defined by 
#' u(c) = (1/(1-gamma)) * c^(1-gamma) if c > cstar
#' u(c) = (1/(1-gamma)) * cstar^(1-gamma) + u'(cstar)*(cstar-c) + 0.5*u''(cstar)*(cstar-c)^2 else
#' @param cons scalar consumption. must be non-negative.
#' @param gamma scalar CRRA parameter
#' @param cutoff minimum level of consumption below which approximation is used
#' @return list with utility and gradient
R_ufun_neg <- function(cons,gamma,cutoff) {

	if (cons<cutoff){

		diff       <- cons - cutoff
		tmpu       <- cutoff^(1-gamma)
		dtmpu.dc   <- tmpu / cutoff
		ddtmpu.dcc <- -gamma * dtmpu.dc / cutoff

		util <- (1/1-gamma) * tmpu + dtmpu.dc * diff + 0.5 * ddtmpu.dcc * diff^2
		grad <- dtmpu.dc + ddtmpu.dcc * diff
		ret  <- list(utility=util,gradient=grad)

	} else {

		util <- (1/1-gamma) * cons^(1-gamma)
		grad <- 1/cons^gamma
		ret  <- list(utility=util,gradient=grad)
	}
	return( ret )
}


#' objective function: euler equation
#'
#' devel R euler equation
#' u'(res - 1/R * save) = beta*R*V'(save)
#' @return euler equation value
R_objective <- function(save,res,R,beta,gamma,cutoff,evfun){

	cons <- res - (1/R) * save
	mu <- R_ufun_neg(cons=cons,gamma=gamma,cutoff=cutoff)$grad

	# caution: will use the splinefun currently in scope.
	ev <- evfun( save, deriv=1 )

	return( mu - beta * R * ev )
}



#' make income grid
#'
#' @export
make.income <- function(m,plot=FALSE){

	# for each age/value of agg state, there is a different grid

	l <- list()
	df <- data.frame(x=seq(10000,1000000,le=100))
	qs <- seq(0.05,0.95,le=m$nY)

	for (z in 1:m$nZ){

		v <- matrix(0,m$nT,m$nY)
		for (ir in 1:nrow(v)){

			v[ir, ] <- qlnorm(p=qs, meanlog=m$meanlogsy[ir] + m$mu[z], sdlog=m$sdlogsy[ir])

		}

		v <- v / m$dollar.scale
		l[[z]] <- v
		
	}

	df <- data.frame()
	if (plot) {
		for(z in 1:m$nZ){
			df2 <- data.frame(x=seq(10000,300000,le=300))
			for (ir in 1:nrow(v)){
				df2[,ir+1] <- dlnorm(x=df2$x,meanlog=m$meanlogsy[ir] + m$mu[z], sdlog=m$sdlogsy[ir])
			}
			names(df2) <- c("x",paste("age",1:m$nT))
			df2$agg <- paste("AggregateState",z)
			df <- rbind(df,df2)
		}
		ml = melt(df,c("x","agg"))
		p1 <- ggplot(subset(ml,variable %in% paste("age",c(1,25,30))),aes(x=x,y=value,color=variable)) + geom_line() + facet_wrap(~agg) + ggtitle("Income Distributions at different ages")
		ggsave(p1,"~/Dropbox/mobility/output/model/lnorm_income.pdf")
		p2 <- ggplot(ml,aes(x=x,y=value,color=variable)) + geom_line() + facet_wrap(~agg) + theme(legend.position="none") + ggtitle("Income Distributions at different ages")
		ggsave(p2,"~/Dropbox/mobility/output/model/lnorm_income2.pdf")
		
	}
	return(l)
}

#' plot asset grids
#'
#' growing house price means growing asset grids.
#' @export
plot.assetgrid <- function(m){
	mv        <- melt(m$grids$a)
	names(mv) <- c("asset","age","value")
	ggplot(mv,aes(x=age,y=value)) + geom_point() + ggtitle("asset grids")
	ggsave(file="~/Dropbox/mobility/output/model/asset_grid.pdf")
	
	mv        <- melt(m$grids$apos)
	names(mv) <- c("asset","age","value")
	ggplot(mv,aes(x=age,y=value)) + geom_point() + ggtitle("asset grids")
	ggsave(file="~/Dropbox/mobility/output/model/apos_grid.pdf")
}


#' make a growing price grid
#'
#' taking maximal and minimal price growth rates,
#' constructs grids for each period
#' @return list with aggregate price matrix where row i is the price grid for age i and with a 3D array that has age by price by location prices
make.prices <- function(m,plot=FALSE){

	# aggregate component
	v <- matrix(m$pinit,m$nP,m$nT)
	for (i in 2:m$nT) v[,i] <- seq( from= v[1,i-1] * m$delta[1], to = v[m$nP,i-1] * m$delta[2], length=m$nP )


	l <- list()
	l$agg <- v

	l$locs <- array(0,c(m$nP,m$nT,m$nL))
	# local component is just a linear shift up or down
	for (i in 1:m$nL) l$locs[ , ,i] <- l$agg + m$pfunc[i]


	if (plot){
		mv <- melt(v)
		names(mv) <- c("price","age","value")
		ggplot(mv,aes(x=age,y=value)) + geom_point() + ggtitle(sprintf("aggregate house price supports starting at p1 = %g times median income \n with growth rates low = %g, high = %g.\n Shows eval points in each period.",m$pinit,m$delta[1],m$delta[2]))
		ggsave(file="~/Dropbox/mobility/output/model/agg_price_grid.pdf")

		pdf(file="~/Dropbox/mobility/output/model/lower_bounds_price.pdf")
		matplot(l$locs[1,, ],type="l",ylab="price level",xlab="age",main="bounds on each region's price support",ylim=range(l$locs))
		matlines(l$locs[m$nP,, ])
		dev.off()
	}

	return(l)
}

	

#' make dummy lifecycle
#'
#' generates a lifecycle profile with on 30 periods with
#' a quadratic shape and a max at 2/3
#' of the maximal value of the grid
make.dummyLC <- function(x,scale=0.15){

	hi <- max(x)
	lo <- min(x)

	b1 <- 1
	b2 <- - 0.019
	b3 <- - 0.0002
	z <- rep(0,length(x))

	for (i in 1:length(x)){
		z[i] <- b1*x[i] + b2*x[i]^2 + b3*x[i]^3
	}
	z <- z*scale
	return(z)
}



