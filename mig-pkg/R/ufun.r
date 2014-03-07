


#' makes a Test Dataset of Individuals for income prediction
#'
#' @param n number of inds
#' @param A number of maximal years in panel
#' @param nm number of movers
#' @family testFunctions
#' @examples
#' tt <- makeTestData(10,3,1)
makeTestData <- function(n,A,nm){

	# everybody starts at 30 and lives up to max 60 say
	ages <- 30:60
	yrs <- 1995:2011
	yrmn <- yrs*100 + sample(1:12,size=length(yrs),replace=T)

	# list of num of years in panel for each guy
	times <- data.table(id=1:n,maxages=sample(A,size=n,replace=TRUE))
	which.yrs <- sample(1:length(yrs),size=n,replace=TRUE)
	times$year <- yrs[which.yrs]
	times$yrmn <- yrmn[which.yrs]
	times[times$year+times$maxages>max(yrs),]$year <- max(yrs) - times[times$year+times$maxages>max(yrs),]$maxages

	setkey(times,id)

	id <- c()
	for (i in 1:n){
	for (j in 1:times$maxages[i]){
		id <- c(id,i)
	}}

	age <- c()
	year <- c()
	yrmn <- c()
	for (i in 1:n){
	for (j in 1:times$maxages[i]){
		age <- c(age,ages[j])
		year <- c(year,times[.(i)][,year] + j)
		yrmn <- c(yrmn,times[.(i)][,yrmn] + j)
	}}

	# states
	data(State_distTable_agg,package="EconData")
	st <- as.character(State_distTable_agg[,unique(from)])

	# initial/constant conditions
	init <- data.table(upid=as.character(unique(id)),state=as.character(sample(st,size=n,replace=T)),born=sample(1920:1990,size=n,replace=T),born.here=sample(c(TRUE,FALSE),size=n,replace=T),college=sample(c(TRUE,FALSE),size=n,replace=T),numkids=sample(0:4,size=n,replace=T))

	coyrs = seq(1900,1990,by=20)
	init[,cohort := as.character(coyrs[findInterval(born,coyrs)])]
	coh <- model.matrix(~ cohort -1, init)
	init <- cbind(init,coh)
	setkey(init,upid)

	tt <- data.table( upid=as.character(id), age=age, year=year, yearmon=yrmn)
	tt[,c("logHHincome",
		  "wealth",    
		  "mortg.rent",
		  "age2") := list(runif(nrow(tt)),runif(nrow(tt)),runif(nrow(tt)),age^2)]   


	# initial location
	setkey(tt,upid)
	tt <- tt[init]

	# movers
	setkey(tt,upid,age)
	mv <- as.character(sample(n,size=nm))
	for (i in mv){
		# if guy is only around for 1 year
		if (tt[.(i)][,length(age)==1]) {
			# choose another guy
			mv[mv==i] <- as.character(sample((1:n)[-as.numeric(mv)],size=1))
		}
	}

	mvtab <- data.table(upid=mv,age=0L,yearmon=0,to="NA",key="upid")

	for (i in mv){
		mvage <- tt[.(i)][,sample(age,size=1)]
		mvyrm <- tt[.(i)][,sample(yearmon,size=1)]
		mvat <- tt[.(i)][,mvage:max(age)]
		cst  <- tt[.(i,mvage)][["state"]]
		sst  <- st[-which(st==cst)]
		mvto <- tt[.(i,mvage)][,sample(sst,size=1)]
		tt[.(i,mvat), state := mvto]

		# add to mvtab
		mvtab[.(i),age := mvage]
		mvtab[.(i),yearmon := mvyrm]
		mvtab[.(i),to  := mvto]
	}

	# drop yrmn from big data
	tt[,yearmon := NULL]

	l <- list(dat=tt,movers=tt[.(mv)],mvtab=mvtab)
	return(l)
}


#' makes a Test Dataset of income regression coefs
#'
#' @param te test dataset from \code{\link{makeTestData}}
#' @param X char vector of explanatory variables, including "(Intercept)"
#' @family testFunctions
makeTestREcoefs <- function(te,X=c("(Intercept)","age","I(age^2)","cohort1920","cohort1940","cohort1960","cohort1980")){

	# 1. for each state in te, create a (1,length(X)) vector of 
	#    fixed effect estimates
	# 2. for each upid in each state, create a RE intercept

	st <- te[,unique(state)]
	setkey(te,state)

	l <- list()

	for (i in st){

		tmp <- te[.(i),list(upid=unique(upid))]
		tmp[,intercept := runif(nrow(tmp),min=-1,max=1)]
		setkey(tmp,upid)

		tmpf <- runif(n=length(X))
		names(tmpf) <- X

		tmp[,state := NULL]

		l[[i]] <- list(fixed=tmpf,RE=tmp)

	}
	return(l)
}








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



