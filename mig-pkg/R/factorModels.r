



#' Dynamic Factor Model for Prices
#'
#' estimates a dynamic factor model
#' @examples
#' data(FHFA_states,package="EconData")
#' l = dynPrices(states$qtr)
dynPrices <- function(dat,maxite=50){

	# detrend data
	dat[,detr := residuals(lm(index_sa ~ quarter*state))]

	# demean series
	dat[,y := .SD[,(detr- mean(detr))/sd(detr)], by=state]

	# plot all
	st = dat[,unique(state)]

	dat[state==st[1],plot(Date,y,type="l",ylim=c(-3,3),main="z-score of deviation from linear trend for all states")]
	for (i in st[-1]) dat[state==i,lines(Date,y)]

	# bring into right shape
	m <- reshape(dat[,list(state,quarter,y)],timevar="quarter",idvar="state",direction="wide")
	st <- m[,state]
	ti <- dat[,unique(quarter)]

	m[,state:=NULL]
	m <- as.matrix(m)

	rownames(m) <- st
	colnames(m) <- ti

	# plot for 6 states
	sst <- sample(st,5)

	par(mfcol=c(3,2))
	for (i in sst){
		plot(m[i,],xlab="",ylab="hpi",bty="L",pch=16,col="blue",type="b",main=i)
	}
	par(mfcol=c(1,1))

	# setup model

	# number of hidden factors
	fac <- 3
	Z3.vals <- list(
				   "z11",    0,    0,
				   "z21", "z22",    0,
				   "z31", "z32","z33",
				   "z41", "z42","z43",
				   "z51", "z52","z53",
				   "z61", "z62","z63",
				   "z71", "z72","z73",
				   "z81", "z82","z83",
				   "z91", "z92","z93",
				   "z101","z102","z103",
				   "z111","z112","z113",
				   "z121","z122","z123",
				   "z131","z132","z133",
				   "z141","z142","z143",
				   "z151","z152","z153",
				   "z161","z162","z163",
				   "z171","z172","z173",
				   "z181","z182","z183",
				   "z191","z192","z193",
				   "z201","z202","z203",
				   "z211","z212","z213",
				   "z221","z222","z223",
				   "z231","z232","z233",
				   "z241","z242","z243",
				   "z251","z252","z253",
				   "z261","z262","z263",
				   "z271","z272","z273",
				   "z281","z282","z283",
				   "z291","z292","z293",
				   "z301","z302","z303",
				   "z311","z312","z313",
				   "z321","z322","z323",
				   "z331","z332","z333",
				   "z341","z342","z343",
				   "z351","z352","z353",
				   "z361","z362","z363",
				   "z371","z372","z373",
				   "z381","z382","z383",
				   "z391","z392","z393",
				   "z401","z402","z403",
				   "z411","z412","z413",
				   "z421","z422","z423",
				   "z431","z432","z433",
				   "z441","z442","z443",
				   "z451","z452","z453",
				   "z461","z462","z463",
				   "z471","z472","z473",
				   "z481","z482","z483",
				   "z491","z492","z493",
				   "z501","z502","z503",
				   "z511","z512","z513")
	Z3 <- matrix(Z3.vals,nrow=nrow(m),ncol=fac,byrow=TRUE)
	Q <- B <- diag(1,fac)

	# structure of error matrix on outcome equation
	R = "diagonal and unequal"

	x0 <- U <- A <- "zero"

	# initial variance
	V0 <- diag(5,fac)

	# list to pass to MARSS
	dfa <- list(Z=Z3,A=A,R=R,B=B,U=U,Q=Q,x0=x0,V0=V0)
	cntl.list <- list(maxit=maxite,trace=1)

	mod3 <- MARSS(m,model=dfa,control=cntl.list)


	# two hidden factors
	fac <- 2
	Z2.vals <- list(
				   "z11",    0, 
				   "z21", "z22",
				   "z31", "z32",
				   "z41", "z42",
				   "z51", "z52",
				   "z61", "z62",
				   "z71", "z72",
				   "z81", "z82",
				   "z91", "z92",
				   "z101","z102",
				   "z111","z112",
				   "z121","z122",
				   "z131","z132",
				   "z141","z142",
				   "z151","z152",
				   "z161","z162",
				   "z171","z172",
				   "z181","z182",
				   "z191","z192",
				   "z201","z202",
				   "z211","z212",
				   "z221","z222",
				   "z231","z232",
				   "z241","z242",
				   "z251","z252",
				   "z261","z262",
				   "z271","z272",
				   "z281","z282",
				   "z291","z292",
				   "z301","z302",
				   "z311","z312",
				   "z321","z322",
				   "z331","z332",
				   "z341","z342",
				   "z351","z352",
				   "z361","z362",
				   "z371","z372",
				   "z381","z382",
				   "z391","z392",
				   "z401","z402",
				   "z411","z412",
				   "z421","z422",
				   "z431","z432",
				   "z441","z442",
				   "z451","z452",
				   "z461","z462",
				   "z471","z472",
				   "z481","z482",
				   "z491","z492",
				   "z501","z502",
				   "z511","z512")
	Z2 <- matrix(Z2.vals,nrow=nrow(m),ncol=fac,byrow=TRUE)
	Q <- B <- diag(1,fac)

	# structure of error matrix on outcome equation
	R = "diagonal and unequal"

	x0 <- U <- A <- "zero"

	# initial variance
	V0 <- diag(5,fac)

	# list to pass to MARSS
	dfa <- list(Z=Z2,A=A,R=R,B=B,U=U,Q=Q,x0=x0,V0=V0)
	cntl.list <- list(maxit=maxite)

	mod2 <- MARSS(m,model=dfa,control=cntl.list)

	return(list(mod2,mod3))

}


















































	
