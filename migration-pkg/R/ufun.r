

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

	if (cons<0){

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
R_objective <- function(save,res,R,beta,gamma,cutoff){

	cons <- res - (1/R) * save
	mu <- R_ufun_neg(cons=cons,gamma=gamma,cutoff=cutoff)$grad

	# caution: will use the splinefun currently in scope.
	ev <- splinefun( save, deriv=1 )

	return( mu - beta * R * ev )
}
