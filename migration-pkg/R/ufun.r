

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


