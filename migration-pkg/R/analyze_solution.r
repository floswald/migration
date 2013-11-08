


#' compute Probability of Moving
#'
#' Adds the probability of moving
#' to each conditional value function object
#' in the blitz list.
#' @param b list with results from \code{\link{dev8}}
#' @return list b with added probs of moving
#' @references for the mean of the gumbel distribution see \url{http://en.wikipedia.org/wiki/Gumbel_distribution}. 
#' That's where the euler-mascheroni constant is needed.
ProbMoving <- function(b) {

	ec <- 0.5772	# the euler-mascheroni constant

	if (length( grep("v_loc_",names(b$Values)) ) == 0 ) {
		error('you must export a field "v_loc_xxxx" for each discrete choice xxxx')
	}

	# initiate zero arrays
	sell <- buy <- rent <- stay <- array(0,dims(b$Values$v_loc_stay))

	# get vbar, which is the mean over logit location shocks
	# vbar.own needs to be computed anyway.

	# calculate probabilities on each state





}
