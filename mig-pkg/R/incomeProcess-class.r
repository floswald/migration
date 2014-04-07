
#' Incoem Process Class
#' 
IncomeProcess <- setClass("IncomeProcess",
	slots = c(RE.coefs= "list",
		      states  = "character",
		      scales  = "data.table",
		      prices  = "data.table",
		      with.FE = "logical",
		      distmat = "matrix")
	)

#' show method for Income process
setMethod("show",signature = "IncomeProcess",
	definition = function(object){
		cat("object of class:",class(object),"\n")
		cat("number of states:",length(object@RE.coefs),"\n")
		cat("states:\n",object@states,"\n")
		cat("income models are of class:",attr(object@RE.coefs,"type"),"\n")
		cat("Distance matrix has dimension:",dim(object@distmat),"\n")
		cat("Aggregate Division prices: \n")
		print(object@prices)
		cat("\nintercept scale factors: \n")
		print(object@scales)
	})


# generic definitions for IncomeProcess methods
setGeneric("estimate.lme",   function(object)                 standardGeneric ("estimate.lme"))
setGeneric("compute.scales", function(object)                 standardGeneric ("compute.scales"))
setGeneric("setScales<-",    function(object,value)           standardGeneric ("setScales<-"))
setGeneric("setPrices<-",    function(object,value)           standardGeneric ("setPrices<-"))
setGeneric("getScales",      function(object)                 standardGeneric ("getScales"))
setGeneric("getStates",      function(object)                 standardGeneric ("getStates"))
setGeneric("getScale",       function(object,st,which)        standardGeneric ("getScale"))
setGeneric("getPrices",      function(object,st,which)        standardGeneric ("getPrices"))
setGeneric("getPrice",       function(object,st,which)        standardGeneric ("getPrice"))
setGeneric("getIncome",      function(object,st      )        standardGeneric ("getIncome"))
setGeneric("inflate",        function(object,data)            standardGeneric ("inflate"))
setGeneric("inflate.sim",    function(object,data,p)          standardGeneric ("inflate.sim"))
setGeneric("inflate2",       function(object,s,pred.all,tmps) standardGeneric ("inflate2"))


# getters
setMethod("getPrices",signature="IncomeProcess",
	definition = function(object){
		return(object@prices)
	}
	)

setMethod("getScales",signature="IncomeProcess",
	definition = function(object){
		return(object@scales)
	}
	)

setMethod("getStates",signature="IncomeProcess",
	definition = function(object){
		return(object@states)
	}
	)

setMethod("getScale",signature=c("IncomeProcess","character","character"),
	definition = function(object,st,which){
		return(object@scales[state==st][[which]])
	}
	)

setMethod("getPrices",signature=c("IncomeProcess","character","character"),
	definition = function(object,st,which){
		return(object@prices[Division==object@scales[state==st,Division]][[which]])
	}
	)

setMethod("getIncome",signature("IncomeProcess","character"),
	definition = function(object,st){
		x <- getScale(object,st=st,which="yscale") * getPrices(object,st=st,which="y")
		return(x)
	})

setMethod("getPrice",signature("IncomeProcess","character"),
	definition = function(object,st){
		x <- getScale(object,st=st,which="pscale") * getPrices(object,st=st,which="p")
		return(x)
	})

#' estimate linear mixed model income process
setMethod("estimate.lme",signature = "IncomeProcess",
	definition = function(object){
		load("~/Dropbox/mobility/SIPP/Sipp_aggby_age.RData")
		x <- RE.HHincome(dat=merged)
		object@RE.coefs <- x
		return(objectst)
	})


setReplaceMethod(f="setScales",signature="IncomeProcess",
	definition = function(object,value){
		stopifnot( is.data.table(value) )
		stopifnot( all( c("yscale","pscale") %in% names(value)  ) ) 
		object@scales <- value
		return(object)
	})

setReplaceMethod(f="setPrices",signature="IncomeProcess",
	definition = function(object,value){
		# you need one price per division! 
		stopifnot( nrow(value) == 9 ) 
		stopifnot( all( c("Division","p","y") %in% names(value)  ) ) 
		object@prices<- value
		return(object)
	})


# setValidity("IncomeProcess",function(object){
# 	# conditions
# 	if ( nrow(prices) != nrow(object@distmat)) return("prices and distmat have not same dim")
# 	if ( ncol(prices) != 2) return('prices must have two columns')
# 	if ( !all.equal(colnames(prices),c("price","income") ) ) return("price needs to columns named price and income")
# })





setMethod("compute.scales",signature="IncomeProcess",
	definition = function(object){

		# get median incomes in 1996 dollars
		d <- makeDivDifferences()


		# income
		# ======

		# compute division means of that
		dy <- d$income$d[,list(mean.inc=mean(medinc)),by=Division]
		dy[,mean.p := d$price$d[,mean(p),by=Division][["V1"]] ]   
		setkey(dy,Division)

		# for each state in RE.coefs, compute
		# scale = coef[1] / dy[divisionOfState[state]]

		data(US_states,package="EconData")
		US_states[,c(1,2,4,5,6) := NULL,with=FALSE]
		US_states <- US_states[complete.cases(US_states)]

		# add aggregated states to FIPS register
		x         <- data.table(state=c("ME.VT","ND.SD.WY"),Division=c("New England","West North Central"))
		US_states <- rbind(US_states[!state %in% c("ME","VT","ND","SD","WY")],x)

		setkey(US_states,Division)

		dy <- dy[US_states]
		setkey(dy,state)

		for (s in dy[,unique(state)]){
			dy[J(s), yscale := object@RE.coefs[[s]]$fixed["(Intercept)"] / mean.inc]

			if (s %in% c("ME.VT","ND.SD.WY")){
				dy[J(s), pscale := d$price$meanp[state %in% unlist(strsplit(s,"\\.")),mean(p)] / mean.p]

			} else {
				dy[J(s), pscale := d$price$meanp[state==s,p] / mean.p]
			}

		}

		setScales(object) <- dy
		return(object)
	}
)


# this definition you want to inflate all states
# and considering agg sim prices
setMethod("inflate.sim",signature=c("IncomeProcess","data.table","data.table"),
	definition = function(object,data,p){

		# set prices
		setPrices(object) <- p

		# inflate data and return
		l <- list()

		# for all current states
		for (s in object@states){

			# for all guys in s
			tmps <- data[state==s]

			# get id-specific intercept
			# and adjust w.r.t. population intercept

			tmps <- getREintercept(tmps,object@RE.coefs[[s]])

			ll <- inflate2(object,s,pred.all=TRUE,tmps)

			l[[s]] <- ll
			rm(tmps,ll)
			gc()

		}

		l <- rbindlist(l)

		return(l)
	}
)

# this definition you want to inflate all states EXCEPT CURRENT
# YOU DONT HAVE SIMULATED PRICES HERE 
setMethod("inflate",signature=c("IncomeProcess","data.table"),
	definition = function(object,data){

		# inflate data and return
		l <- list()

		# for all current states
		for (s in object@states){

			# for all guys in s
			tmps <- data[state==s]

			# get id-specific intercept
			# and adjust w.r.t. population intercept

			tmps <- getREintercept(tmps,object@RE.coefs[[s]])

			ll <- inflate2(object,s,pred.all=FALSE,tmps)

			l[[s]] <- ll
			rm(tmps,ll)
			gc()

		}

		l <- rbindlist(l)

		return(l)
	}
)

setMethod("inflate2",signature=c("IncomeProcess","character","logical","data.table"),
	definition = function(object,s,pred.all,tmps){

		ll <- list()
		# get model matrix with explanatory variables
		m <- getIncomeM(with.FE=object@with.FE,tmps)

		# if you dont want to predict all,
		# that means you are building the logit
		# estimation dataset for the first time.
		# so i will not fill in the simulated prices
		if(!pred.all){

			ll[[s]] <- copy(tmps)
			ll[[s]][,move.to  := state]
			ll[[s]][,distance := 0]

			
			# predict states other than s
			prst <- object@states[-which(object@states==s)]

			# loop over all states in j
			for (j in prst){

				# get regression coefficients 
				# pertaining to state j

				if (object@with.FE){

					be <- object@RE.coefs[[j]]$fixed
					be <- c(be[1],1,be[-1])	# be = [beta0, 1, beta1, ... betak]

				} else {

					be <- object@RE.coefs[[j]]$fixed
				}

				# copy origin state data.table
				# into slot j of list
				ll[[j]] <- copy(tmps)

				# change origin
				ll[[j]][, state := s ]
				# change destination
				ll[[j]][,move.to := j ]
				# get distance
				ll[[j]][,distance := object@distmat[s,j] ]
				
				# predict income there
				ll[[j]][,logHHincome := myPredict(data=m,beta=be)]

			}

		}	else {

			# predict all states 
			prst <- object@states

			# loop over all states in j
			for (j in prst){

				# get regression coefficients 
				# pertaining to state j

				if (object@with.FE){

					be <- object@RE.coefs[[j]]$fixed
					be <- c(be[1],1,be[-1])	# be = [beta0, 1, beta1, ... betak]

				} else {

					be <- object@RE.coefs[[j]]$fixed
				}

				# copy origin state data.table
				# into slot j of list
				ll[[j]] <- copy(tmps)
				
				# change intercept with 
				# value porportional to current
				# division income index divY
				be[1] <- getIncome(object,j)

				
				# fill in appropriate house value
				ll[[j]][,HValue96 := getPrice(object,j)]
				


				# change origin
				ll[[j]][, state := s ]
				# change destination
				ll[[j]][,move.to := j ]
				# get distance
				ll[[j]][,distance := object@distmat[s,j] ]
				
				# predict income there
				ll[[j]][,logHHincome := myPredict(data=m,beta=be)]

			}

		}
		ll <- rbindlist(ll)
		ll[,c("cohort1920","cohort1940","cohort1960","cohort1980") := NULL]
		attr(ll,"origin") <- s
		attr(ll,"with.FE") <- object@with.FE
		attr(ll,"predicted.all") <- pred.all

		return(ll)
	}
)



# constructor
InitIncomeProcess <- function(coeffile="~/Dropbox/mobility/output/model/BBL/inc-process/income-REcoefs.rds",distmat){
	coefs <- readRDS(file=coeffile)

	# test coefs
	stopifnot( identical(attr(coefs,"type") ,"RE.coefs"))

	# tests that names of distmat and coefs correspond
	stopifnot(names(coefs) %in% colnames(distmat))


	y <- new("IncomeProcess",RE.coefs=coefs,states=rownames(distmat),scales=data.table(),prices=data.table(),with.FE=TRUE,distmat=distmat)

	# compute scales
	y <- compute.scales(y)

	return(y)
}
