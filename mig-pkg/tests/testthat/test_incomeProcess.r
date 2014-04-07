



context("testing the IncomeProcess class")

data(State_distMat_agg,package="EconData")

test_that("constructor throws errors as expected",{

	# create a false RE.coefs list
	cc <- lapply(letters,function(x) return(x))
	tmp <- tempfile("coefs",fileext=".rds")
	saveRDS(cc,file=tmp)


	expect_error( InitIncomeProcess(coeffile = tmp, distmat=State_distMat_agg))

	# create a too short distmat


	expect_error( InitIncomeProcess(distmat=State_distMat_agg[-1,-1]))


	# returns an IncomeProcess

	expect_that( InitIncomeProcess(distmat=State_distMat_agg) , is_a("IncomeProcess") )
})



test_that('setters for prices works correctly',{

	y <- InitIncomeProcess(distmat=State_distMat_agg)

	# make some prices
	p <- data.table(Division=LETTERS[1:9],p=runif(9),y=runif(9))

	setPrices(y) <- p

	expect_that( all.equal(getPrices(y) , p ), is_true() ) 

})




test_that('compute.scales works as expected',{

	# create a RE.coefs list
	ff <- list()
	ff$fixed <- 2
	names(ff$fixed) <- "(Intercept)"

	cc <- lapply(colnames(State_distMat_agg),function(x) return(ff))
	names(cc) <- colnames(State_distMat_agg)

	# set the right attr
	attr(cc,"type") <- "RE.coefs"

	tmp <- tempfile("coefs",fileext=".rds")
	saveRDS(cc,file=tmp)

	# test data
	d <- makeDivDifferences()

	# compute division means of that
	dy <- d$income$d[,list(div.mean=mean(medinc)),by=Division]
	dy[,mean.p := d$price$d[,mean(p),by=Division][["V1"]] ]   
	setkey(dy,Division)

	# for each state in RE.coefs, compute
	# scale = coef[1] / dy[divisionOfState[state]]

	data(US_states,package="EconData")
	US_states[,c(1,2,4,5,6) := NULL,with=FALSE]
	US_states <- US_states[complete.cases(US_states)]

	# add aggregated states to FIPS register
	x         <- data.table(state=c("ME.VT","ND.SD.WY"),Division=c("New England"	,"West North Central"))
	US_states <- rbind(US_states[!state %in% c("ME","VT","ND","SD","WY")],x)
	setkey(US_states,Division)

	dy <- dy[US_states]
	setkey(dy,state)

	dy[,yscale := 2 / div.mean]

	for (s in dy[,unique(state)]){

		if (s %in% c("ME.VT","ND.SD.WY")){
			dy[J(s), pscale := d$price$meanp[state %in% unlist(strsplit(s,"\\.")),mean(p)] / mean.p]
		} else {
			dy[J(s), pscale := d$price$meanp[state==s,p] / mean.p]
		}

	}


	# setup object
	y <- InitIncomeProcess(coeffile = tmp, distmat=State_distMat_agg)

	y <- compute.scales(y)	

	my.scales <- getScales(y)

	expect_that( all.equal( my.scales[,pscale], dy[,pscale]), is_true() )
	expect_that( all.equal( my.scales[,yscale], dy[,yscale]), is_true() )


})
# d <- data.table(expand.grid(upid = 1:10, state = getStates(y))) 
# 	d[,intercept := runif(nrow(d))]
# 	d[,age := sample(20:100,size=nrow(d),replace=TRUE)]
# 	d[,cohort:=sample(c("1920","1940","1960","1980"),size=nrow(d),replace=TRUE)]
# 	d[,age2 := age^2]
# 	d <- cbind(d, model.matrix(~ cohort -1,d))
# 	d[,logHHincome := 0]
# 	d[,HValue96 := 0]

test_that('inflate2 is working correctly without plugging in sim data',{

	# get an object
	y <- InitIncomeProcess(distmat=State_distMat_agg)

	# get a tmps dataset
	# must have list(1,intercept,age,age2,cohort1920,cohort1940,cohort1960,cohort1980)
	# 
	d <- data.table(upid = 1:10, state = "CA") 
	d[,intercept := runif(nrow(d))]
	d[,age := sample(20:100,size=nrow(d),replace=TRUE)]
	d[,cohort:= c("1920","1940","1960","1980")]
	d[,age2 := age^2]
	d <- cbind(d, model.matrix(~ cohort -1,d))
	d[,logHHincome := 0]
	d[,HValue96 := 0]

	# call inflate2
	l <- inflate2(y,"CA",FALSE,d)

	# check if indeed replicated

	expect_that( nrow(d)*length(getStates(y)) == nrow(l), is_true() )

	# check that "CA" has zeros in logHHincoem and Hvalue96
	expect_that( all(l[move.to=="CA",logHHincome] == 0), is_true())

	# everybody else does not have zero income
	expect_that( all(l[!(move.to=="CA"),logHHincome] != 0), is_true())

	# check that nobody got a price assigned
	expect_that( all(l[,HValue96] == 0), is_true())



})


test_that('inflate2 is working correctly WITH sim data',{

	# get an object
	y <- InitIncomeProcess(distmat=State_distMat_agg)

	# get a tmps dataset
	# must have list(1,intercept,age,age2,cohort1920,cohort1940,cohort1960,cohort1980)
	# 
	d <- data.table(upid = 1:10, state = "CA") 
	d[,intercept := runif(nrow(d))]
	d[,age := sample(20:100,size=nrow(d),replace=TRUE)]
	d[,cohort:= c("1920","1940","1960","1980")]
	d[,age2 := age^2]
	d <- cbind(d, model.matrix(~ cohort -1,d))
	d[,logHHincome := 0]
	d[,HValue96 := 0]

	# get a price data.table
	p <- data.table(Division=getScales(y)[,unique(Division)],p=runif(9),y=runif(9))
	setPrices(y) <- p

	# call inflate2
	# with pred.all == TRUE, change values 
	l <- inflate2(y,"CA",TRUE,d)

	# check if indeed replicated

	expect_that( nrow(d)*length(getStates(y)) == nrow(l), is_true() )

	# check that "CA" has not zeros in logHHincoem and Hvalue96
	expect_that( all(l[move.to=="CA",logHHincome] != 0), is_true())

	# check that everybody got their price assigned
	# in the place they move to

	divs <- getScales(y)
	st <- getStates(y)

	for (s in st){

		expect_that( all( l[move.to==s,HValue96] -  p[Division== divs[state==s,Division], p ] * getScale(y,st=s,which="pscale") == 0), is_true() )
	}


})

























