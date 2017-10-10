

load("~/Dropbox/mobility/SIPP/Sipp_aggby_age.RData")
load("~/Dropbox/mobility/output/model/BBL/income-REcoefs.RData")

context('test building of BBL dataset')

test_that("initial conditions dataset is correct",{
	
		  frac <- 0.01	# one percent sample in each state
		  BBL <- list(n=frac,FE=TRUE)

		  d <- buildBBLData(merged,RE.coefs,BBLpars=BBL,saveto=NULL)

		  
		  expect_that( d , is_a( "data.table" ) )

		  # not all NA
		  expect_that( nrow(d[complete.cases(d)]) > 0 , is_true() ) 

		  # there are N x K observations
		  expect_that( d[,length(unique(upid))*length(unique(move.to))] == nrow(d) , is_true() ) 
		  expect_true( attr(d,"type") == "BBLInit" )
		  expect_true( all.equal( attr(d,"BBLpars"), BBL ) )


})



context('test output of CreateBBLData')

test_that('CreateBBLData has correct shape',{


		  frac <- 0.01	# one percent sample in each state
		  BBL <- list(n=frac,FE=TRUE,maxAge=60)

		  RFm <- list(savings=NULL,housing=NULL,location=NULL,RE.coefs=RE.coefs)


		  expect_error( CreateBBLData(data=merged,RFmodels=NULL,BBLpars=BBL) )

		  expect_that( CreateBBLData(data=merged,RFmodels=RFm,BBLpars=BBL), is_a( "data.table") )

})
