

# test packge functions
library(migration)


# get auxiliary test data

n  <- 100	# number of people
A  <- 10	# max number of ages
nm <- 5		# number of movers
tt <- makeTestData(n,A,nm)

# make income equation coefs
RE.coefs <- makeTestREcoefs(te=tt$dat)

data(State_distMat_agg,package="EconData")



# test getREintercept
# ===================

test_that("getREintercept adds the correct intercept",{

	s       <- sample(names(RE.coefs),size=1)
	tmps    <- tt$dat[state==s]
	
	tmps <- getREintercept(tmps,RE.coefs[[s]])
	expect_that( all.equal( tmps[,unique(intercept)] , RE.coefs[[s]]$RE[,intercept] - RE.coefs[[s]]$fixed[[1]] ), is_true() )

})



# test prediction2 
# ================

test_that("makePrediction2 has correct attributes", {

	stn     <- names(RE.coefs)
	s       <- sample(stn,size=1)
	tmps    <- tt$dat[state==s]
	tmps <- getREintercept(tmps,RE.coefs[[s]])

	with.FE <- TRUE		
	m <- getIncomeM(with.FE,tmps)

	t1 <- makePrediction2(s,RE.coefs,m,with.FE,tmps,State_distMat_agg)

	expect_that( t1, is_a("data.table") )
	expect_equal( attr(t1,"origin"), s )
	expect_equal( attr(t1,"with.FE"), with.FE )
	expect_equal( attr(t1,"pred.for"),  stn[-which(stn==s)])
	
	with.FE <- FALSE
	m <- getIncomeM(with.FE,tmps)

	t1 <- makePrediction2(s,RE.coefs,m,with.FE,tmps,State_distMat_agg)

	expect_that( t1, is_a("data.table") )
	expect_equal( attr(t1,"origin"), s )
	expect_equal( attr(t1,"with.FE"), with.FE )
	expect_equal( attr(t1,"pred.for"),  stn[-which(stn==s)])
	})


test_that("makePrediction2 output is correct", {

	s       <- sample(names(RE.coefs),size=1)
	tmps    <- tt$dat[state==s]
	tmps <- getREintercept(tmps,RE.coefs[[s]])
	with.FE <- TRUE		
	m <- getIncomeM(with.FE,tmps)

	t1 <- makePrediction2(s,RE.coefs,m,with.FE,tmps,State_distMat_agg)
	setkey(t1,state,move.to)

	# if move to
	for (st in t1[,unique(state)]) {

		expect_equal( nrow(t1[.(st)]) / (length(attr(t1,"pred.for")) + 1), nrow(tmps) )
		expect_that( all.equal(t1[.(s,st)][,wealth], tmps[,wealth] ), is_true())
		expect_that( all.equal(t1[.(s,st)][,age], tmps[,age] ), is_true())

		if (st==s){
			expect_that( all(t1[.(s,st)][,logHHincome] == tmps[,logHHincome] ), is_true(), label="st==s")
			expect_that( all(t1[.(s,st)][,move.to] == tmps[,state] ), is_true(), label="st==s")
			expect_that( all(t1[.(s,st)][,distance] == 0 ), is_true(), label="st==s")
		} else {
			expect_that( all(t1[.(s,st)][,logHHincome] != tmps[,logHHincome] ), is_true(), label="st!=s")
			expect_that( all(t1[.(s,st)][,move.to] != tmps[,state] ), is_true(), label="st!=s")
			expect_that( all(t1[.(s,st)][,move.to] == st ), is_true(), label="st!=s")
			expect_that( all(t1[.(s,st)][,distance] == State_distMat_agg[s,st] ), is_true(), label="st!=s")
		}
	}
})




# test prediction1 
# ================

test_that("makePrediction1 has correct size", {


	with.FE <- TRUE		
	t1 <- makePrediction1(tt$dat,RE.coefs,with.FE,State_distMat_agg)

	expect_equal( nrow(t1), nrow(tt$dat)*tt$dat[,length(unique(state))])
})
      

# test DTSetChoice
# ================

test_that("DTSetChoice has correct output",{

	with.FE <- TRUE		
	t1 <- makePrediction1(tt$dat,RE.coefs,with.FE,State_distMat_agg)

	t1[,choice := FALSE]
	setkey(t1,upid,age)
	setkey(tt$mvtab,upid,age)

	t1 <- 	t1[tt$mvtab]

	t2 <- DTSetChoice(t1)


})


# test mergePredIncomeMovingHist 
# ================v============

test_that("mergePredIncomeMovingHist correct output", {



})



