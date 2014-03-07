

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


context("income predictions.")

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
     


test_that("test that homevalues merge is correct",{

	tdat <- makePrediction1(tt$dat,RE.coefs,with.FE=TRUE,State_distMat_agg)
	l    <- mergeHomeValues(tdat)

	yr   <- l[,list(year=unique(year)),by=move.to]
	HV   <- getHomeValues()
	
	HV = HV[State %in% yr[,unique(move.to)] & year %in% yr[,unique(year)]]

	setkey(HV,year,State)
	setkey(l,year,move.to)

	expect_that( l[,list(sd=sd(HValue96,na.rm=T)),by=list(upid,age)][,min(sd) > 0], is_true() , label="Hvalue varies by person and age")
	expect_that( all.equal( l[,unique(HValue96)], HV[,unique(HValue96)] ), is_true() )

})



context("Discrete choice tests.")

# test DTSetChoice
# ================

test_that("DTSetChoice has correct output",{


	tdat <- makePrediction1(tt$dat,RE.coefs,with.FE=TRUE,State_distMat_agg)
	tdat[,choice := FALSE]
	tdat[,stay   := FALSE]

	setkey(tdat,upid,age)
	setkey(tt$mvtab,upid,age)

	# get the movers in the age where they move
	tdat <- 	tdat[tt$mvtab]

	# set their discrete choice to TRUE in the age they moved
	t2 <- DTSetChoice(tdat)

	expect_true( all(t2[move.to == to][, choice]) )
	expect_true( !any(t2[move.to == to][, stay]) )

})


# test mergePredIncomeMovingHist 
# ================v============

test_that("mergePredIncomeMovingHist correct output", {

	tdat <- makePrediction1(tt$dat,RE.coefs,with.FE=TRUE,State_distMat_agg)
	tdat[,choice := FALSE]
	tdat[,stay   := FALSE]

	# values to test:
	l <- mergePredIncomeMovingHist(tdat,tt$mvtab)


	# control values:
	setkey(tdat,upid,age)
	setkey(tt$mvtab,upid,age)

	# get the movers in the age where they move
	mdat <- 	tdat[tt$mvtab]
	mdat[,upid2 := paste0(upid,"_",yearmon) ]
	mdat[,c("to","yearmon") := NULL]


	# test: in l, all non-movers can have only one "stay" per age
	# 1) get movers from l

	setkey(l,upid,age)
	setkey(mdat,upid,age)

	# separate movers and non movers
	nonmv.from.l <- l[!J(mdat[,list(upid,age)])]	# NEVER move
	   mv.from.l <- l[   mdat[,list(upid,age)] ]	# move in one of many cases

	# in an age where you are a mover, you cannot be a nonmover
	mv.from.l1 <- mv.from.l[,list(id=unique(upid)),by=age]
	for (i in 1:nrow(mv.from.l1)){

		expect_that( nrow(nonmv.from.l[upid==mv.from.l1[i][["id"]] & age==mv.from.l1[i][["age"]]]), equals( 0 ) )
	}

	# in mover section, there is nobody apart from upid who is moving at age
	setkey(mv.from.l,age,upid)
	expect_that( nrow( mv.from.l[!J(mv.from.l1)] ), equals( 0 ) )

	# in non-mover section, people who move in mv.from.l are stayers
	setkey(nonmv.from.l,age,upid)

	for (i in 1:nrow(mv.from.l1)){

		expect_that( l[upid==mv.from.l1[i][["id"]] & age!=mv.from.l1[i][["age"]] ][choice==TRUE,all(stay)], is_true() )
		expect_that( l[upid==mv.from.l1[i][["id"]] & age!=mv.from.l1[i][["age"]] ][choice==TRUE,all(distance==0)],  is_true(  ) )
		expect_that( l[upid==mv.from.l1[i][["id"]] & age!=mv.from.l1[i][["age"]] ][choice==TRUE,all.equal(state,move.to)], is_true() )

	}


	# in non-mover section, all who never where movers, are always stayers
	setkey(nonmv.from.l,upid)
	expect_that( nonmv.from.l[!J(mv.from.l1[,unique(id)])][choice==TRUE,all(stay)], is_true() )
	expect_that( nonmv.from.l[!J(mv.from.l1[,unique(id)])][choice==TRUE,all.equal(state,move.to)], is_true() )

	# in entire dataset, wherever move.to==
	expect_that( l[move.to==state, all(stay)], is_true() )

})




# test logit predictions
# ================

context("Predictions from logit location choice model")


test_that("makeMovingIndicatorsLogit is correct",{

	tdat <- makePrediction1(tt$dat,RE.coefs,with.FE=TRUE,State_distMat_agg)
	tdat[,choice := FALSE]
	tdat[,stay   := FALSE]

	l <- mergePredIncomeMovingHist(tdat,tt$mvtab)

	# get predicted probs of moving for each upid2
	p <- replicate(n=l[,length(unique(move.to))],runif(n=l[,length(unique(upid2))]))
	colnames(p) <- l[,unique(move.to)[order(unique(move.to))]]
	p <- p / matrix(rowSums(p),nrow(p),ncol(p))

	p <- data.table(p)
	p[,upid2 := l[,unique(upid2)] ]
	
	m <- melt(p,id.vars="upid2",variable.factor=FALSE,verbose=TRUE,variable.name="move.to",value.name="prediction")

	setkey(m,upid2,move.to)
	setkey(l,upid2,move.to)

	# compute test values
	m <- simulateMoveLogit(m)
	r <- copy(l)

	# add to original data
	l[,sim.move := m[,sim.move] ]
	l[,stay.model := FALSE]
	l[move.to==sim.move & distance==0, stay.model:=TRUE]
	
	l[,stay.data:= FALSE]
	l[distance==0 & choice==TRUE, stay.data := TRUE]

	expect_true( all.equal( l[,stay.data], l[,stay] ), label="original definition of stay and backed out def in stay.data" )


	# test whether "move.to" columns are correct
	l[,move.to.model := NA_character_ ]
	l[move.to==sim.move & distance > 0, move.to.model := sim.move]
	l[,move.to.data := NA_character_ ]
	l[distance > 0 & choice==TRUE, move.to.data:= move.to]

	r <- makeMovingIndicatorsLogit(r,m)

	expect_true( all( l[!is.na(move.to.model),move.to.model] == r[!is.na(move.to.model),move.to.model] ) )
	expect_true( all( l[!is.na(move.to.data),move.to.data] == r[!is.na(move.to.data),move.to.data] ) )

}
)






