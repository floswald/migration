
library(migration)


# example for dev8 with location attributes
# =========================================

# savings: grid search
# housing: yes/no. adding house price uncertainty now.
# utility: CRRA with housing utility
# location: discrete choice over locations
# locations differ by amenity, price and income distributions
# moving cost: dist(here,there)

# assume for this example that locations differ by amenity, house price and incomce supports

# renter state: V = max( rent, buy )
# owner state: V = max( stay , sell )

nA <- 10L; nY <- 4L; nP <- 3L; nL <- 5L; nT <- 5L

G           <- rouwenhorst(rho=0.9,n=nY,sigma=0.1)$Pmat
Gp          <- rouwenhorst(rho=0.9,n=nP,sigma=0.16)$Pmat
dims        <- c(nA,nY,nP,nL,nL,nT)
names(dims) <- c("a","y","p","here","there","age")

dataR <- list( dims=dims,dimshere = c(nA,nY,nP,nL,nT),
               theta = 0.2,beta=0.95,gamma=1.4,
               myNA=-99,rent=0.05,R=1/(1+0.04),down=0.2,
               G = as.numeric(G),
               Gp = as.numeric(Gp),verbose=1L);
               
idx <- list()
idx$L <- 1:nL
grids <- list()
grids$L <- seq(0.1,2,length=nL)	# cities can be 10% or 200% of baseline city
grids$p <- matrix(seq(1,10,le=nP),nP,nL) + matrix(seq(0,3,le=nL),nP,nL,byrow=T)
grids$y <- matrix(seq(1,3,le=nY),nY,nL) + matrix(seq(0,2,le=nL),nY,nL,byrow=T)
grids$a <- seq(-(1-dataR$down)*max(grids$p),10,length=nA)

# OTHER's statespace: add dimension "here" AND "there".

SS <- data.table(expand.grid(a=grids$a,iy=1:nY,ip=1:nP,here=idx$L,there=idx$L,it=1:nT))
for (yl in 1:nL) SS[here==yl, yhere:= grids$y[iy,yl] ]
for (pl in 1:nL) SS[here==pl, phere:= grids$p[ip,pl] ]
for (yl in 1:nL) SS[there==yl, ythere:= grids$y[iy,yl] ]
for (pl in 1:nL) SS[there==pl, pthere:= grids$p[ip,pl] ]


# We compute available resources at each state (a,y,p,here,there,age) here.
# we code r for 'resources'

SS[,rstay := a + yhere + 0.3*it ]	
SS[,rbuy  := a + yhere + 0.3*it - phere ]
SS[,rsell := a + yhere + 0.3*it - dataR$rent*phere + phere]
SS[,rrent := a + yhere + 0.3*it - dataR$rent*phere ]

# restrictions
# ============

# I enforce restrictions on the state space by assigning a 
# large negative number "myNA" to illegal states.

# final period values:
# in the final period, you are either owner or
# renter. Currently, both have the same final value
# i.e. only a function of assets. 
# * This makes a "repayment mortgage" of the current setup.
# * allowing owners to check out of the model with a + p > 0
#     would imply that they only have to make interest payments 
#     over the lifecycle and can hand back the capital at the end of life
# * this is more like a interest only mortgage

for (ih in 1:nL){
	for (ith in 1:nL){

		# if here is not there
		if (ih !=ith){

				# stayer: 
				SS[here==ih & there==ith, rstay := a + ythere + 0.3*it + phere - pthere]		# 

				# seller:
				SS[here==ih & there==ith, rsell := a + ythere + 0.3*it + phere - pthere*dataR$rent]		 

				# renter:
				SS[here==ih & there==ith, rrent := a + ythere + 0.3*it         - pthere*dataR$rent]		
				
				# buyer:
				SS[here==ih & there==ith, rbuy  := a + ythere + 0.3*it         - pthere  ]		
		}

	}
}


# final period restricions
# ========================

# I use the resources of stay and rent
# to precompute and pass final period 
# values to the routine.
# owner:
SS[it==nT & a>0,rstay := log(a) ]
SS[it==nT & a<0,rstay := dataR$myNA ]

# renter:
SS[a>0 & it==nT, rrent := log(a)    ]

# other restrictions on asset space:
# ----------------------------------

# buyer: cannot have negative assets
SS[a<0 & it!=nT,             rbuy  := dataR$myNA]

# renter: cannot have negative assets
SS[a<0         , rrent := dataR$myNA]

# seller: can have negative assets


# LOCATION statespace
# ===================

# this table holds utility moving costs from here to there.
SS.loc <- data.table(expand.grid(here=grids$L,there=grids$L))
SS.loc[,mcost := abs(here-there)]
move.cost <- SS.loc[,matrix(mcost,nL,nL,)]
rownames(move.cost) <- paste0("here",1:nL)
colnames(move.cost) <- paste0("there",1:nL)

amenity <- rev(grids$L)

# borrowing limits 
# ================

# borrowing limit function for owners: whenever here != there, you are considered an owner moving, so you are constrained
# borrowing limit function for buyers: it does not matter where you are (here irrelevant), you always are constrained
iborrow.own <- array(-1,c(nL,nL,nP))	# set to illegal index
iborrow.buy <- array(-1,c(nL,nP))	
for (here in 1:nL){
	for (there in 1:nL){
		for (ip in 1:nP){
			# owner moving from here to there
			idx <- which(grids$a < -(1-dataR$down)*grids$p[ip,there])
			if (length(idx)>0) iborrow.buy[there,ip] <- max(idx)	# buyer's here location is irrelevant
			if (length(idx)>0 & here!=there ) iborrow.own[here,there,ip] <- max(idx)	# owner is only constrained if he actually moves to buy. if stays here, he is not moving, thus not constrained.
		}
	}
}

iborrow.rent <- max(which(grids$a < 0))	# iborrow.rent + 1 is first legal index

dataR$blim_own <- as.numeric(iborrow.own)
dataR$blim_buy <- as.numeric(iborrow.buy)
dataR$blim_rent <- iborrow.rent 

# tensors of resources at each state
# ==================================

RR <- SS[,array(rrent,c(dataR$dims))]	# rent
RB <- SS[,array(rbuy,c(dataR$dims))]		# buy
RS <- SS[,array(rsell,c(dataR$dims))]	# sell
RO <- SS[,array(rstay,c(dataR$dims))]	# "O" is for "Owner", i.e. "stay"

dataR$resR <- as.numeric(RR)
dataR$resB <- as.numeric(RB)
dataR$resS <- as.numeric(RS)
dataR$resO <- as.numeric(RO)

dataR$MoveCost <- as.numeric(move.cost)
dataR$Amenity  <- amenity
dataR$agrid    <- grids$a

######################################################
# Calculating an R solution to this lifecycle problem
######################################################

Rtime <- proc.time()

# consumption tensors for R
CR <- array(0,c(dataR$dims,nA))	# rent
CB <- array(0,c(dataR$dims,nA))		# buy
CS <- array(0,c(dataR$dims,nA))	# sell
CO <- array(0,c(dataR$dims,nA))	# "O" is for "Owner", i.e. "stay"

xR = array(0,c(dataR$dims,nA))
xB = array(0,c(dataR$dims,nA))
xS = array(0,c(dataR$dims,nA))
xO = array(0,c(dataR$dims,nA))

# upper most envelopes of conditional values: given all the rest, which location?
# ===============================================================================

Vown = array(1,dataR$dimshere)
Vrent = array(2,dataR$dimshere)
# Their expected values
EVown = array(0,dataR$dimshere)
EVrent = array(0,dataR$dimshere)
# and associated indicator functions
LocationO <- array(0,dataR$dimshere)
LocationR <- array(0,dataR$dimshere)

# value functions conditional on (here,there)
# ===========================================

Wrent <- array(0,dataR$dims)
Wown  <- array(0,dataR$dims)
# and associated indicator functions
TenureO <- array(0,dataR$dims)
TenureR <- array(0,dataR$dims)

# value functions conditional on (here,there) and tenure
# ======================================================

# values at all Locatoin combinations (here,there)
VLR = array(3,dataR$dims)
VLB = array(4,dataR$dims)
VLS = array(2,dataR$dims)
VLO = array(1,dataR$dims)

# savings functions at each location
saveLO = array(1,dataR$dims)
saveLS = array(2,dataR$dims)
saveLR = array(3,dataR$dims)
saveLB = array(4,dataR$dims)

# conditional consumption functions
consLR = array(0,dataR$dims)
consLB = array(0,dataR$dims)
consLS = array(0,dataR$dims)
consLO = array(0,dataR$dims)

# final period values
EVrent[ , , , ,nT] <- SS[it==nT,array(rrent,dataR$dimshere[-5])]
EVown[ , , , ,nT]  <- SS[it==nT,array(rstay,dataR$dimshere[-5])]

#            dimensions    a y p h      a y p h      y y'     p p'
integr <- tensorFunction(R[i,m,n,l] ~ V[i,j,k,l] * G[m,j] * X[n,k] )
         
# temporary consumption value
ctmp <- 0

# loop over STATES
for (ti in (nT-1):1) {
    for (ia in 1:nA) {
		 for(iy in 1:nY) {
			 for (ip in 1:nP){
				 for(here in 1:nL){

					 # loop over CHOICES
					 for (there in 1:nL) {
						 # savings options at each (here,there) combination
						 for (ja in 1:nA){

							 # renter and seller
							 # =================

							 # renter
							 # ------

							 # compute consumption at that savigns choice
							 CR[ia,iy,ip,here,there,ti,ja] <- ctmp <- RR[ia,iy,ip,here,there,ti] - dataR$R*grids$a[ ja ]
							  
							 # check feasibility of that savings choice by looking at implied consumption 
							 # and if index of savings is below constraint value
							 if (ctmp < 0 | !is.finite(ctmp) | ja <= iborrow.rent ){
								xR[ia,iy,ip,here,there,ti,ja] = dataR$myNA
							 } else {
								xR[ia,iy,ip,here,there,ti,ja] =  R_ufun(ctmp,dataR$gamma,0) + dataR$beta*EVrent[ja,iy,ip,there,ti+1] - move.cost[here,there] + amenity[there]
							 }
						 
							 # seller 
							 # ------
							 
							 CS[ia,iy,ip,here,there,ti,ja] <- ctmp <- RS[ia,iy,ip,here,there,ti] - dataR$R*grids$a[ ja ]

							 if (ctmp < 0 | !is.finite(ctmp) | ja <= iborrow.rent ){
								xS[ia,iy,ip,here,there,ti,ja] = dataR$myNA
							 } else {
								xS[ia,iy,ip,here,there,ti,ja] =  R_ufun(ctmp,dataR$gamma,0) + dataR$beta*EVrent[ja,iy,ip,there,ti+1] - move.cost[here,there] + amenity[there]
							 }

							 # owner and buyer
							 # ===============

							 # buyer
							 # -----

							 # compute consumption at that savigns choice
							 CB[ia,iy,ip,here,there,ti,ja] <- ctmp <- RB[ia,iy,ip,here,there,ti] - dataR$R*grids$a[ ja ]

							 if (ctmp < 0 | !is.finite(ctmp) | ja <= iborrow.buy[there,ip] ){
								xB[ia,iy,ip,here,there,ti,ja] = dataR$myNA
							 } else {
								xB[ia,iy,ip,here,there,ti,ja] =  R_ufun(ctmp,dataR$gamma,dataR$theta) + dataR$beta*EVown[ja,iy,ip,there,ti+1] - move.cost[here,there] + amenity[there]
							 }
							 
							 # owner 
							 # -----

							 # compute consumption at that savigns choice
							 CO[ia,iy,ip,here,there,ti,ja] <- ctmp <- RO[ia,iy,ip,here,there,ti] - dataR$R*grids$a[ ja ]
							 
							 if (ctmp < 0 | !is.finite(ctmp) | ja <= iborrow.own[here,there,ip] ){
								xO[ia,iy,ip,here,there,ti,ja] = dataR$myNA
							 } else {
								xO[ia,iy,ip,here,there,ti,ja] =  R_ufun(ctmp,dataR$gamma,dataR$theta) + dataR$beta*EVown[ja,iy,ip,there,ti+1] - move.cost[here,there] + amenity[there]
							 }
						 }

						 # maximize over savings choice in each location problem
						 # =====================================================

						 # conditional values renter state at each (here,there)
						 VLR[ia,iy,ip,here,there,ti] = max(xR[ia,iy,ip,here,there,ti, ])
						 VLB[ia,iy,ip,here,there,ti] = max(xB[ia,iy,ip,here,there,ti, ])
						 # conditional savings renter state
						 saveLR[ia,iy,ip,here,there,ti] = which.max(xR[ia,iy,ip,here,there,ti, ])
						 consLR[ia,iy,ip,here,there,ti] = CR[ia,iy,ip,here,there,ti,saveLR[ia,iy,ip,here,there,ti]]

						 saveLB[ia,iy,ip,here,there,ti] = which.max(xB[ia,iy,ip,here,there,ti, ])
						 consLB[ia,iy,ip,here,there,ti] = CB[ia,iy,ip,here,there,ti,saveLB[ia,iy,ip,here,there,ti]]
						 
						 # conditional values owner state
						 VLS[ia,iy,ip,here,there,ti] = max(xS[ia,iy,ip,here,there,ti, ])
						 VLO[ia,iy,ip,here,there,ti] = max(xO[ia,iy,ip,here,there,ti, ])
						 # conditional savings owner state
						 saveLO[ia,iy,ip,here,there,ti] = which.max(xO[ia,iy,ip,here,there,ti, ])
						 consLO[ia,iy,ip,here,there,ti] = CO[ia,iy,ip,here,there,ti,saveLO[ia,iy,ip,here,there,ti]]

						 saveLS[ia,iy,ip,here,there,ti] = which.max(xS[ia,iy,ip,here,there,ti, ])
						 consLS[ia,iy,ip,here,there,ti] = CS[ia,iy,ip,here,there,ti,saveLS[ia,iy,ip,here,there,ti]]

						 # in each location, find discrete choice between (rent,buy) for renter and (stay,sell) for owners
						 # ===============================================================================================

						 # max val renter at each location (here,there)
						 Wrent[ia,iy,ip,here,there,ti]   = max(VLR[ia,iy,ip,here,there,ti],VLB[ia,iy,ip,here,there,ti])			# Vrent = max(rent(here,there), buy(here,there))
						 TenureR[ia,iy,ip,here,there,ti] = which.max(c(VLR[ia,iy,ip,here,there,ti],VLB[ia,iy,ip,here,there,ti]))	# Drent = which.max(rent(here), buy(here))
					 
						 # max val owner state
						 Wown[ia,iy,ip,here,there,ti]    = max(VLO[ia,iy,ip,here,there,ti],VLS[ia,iy,ip,here,there,ti])
						 TenureO[ia,iy,ip,here,there,ti] = which.max(c(VLO[ia,iy,ip,here,there,ti],VLS[ia,iy,ip,here,there,ti]))

					 }


					 # maximize over location choice in each subproblem
					 # ================================================

					 # here you will need a "there"-specific vector of logit shocks applied to Vrent and Vown
					 # it will be common to all discrete choices, varying only by location "there"

					 Vrent[ia,iy,ip,here,ti]  <- max(Wrent[ia,iy,ip,here, ,ti])			
					 LocationR[ia,iy,ip,here,ti] <- which.max(Wrent[ia,iy,ip,here, ,ti])

					 Vown[ia,iy,ip,here,ti]  <- max(Wown[ia,iy,ip,here, ,ti])        	
					 LocationO[ia,iy,ip,here,ti] <- which.max(Wown[ia,iy,ip,here, ,ti])

				 }
			 }
		 }
		 # integrate
		 # =========
	
		 tmpO = Vown[ , , , ,ti]
		 tmpR = Vrent[ , , , ,ti]
		 EVown[ , , , ,ti] = integr(tmpO, G, Gp)
		 EVrent[ , , , ,ti] = integr(tmpR, G, Gp)
     }
}

Rtime <- proc.time() - Rtime
      
# Calculating the blitz solution to the equivalent
# ================================================

blitz <- dev8(data=dataR)

# timings
print(Rtime)
print(sum(blitz$time/1e9))


# check outputs
# =============

print(all.equal(Vown,blitz$Values$Vown,tolerance=1e-15))
print(all.equal(Vrent,blitz$Values$Vrent,tolerance=1e-15))
print(all.equal(EVown,blitz$Values$EVown))
print(all.equal(EVrent,blitz$Values$EVrent))

print(all.equal(TenureR,blitz$policies$TenureRent))
print(all.equal(TenureO,blitz$policies$TenureOwn))

print(all.equal(LocationR,blitz$policies$LocationRent))
print(all.equal(LocationO,blitz$policies$LocationOwn))

print(all.equal(Wown,blitz$Values$Wown))
print(all.equal(Wrent,blitz$Values$Wrent))

print(all.equal(saveLO,blitz$policies$s_loc_stay))
print(all.equal(saveLS,blitz$policies$s_loc_sell))
print(all.equal(saveLR,blitz$policies$s_loc_rent))
print(all.equal(saveLB,blitz$policies$s_loc_buy ))
