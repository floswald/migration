
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

nA <- 10L; nY <- 2L; nT <- 4L; nH <- 2L; nP <- 3L; nL <- 4L
G <- rouwenhorst(rho=0.9,n=nY,sigma=0.1)$Pmat
Gp <- rouwenhorst(rho=0.9,n=nP,sigma=0.16)$Pmat
dims <- c(nA,nY,nP,nL,nL,nT)
names(dims) <- c("a","y","p","here","there","age")
dataR <- list( dims=dims,dimshere = c(nA,nY,nP,nL,nT),
               theta = 0.2,beta=0.95,gamma=1.4,
               myNA=-99,rent=0.05,R=1/(1+0.04),down=0.2,
               G = as.numeric(G),
               Gp = as.numeric(Gp));
               
idx <- list()
idx$L <- 1:nL
grids <- list()
grids$L <- seq(0.1,2,length=nL)	# cities can be 10% or 200% of baseline city
grids$p <- matrix(seq(1,10,le=nP),nP,nL) + matrix(seq(0,3,le=nL),nP,nL,byrow=T)
grids$y <- matrix(seq(1,3,le=nY),nY,nL) + matrix(seq(0,2,le=nL),nY,nL,byrow=T)
grids$a <- seq(-(1-dataR$down)*max(grids$p),10,length=nA)

# OTHER's statespace: add dimension "here" AND "there".

SS <- data.table(expand.grid(a=grids$a,iy=1:nY,ip=1:nP,here=idx$L,there=idx$L,it=1:nT,save=grids$a))
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


# final period restricion owner:
# net wealth
SS[it==nT & a>0,rstay := log(a) ]
SS[it==nT & a<0,rstay := dataR$myNA ]
for (ih in 1:nL){
	for (ith in 1:nL){

		# if here is not there
		if (ih !=ith){

				# stayer: 
				SS[here==ih & there==ith, cstay := a + ythere + 0.3*it + phere - pthere - dataR$R*save ]		# 
				SS[here==ih & there==ith & save < -(1-dataR$down)*pthere , cstay := dataR$myNA ]		# if you move and buy at the new place, must observe the borrowing constraint

				# seller:
				SS[here==ih & there==ith, csell := a + ythere + 0.3*it + phere - pthere*dataR$rent - dataR$R*save ]		 

				# renter:
				SS[here==ih & there==ith, crent := a + ythere + 0.3*it         - pthere*dataR$rent - dataR$R*save ]		
				
				# buyer:
				SS[here==ih & there==ith, cbuy  := a + ythere + 0.3*it         - pthere            - dataR$R*save ]		
				SS[here==ih & there==ith & save < -(1-dataR$down)*pthere , cbuy := dataR$myNA ]		# if you move and buy at the new place, must observe the borrowing constraint
		}

	}
}


# remove "save" dimension from R.
# ===============================

# this borrowing limit function is identical for owners and buyer alike
iborrow.own <- array(-1,c(nL,nL,nP))	# set illegal index
for (here in 1:nL){
	for (there in 1:nL){
		for (ip in 1:nP){
			# owner moving from here to there
			idx <- which(grids$a < -(1-dataR$down)*grids$p[ip,there])
			if (length(idx)>0) iborrow.own[here,there,ip] <- max(idx)	# which is the largest savings index s.t. lower than borrowing limit. next index is legal.
		}
	}
}

iborrow.rent <- max(which(grids$a < 0))	# iborrow.rent + 1 is first legal index


# buyer: cannot have negative assets
SS[a<0 & it!=nT,             cbuy  := dataR$myNA]

# renter: cannot have negative assets
# renter: cannot borrow
# renter: final utility
SS[a<0         , crent := dataR$myNA]
SS[save<0      , crent := dataR$myNA]
SS[a>0 & it==nT, crent := log(a)    ]

# seller: can have negative assets
# seller: but cannot borrow. must pay off housing debt upon sale.
SS[save<0, csell := dataR$myNA]

# LOCATION statespace
# ============
# this table will hold amenity values at each location as well as
# utility moving costs from here to there.
SS.loc <- data.table(expand.grid(here=grids$L,there=grids$L))
SS.loc[,mcost := abs(here-there)]
move.cost <- SS.loc[,matrix(mcost,nL,nL,)]
rownames(move.cost) <- paste0("here",1:nL)
colnames(move.cost) <- paste0("there",1:nL)

amenity <- rev(grids$L)

# tensors
# =======

CR <- SS[,array(crent,c(dataR$dims,nA))]
CB <- SS[,array(cbuy,c(dataR$dims,nA))]
CS <- SS[,array(csell,c(dataR$dims,nA))]
CO <- SS[,array(cstay,c(dataR$dims,nA))]	# "O" is for "Owner", i.e. "stay"

xR = array(0,c(dataR$dims,nA))
xB = array(0,c(dataR$dims,nA))
xS = array(0,c(dataR$dims,nA))
xO = array(0,c(dataR$dims,nA))

dataR$consR <- as.numeric(CR)
dataR$consB <- as.numeric(CB)
dataR$consS <- as.numeric(CS)
dataR$consO <- as.numeric(CO)

dataR$MoveCost <- as.numeric(move.cost)
dataR$Amenity  <- amenity

######################################################
# Calculating an R solution to this lifecycle problem
######################################################

Rtime <- proc.time()
# envelopes of conditional values
Vown = array(0,dataR$dimshere)
Vrent = array(0,dataR$dimshere)
# Their expected values
EVown = array(0,dataR$dimshere)
EVrent = array(0,dataR$dimshere)

# discrete choice functions for location choice
move_stay <- array(0,dataR$dimshere)
move_buy <- array(0,dataR$dimshere)
move_sell <- array(0,dataR$dimshere)
move_rent <- array(0,dataR$dimshere)

# discrete choice amoung conditional value
 							#             1       2     ...  J      J+1      J+2    ...   2J
DO = array(0,dataR$dimshere)	# which.max( stay_i,stay_j, ... stay_J, sell_i, sell_j, ... ,sell_J ) is an integer in {1,...,2J} and we ComputeStay, ComputeSell, then partial reduce along "there" dim
DR = array(0,dataR$dimshere)    # which.max( rent_i,rent_j, ... rent_J, buy_i, buy_j, ... ,buy_J )

# values at all Locatoin combinations (here,there)
VLR = array(3,dataR$dims)
VLB = array(4,dataR$dims)
VLS = array(2,dataR$dims)
VLO = array(1,dataR$dims)

# values conditional on maximal choice over "there" 
VR = array(3,dataR$dimshere)
VB = array(4,dataR$dimshere)
VS = array(2,dataR$dimshere)
VO = array(1,dataR$dimshere)

# savings functions at each location
saveLO = array(1,dataR$dims)
saveLS = array(2,dataR$dims)
saveLR = array(3,dataR$dims)
saveLB = array(4,dataR$dims)

# conditional consumption functions
consR = array(0,dataR$dims)
consB = array(0,dataR$dims)
consS = array(0,dataR$dims)
consO = array(0,dataR$dims)

# final period values
EVrent[ , , , ,nT] <- CR[ , , , ,1,nT,nA]
EVown[ , , , ,nT]  <- CO[ , , , ,1,nT,nA]

#            dimensions    a y p h      a y p h      y y'     p p'
integr <- tensorFunction(R[i,m,n,l] ~ V[i,j,k,l] * G[m,j] * X[n,k] )
                       
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
							 # renter
							 if (CR[ia,iy,ip,here,there,ti,ja] < 0 | !is.finite(CR[ia,iy,ip,here,there,ti,ja])){
								xR[ia,iy,ip,here,there,ti,ja] = dataR$myNA
							 } else {
								xR[ia,iy,ip,here,there,ti,ja] =  R_ufun(CR[ia,iy,ip,here,there,ti,ja],dataR$gamma,0) + dataR$beta*EVrent[ja,iy,ip,there,ti+1] - move.cost[here,there] + amenity[there]
							 }
							 # buyer
							 if (CB[ia,iy,ip,here,there,ti,ja] < 0 | !is.finite(CB[ia,iy,ip,here,there,ti,ja])){
								xB[ia,iy,ip,here,there,ti,ja] = dataR$myNA
							 } else {
								xB[ia,iy,ip,here,there,ti,ja] =  R_ufun(CB[ia,iy,ip,here,there,ti,ja],dataR$gamma,dataR$theta) + dataR$beta*EVown[ja,iy,ip,there,ti+1] - move.cost[here,there] + amenity[there]
							 }
							 # seller
							 if (CS[ia,iy,ip,here,there,ti,ja] < 0 | !is.finite(CS[ia,iy,ip,here,there,ti,ja])){
								xS[ia,iy,ip,here,there,ti,ja] = dataR$myNA
							 } else {
								xS[ia,iy,ip,here,there,ti,ja] =  R_ufun(CS[ia,iy,ip,here,there,ti,ja],dataR$gamma,0) + dataR$beta*EVrent[ja,iy,ip,there,ti+1] - move.cost[here,there] + amenity[there]
							 }
							 # owner
							 if (CO[ia,iy,ip,here,there,ti,ja] < 0 | !is.finite(CO[ia,iy,ip,here,there,ti,ja])){
								xO[ia,iy,ip,here,there,ti,ja] = dataR$myNA
							 } else {
								xO[ia,iy,ip,here,there,ti,ja] =  R_ufun(CO[ia,iy,ip,here,there,ti,ja],dataR$gamma,dataR$theta) + dataR$beta*EVown[ja,iy,ip,there,ti+1] - move.cost[here,there] + amenity[there]
							 }
						 }

						 # maximize over savings choice in each location problem
						 # =====================================================

						 # conditional values renter state at each (here,there)
						 VLR[ia,iy,ip,here,there,ti] = max(xR[ia,iy,ip,here,there,ti, ])
						 VLB[ia,iy,ip,here,there,ti] = max(xB[ia,iy,ip,here,there,ti, ])
						 # conditional savings renter state
						 saveLR[ia,iy,ip,here,there,ti] = which.max(xR[ia,iy,ip,here,there,ti, ])
						 saveLB[ia,iy,ip,here,there,ti] = which.max(xB[ia,iy,ip,here,there,ti, ])
						 
						 # conditional values owner state
						 VLS[ia,iy,ip,here,there,ti] = max(xS[ia,iy,ip,here,there,ti, ])
						 VLO[ia,iy,ip,here,there,ti] = max(xO[ia,iy,ip,here,there,ti, ])
						 # conditional savings owner state
						 saveLO[ia,iy,ip,here,there,ti] = which.max(xO[ia,iy,ip,here,there,ti, ])
						 saveLS[ia,iy,ip,here,there,ti] = which.max(xS[ia,iy,ip,here,there,ti, ])

					 }
					 # maximize over location choice in each subproblem
					 # ================================================
					 VR[ia,iy,ip,here,ti]  <- max(VLR[ia,iy,ip,here, ,ti])			# what is the value of being here conditional on renting?
					 move_rent[ia,iy,ip,here,ti] <- which.max(VLR[ia,iy,ip,here, ,ti])	# where do you move to from here  conditional on renting?

					 VB[ia,iy,ip,here,ti]  <- max(VLB[ia,iy,ip,here, ,ti])        	# what is the value of being here conditional on buying?
					 move_buy[ia,iy,ip,here,ti] <- which.max(VLB[ia,iy,ip,here, ,ti])  	# where do you move to from here  conditional on buying?

					 VO[ia,iy,ip,here,ti]  <- max(VLO[ia,iy,ip,here, ,ti])          # what is the value of being here conditional on owning?
					 move_stay[ia,iy,ip,here,ti] <- which.max(VLO[ia,iy,ip,here, ,ti])    # where do you move to from here  conditional on owning?

					 VS[ia,iy,ip,here,ti]  <- max(VLS[ia,iy,ip,here, ,ti])       	# what is the value of being here conditional on selling?
					 move_sell[ia,iy,ip,here,ti] <- which.max(VLS[ia,iy,ip,here, ,ti]) 	# where do you move to from here  conditional on selling?

				 
					 # in each location find maximal value
					 # ===================================

					 # max val renter state
					 Vrent[ia,iy,ip,here,ti] = max(VR[ia,iy,ip,here,ti],VB[ia,iy,ip,here,ti])			# Vrent = max(rent(here), buy(here))
					 DR[ia,iy,ip,here,ti]    = which.max(c(VR[ia,iy,ip,here,ti],VB[ia,iy,ip,here,ti]))	# Drent = which.max(rent(here), buy(here))


					 # max val owner state
					 Vown[ia,iy,ip,here,ti]  = max(VO[ia,iy,ip,here,ti],VS[ia,iy,ip,here,ti])
					 DO[ia,iy,ip,here,ti]    = which.max(c(VO[ia,iy,ip,here,ti],VS[ia,iy,ip,here,ti]))
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


# get conditional consumption functions
# =====================================
#consR <- array(matrix(CR[ , , ,1:(nT-1), ],nA*nY*nP*(nT-1),nA)[cbind(1:(nA*nY*nP*(nT-1)),as.numeric(saveR[ , , ,1:(nT-1)]))], c(nA,nY,nP,nT-1))
#consB <- array(matrix(CB[ , , ,1:(nT-1), ],nA*nY*nP*(nT-1),nA)[cbind(1:(nA*nY*nP*(nT-1)),as.numeric(saveB[ , , ,1:(nT-1)]))], c(nA,nY,nP,nT-1))
#consS <- array(matrix(CS[ , , ,1:(nT-1), ],nA*nY*nP*(nT-1),nA)[cbind(1:(nA*nY*nP*(nT-1)),as.numeric(saveS[ , , ,1:(nT-1)]))], c(nA,nY,nP,nT-1))
#consO <- array(matrix(CStay[ , , ,1:(nT-1), ],nA*nY*nP*(nT-1),nA)[cbind(1:(nA*nY*nP*(nT-1)),as.numeric(saveO[ , , ,1:(nT-1)]))], c(nA,nY,nP,nT-1))
# =====================================

# check outputs
# =============

print(all.equal(Vown,blitz$Values$Vown))
print(all.equal(Vrent,blitz$Values$Vrent))
print(all.equal(EVown,blitz$Values$EVown))
print(all.equal(EVrent,blitz$Values$EVrent))
print(all.equal(DO,blitz$policies$Down))
print(all.equal(DR,blitz$policies$Drent))

print(all.equal(VO,blitz$Values$vstay))
print(all.equal(VR,blitz$Values$vrent))
print(all.equal(VS,blitz$Values$vsell))
print(all.equal(VB,blitz$Values$vbuy))

print(all.equal(move_stay,blitz$policies$move_stay))
print(all.equal(move_sell,blitz$policies$move_sell))
print(all.equal(move_rent,blitz$policies$move_rent))
print(all.equal(move_buy ,blitz$policies$move_buy ))

print(all.equal(saveLO,blitz$policies$s_loc_stay))
print(all.equal(saveLS,blitz$policies$s_loc_sell))
print(all.equal(saveLR,blitz$policies$s_loc_rent))
print(all.equal(saveLB,blitz$policies$s_loc_buy ))
