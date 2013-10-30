

# example for dev8
# ================

# savings: grid search
# housing: yes/no
# utility: CRRA with housing utility
# location: discrete choice over locations
# moving cost: dist(here,there)

# assume for this example that locations differ only by a fixed
# number. i.e. locations are ordered from 1:nL and nL gives biggest
# value. the moving cost is just the distance in the indices.

# this problem adds two dimension so previous versions: here and there.
# 1) moving from here to here is of course the same as staying. 
# 2) one cannot stay from here to there, one must sell here and buy or rent there.

# renter state: V = max( rent, buy )
# owner state: V = max( stay , sell )

nA <- 10L; nY <- 2L; nT <- 3L; nH <- 2L; nP <- 3L; nL <- 2L
G <- rouwenhorst(rho=0.9,n=nY,sigma=0.1)$Pmat
dims <- c(nA,nY,nP,nL,nL,nT)
names(dims) <- c("a","y","p","here","there","age")
dataR <- list( dims=dims,dimshere = c(nA,nY,nP,nL,nT),
               theta = 0.2,beta=0.95,gamma=1.4,
               myNA=-99,rent=0.05,R=1/(1+0.04),down=0.2,
               G = as.numeric(rouwenhorst(rho=0.9,n=nY,sigma=0.1)$Pmat))
               
grids <- list()
grids$p <- seq(1,10,length=nP)
grids$a <- seq(-(1-dataR$down)*max(grids$p),10,length=nA)
grids$L <- seq(1,3,length=nL)

# OTHER's statespace: add dimension "here" AND "there".

SS <- data.table(expand.grid(a=grids$a,y=1:nY,ip=1:nP,here=grids$L,there=grids$L,it=1:nT,save=grids$a))
SS[,p:=grids$p[ip] ]

# both income and prices are mappings from here and there to y and p.
# for now no location specific costs/differences in prices. add that later
SS[,cstay := a + y + 0.3*it - dataR$R*save]	
SS[,cbuy  := a + y + 0.3*it - p - dataR$R*save ]
SS[,csell := a + y + 0.3*it - dataR$rent + p - dataR$R*save ]
SS[,crent := a + y + 0.3*it - dataR$rent     - dataR$R*save ]

# restrictions
# ============

# stayer: 
SS[it==nT & a+y+p>0,cstay := log(a+y+p) ]
SS[it==nT & a+y+p<0,cstay := dataR$myNA ]
#SS[here != there, cstay := a + y + 0.3*it + p[here] - p[there] - dataR$R*save ] # TODO
#SS[here != there & save< -(1-dataR$down)*p[there], cstay := dataR$myNA ] # if you are an owner moving, you must buy a new house and the borrowing constraint applies
SS[here != there & save< -(1-dataR$down)*p & it!=nT, cstay := dataR$myNA ] # if you are an owner moving, you must buy a new house and the borrowing constraint applies

# buyer: cannot have negative assets
# buyer: cannot borrow more than (1-down) times value of house
SS[a<0 & it!=nT,             cbuy  := dataR$myNA]
SS[save < -(1-dataR$down)*p, cbuy  := dataR$myNA]

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

amenity <- grids$L

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
dataR$Amenity  <- grids$L

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

integr <- tensorFunction(R[i,m,k,l] ~ V[i,j,k,l] * G[m,j] )
                       
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
		 EVown[ , , , ,ti] = integr(tmpO, G)
		 EVrent[ , , , ,ti] = integr(tmpR, G)
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

print(all.equal(Vown,blitz$Vown))
print(all.equal(Vrent,blitz$Vrent))
print(all.equal(EVown,blitz$EVown))
print(all.equal(EVrent,blitz$EVrent))
print(all.equal(DO,blitz$Down))
print(all.equal(DR,blitz$Drent))

print(all.equal(VO,blitz$vstay))
print(all.equal(VR,blitz$vrent))
print(all.equal(VS,blitz$vsell))
print(all.equal(VB,blitz$vbuy))

print(all.equal(move_stay,blitz$move_stay))
print(all.equal(move_sell,blitz$move_sell))
print(all.equal(move_rent,blitz$move_rent))
print(all.equal(move_buy ,blitz$move_buy ))

print(all.equal(saveLO,blitz$s_loc_stay))
print(all.equal(saveLS,blitz$s_loc_sell))
print(all.equal(saveLR,blitz$s_loc_rent))
print(all.equal(saveLB,blitz$s_loc_buy ))
