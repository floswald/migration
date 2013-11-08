

library(migration)


# example for dev8 with location attributes
# =========================================

# LARGE STATE SPACE. NO R COMPUTATION.

# savings: grid search
# housing: yes/no. adding house price uncertainty now.
# utility: CRRA with housing utility
# location: discrete choice over locations
# locations differ by amenity, price and income distributions
# moving cost: dist(here,there)

# assume for this example that locations differ by amenity, house price and incomce supports

# renter state: V = max( rent, buy )
# owner state: V = max( stay , sell )

cat('START building the state space\n')

Rtime <- proc.time()

nA <- 50L; nY <- 4L; nP <- 3L; nL <- 10L; nT <- 30L

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


dataR$resR <- SS[,rrent]	# rent
dataR$resB <- SS[,rbuy]		# buy
dataR$resS <- SS[,rsell]	# sell
dataR$resO <- SS[,rstay]	# "O" is for "Owner", i.e. "stay"

dataR$MoveCost <- as.numeric(move.cost)
dataR$Amenity  <- amenity
dataR$agrid    <- grids$a

# free some memory
rm(SS,SS.loc)
gc()

Rtime <- proc.time() - Rtime
cat(sprintf('FINISHED building the state space after %g seconds.\nSending off to C++ now.',Rtime[3]) )

# Calculating the blitz solution to the equivalent
# ================================================

b <- dev8(data=dataR)

# timings
print("blitz time:")
print(sum(b$policies$time/1e9))


