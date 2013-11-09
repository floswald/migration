

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


Zero.MoveCost <- FALSE 
Zero.Amenity <-TRUE 

cat(sprintf('START building the state space with\n\nZero.MoveCost %s and \nZero.Amenity %s',Zero.MoveCost,Zero.Amenity))


Rtime <- proc.time()

m <- list()

m$nA <- 50L
m$nY <- 3L
m$nP <- 4
m$nL <- 10L
m$nT <- 30

m$dims <- rep(0,5)
names(m$dims) <- c("a","y","p","here","age")

m$dims <- with(m, c(nA,nY,nP,nL,nT))

G           <- rouwenhorst(rho=0.9,n=m$nY,sigma=0.1)$Pmat
Gp          <- rouwenhorst(rho=0.9,n=m$nP,sigma=0.16)$Pmat

m$dimshere = m$dims
m$theta    = 0.05
m$beta     = 0.95
m$gamma    = 1.4
m$myNA     = -99
m$rent     = 0.01
m$R        = 1/(1+0.04)
m$down     = 0.2
m$G        = as.numeric(G)
m$Gp       = as.numeric(Gp)
m$verbose  = 1L
m$Zero <- list()
m$Zero$amenity <- Zero.Amenity
m$Zero$movecost <- Zero.MoveCost
               
idx       <- list()
idx$L     <- 1:m$nL
m$grids   <- list()
m$grids$L <- seq(0.1,2,length=m$nL)	# cities can be 10% or 200% of baseline city
m$grids$p <- with(m, matrix(seq(4,15,le=nP),nP,nL) + matrix(seq(0,8,le=nL),nP,nL,byrow=T))
m$grids$y <- with(m, matrix(seq(1,3,le=nY),nY,nL) + matrix(seq(0,2,le=nL),nY,nL,byrow=T))
m$grids$a  <- seq(-(1-m$down)*max(m$grids$p),max(m$grids$p),length=m$nA)
m$grids$apos <- m$grids$a[m$grids$a>0]
m$grids$azero <- min(which(m$grids$a>0))
m$idx <- list()
m$idx$apos <- which(m$grids$a>0)

# m$grids$a <- with(m, grid.maker(bounds = c(-(1-m$down)*max(grids$p),max(grids$p)),num.points=nA, spacing="log.g"))

# OTHER's statespace: add dimension "here" AND "there".

SS <- data.table(expand.grid(a=m$grids$a,iy=1:m$nY,ip=1:m$nP,here=idx$L,there=idx$L,it=1:m$nT))
for (yl in 1:m$nL) SS[here==yl, yhere:= m$grids$y[iy,yl] ]
for (pl in 1:m$nL) SS[here==pl, phere:= m$grids$p[ip,pl] ]
for (yl in 1:m$nL) SS[there==yl, ythere:= m$grids$y[iy,yl] ]
for (pl in 1:m$nL) SS[there==pl, pthere:= m$grids$p[ip,pl] ]


# We compute available resources at each state (a,y,p,here,there,age) here.
# we code r for 'resources'

SS[,rstay := a + yhere + 0.3*it ]	
SS[,rbuy  := a + yhere + 0.3*it - phere ]
SS[,rsell := a + yhere + 0.3*it - m$rent*phere + phere]
SS[,rrent := a + yhere + 0.3*it - m$rent*phere ]

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

for (ih in 1:m$nL){
	for (ith in 1:m$nL){

		# if here is not there
		if (ih !=ith){

				# stayer: 
				SS[here==ih & there==ith, rstay := a + ythere + 0.3*it + phere - pthere]		# 

				# seller:
				SS[here==ih & there==ith, rsell := a + ythere + 0.3*it + phere - pthere*m$rent]		 

				# renter:
				SS[here==ih & there==ith, rrent := a + ythere + 0.3*it         - pthere*m$rent]		
				
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
SS[it==m$nT & a>0,rstay := log(a) ]
SS[it==m$nT & a<0,rstay := m$myNA ]

# renter:
SS[a>0 & it==m$nT, rrent := log(a)    ]

# other restrictions on asset space:
# ----------------------------------

# buyer: cannot have negative assets
SS[a<0 & it!=m$nT,             rbuy  := m$myNA]
# buyer: if 
SS[a<0 & it!=m$nT,             rbuy  := m$myNA]

# renter: cannot have negative assets
SS[a<0         , rrent := m$myNA]

# seller: can have negative assets


# LOCATION statespace
# ===================

# this table holds utility moving costs from here to there.
SS.loc <- with(m, data.table(expand.grid(here=grids$L,there=grids$L)))

if (Zero.MoveCost){
	SS.loc[,mcost := 0]
} else {
	SS.loc[,mcost := abs(here-there)]
}

m$R_move.cost <- with(m, SS.loc[,matrix(mcost,nL,nL,)])
rownames(m$R_move.cost) <- paste0("here",1:m$nL)
colnames(m$R_move.cost) <- paste0("there",1:m$nL)

if (Zero.Amenity){
	amenity <- rep(0,m$nL)
} else {
	amenity <- rev(m$grids$L)
}

# borrowing limits 
# ================

# borrowing limit function for owners: whenever here != there, you are considered an owner moving, so you are constrained
# borrowing limit function for buyers: it does not matter where you are (here irrelevant), you always are constrained
iborrow.own <- with(m, array(-1,c(nL,nL,nP)))	# set to illegal index
iborrow.buy <- with(m, array(-1,c(nL,nP)))
for (here in 1:m$nL){
	for (there in 1:m$nL){
		for (ip in 1:m$nP){
			# owner moving from here to there
			idx <- which(m$grids$a < -(1-m$down)*m$grids$p[ip,there])
			if (length(idx)>0) iborrow.buy[there,ip] <- max(idx)	# buyer's here location is irrelevant
			if (length(idx)>0 & here!=there ) iborrow.own[here,there,ip] <- max(idx)	# owner is only constrained if he actually moves to buy. if stays here, he is not moving, thus not constrained.
		}
	}
}

iborrow.rent <- max(which(m$grids$a < 0))	# iborrow.rent + 1 is first legal index

m$blim_own <- as.numeric(iborrow.own)
m$blim_buy <- as.numeric(iborrow.buy)
m$blim_rent <- iborrow.rent 


m$resR <- SS[,rrent]	# rent
m$resB <- SS[,rbuy]		# buy
m$resS <- SS[,rsell]	# sell
m$resO <- SS[,rstay]	# "O" is for "Owner", i.e. "stay"

m$MoveCost <- as.numeric(m$R_move.cost)
m$Amenity  <- amenity
m$agrid    <- m$grids$a

# free some memory
#rm(SS,SS.loc)
#gc()

Rtime <- proc.time() - Rtime
cat(sprintf('FINISHED building the state space after %g seconds.\nSending off to C++ now.',Rtime[3]) )

# Calculating the blitz solution to the equivalent
# ================================================

b <- dev8(data=m)

# timings
print("blitz time:")
print(sum(b$policies$time/1e9))

#save(b,m,file='example11.RData')

plot.CCPmoving(m,b)


