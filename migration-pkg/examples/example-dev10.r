

# example for dev9
# ================

# savings: continuous optimization
# utility: CRRA 

# this is a tester for the continuous optimization.
# code not used for production.

nA <- 50L; nY <- 2L; nT <- 3L
G <- rouwenhorst(rho=0.9,n=nY,sigma=0.1)$Pmat
dims <- c(nA,nY,nT)
names(dims) <- c("a","y","age")
dataR <- list( dims=dims,
               theta = 0.2,beta=0.95,gamma=1.4,cutoff = 0.01,
			   R = 1.04,
               G = as.numeric(rouwenhorst(rho=0.9,n=nY,sigma=0.1)$Pmat))
               
grids <- list()
grids$a <- seq(-2,10,length=nA)
grids$arent <- seq(0,10,length=nA)

# OTHER's statespace: add dimension "here" AND "there".

SS <- data.table(expand.grid(a=grids$a,y=1:nY,it=1:nT))

# both income and prices are mappings from here and there to y and p.
# for now no location specific costs/differences in prices. add that later
SS[,cstay := a + y + 0.3*it]	
SS[,csell := a + y + 0.3*it ]

# restrictions
# ============

# stayer: 
SS[it==nT ,cstay := a ]

# seller: can have negative assets
# seller: but cannot borrow. must pay off housing debt upon sale.
SS[it==nT, csell := a]

# tensors
# =======


ResS <- SS[,array(csell,c(dataR$dims))]
ResO <- SS[,array(cstay,c(dataR$dims))]	# "O" is for "Owner", i.e. "stay"

dataR$ResS <- as.numeric(ResS)
dataR$ResO <- as.numeric(ResO)
dataR$agrid_own <- grids$a
dataR$agrid_rent <- grids$arent

######################################################
# Calculating an R solution to this lifecycle problem
######################################################

Rtime <- proc.time()
# envelopes of conditional values
Vown = array(0,dataR$dims)
Vsell = array(0,dataR$dims)
# Their expected values
EVown = array(0,dataR$dims)
EVrent = array(0,dataR$dims)

# savings functions at each location
saveO = array(1,dataR$dims)
errorO = array(1,dataR$dims)
saveS = array(2,dataR$dims)
errorS = array(1,dataR$dims)

# final period values
for (ia in 1:nA){
	for (iy in 1:nY){
		EVrent[ ia, iy,nT] <- R_ufun_neg(cons=ResS[ia,iy ,nT],gamma=dataR$gamma,cutoff=dataR$cutoff)$utility
		EVown[ ia, iy,nT]  <- R_ufun_neg(cons=ResO[ia,iy ,nT],gamma=dataR$gamma,cutoff=dataR$cutoff)$utility
	}
}
stop()
integr <- tensorFunction(R[i,m] ~ V[i,j] * G[m,j] )
                       
# loop over owner STATES
for (ti in (nT-1):1) {
	# owner states
	 for(iy in 1:nY) {
		 f.ev <- splinefun( x=grids$a, y=EVown[ ,iy,ti] )
		for (ia in 1:nA) {

			tmp <- uniroot(R_objective,interval=c(0,0.99*ResO[ia,iy,ti]),res=ResO[ia,iy,ti],R=dataR$R,beta=dataR$beta,gamma=dataR$gamma,cutoff=dataR$cutoff)
			errorO[ia,iy,ti] <- tmp$f.root
			saveO[ia,iy,ti] <- tmp.root
		}
	 }
	# renter states
	 for(iy in 1:nY) {
		 f.ev <- splinefun( x=grids$arent, y=EVrent[ ,iy,ti] )
		for (ia in 1:nA) {

			tmp <- uniroot(R_objective,interval=c(0,0.99*ResS[ia,iy,ti]),res=ResS[ia,iy,ti],R=dataR$R,beta=dataR$beta,gamma=dataR$gamma,cutoff=dataR$cutoff)
			errorS[ia,iy,ti] <- tmp$f.root
			saveS[ia,iy,ti] <- tmp.root
		}
	 }

	 # integrate stuff
	 tmpO = Vown[ , ,ti]
	 tmpR = Vrent[ , ,ti]
	 EVown[  , ,ti] = integr(tmpO, G)
	 EVrent[ , ,ti] = integr(tmpR, G)

}



Rtime <- proc.time() - Rtime

# Calculating the blitz solution to the equivalent
# ================================================
      
#blitz <- dev8(data=dataR)

## timings
#print(Rtime)
#print(sum(blitz$time/1e9))


## get conditional consumption functions
## =====================================
##consR <- array(matrix(CR[ , , ,1:(nT-1), ],nA*nY*nP*(nT-1),nA)[cbind(1:(nA*nY*nP*(nT-1)),as.numeric(saveR[ , , ,1:(nT-1)]))], c(nA,nY,nP,nT-1))
##consB <- array(matrix(CB[ , , ,1:(nT-1), ],nA*nY*nP*(nT-1),nA)[cbind(1:(nA*nY*nP*(nT-1)),as.numeric(saveB[ , , ,1:(nT-1)]))], c(nA,nY,nP,nT-1))
##consS <- array(matrix(CS[ , , ,1:(nT-1), ],nA*nY*nP*(nT-1),nA)[cbind(1:(nA*nY*nP*(nT-1)),as.numeric(saveS[ , , ,1:(nT-1)]))], c(nA,nY,nP,nT-1))
##consO <- array(matrix(CStay[ , , ,1:(nT-1), ],nA*nY*nP*(nT-1),nA)[cbind(1:(nA*nY*nP*(nT-1)),as.numeric(saveO[ , , ,1:(nT-1)]))], c(nA,nY,nP,nT-1))
## =====================================

## check outputs
## =============

#print(all.equal(Vown,blitz$Vown))
#print(all.equal(Vrent,blitz$Vrent))
#print(all.equal(EVown,blitz$EVown))
#print(all.equal(EVrent,blitz$EVrent))
#print(all.equal(DO,blitz$Down))
#print(all.equal(DR,blitz$Drent))

#print(all.equal(VO,blitz$vstay))
#print(all.equal(VR,blitz$vrent))
#print(all.equal(VS,blitz$vsell))
#print(all.equal(VB,blitz$vbuy))

#print(all.equal(move_stay,blitz$move_stay))
#print(all.equal(move_sell,blitz$move_sell))
#print(all.equal(move_rent,blitz$move_rent))
#print(all.equal(move_buy ,blitz$move_buy ))

#print(all.equal(saveLO,blitz$s_loc_stay))
#print(all.equal(saveLS,blitz$s_loc_sell))
#print(all.equal(saveLR,blitz$s_loc_rent))
#print(all.equal(saveLB,blitz$s_loc_buy ))
