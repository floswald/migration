\name{dev6}
\alias{dev6}
\title{dev6: discrete optimization, housing choice}
\usage{
  dev6(data)
}
\description{
  dev6: discrete optimization, housing choice
}
\examples{





nA <- 50L; nY <- 5L; nT <- 5L; nH <- 2L; nP <- 3L
G <- rouwenhorst(rho=0.9,n=nY,sigma=0.1)$Pmat
dataR <- list( dims = c(nA,nY,nP,nT),
               theta = 1.2,beta=0.95,
               myNA=-99,rent=0.05,R=1/(1+0.04),down=0.2,
               G = as.numeric(rouwenhorst(rho=0.9,n=nY,sigma=0.1)$Pmat))
               
grids <- list()
grids$p <- seq(1,10,length=nP)
grids$a <- seq(-(1-dataR$down)*max(grids$p),10,length=nA)

# renter state: V = max( rent, buy )

# sR means you are renter
sR <- data.table(expand.grid(a=grids$a,y=1:nY,p=grids$p,it=1:nT,save=grids$a))
sR[,cons := a + y + 0.3*it - dataR$rent - dataR$R*save]
sR[it==nT & a>0,cons := log(a) ]
sR[a<0, cons := dataR$myNA]
sR[save<0, cons := dataR$myNA]

# sB is buyer: admissible savings depends on current house price.
sB <- data.table(expand.grid(a=grids$a,y=1:nY,p=grids$p,it=1:nT,save=grids$a))
sB[,cons := a + y + 0.3*it - p - dataR$R*save]
sB[a<0 & it!=nT ,cons := dataR$myNA]
sB[save < -(1-dataR$down)*p, cons := dataR$myNA, by=list(y,p,it) ]

# owner state: V = max( stay , sell )

# sS means you are seller->renter
sS <- data.table(expand.grid(a=grids$a,y=1:nY,p=grids$p,it=1:nT,save=grids$a))
sS[,cons := a + y + 0.3*it - dataR$rent + p - dataR$R*save]
sS[save<0, cons := dataR$myNA]

# sO means you are owner
sO <- data.table(expand.grid(a=grids$a,y=1:nY,p=grids$p,it=1:nT,save=grids$a))
sO[,cons := a + y + 0.3*it - dataR$R*save]
sO[it==nT & a+y+p>0,cons := log(a+y+p)]
sO[it==nT & a+y+p<0,cons := dataR$myNA]

# tensors
CR <- sR[,array(cons,c(dataR$dims,nA))]
CB <- sB[,array(cons,c(dataR$dims,nA))]
CS <- sS[,array(cons,c(dataR$dims,nA))]
CO <- sO[,array(cons,c(dataR$dims,nA))]

xR = array(0,c(dataR$dims,nA))
xB = array(0,c(dataR$dims,nA))
xS = array(0,c(dataR$dims,nA))
xO = array(0,c(dataR$dims,nA))

dataR$consR <- sR[,cons]
dataR$consB <- sB[,cons]
dataR$consS <- sS[,cons]
dataR$consO <- sO[,cons]
blitz <- dev6(data=dataR)

######################################################
# Calculating an R solution to this lifecycle
######################################################

Rtime <- proc.time()
# envelopes of conditional values
WO = array(0,dataR$dims)
WR = array(0,dataR$dims)
# Their expected values
EWO = array(0,dataR$dims)
EWR = array(0,dataR$dims)

# discrete choice amoung conditional values
DO = array(0,dataR$dims)
DR = array(0,dataR$dims)

# conditional values
VR = array(3,dataR$dims)
VB = array(4,dataR$dims)
VS = array(2,dataR$dims)
VO = array(1,dataR$dims)

# conditional savings functions
saveR = array(0,dataR$dims)
saveB = array(0,dataR$dims)
saveS = array(0,dataR$dims)
saveO = array(0,dataR$dims)

# conditional consumption functions
consR = array(0,c(nA,nY,nP,nT-1))
consB = array(0,c(nA,nY,nP,nT-1))
consS = array(0,c(nA,nY,nP,nT-1))
consO = array(0,c(nA,nY,nP,nT-1))

# final period values
EWR[ , , ,nT] <- sR[it==nT&save==grids$a[nA],array(cons,c(nA,nY,nP))]
EWO[ , , ,nT] <- sO[it==nT&save==grids$a[nA],array(cons,c(nA,nY,nP))]
integr <- tensorFunction(R[i,m,k] ~ V[i,j,k] * G[m,j] )
                           
for (ti in (nT-1):1) {
    for (ia in 1:nA) {
		 for (ip in 1:nP){
	         for(iy in 1:nY) {
				 for (ja in 1:nA){
					 # renter
					 if (CR[ia,iy,ip,ti,ja] < 0 | !is.finite(CR[ia,iy,ip,ti,ja])){
						xR[ia,iy,ip,ti,ja] = dataR$myNA
					 } else {
						xR[ia,iy,ip,ti,ja] =  log(CR[ia,iy,ip,ti,ja])  + dataR$beta*EWR[ja,iy,ip,ti+1]
					 }
					 # buyer
					 if (CB[ia,iy,ip,ti,ja] < 0 | !is.finite(CB[ia,iy,ip,ti,ja])){
						xB[ia,iy,ip,ti,ja] = dataR$myNA
					 } else {
						xB[ia,iy,ip,ti,ja] =  log(CB[ia,iy,ip,ti,ja])  + dataR$beta*EWO[ja,iy,ip,ti+1]
					 }
					 # seller
					 if (CS[ia,iy,ip,ti,ja] < 0 | !is.finite(CS[ia,iy,ip,ti,ja])){
						xS[ia,iy,ip,ti,ja] = dataR$myNA
					 } else {
						xS[ia,iy,ip,ti,ja] =  log(CS[ia,iy,ip,ti,ja])  + dataR$beta*EWR[ja,iy,ip,ti+1]
					 }
					 # owner
					 if (CO[ia,iy,ip,ti,ja] < 0 | !is.finite(CO[ia,iy,ip,ti,ja])){
						xO[ia,iy,ip,ti,ja] = dataR$myNA
					 } else {
						xO[ia,iy,ip,ti,ja] =  log(CO[ia,iy,ip,ti,ja])  + dataR$beta*EWO[ja,iy,ip,ti+1]
					 }
				 }

    			 # renter state
				 # ============

				 # conditional values renter state
				 VR[ia,iy,ip,ti] = max(xR[ia,iy,ip,ti, ])
				 VB[ia,iy,ip,ti] = max(xB[ia,iy,ip,ti, ])
				 # conditional savings renter state
				 saveR[ia,iy,ip,ti] = which.max(xR[ia,iy,ip,ti, ])
				 saveB[ia,iy,ip,ti] = which.max(xB[ia,iy,ip,ti, ])
				 # max val renter state
				 WR[ia,iy,ip,ti] = max(VR[ia,iy,ip,ti],VB[ia,iy,ip,ti])
				 DR[ia,iy,ip,ti] = which.max(c(VR[ia,iy,ip,ti],VB[ia,iy,ip,ti]))

    			 # owner state
				 # ============

				 # conditional values owner state
				 VS[ia,iy,ip,ti] = max(xS[ia,iy,ip,ti, ])
				 VO[ia,iy,ip,ti] = max(xO[ia,iy,ip,ti, ])
				 # conditional savings owner state
				 saveO[ia,iy,ip,ti] = which.max(xO[ia,iy,ip,ti, ])
				 saveS[ia,iy,ip,ti] = which.max(xS[ia,iy,ip,ti, ])
				 # max val owner state
				 WO[ia,iy,ip,ti] = max(VO[ia,iy,ip,ti],VS[ia,iy,ip,ti])
				 DO[ia,iy,ip,ti] = which.max(c(VO[ia,iy,ip,ti],VS[ia,iy,ip,ti]))
				
	 			}	
			}
        }
	# integrate
	# =========
	
	 tmpO = WO[ , , ,ti]
	 tmpR = WR[ , , ,ti]
	 EWO[ , , ,ti] = integr(tmpO, G)
	 EWR[ , , ,ti] = integr(tmpR, G)
     }
Rtime <- proc.time() - Rtime

# timings
print(Rtime)
print(sum(blitz$time/1e9))
print(diff(blitz$time/1e9))

# get conditional consumption functions
# =====================================
consR <- array(matrix(CR[ , , ,1:(nT-1), ],nA*nY*nP*(nT-1),nA)[cbind(1:(nA*nY*nP*(nT-1)),as.numeric(saveR[ , , ,1:(nT-1)]))], c(nA,nY,nP,nT-1))
consB <- array(matrix(CB[ , , ,1:(nT-1), ],nA*nY*nP*(nT-1),nA)[cbind(1:(nA*nY*nP*(nT-1)),as.numeric(saveB[ , , ,1:(nT-1)]))], c(nA,nY,nP,nT-1))
consS <- array(matrix(CS[ , , ,1:(nT-1), ],nA*nY*nP*(nT-1),nA)[cbind(1:(nA*nY*nP*(nT-1)),as.numeric(saveS[ , , ,1:(nT-1)]))], c(nA,nY,nP,nT-1))
consO <- array(matrix(CO[ , , ,1:(nT-1), ],nA*nY*nP*(nT-1),nA)[cbind(1:(nA*nY*nP*(nT-1)),as.numeric(saveO[ , , ,1:(nT-1)]))], c(nA,nY,nP,nT-1))
# =====================================
print(all.equal(WO,blitz$WO))
print(all.equal(WR,blitz$WR))
print(all.equal(EWO,blitz$EVown))
print(all.equal(EWR,blitz$EVrent))
print(all.equal(DO,blitz$DO))
print(all.equal(DR,blitz$DR))

print(all.equal(VO,blitz$vstay))
print(all.equal(VR,blitz$vrent))
print(all.equal(VS,blitz$vsell))
print(all.equal(VB,blitz$vbuy))

print(all.equal(saveO,blitz$saveO))
print(all.equal(saveR,blitz$saveR))
print(all.equal(saveS,blitz$saveS))
print(all.equal(saveB,blitz$saveB))
}

