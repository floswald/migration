


nA <- 5L; nY <- 3L; nT <- 3L; nH <- 2L; nP <- 2L

dataR <- list( dims = c(nA,nY,nP,nT),
               theta = 1.2,beta=0.95,myNA=-99,rent=0.05,R=1/(1+0.04),down=0.2)
               
grids <- list()
grids$p <- seq(1,10,length=nP)
grids$aR <- seq(0,10,length=nA)
grids$aO <- seq(-(1-dataR$down)*max(grids$p),10,length=nA)


# sR means you are renter
sR <- data.table(expand.grid(a=grids$aO,y=1:nY,p=grids$p,it=1:nT,save=grids$aO))
sR[,cons := a + y + 0.3*it - dataR$rent - dataR$R*save]
sR[a<0, cons := dataR$myNA]
sR[save<0, cons := dataR$myNA]

# sB is buyer: admissible savings depends on current house price.
sB <- data.table(expand.grid(a=grids$aO,y=1:nY,p=grids$p,it=1:nT,save=grids$aO))
sB[,cons := a + y + 0.3*it - p - dataR$R*save]
sB[a<0, cons := dataR$myNA]
sB[save < -(1-dataR$down)*p, cons := dataR$myNA, by=list(y,p,it) ]

# sS means you are seller->renter
sS <- data.table(expand.grid(a=grids$aO,y=1:nY,p=grids$p,it=1:nT,save=grids$aO))
sS[,cons := a + y + 0.3*it - dataR$rent + p - dataR$R*save]
sS[save<0, cons := dataR$myNA]

# sO means you are owner
sO <- data.table(expand.grid(a=grids$aO,y=1:nY,p=grids$p,it=1:nT,save=grids$aO))
sO[,cons := a + y + 0.3*it - dataR$R*save]

# tensors
CR <- sR[,array(cons,c(dataR$dims,nA))]
CB <- sR[,array(cons,c(dataR$dims,nA))]
CS <- sR[,array(cons,c(dataR$dims,nA))]
CO <- sR[,array(cons,c(dataR$dims,nA))]

WR = array(0,c(dataR$dims,nA))
WB = array(0,c(dataR$dims,nA))
WS = array(0,c(dataR$dims,nA))
WO = array(0,c(dataR$dims,nA))

blitz <- dev3(data=dataR)

######################################################
# Calculating an R solution to this lifecycle
######################################################

# overall value
V = array(0,dataR$dims)
V[ , , ,nT] = sR[it==nT,log(a)]

# sub value renter owner
VVO = array(0,dataR$dims)
VVR = array(0,dataR$dims)

# conditional values
VR = array(0,dataR$dims)
VB = array(0,dataR$dims)
VS = array(0,dataR$dims)
VO = array(0,dataR$dims)

for (ti in (nT-1):1) {
    for (ia in 1:nA) {
         for(iy in 1:nY) {
			 for (ip in 1:nP){
				 for (ja in 1:nA){
					 # renter
					 if (CR[ia,iy,ip,ti,ja] < 0 | !is.finite(CR[ia,iy,ip,ti,ja])){
						WR[ia,iy,ti,ja] = dataR$myNA
					 } else {
						WR[ia,iy,ip,ti,ja] =  log(CR[ia,iy,ip,ti,ja])  + dataR$beta*VVR[ja,iy,ip,ti+1]
					 }
					 # buyer
					 if (CB[ia,iy,ip,ti,ja] < 0 | !is.finite(CB[ia,iy,ip,ti,ja])){
						WB[ia,iy,ti,ja] = dataR$myNA
					 } else {
						WB[ia,iy,ip,ti,ja] =  log(CB[ia,iy,ip,ti,ja])  + dataR$beta*VVO[ja,iy,ip,ti+1]
					 }
					 # seller
					 if (CS[ia,iy,ip,ti,ja] < 0 | !is.finite(CS[ia,iy,ip,ti,ja])){
						WS[ia,iy,ti,ja] = dataR$myNA
					 } else {
						WS[ia,iy,ip,ti,ja] =  log(CS[ia,iy,ip,ti,ja])  + dataR$beta*VVR[ja,iy,ip,ti+1]
					 }
					 # owner
					 if (CO[ia,iy,ip,ti,ja] < 0 | !is.finite(CO[ia,iy,ip,ti,ja])){
						WO[ia,iy,ti,ja] = dataR$myNA
					 } else {
						WO[ia,iy,ip,ti,ja] =  log(CO[ia,iy,ip,ti,ja])  + dataR$beta*VVO[ja,iy,ip,ti+1]
					 }
				 }
				 # conditional values renter state
				 VR[ia,iy,ip,ti] = max(WR[ia,iy,ip,ti, ])
				 VB[ia,iy,ip,ti] = max(WB[ia,iy,ip,ti, ])
				 # max val renter state
				 VVR[ia,iy,ip,ti] = max(VR[ia,iy,ip,ti],VB[ia,iy,ip,ti])
				 DDR[ia,iy,ip,ti] = which.max(c(VR[ia,iy,ip,ti],VB[ia,iy,ip,ti]))

				 # conditional values owner state
				 VS[ia,iy,ip,ti] = max(WS[ia,iy,ip,ti, ])
				 VO[ia,iy,ip,ti] = max(WO[ia,iy,ip,ti, ])
				 # max val owner state
				 VVO[ia,iy,ip,ti] = max(VO[ia,iy,ip,ti],VS[ia,iy,ip,ti])
				 DDO[ia,iy,ip,ti] = which.max(c(VO[ia,iy,ip,ti],VS[ia,iy,ip,ti]))

				 # vmax
				 V[ia,iy,ip,ti] = max(VVR[ia,iy,ip,ti],VB[ia,iy,ip,ti],VS[ia,iy,ip,ti],VO[ia,iy,ip,ti])
				 D[ia,iy,ip,ti] = which.max(c(VR[ia,iy,ip,ti],VB[ia,iy,ip,ti],VS[ia,iy,ip,ti],VO[ia,iy,ip,ti]))
			 }
         }
     }
}
