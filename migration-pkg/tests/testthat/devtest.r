
library(testthat)
library(migration)


context("test output values of several example problems")

# devel test 1: construct consumption tensor
# ==========================================

nA <- 5L; nY <- 3L; nT <- 3L
s1 <- data.table(expand.grid(a=seq(0,10,length=nA),y=1:nY,it=1:nT))
s1[,cash := a + y + 0.3*it]
s2 <- copy(s1)
s2[,cash := a + y + 0.2*it + 0.09*it^2]
save <- seq(0,10,length=nA)
dataR <- list( cash1 = s1[["cash"]],
               cash2 = s2[["cash"]],
               dims = c(nA,nY,nT),
               savings = save)
blitz <- devC(data=dataR)
s3    <- data.table(expand.grid(a=seq(0,10,length=nA),y=1:nY,it=1:nT,save=save))
s3[,cons := a + y + 0.3*it - save]
Rcons <- s3[,array(cons,c(nA,nY,nT,nA))]

test_that("consumption tensor from blitz is equal to tensor in R",{
		 expect_that(all.equal(s3[,array(cons,c(nA,nY,nT,nA))],blitz$vmax), is_true() ) } )


# devel test 2: evaluate utility in final period
# ==========================================

nA <- 5L; nY <- 3L; nT <- 3L
s1 <- data.table(expand.grid(a=seq(0,10,length=nA),y=1:nY,it=1:nT))
s1[,cash := a + y + 0.3*it]
s2 <- copy(s1)
s2[,cash := a + y + 0.2*it + 0.09*it^2]
save <- seq(0,10,length=nA)
dataR <- list( cash1 = s1[["cash"]],
               cash2 = s2[["cash"]],
               dims = c(nA,nY,nT),
               savings = save,
               theta = 1.2)
blitz <- dev1(data=dataR)




# devel test 3: evaluate the conditional value function
# ==============================================

nA <- 5L; nY <- 3L; nT <- 3L
s1 <- data.table(expand.grid(a=seq(0,10,length=nA),y=1:nY,it=1:nT))
s1[,cash := a + y + 0.3*it]
s2 <- copy(s1)
s2[,cash := a + y + 0.2*it + 0.09*it^2]
save <- seq(0,10,length=nA)
dataR <- list( cash1 = s1[["cash"]],
               cash2 = s2[["cash"]],
               dims = c(nA,nY,nT),
               savings = save,
               theta = 1.2,beta=0.95,myNA=-99)
blitz <- dev2(data=dataR)
V1_R = s1[,array(cash,c(nA,nY,nT))]
V1_R[ , ,3] = log(V1_R[ , ,3])
for (ti in 2:1) {
    for (ia in 1:nA) {
         for(iy in 1:nY) {
			if (V1_R[ia,iy,ti]<0) {
				V1_R[ia,iy,ti] <- dataR$myNA
			} else {
				V1_R[ia,iy,ti] = log(V1_R[ia,iy,ti]) + dataR$beta*V1_R[ia,iy,ti+1]
			}
         }
     }
}

test_that("V1 in R and blitz++ are the same",{
		 expect_that(all.equal(V1_R,blitz$V1), is_true() ) } )


# devel test 4: test solution to 3 period lifecycle problem
# ==============================================


nA <- 5L; nY <- 3L; nT <- 3L
s1 <- data.table(expand.grid(a=seq(0,10,length=nA),y=1:nY,it=1:nT))
s1[,cash := a + y + 0.3*it]
save <- seq(0,10,length=nA)
s3 <- data.table(expand.grid(a=seq(0,10,length=nA),y=1:nY,it=1:nT,save=save))
s3[,cons := a + y + 0.3*it - save]
dataR <- list( cash1 = s1[["cash"]],
               dims = c(nA,nY,nT),
               savings = save,
               theta = 1.2,beta=0.95,myNA=-99)
blitz <- dev3(data=dataR)

######################################################
# Calculating an R solution to this 3-period lifecycle
######################################################

V = array(0,c(nA,nY,nT))
V[ , ,3] = s1[it==3,log(cash)]
C = s3[,array(cons,c(nA,nY,nT,nA))]
W = array(0,c(nA,nY,nT,nA))
for (ti in 2:1) {
    for (ia in 1:nA) {
         for(iy in 1:nY) {
             for (ja in 1:nA){
                 if (C[ia,iy,ti,ja] > 0){
		     		W[ia,iy,ti,ja] =  log(C[ia,iy,ti,ja])  + dataR$beta*V[ja,iy,ti+1]
			     } else {
			        W[ia,iy,ti,ja] = dataR$myNA
			     }
		     }
			V[ia,iy,ti] = max(W[ia,iy,ti, ])
         }
     }
}
############################################
# testing R and blitz++ solutions
############################################

test_that("test 4: V equal in R and C++",{
		 expect_that(all.equal(V,blitz$V1), is_true()) } )

test_that("test 4: Cons equal in R and C++",{
		 expect_that(all.equal(C,blitz$cons), is_true()) } )

test_that("test 4: Full tensor W equal in R and C++",{
		 expect_that(all.equal(W[ , ,1, ],blitz$w), is_true()) } )



# devel test 5: test solution to 3 period lifecycle problem
# with discrete choice in periods 1 and 2
# ==============================================


nA <- 5L; nY <- 3L; nT <- 3L
s <- data.table(expand.grid(a=seq(0,10,length=nA),y=1:nY,it=1:nT))
s[,cash1 := a + y + 0.3*it]
s[,cash2 := a + y + 0.2*it + 0.09*it^2]
save <- seq(0,10,length=nA)
ss <- data.table(expand.grid(a=seq(0,10,length=nA),y=1:nY,it=1:nT,save=save))
ss[,cons1 := a + y + 0.3*it - save]
ss[,cons2 := a + y + 0.2*it + 0.09*it^2 - save]
dataR <- list( cash1 = s[["cash1"]],
               cash2 = s[["cash2"]],
               dims = c(nA,nY,nT),
               savings = save,
               theta = 1.2,beta=0.95,myNA=-99)
blitz <- dev4(data=dataR)

V = array(0,c(nA,nY,nT))  # max( v1, v2 )
D = array(0,c(nA,nY,nT))  # which.max( v1, v2 )
v1 = array(0,c(nA,nY,nT))
v2 = array(0,c(nA,nY,nT))
V[ , ,3] = s[it==3,log(cash1)]
C1 = ss[,array(cons1,c(nA,nY,nT,nA))]
C2 = ss[,array(cons2,c(nA,nY,nT,nA))]

W1 = array(0,c(nA,nY,nT,nA))
W2 = array(0,c(nA,nY,nT,nA))
for (ti in 2:1) {
    for (ia in 1:nA) {
         for(iy in 1:nY) {
             for (ja in 1:nA){
				 # optimal cons option 1
                 if (C1[ia,iy,ti,ja] > 0){
		     		W1[ia,iy,ti,ja] = log(C1[ia,iy,ti,ja]) + dataR$beta*V[ja,iy,ti+1]
			     } else {
			        W1[ia,iy,ti,ja] = dataR$myNA
			     }
				 # optimal cons option 2
                 if (C2[ia,iy,ti,ja] > 0){
		     		W2[ia,iy,ti,ja] = log(C2[ia,iy,ti,ja]) + dataR$beta*V[ja,iy,ti+1]
			     } else {
			        W2[ia,iy,ti,ja] = dataR$myNA
			     }
		     }
			v1[ia,iy,ti] = max(W1[ia,iy,ti, ])
			v2[ia,iy,ti] = max(W2[ia,iy,ti, ])
			V[ia,iy,ti] = max(v1[ia,iy,ti], v2[ia,iy,ti])
			D[ia,iy,ti] = which.max(c(v1[ia,iy,ti], v2[ia,iy,ti]))
         }
     }
}

test_that("test 5: V equal in R and C++",{
		 expect_that(all.equal(V,blitz$V), is_true()) } )

test_that("test 5: D equal in R and C++",{
		 expect_that(all.equal(D,blitz$D), is_true()) } )

test_that("test 5: full Cons 1 equal in R and C++",{
		 expect_that(all.equal(C1,blitz$cons1), is_true()) } )

test_that("test 5: full Cons 2 equal in R and C++",{
		 expect_that(all.equal(C2,blitz$cons2), is_true()) } )

test_that("test 5: conditional value 1 equal in R and C++",{
		 expect_that(all.equal(v1,blitz$v1), is_true()) } )

test_that("test 5: conditional value 2 equal in R and C++",{
		 expect_that(all.equal(v2,blitz$v2), is_true()) } )

test_that("test 5: optimal cons 1 equal in R and C++",{
		 expect_that(all.equal(Cstar1,CstarBlitz1), is_true()) } )

test_that("test 5: optimal cons 2 equal in R and C++",{
		 expect_that(all.equal(Cstar2,CstarBlitz2), is_true()) } )



# devel test 6: test solution to 3 period lifecycle problem
# with discrete housing choice in periods 1 and 2 and discretized savings choice
# ==============================================


nA <- 50L; nY <- 5L; nT <- 5L; nH <- 2L; nP <- 3L

dataR <- list( dims = c(nA,nY,nP,nT),
               theta = 1.2,beta=0.95,myNA=-99,rent=0.05,R=1/(1+0.04),down=0.2)
               
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
blitz <- dev5(data=dataR)

######################################################
# Calculating an R solution to this lifecycle
######################################################

Rtime <- proc.time()
# envelopes of conditional values
WO = array(0,dataR$dims)
WR = array(0,dataR$dims)

# discrete choice amoung conditional values
DO = array(0,dataR$dims)
DR = array(0,dataR$dims)

# conditional values
VR = array(0,dataR$dims)
VB = array(0,dataR$dims)
VS = array(0,dataR$dims)
VO = array(0,dataR$dims)

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
WR[ , , ,nT] <- sR[it==nT&save==grids$a[nA],array(cons,c(nA,nY,nP))]
WO[ , , ,nT] <- sO[it==nT&save==grids$a[nA],array(cons,c(nA,nY,nP))]
# WR[ , , ,nT] <- sT[,array(log(cashR),c(nA,nY,nP))]
# WO[ , , ,nT] <- sT[,array(log(cashO),c(nA,nY,nP))]
# WR[is.nan(WR)] <- dataR$myNA
# WO[is.nan(WR)] <- dataR$myNA

for (ti in (nT-1):1) {
    for (ia in 1:nA) {
         for(iy in 1:nY) {
			 for (ip in 1:nP){
				 for (ja in 1:nA){
					 # renter
					 if (CR[ia,iy,ip,ti,ja] < 0 | !is.finite(CR[ia,iy,ip,ti,ja])){
						xR[ia,iy,ip,ti,ja] = dataR$myNA
					 } else {
						xR[ia,iy,ip,ti,ja] =  log(CR[ia,iy,ip,ti,ja])  + dataR$beta*WR[ja,iy,ip,ti+1]
					 }
					 # buyer
					 if (CB[ia,iy,ip,ti,ja] < 0 | !is.finite(CB[ia,iy,ip,ti,ja])){
						xB[ia,iy,ip,ti,ja] = dataR$myNA
					 } else {
						xB[ia,iy,ip,ti,ja] =  log(CB[ia,iy,ip,ti,ja])  + dataR$beta*WO[ja,iy,ip,ti+1]
					 }
					 # seller
					 if (CS[ia,iy,ip,ti,ja] < 0 | !is.finite(CS[ia,iy,ip,ti,ja])){
						xS[ia,iy,ip,ti,ja] = dataR$myNA
					 } else {
						xS[ia,iy,ip,ti,ja] =  log(CS[ia,iy,ip,ti,ja])  + dataR$beta*WR[ja,iy,ip,ti+1]
					 }
					 # owner
					 if (CO[ia,iy,ip,ti,ja] < 0 | !is.finite(CO[ia,iy,ip,ti,ja])){
						xO[ia,iy,ip,ti,ja] = dataR$myNA
					 } else {
						xO[ia,iy,ip,ti,ja] =  log(CO[ia,iy,ip,ti,ja])  + dataR$beta*WO[ja,iy,ip,ti+1]
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
}

test_that("test 6: WO equal in R and C++",{
		 expect_that(all.equal(WO,blitz$WO), is_true()) } )

test_that("test 6: WR equal in R and C++",{
		 expect_that(all.equal(WR,blitz$WR), is_true()) } )

test_that("test 6: DO equal in R and C++",{
		 expect_that(all.equal(DO,blitz$DO), is_true()) } )

test_that("test 6: DR equal in R and C++",{
		 expect_that(all.equal(DR,blitz$DR), is_true()) } )

test_that("test 6: VR equal in R and C++",{
		 expect_that(all.equal(VR,blitz$VR), is_true()) } )

test_that("test 6: VS equal in R and C++",{
		 expect_that(all.equal(VS,blitz$VS), is_true()) } )

test_that("test 6: VO equal in R and C++",{
		 expect_that(all.equal(VO,blitz$VO), is_true()) } )

test_that("test 6: VB equal in R and C++",{
		 expect_that(all.equal(VB,blitz$VB), is_true()) } )

test_that("test 6: saveR equal in R and C++",{
		 expect_that(all.equal(saveR,blitz$saveR), is_true()) } )

test_that("test 6: saveO equal in R and C++",{
		 expect_that(all.equal(saveO,blitz$saveO), is_true()) } )

test_that("test 6: saveB equal in R and C++",{
		 expect_that(all.equal(saveB,blitz$saveB), is_true()) } )

test_that("test 6: saveS equal in R and C++",{
		 expect_that(all.equal(saveS,blitz$saveS), is_true()) } )



