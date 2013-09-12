
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

test_that("test 5: optimal savings 1 equal in R and C++",{
		 expect_that(all.equal(S1,blitz$save1), is_true()) } )

test_that("test 5: optimal savings 2 equal in R and C++",{
		 expect_that(all.equal(S2,blitz$save2), is_true()) } )

test_that("test 5: optimal cons 1 equal in R and C++",{
		 expect_that(all.equal(Cstar1,CstarBlitz1), is_true()) } )

test_that("test 5: optimal cons 2 equal in R and C++",{
		 expect_that(all.equal(Cstar2,CstarBlitz2), is_true()) } )





