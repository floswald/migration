
nA <- 20L; nY <- 3L; nT <- 3L
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

######################################################
# Calculating an R solution to this 3-period lifecycle
# with discrete choice in periods 1 and 2.
######################################################

V = array(0,c(nA,nY,nT))  # max( v1, v2 )
D = array(0,c(nA,nY,nT))  # which.max( v1, v2 )
v1 = array(0,c(nA,nY,nT))
v2 = array(0,c(nA,nY,nT))
S1 = array(0,c(nA,nY,nT))
S2 = array(0,c(nA,nY,nT))
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
			S1[ia,iy,ti] = which.max(W1[ia,iy,ti, ])
			v2[ia,iy,ti] = max(W2[ia,iy,ti, ])
			S2[ia,iy,ti] = which.max(W2[ia,iy,ti, ])
			V[ia,iy,ti] = max(v1[ia,iy,ti], v2[ia,iy,ti])
			D[ia,iy,ti] = which.max(c(v1[ia,iy,ti], v2[ia,iy,ti]))
         }
     }
}
Cstar1 = array(matrix(C1[ , ,1:(nT-1), ],nA*nY*(nT-1),nA)[cbind(1:(nA*nY*(nT-1)),as.numeric(S1[ , ,1:(nT-1)]))], c(nA,nY,nT-1))
Cstar2 = array(matrix(C2[ , ,1:(nT-1), ],nA*nY*(nT-1),nA)[cbind(1:(nA*nY*(nT-1)),as.numeric(S2[ , ,1:(nT-1)]))], c(nA,nY,nT-1))
CstarBlitz1 = array( matrix(C1[ , ,1:(nT-1), ],nA*nY*(nT-1),nA)[cbind(1:(nA*nY*(nT-1)),as.numeric(blitz$save1[ , ,1:(nT-1)]) ) ], c(nA,nY,nT-1))
CstarBlitz2 = array( matrix(C2[ , ,1:(nT-1), ],nA*nY*(nT-1),nA)[cbind(1:(nA*nY*(nT-1)),as.numeric(blitz$save2[ , ,1:(nT-1)]) ) ], c(nA,nY,nT-1))

############################################
# comparing R and blitz++ solutions
############################################

print(all.equal(V,blitz$V))
print(all.equal(C1,blitz$cons1))
print(all.equal(C2,blitz$cons2))
print(all.equal(v1,blitz$v1))
print(all.equal(v2,blitz$v2))
print(all.equal(S1,blitz$save1))
print(all.equal(S2,blitz$save2))
print(all.equal(Cstar1,CstarBlitz1))
print(all.equal(Cstar2,CstarBlitz2))
