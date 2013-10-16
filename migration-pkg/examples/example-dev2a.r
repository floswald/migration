
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
blitz <- dev2a(data=dataR)
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
print(all.equal(V1_R,blitz$V1))
