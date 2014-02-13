
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
print(all.equal(s1[it==3,array(log(cash),c(nA,nY))],blitz$V1[ , ,3]))
