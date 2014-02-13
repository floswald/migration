
# minimal blitz example.
# checks mapping R to blitz

x  <- sample(1:120,size=120,replace=F)
option1 <- array(x[1:60],c(2,5,6))
option2 <- array(x[61:100],c(2,5,6))
savings  <- array(runif(n=60),c(2,5,6))
dataR <- list(option1 = option1, option2 = option2, savings=savings)
blitz <- dev_blitz1(data=dataR)
print(blitz)
r = list()
As <- option1 - savings
A2s <- option2 - savings
V1 = apply(As,c(1,2),max)
V2 = apply(A2s,c(1,2),max)
V12 = array(c(V1,V2),c(2,5,2))
r$vmax = apply(V12,c(1,2),max)
r$dchoice = apply(V12,c(1,2),which.max)
print(all.equal(r,blitz))
