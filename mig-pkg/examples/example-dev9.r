


# example for dev8 but larger state space: No R.
# ==============================================

rm(list=ls())
library(migration)

# savings: grid search
# housing: yes/no
# utility: CRRA with housing utility
# location: discrete choice over locations
# moving cost: dist(here,there)

# this example does not computation in R. Implements more points.

# assume for this example that locations differ only by a fixed
# number. i.e. locations are ordered from 1:nL and nL gives biggest
# value. the moving cost is just the distance in the indices.

# this problem adds two dimension so previous versions: here and there.
# 1) moving from here to here is of course the same as staying. 
# 2) one cannot stay from here to there, one must sell here and buy or rent there.

# renter state: V = max( rent, buy )
# owner state: V = max( stay , sell )

cat("start building state space\n")

nA <- 50L; nY <- 2L; nT <- 20L; nP <- 3L; nL <- 10L
G <- rouwenhorst(rho=0.9,n=nY,sigma=0.1)$Pmat
dims <- c(nA,nY,nP,nL,nL,nT)
cat('trying to allocate a data.table with',prod(dims)*nA,'rows\n')
names(dims) <- c("a","y","p","here","there","age")
dataR <- list( dims=dims,dimshere = c(nA,nY,nP,nL,nT),
               theta = 0.2,beta=0.95,gamma=1.4,
               myNA=-99,rent=0.05,R=1/(1+0.04),down=0.2,
               G = as.numeric(rouwenhorst(rho=0.9,n=nY,sigma=0.1)$Pmat))
               
grids <- list()
grids$p <- seq(1,10,length=nP)
grids$a <- seq(-(1-dataR$down)*max(grids$p),10,length=nA)
grids$L <- seq(1,3,length=nL)

# OTHER's statespace: add dimension "here" AND "there".

SS <- data.table(expand.grid(a=grids$a,y=1:nY,ip=1:nP,here=grids$L,there=grids$L,it=1:nT,save=grids$a))
SS[,p:=grids$p[ip] ]

# both income and prices are mappings from here and there to y and p.
# for now no location specific costs/differences in prices. add that later
SS[,cstay := a + y + 0.3*it - dataR$R*save]	
SS[,cbuy  := a + y + 0.3*it - p - dataR$R*save ]
SS[,csell := a + y + 0.3*it - dataR$rent + p - dataR$R*save ]
SS[,crent := a + y + 0.3*it - dataR$rent     - dataR$R*save ]

# restrictions
# ============

# stayer: 
SS[it==nT & a+y+p>0,cstay := log(a+y+p) ]
SS[it==nT & a+y+p<0,cstay := dataR$myNA ]
#SS[here != there, cstay := a + y + 0.3*it + p[here] - p[there] - dataR$R*save ] # TODO
#SS[here != there & save< -(1-dataR$down)*p[there], cstay := dataR$myNA ] # if you are an owner moving, you must buy a new house and the borrowing constraint applies
SS[here != there & save< -(1-dataR$down)*p & it!=nT, cstay := dataR$myNA ] # if you are an owner moving, you must buy a new house and the borrowing constraint applies

# buyer: cannot have negative assets
# buyer: cannot borrow more than (1-down) times value of house
SS[a<0 & it!=nT,             cbuy  := dataR$myNA]
SS[save < -(1-dataR$down)*p, cbuy  := dataR$myNA]

# renter: cannot have negative assets
# renter: cannot borrow
# renter: final utility
SS[a<0         , crent := dataR$myNA]
SS[save<0      , crent := dataR$myNA]
SS[a>0 & it==nT, crent := log(a)    ]

# seller: can have negative assets
# seller: but cannot borrow. must pay off housing debt upon sale.
SS[save<0, csell := dataR$myNA]

# LOCATION statespace
# ============
# this table will hold amenity values at each location as well as
# utility moving costs from here to there.
SS.loc <- data.table(expand.grid(here=grids$L,there=grids$L))
SS.loc[,mcost := abs(here-there)]
move.cost <- SS.loc[,matrix(mcost,nL,nL,)]
rownames(move.cost) <- paste0("here",1:nL)
colnames(move.cost) <- paste0("there",1:nL)

amenity <- grids$L

# tensors
# =======

CR <- SS[,array(crent,c(dataR$dims,nA))]
CB <- SS[,array(cbuy,c(dataR$dims,nA))]
CS <- SS[,array(csell,c(dataR$dims,nA))]
CO <- SS[,array(cstay,c(dataR$dims,nA))]	# "O" is for "Owner", i.e. "stay"

dataR$consR <- as.numeric(CR)
dataR$consB <- as.numeric(CB)
dataR$consS <- as.numeric(CS)
dataR$consO <- as.numeric(CO)

dataR$MoveCost <- as.numeric(move.cost)
dataR$Amenity  <- grids$L

cat("done building state space\n")

blitz <- dev8(data=dataR)

print(sum(blitz$time/1e9))


