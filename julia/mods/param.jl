	


# module param


import Base.show

export Param

# define the struct

type Param

	# utility function parameters
	beta    :: Float64			# discount factor
	gamma   :: Float64			# CRRA
	mgamma  :: Float64			# (1-CRRA)
	imgamma :: Float64			# 1/(1-CRRA)
	lambda  :: Float64          # default penalty
	tau     :: Array{Float64,1} # values for tau
	taudist :: Array{Float64,1} # distribution of tau
	xi      :: Float64          # utility of housing
	omega   :: Array{Float64,1} # final period utility parameters, omega1 and omega2

	# other parameters
	MC    :: Array{Float64,1} # parameters in moving cost: alpha1, alpha2, alpha3
	kappa :: Array{Float64,1} # rent to price ratio in each region
	phi   :: Float64		  # fixed cost of selling

	rhop  :: Array{Float64,1} # vector of AR1 coefs for each region price deviation
	rhoy  :: Array{Float64,1} # vector of AR1 coefs for each region income deviation
	rhoz  :: Array{Float64,1} # vector of AR1 coefs for each region individual-specific income shock
	rhoP  :: Float64          # AR1 coef for the national process
	# rhoY  :: Float64          # AR1 coef for the national process

	R  :: Float64 	# gross interest rate savings: 1+r
	Rm :: Float64 	# gross interest rate mortgage: 1+rm
	chi:: Float64   # downpayment ratio
	myNA:: Float64
	maxAge::Int 	# maximal age in model
	minAge::Int 	# maximal age in model
	euler::Float64
	medinc::Float64 # median income in 1996

	# numerical setup
	# points in each dimension
	na   ::Int 	# number of asset points
	nz   ::Int 	# number of inidividual income states
	nh   ::Int 	# number of housing states
	nt   ::Int 	# number of periods
	ntau ::Int 	# number of types
	nP   ::Int 	# aggregate price levels
	# nY   ::Int 	# aggregate income levels
	nJ   ::Int 	# number of origin locations
	            # nk = nj - 1 is the number of destination locations
	np   ::Int # number of price levels in each location
	ny   ::Int # number of income levels by location

	# bounds on grids
	bounds  :: Dict{ASCIIString,(Float64,Float64)}
	pbounds :: Dict{ASCIIString,Array{Any,2}}


	dimvec ::(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) # total number of dimensions
	dimvec2::(Int,Int,Int,Int,Int,Int,Int,Int,Int) # total - housing
	dimnames::Array{ASCIIString}
	regnames::Array{ASCIIString}

	Ageprof::Array{Any,2}
	IncomeParams::Array{Any,2}
	distance::Array{Any,2}



	# constructor assigning initial values
	function Param(size::Int)

		bounds = ["asset_own" => (-10.0,10.0)]   
		bounds["asset_rent"] = (0.01,10.0)
		bounds["tau"]        = (0.0,1.0)
		# bounds["Y"]          = (0.5,1.5)

		# get bounds data from R
		xy = readcsv("/Users/florianoswald/Dropbox/mobility/output/model/R2julia/divincome.csv")
		xy = xy[2:end,2:end]
		pbounds = ["y" => xy[:,3:4]]

		xp = readcsv("/Users/florianoswald/Dropbox/mobility/output/model/R2julia/divprice.csv")
		xp = xp[2:end,2:end]
		pbounds["p"] = xp[:,3:4]

		regnames = xy[:,1]

		AgeP = readcsv("/Users/florianoswald/Dropbox/mobility/output/model/BBL/inc-process/Div-AgeProfile.csv")
		AgeP = AgeP[2:end,:]
		IncomeParams = readcsv("/Users/florianoswald/Dropbox/mobility/output/model/BBL/inc-process/Div-IncParams.csv")

		dist = readcsv("/Users/florianoswald/Dropbox/mobility/output/model/R2julia/distance.csv")

		xP = readcsv("/Users/florianoswald/Dropbox/mobility/output/model/R2julia/p2y.csv")
		xP = xP[2:end,2:end]
		xP = xP[xP[:,1] .> 1995,:]
		bounds["P"]  = (minimum(xP[:,2]),maximum(xP[:,2]))

		medinc = readcsv("/Users/florianoswald/Dropbox/mobility/output/model/R2julia/normalize.csv")

		if size==1

			# super small: use for tests
			na    = 5
			nz    = 3
			nh    = 2
			ntau  = 2
			nP    = 2
			# nY    = 3
			np    = 4
			ny    = 2
			nJ    = 3
			nt    = 4

		elseif size==2
			# small: runs on my box
			na    = 15
			nz    = 4
			nh    = 2
			ntau  = 2
			nP    = 3
			# nY    = 3
			np    = 3
			ny    = 3
			nJ    = 9
			nt    = 30

		elseif size==3
		# big: maximal size for memory on my box
		# much too slow
			na    = 40
			nz    = 4
			nh    = 2
			ntau  = 2
			nP    = 3
			# nY    = 3
			np    = 4
			ny    = 3
			nJ    = 9
			nt    = 30

		end		

		# TODO
		# find the middle indices of p and y
		# to compute "unconditional expectation"

		dimvec  = (nJ, ny, np, nP, nz, na, nh, ntau,  nJ, nt-1 )
		dimvec2 = (ny, np, nP, nz, na, nh, ntau,  nJ, nt-1 )
		dimnames = ["k", "y", "p", "P", "z", "a", "h", "tau", "j", "age" ]

		beta    = 0.95
		gamma   = 2
		mgamma  = 1-gamma
		imgamma = 1/mgamma
		lambda  = 10
		# tau     = linspace(0,1,ntau)
		tau     = linspace(bounds["tau"][1],bounds["tau"][2],ntau)
		taudist = rand(ntau)
		taudist = taudist/sum(taudist)
		xi      = 0.5
		omega   = [0.1, 0.1]

		# other parameters
		MC    = [0.1, 0.2, 0.3] # parameters in moving cost: alpha1, alpha2, alpha3
		kappa = [rand() for i=1:9] # rent to price ratio in each region
		phi   = 0.06		  # fixed cost of selling


		x = readcsv("/Users/florianoswald/Dropbox/mobility/output/model/R2julia/rho-income.csv")
		rhoy = x[2:end,3]
		x = readcsv("/Users/florianoswald/Dropbox/mobility/output/model/R2julia/rho-price.csv")
		rhop =  x[2:end,3]

		rhoz = IncomeParams[2:end,2]
		rhoP = rand()
		# rhoY = rand()

		R   = 1.03 	# gross interest rate savings: 1+r
		Rm  = 1.06 	# gross interest rate mortgage: 1+rm
		chi = 0.2   # downpayment ratio
		myNA = -999
		maxAge = 50
		minAge = 20
		euler = 0.5772	# http://en.wikipedia.org/wiki/Gumbel_distribution

		# create object

			# return new(beta,gamma,mgamma,imgamma,lambda,tau,taudist,xi,omega,MC,kappa,phi,rhop,rhoy,rhoz,rhoP,R,Rm,chi,myNA,maxAge,minAge,euler,na,nz,nh,nt,ntau,nP,nJ,np,ny)
			return new(beta,gamma,mgamma,imgamma,lambda,tau,taudist,xi,omega,MC,kappa,phi,rhop,rhoy,rhoz,rhoP,R,Rm,chi,myNA,maxAge,minAge,euler,medinc,na,nz,nh,nt,ntau,nP,nJ,np,ny,bounds,pbounds,dimvec,dimvec2,dimnames,regnames,AgeP,IncomeParams,dist)
	end

	
end


# define member functions

function nPoints(p::Param)
	r = prod(p.dimvec)
	return r
end
# show(io::IO, p::Param) = print(io,"number of points=$(p.na*p.nz*p.nt)")
show(io::IO, p::Param) = if nPoints(p) > 160000000 print(io,"number of savings problems to solve:$(nPoints(p))\nnumber of dims  :$(length(p.dimvec))\n CAUTION: this will not fit in 16GB of RAM!") else print(io,"number of dims  :$(length(p.dimvec))\nnumber of savings problems to solve:$(nPoints(p))") end




