


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
	rhoY  :: Float64          # AR1 coef for the national process

	R  :: Float64 	# gross interest rate savings: 1+r
	Rm :: Float64 	# gross interest rate mortgage: 1+rm
	chi:: Float64   # downpayment ratio
	myNA:: Float64
	maxAge::Int 	# maximal age in model
	minAge::Int 	# maximal age in model
	euler::Float64

	# numerical setup
	# points in each dimension
	na   ::Int 	# number of asset points
	nz   ::Int 	# number of inidividual income states
	nh   ::Int 	# number of housing states
	nt   ::Int 	# number of periods
	ntau ::Int 	# number of types
	nP   ::Int 	# aggregate price levels
	nY   ::Int 	# aggregate income levels
	nJ   ::Int 	# number of origin locations
	            # nk = nj - 1 is the number of destination locations
	np   ::Int # number of price levels in each location
	ny   ::Int # number of income levels by location

	# bounds on grids
	bounds  :: Dict{ASCIIString,(Float64,Float64)}
	pbounds :: Dict{ASCIIString,Dict{Int,Any}}


	dimvec ::(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) # total number of dimensions
	dimvec2::(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) # total - housing
	dimvec3::(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) # total - housing - location



	# constructor assigning initial values
	function Param()

		bounds = ["asset_own" => (-10.0,10.0)]   
		bounds["asset_rent"] = (0.01,10.0)
		bounds["tau"]        = (0.0,1.0)
		bounds["P"]          = (1.0,5.0)
		bounds["Y"]          = (0.5,1.5)
		pbounds = ["p" => [i => sort(rand(2)) for i = 1:9] ]
		pbounds["y"] = [i => sort(rand(2)) for i = 1:9] 

		na    = 10
		nz    = 4
		nh    = 2
		nt    = 10
		ntau  = 2
		nP    = 3
		nY    = 2
		nJ    = 9
		np    = 3
		ny    = 4

		dimvec  = (na, nz, nh, ntau, nP, nY, np ,ny, nJ, (nt-1),  nh, nJ)
		dimvec2 = (na, nz, nh, ntau, nP, nY, np ,ny, nJ, (nt-1),  nJ)
		dimvec3 = (na, nz, nh, ntau, nP, nY, np ,ny, nJ, (nt-1)) 

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
		omega   = [1.0, 0.8]

		# other parameters
		MC    = [0.1, 0.2, 0.3] # parameters in moving cost: alpha1, alpha2, alpha3
		kappa = [rand() for i=1:9] # rent to price ratio in each region
		phi   = 0.06		  # fixed cost of selling

		rhop = rand(nJ)
		rhoy = rand(nJ)
		rhoz = rand(nJ)
		rhoP = rand()
		rhoY = rand()

		R   = 1.03 	# gross interest rate savings: 1+r
		Rm  = 1.06 	# gross interest rate mortgage: 1+rm
		chi = 0.2   # downpayment ratio
		myNA = -999
		maxAge = 50
		minAge = 50
		euler = 0.5772	# http://en.wikipedia.org/wiki/Gumbel_distribution

		# create object

			return new(beta,gamma,mgamma,imgamma,lambda,tau,taudist,xi,omega,MC,kappa,phi,rhop,rhoy,rhoz,rhoP,rhoY,R,Rm,chi,myNA,maxAge,minAge,euler,na,nz,nh,nt,ntau,nP,nY,nJ,np,ny,bounds,pbounds,dimvec,dimvec2,dimvec3)
	end

	
end


# define member functions

function nPoints(p::Param)
	r = p.na * p.nz * p.nh * (p.nt-1) * p.ntau * p.nP * p.nY * p.nJ * (p.nJ - 1) * p.np * p.ny
	return r
end
# show(io::IO, p::Param) = print(io,"number of points=$(p.na*p.nz*p.nt)")
show(io::IO, p::Param) = print(io,"number of points:$(nPoints(p))\nnumber of dims  :$(length(p.dimvec))")




