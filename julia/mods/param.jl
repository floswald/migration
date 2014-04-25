


# module param


import Base.show

export Param

# define the struct

type Param

	# utility function parameters
	beta    :: Float64
	gamma   :: Float64
	lambda  :: Float64          # default penalty
	psi     :: Array{Float64,1} # values for psi
	psidist :: Array{Float64,1} # distribution of psi
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

	# numerical setup
	# points in each dimension
	na   ::Int 	# number of asset points
	nz   ::Int 	# number of inidividual income states
	nh   ::Int 	# number of housing states
	nt   ::Int 	# number of periods
	npsi ::Int 	# number of types
	nP   ::Int 	# aggregate price levels
	nY   ::Int 	# aggregate income levels
	nJ   ::Int 	# number of origin locations
	            # nk = nj - 1 is the number of destination locations
	np   ::Int # number of price levels in each location
	ny   ::Int # number of income levels by location

	# bounds on grids
	bounds  :: Dict{ASCIIString,(Float64,Float64)}
	pbounds :: Dict{ASCIIString,Dict{Int,Array{Float64,1}}}


	dimvec ::(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) # total number of dimensions
	dimvec2::(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) # total number of dimensions



	# constructor assigning initial values
	function Param()

		bounds = ["asset_own" => (-10.0,10.0)]   
		bounds["asset_rent"] = (0.01,10.0)
		bounds["psi"]        = (0.0,1.0)
		bounds["P"]        = (1.0,5.0)
		bounds["Y"]        = (0.5,1.5)
		pbounds = Dict{ASCIIString,Dict{Int,(Float64,Float64)}}
		pbounds = ["p" => [i => sort(rand(2)) for i = 1:9] ]
		pbounds["y"] = [i => sort(rand(2)) for i = 1:9] 

		na    = 10
		nz    = 4
		nh    = 2
		nt    = 10
		npsi  = 2
		nP    = 2
		nY    = 2
		nJ    = 9
		np    = 4
		ny    = 4

		dimvec  = ((nt-1), na, nz, nh, npsi, nP, nY, nJ ,np ,ny, (nJ-1))
		dimvec2 = ((nt-1), na, nz, nh, npsi, nP, nY, nJ ,np ,ny)

		beta    = 0.95
		gamma   = 2
		lambda  = 10
		# psi     = linspace(0,1,npsi)
		psi     = linspace(bounds["psi"][1],bounds["psi"][2],npsi)
		psidist = rand(npsi)
		psidist = psidist/sum(psidist)
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


		# create object

		return new(beta,gamma,lambda,psi,psidist,xi,omega,MC,kappa,phi,rhop,rhoy,rhoz,rhoP,rhoY,R,Rm,chi,na,nz,nh,nt,npsi,nP,nY,nJ,np,ny,bounds,pbounds,dimvec,dimvec2)
	end

	
end


# define member functions

function nPoints(p::Param)
	r = p.na * p.nz * p.nh * (p.nt-1) * p.npsi * p.nP * p.nY * p.nJ * (p.nJ - 1) * p.np * p.ny
	return r
end
# show(io::IO, p::Param) = print(io,"number of points=$(p.na*p.nz*p.nt)")
show(io::IO, p::Param) = print(io,"number of points:$(nPoints(p))\nnumber of dims  :$(length(p.dimvec))")




