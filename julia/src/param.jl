	


# define the struct


type Param

	# free parameters
	# ===============

	# utility function parameters
	beta    :: Float64			# discount factor
	gamma   :: Float64			# CRRA
	mgamma  :: Float64			# (1-CRRA)
	imgamma :: Float64			# 1/(1-CRRA)
	lambda  :: Float64          # default penalty
	tau     :: Float64 			# value of tau for high type
	taudist :: Float64 			# population prob of being high type
	xi      :: Float64          # utility of housing
	omega1  :: Float64 			# final period utility parameters
	omega2  :: Float64			# final period utility parameters

	# other parameters
	MC1    :: Float64 # parameters in moving cost: alpha1, alpha2, alpha3
	MC2    :: Float64 # parameters in moving cost: alpha1, alpha2, alpha3
	MC3    :: Float64 # parameters in moving cost: alpha1, alpha2, alpha3

	# fixed parameters
	# ================

	kappa  :: Array{Float64,1} # rent to price ratio in each region
	phi    :: Float64		  # fixed cost of selling
	rhop   :: Array{Float64,1} # vector of AR1 coefs for each region price deviation
	rhoy   :: Array{Float64,1} # vector of AR1 coefs for each region income deviation
	# rhoz  :: Array{Float64,1} # vector of AR1 coefs for each region individual-specific income shock
	rhoP   :: Float64          # AR1 coef for the national process
	# rhoY  :: Float64          # AR1 coef for the national process

	R      :: Float64 	# gross interest rate savings: 1+r
	Rm     :: Float64 	# gross interest rate mortgage: 1+rm
	chi    :: Float64   # downpayment ratio
	myNA   :: Float64
	maxAge :: Int 	# maximal age in model
	minAge :: Int 	# maximal age in model
	ages   :: UnitRange{Int} 	# maximal age in model
	euler  :: Float64

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

	# used in simulations
	nsim::Int # number of individuals in each location

	# constructor assigning initial values
	function Param(size::Int)



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
			nsim  = 100

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
			nsim  = 10000

		elseif size==3
		# big: maximal size for memory on my box
		# much too slow
			na    = 40
			nz    = 4
			nh    = 2
			ntau  = 2
			nP    = 3
			# nY    = 3
			np    = 3
			ny    = 3
			nJ    = 9
			nt    = 30
			nsim  = 10000

		end		

		beta    = 0.95
		gamma   = 2.0
		mgamma  = 1.0-gamma
		imgamma = 1.0/mgamma
		lambda  = 10.0
		tau     = 1.0
		taudist = 0.2
		xi      = 0.5
		omega1  = 0.1
		omega2  = 0.1

		# other parameters
		# MC    = [0.5, 0.0002, 0.3] # parameters in moving cost: (h) alpha1, (dist) alpha2, (age) alpha3
		MC1    = 0.1
		MC2    = 0.001
		MC3    = 0.1
		kappa = Float64[rand()*0.01 for i=1:9] # rent to price ratio in each region
		phi   = 0.06		  # fixed cost of selling


		x = readcsv("/Users/florianoswald/Dropbox/mobility/output/model/R2julia/rho-income.csv")
		rhoy = x[2:end,3]
		x = readcsv("/Users/florianoswald/Dropbox/mobility/output/model/R2julia/rho-price.csv")
		rhop =  x[2:end,3]

		# rhoz = IncomeParams[2:end,2]
		rhoP = 0.9
		# rhoY = rand()

		R      = 1.03 	# gross interest rate savings: 1+r
		Rm     = 1.06 	# gross interest rate mortgage: 1+rm
		chi    = 0.2   # downpayment ratio
		myNA   = -999
		minAge = 20
		maxAge = minAge + nt
		ages   = minAge:maxAge
		euler  = 0.5772	# http://en.wikipedia.org/wiki/Gumbel_distribution

		# create object

			return new(beta,gamma,mgamma,imgamma,lambda,tau,taudist,xi,omega1,omega2,MC1,MC2,MC3,kappa,phi,rhop,rhoy,rhoP,R,Rm,chi,myNA,maxAge,minAge,ages,euler,na,nz,nh,nt,ntau,nP,nJ,np,ny,nsim)
	end

	
end


# define member functions

# takes a dict of params_to_sample
function update!(p::Param,pd::Dict)
	for (k,v) in pd
		setfield!(p,symbol(k),v)
	end
end







function show(io::IO, p::Param)
	print(io,"number of points=$(p.na*p.nz*p.nh*p.ntau*p.nP*p.np*p.ny*p.nJ*p.nt)\n")
	print(io,"free params:
	beta    = $(p.beta)
	gamma   = $(p.gamma)		      
	lambda  = $(p.lambda)
	tau     = $(p.tau)
	taudist = $(p.taudist)
	xi      = $(p.xi)
	omega1  = $(p.omega1)
	omega2  = $(p.omega2)
	MC1     = $(p.MC1)
	MC2     = $(p.MC2)
	MC3     = $(p.MC3)")
end




