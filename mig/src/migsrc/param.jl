	


# define the struct


type Param

	# free parameters
	# ===============

	# utility function parameters
	gamma   :: Float64			# CRRA
	mgamma  :: Float64			# (1-CRRA)
	imgamma :: Float64			# 1/(1-CRRA)
	lambda  :: Float64          # default penalty
	tau     :: Float64 			# value of tau for high type
	taudist :: Float64 			# population prob of being high cost type
	xi1     :: Float64          # utility of housing for HHsize 1
	xi2     :: Float64          # utility of housing
	omega1  :: Float64 			# final period utility parameters
	omega2  :: Float64			# final period utility parameters

	# moving cost parameters
	MC0    :: Float64 # parameters in moving cost: alpha0, intercept
	MC1    :: Float64 # parameters in moving cost: alpha1, alpha2, alpha3
	MC2    :: Float64 # parameters in moving cost: alpha1, alpha2, alpha3
	MC3    :: Float64 # parameters in moving cost: alpha1, alpha2, alpha3
	MC3_2  :: Float64 # parameters in moving cost: alpha1, alpha2, alpha3
	MC4    :: Float64 # parameters in moving cost: alpha1, alpha2, alpha3

	# fixed parameters
	# ================

	beta   :: Float64			# discount factor
	kappa  :: Array{Float64,1} # rent to price ratio in each region
	phi    :: Float64		  # fixed cost of selling
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
	sscale :: Float64

	# numerical setup
	# points in each dimension
	na    :: Int 	# number of asset points
	namax :: Int 	# number of asset points
	nz    :: Int 	# number of inidividual income states
	nh    :: Int 	# number of housing states
	nt    :: Int 	# number of periods
	ntau  :: Int 	# number of types
	nJ    :: Int 	# number of origin locations
	            # nk = nj - 1 is the number of destination locations
	np    :: Int # number of price levels in each location
	ny    :: Int # number of income levels by location
	ns    :: Int # number of HHsizes

	# used in simulations
	nsim::Int # number of individuals in each location
	rseed::Int # seed for the random number generators

	verbose :: Int

	# constructor assigning initial values
	function Param(size::Int,verbose=0)

		if size==1

			# super small: use for tests
			na    = 10
			namax  = 10
			nz    = 3
			nh    = 2
			ntau  = 2
			np    = 2
			ny    = 2
			nJ    = 9
			nt    = 8
			ns    = 2
			nsim  = 100

		elseif size==2
			# small: runs on my box
			na    = 17
			namax    = 21
			nz    = 4
			nh    = 2
			ntau  = 2
			np    = 3
			ny    = 3
			nJ    = 9
			nt    = 30
			ns    = 2
			nsim  = 55000

		end		

		beta     = 0.96
		gamma    = 1.4	
		mgamma   = 1.0-gamma
		imgamma  = 1.0/mgamma
		lambda   = 0.5
		tau      = 100.0
		taudist  = 0.8
		xi1      = 0.001
		xi2      = 0.002
		omega1   = 0.000
		omega2   = 0.000

		MC0    = 3.9
		MC1    = 0.01
		MC2    = 0.0
		MC3    = 0.01
		MC3_2  = -0.0005
		MC4    = 0.1


		kappa  = Float64[0.01 for i=1:9] # rent to price ratio in each region
		phi    = 0.06		  # fixed cost of selling
		rhoP   = 0.9

		R      = 1.03 	# gross interest rate savings: 1+r
		Rm     = 1.06 	# gross interest rate mortgage: 1+rm
		chi    = 0.2   # downpayment ratio
		myNA   = -99.0
		minAge = 21
		maxAge = minAge + nt
		ages   = minAge:maxAge
		euler  = 0.5772	# http://en.wikipedia.org/wiki/Gumbel_distribution
		sscale = 0.8 	# with kids your consumption goes down 20%

		rseed = 1052014

		# create object

			return new(gamma,mgamma,imgamma,lambda,tau,taudist,xi1,xi2,omega1,omega2,MC0,MC1,MC2,MC3,MC3_2,MC4,beta,
				       kappa,phi,rhoP,R,Rm,chi,myNA,maxAge,minAge,ages,euler,sscale,na,namax,nz,nh,nt,ntau,nJ,np,ny,ns,nsim,rseed,verbose)
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
	print(io,"number of max problems=$(p.na*p.nz*p.nh*p.nh*p.ntau*p.np*p.ny*p.nJ*p.nJ*p.nt*p.ns)\n")
	print(io,"free params:
	gamma   = $(p.gamma)		      
	lambda  = $(p.lambda)
	tau     = $(p.tau)
	taudist = $(p.taudist)
	xi1     = $(p.xi1)
	xi2     = $(p.xi2)
	omega1  = $(p.omega1)
	omega2  = $(p.omega2)
	MC1     = $(p.MC1)
	MC2     = $(p.MC2)
	MC3     = $(p.MC3)
	MC4     = $(p.MC4)")
end




