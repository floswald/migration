	


# define the struct


type Param

	# free parameters
	# ===============

	# utility function parameters
	gamma   :: Float64			# CRRA
	mgamma  :: Float64			# (1-CRRA)
	imgamma :: Float64			# 1/(1-CRRA)
	tau     :: Float64 			# value of tau for high type
	taudist :: Float64 			# population prob of being high cost type
	xi1     :: Float64          # utility of housing for HHsize 1
	xi2     :: Float64          # utility of housing
	omega1  :: Float64 			# final period utility parameters
	omega2  :: Float64			# final period utility parameters
	omega3  :: Float64			# final period utility parameters

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

	R      :: Float64 	# gross interest rate savings: 1+r
	Rm     :: Float64 	# gross interest rate mortgage: 1+rm
	chi    :: Float64   # downpayment ratio
	myNA   :: Float64
	maxAge :: Int 	# maximal age in model
	minAge :: Int 	# maximal age in model
	ages   :: UnitRange{Int} 	# maximal age in model
	euler  :: Float64
	sscale :: Float64

	# policy parameters
	# =================

	policy :: ASCIIString    # name of policy
	mort_LumpSum :: Vector{Float64}  # redestributing amounts
	ctax   :: Float64    # proportional consumption scale. used to find equalizing utility

	# numerical setup
	# points in each dimension
	na    :: Int 	# number of asset points
	namax :: Int 	# number of asset points
	nz    :: Int 	# number of inidividual income states
	nh    :: Int 	# number of housing states
	nt    :: Int 	# number of periods
	ntau  :: Int 	# number of types
	nJ    :: Int 	# number of locations
	np    :: Int # number of price levels in each location
	ny    :: Int # number of income levels by location
	ns    :: Int # number of HHsizes

	# used in simulations
	nsim::Int # number of individuals in each location

	verbose :: Int

	# constructor assigning initial values
	# function Param(size::Int;verbose=0,mLumpSum)
	function Param(size::Int,opts=Dict())

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
			nt    = 32
			ns    = 2
			nsim  = 50000

		end		

		beta     = 0.96
		gamma    = 1.4	
		mgamma   = 1.0-gamma
		imgamma  = 1.0/mgamma
		tau      = 100
		taudist  = 0.7
		xi1      = 0.016
		xi2      = 0.039
		omega1   = 1.300
		omega2   = 1.0
		omega3   = 4.8

		MC0    = 3.05  	 	# intercept
		MC1    = 0.017  	 	# age
		MC2    = 0.0013 		# age2
		MC3    = 0.26 		# owmer
		# MC3    = 0.00      # dist
		MC3_2  = 0.00 		# 
		MC4    = 0.36 		# kids


		kappa  = Float64[0.01 for i=1:9] # rent to price ratio in each region
		phi    = 0.00		  # fixed cost of selling

		R      = 1.03 	# gross interest rate savings: 1+r
		Rm     = 1.06 	# gross interest rate mortgage: 1+rm
		chi    = 0.1   # downpayment ratio
		myNA   = -99.0
		minAge = 20
		maxAge = minAge + nt
		ages   = minAge:maxAge
		euler  = 0.5772	# http://en.wikipedia.org/wiki/Gumbel_distribution
		sscale = 0.8 	# with kids your consumption goes down 20%

		# policy setup
		if length(opts) > 0 
			pname = get(opts,"policy","NULL")
			lumpsum = get(opts,"lumpsum",[0.0])
			verbose = get(opts,"verbose",0)
		else
			pname = "NULL"
			lumpsum = [0.0]
			verbose = 0
		end
		ctax = 1.0 

		# create object

			return new(gamma,mgamma,imgamma,tau,taudist,xi1,xi2,omega1,omega2,omega3,MC0,MC1,MC2,MC3,MC3_2,MC4,beta,
				       kappa,phi,R,Rm,chi,myNA,maxAge,minAge,ages,euler,sscale,pname,lumpsum,ctax,na,namax,nz,nh,nt,ntau,nJ,np,ny,ns,nsim,verbose)
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
	tau     = $(p.tau)
	taudist = $(p.taudist)
	xi1     = $(p.xi1)
	xi2     = $(p.xi2)
	omega1  = $(p.omega1)
	omega2  = $(p.omega2)
	omega3  = $(p.omega3)
	MC0     = $(p.MC0)
	MC1     = $(p.MC1)
	MC2     = $(p.MC2)
	MC3     = $(p.MC3)
	MC3_2   = $(p.MC3_2)
	MC4     = $(p.MC4)
	policy  = $(p.policy)")
end




