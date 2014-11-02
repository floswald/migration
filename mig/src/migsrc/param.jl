	


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

	# moving cost parameters
	MC0    :: Float64 # parameters in moving cost: alpha0, intercept
	MC1    :: Float64 # parameters in moving cost: alpha1, alpha2, alpha3
	MC2    :: Float64 # parameters in moving cost: alpha1, alpha2, alpha3
	MC3    :: Float64 # parameters in moving cost: alpha1, alpha2, alpha3
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

	policy       :: ASCIIString    # name of policy
	redistribute :: Vector{Float64}  # redestributing amounts
	ctax         :: Float64    # proportional consumption scale. used to find equalizing utility
	shockReg     :: Int
	shockAge     :: Int
	shockVal     :: Vector{Float64}
	noMC         :: Bool

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
	nsim::Int # number of individuals. this will be changed by cohort setup in model

	verbose :: Int

	# constructor assigning initial values
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
			na    = 17
			namax = 21
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
		taudist  = 0.68
		xi1      = 0.012
		xi2      = 0.052
		omega1   = 1.0
		omega2   = 1.04

		MC0    = 3.05  	 	# intercept
		MC1    = 0.017  	 	# age
		MC2    = 0.0013 		# age2
		MC3    = 0.26 		# owmer
		# MC3    = 0.00      # dist
		MC4    = 0.36 		# kids


		kappa  = Float64[0.01 for i=1:9] # rent to price ratio in each region
		phi    = 0.06		  # fixed cost of selling

		R      = 1.03 	# gross interest rate savings: 1+r
		Rm     = 1.055 	# gross interest rate mortgage: 1+rm   sommer + sullivan. 
		chi    = 0.2   # downpayment ratio
		myNA   = -99.0
		minAge = 20
		maxAge = minAge + nt
		ages   = minAge:maxAge
		euler  = 0.5772	# http://en.wikipedia.org/wiki/Gumbel_distribution
		sscale = 1.0 	# with kids your consumption goes down 20%

		# policy and shock setup
		if length(opts) > 0 
			pname    = get(opts,"policy","NULL")
			lumpsum  = get(opts,"redistribute",[0.0])
			verbose  = get(opts,"verbose",0)
			shockReg = get(opts,"shockRegion",0)
			shockAge = get(opts,"shockAge",100)
			shockVal = get(opts,"shockVal",ones(nt-1))	# multiplicative factor

			# check policy name is valid
			if pname != "NULL"
				if !in(pname,["noShocks","smallShocks","mortgageSubsidy_padjust","halfMC","doubleMC","tripleMC","mortgageSubsidy","moneyMC","yshock","pshock","noSaving","noBuying","highMC","pshock_noBuying","pshock_highMC","pshock_noSaving"])
					warn("your policy $pname is not in the set of admissible policies")
				end
			end

		else
			pname    = "NULL"
			lumpsum  = [0.0]
			verbose  = 0
			shockReg = 0
			shockAge = 100
			shockVal = ones(nt-1)
		end
		noMC = false	# default: there IS a moving cost. experiments switch this off in certain periods.
		ctax = 1.0 

		# create object

		return new(gamma,mgamma,imgamma,tau,taudist,xi1,xi2,omega1,omega2,MC0,MC1,MC2,MC3,MC4,beta,kappa,phi,R,Rm,chi,myNA,maxAge,minAge,ages,euler,sscale,pname,lumpsum,ctax,shockReg,shockAge,shockVal,noMC,na,namax,nz,nh,nt,ntau,nJ,np,ny,ns,nsim,verbose)
	end
end


# define member functions

# takes a dict of params_to_sample
function update!(p::Param,pd::Dict)
	for (k,v) in pd
		setfield!(p,symbol(k),v)
	end
end


function print(p::Param,file_est::ASCIIString,file_set::ASCIIString)
	est = [:gamma,:tau,:taudist,:xi1,:xi2,:omega1,:omega2,:MC0,:MC1,:MC2,:MC3,:MC4]
	set = [:beta,:phi,:R,:Rm,:chi]
	f_estimate = open(file_est,"w")
	f_set      = open(file_set,"w")

	pest = Dict{ASCIIString,Float64}()
	for i in est
		pest[string(i)] = getfield(p,i)
	end

	pset = Dict{ASCIIString,Float64}()
	for i in set
		pset[string(i)] = getfield(p,i)
	end
	# add manually 
	pset["rho"] = 0.96
	pset["std_u"] = 0.118

	JSON.print(f_estimate,pest)
	JSON.print(f_set,pset)
	close(f_estimate)
	close(f_set)
end



function show(io::IO, p::Param)
	print(io,"number of max problems=$(p.na*p.nz*p.nh*p.nh*p.ntau*p.np*p.ny*p.nJ*p.nJ*p.nt*p.ns)\n")
	print(io,"free params:
	gamma    = $(p.gamma)		
	tau      = $(p.tau)
	taudist  = $(p.taudist)
	xi1      = $(p.xi1)
	xi2      = $(p.xi2)
	omega1   = $(p.omega1)
	omega2   = $(p.omega2)
	MC0      = $(p.MC0)
	MC1      = $(p.MC1)
	MC2      = $(p.MC2)
	MC3      = $(p.MC3)
	MC4      = $(p.MC4)
	ctax     = $(p.ctax)
	shockReg = $(p.shockReg)
	shockAge = $(p.shockAge)
	policy   = $(p.policy)")
end




