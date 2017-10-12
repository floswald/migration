	


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
	amenity_ENC :: Float64  # amenity value of living in ENC
	amenity_ESC :: Float64  # amenity value of living in ESC
	amenity_MdA :: Float64  # amenity value of living in MdA
	amenity_Mnt :: Float64  # amenity value of living in Mnt
	amenity_NwE :: Float64  # amenity value of living in NwE
	amenity_Pcf :: Float64  # amenity value of living in Pcf
	amenity_StA :: Float64  # amenity value of living in StA
	amenity_WNC :: Float64  # amenity value of living in WNC
	amenity_WSC :: Float64  # amenity value of living in WSC

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
	sscale :: Vector{Float64}

	# policy parameters
	# =================

	policy       :: String    # name of policy
	redistribute :: Vector{Float64}  # redestributing amounts
	ctax         :: Float64    # proportional consumption scale. used to find equalizing utility
	shockReg     :: Int
	shockAge     :: Int
	shockVal     :: Vector{Float64}
	shockVal_y     :: Vector{Float64}
	shockVal_p     :: Vector{Float64}
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
	function Param(size::Int;opts::Dict=Dict())

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
			# na    = 21
			# namax = 50
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
		tau      = 100.0
		taudist  = 0.65
		xi1      = 0.008
		xi2      = 0.052
		omega1   = 1.0
		omega2   = 5.1
		# omega2   = 3.26721
		# omega2   = 6.1

		# MC0    = 2.71  	 	# intercept
		MC0    = 2.77  	 	# intercept
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

		# get data
		# --------

		io = setPaths()
		popweights = DataFrame(FileIO.load(joinpath(io["indir"],"prop.rda"))["prop"])
		sort!(popweights,cols=1)

		# set amenity to popweights initially
		amenity = convert(Array,popweights[:proportion])

		# change initial value a bit
		amenity[1] -= 0.03
		amenity[3] -= 0.01
		amenity[4] += 0.04
		amenity[5] += 0.01
		amenity[6] += 0.035
		amenity[7] -= 0.01
		amenity[8] -= 0.01

		# plug into members
		amenity_ENC = amenity[1]
		amenity_ESC = amenity[2]
		amenity_MdA = amenity[3]
		amenity_Mnt = amenity[4]
		amenity_NwE = amenity[5]
		amenity_Pcf = amenity[6]
		amenity_StA = amenity[7]
		amenity_WNC = amenity[8]
		amenity_WSC = amenity[9]

		noMC = false	# default: there IS a moving cost. experiments switch this off in certain periods.
		ctax = 1.0 

		# policy and shock setup
		# if length(opts) > 0 
		# 	pname    = get(opts,"policy","NULL")
		# 	lumpsum  = get(opts,"redistribute",[0.0])
		# 	verbose  = get(opts,"verbose",0)
		# 	shockReg = get(opts,"shockRegion",0)
		# 	shockAge = get(opts,"shockAge",100)
		# 	shockVal = get(opts,"shockVal",ones(nt-1))	# multiplicative factor
		# 	shockVal_y = get(opts,"shockVal_y",ones(nt-1))	# multiplicative factor
		# 	shockVal_p = get(opts,"shockVal_p",ones(nt-1))	# multiplicative factor

		# 	# check policy name is valid
		# 	# if pname != "NULL"
		# 	# 	if !in(pname,["noShocks","smallShocks","mortgageSubsidy_padjust","halfMC","doubleMC","tripleMC","mortgageSubsidy","moneyMC","ypshock","ypshock3","yshock","pshock","noSaving","noBuying","highMC","pshock_noBuying","pshock_highMC","yshock_highMC","pshock_noSaving"])
		# 	# 		warn("your policy $pname is not in the set of admissible policies")
		# 	# 	end
		# 	# end

		# else
			pname    = "NULL"
			lumpsum  = [0.0]
			verbose  = 0
			shockReg = 0
			shockAge = 100
			shockVal = ones(nt-1)
			shockVal_y = ones(nt-1)
			shockVal_p = ones(nt-1)
		# end
		# create object

		out = new(gamma,mgamma,imgamma,tau,taudist,xi1,xi2,omega1,omega2,amenity_ENC,amenity_ESC,amenity_MdA,amenity_Mnt,amenity_NwE,amenity_Pcf,amenity_StA,amenity_WNC,amenity_WSC,MC0,MC1,MC2,MC3,MC4,beta,kappa,phi,R,Rm,chi,myNA,maxAge,minAge,ages,euler,[1.0;sscale],pname,lumpsum,ctax,shockReg,shockAge,shockVal,shockVal_y,shockVal_p,noMC,na,namax,nz,nh,nt,ntau,nJ,np,ny,ns,nsim,verbose)

		# override defaults
		if length(opts) > 0
            for (k,v) in opts
                setfield!(out,k,v)
            end
        end

        return out
	end
end


# define member functions

# takes a dict of params_to_sample
function update!(p::Param,pd::Dict)
	for (k,v) in pd
		setfield!(p,Symbol(k),v)
	end
end


function print(p::Param,file_est::String,file_set::String)
	est = [:gamma,:tau,:taudist,:xi1,:xi2,:omega1,:omega2,:MC0,:MC1,:MC2,:MC3,:MC4]
	set = [:beta,:phi,:R,:Rm,:chi]
	f_estimate = open(file_est,"w")
	f_set      = open(file_set,"w")

	pest = Dict{String,Float64}()
	for i in est
		pest[string(i)] = getfield(p,i)
	end

	pset = Dict{String,Float64}()
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
	print(io,"number of states      =$(p.na*p.nz*p.np*p.ny*p.nh*p.ntau*p.nJ*p.nt*p.ns)\n")
	print(io,"number of max problems=$(p.na*p.nz*p.np*p.ny*p.nh*p.ntau*p.nJ*p.nt*p.ns*p.nh*p.nJ)\n")
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




