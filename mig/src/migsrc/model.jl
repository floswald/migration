
# setting up a model


type Model 

	# values and policies conditional on moving to k
	# dimvec  = (nJ, ns, nz, ny, np, ntau, na, nh,  nJ, nt-1 )
	# dimvec  = (nhh, nJ, na, nh, nJ, ntau, ns, np, ny, nz, nt-1 )
	v     :: Array{Float64,10}
	vh    :: Array{Float64,11}	# v of stay cond on hh choice: (nh, nJ, ns, ny, np, nz, ntau,  na, nh, nJ, nt-1 )
	vfeas :: Array{Bool,1}	# feasibility map
	sh    :: Array{Float64,11}
	ch    :: Array{Float64,11}
	cash  :: Array{Float64,11}
	canbuy  :: Array{Bool,10}
	rho   :: Array{Float64,10}
	dh    :: Array{Int,10}

	# top-level value maxed over housing and location
	# dimvec2 = (ns, ny, np, nz, ntau,  na, nh, nJ, nt-1 )
	EV   :: Array{Float64,9}
	vbar :: Array{Float64,9}

	# expected final period value
	# dimensions: a,h,j,pj,py
	EVfinal :: Array{Float64,5}

	# index of the first asset element > 699
	aone :: Int

	amenities::Vector{Float64}

	# grids
	grids   :: Dict{String,Array{Float64,1}}
	gridsXD :: Dict

	dimvec  ::Tuple{Int,Int,Int,Int,Int,Int,Int,Int,Int,Int} # total number of dimensions
	dimvecH ::Tuple{Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int} # dimvec conditional on H choice
	dimvec2::Tuple{Int,Int,Int,Int,Int,Int,Int,Int,Int} # total - housing
	# dimnames::Array{String}
	proportion::DataFrame
	dimnames::DataFrame
	regnames::DataFrame
	agedist ::DataFrame
	distance::Array{Any,2}
	Inc_coefs::DataFrame
	Inc_ageprofile::Array{Float64,2}
	Inc_shocks::Array{Float64,2}
	Init_asset::LogNormal

	Regmods_YP::Dict{Int,Matrix{Float64}}   # regional VAR models. row 1 is Y, row 2 is P
	sigma_reg::Dict   # covariance matrices of residuals form regional VAR models.
	PYdata::DataFrame
	pred_ydf::DataFrame
	pred_pdf::DataFrame
	pred_y::Matrix
	pred_p::Matrix
	zrange::Tuple{Float64,Float64}

	# cohort settings
	# ---------------
	coh_yrs    :: Dict{Int,Range{Int}}
	coh_idx    :: Dict{Int,Range{Int}}
	coh_breaks :: Array{Int}
	coh_n      :: Dict{Int,Int}
	coh_members:: Dict{Int,Range{Int}}

	# random generator setup
	# ----------------------
	zshock  :: Vector{Float64}
	sshock  :: Vector{Float64}
	mshock  :: Vector{Float64}
	zshock0     :: Vector{Float64}
	eps_dist :: Distributions.Gumbel{Float64}
	eps_shock :: Array{Vector{Float64}}

	# policy settings 
	# ---------------
	sinai::DataFrame

	# copula objects
	# --------------
	# copula::NormalCopula
	# cop_quants::Matrix
	copdf::DataFrame
	cop::Matrix{Float64}
	cop_cdf::Matrix{Float64}
	cop_shock::Vector


	# constructor
    function Model(p::Param)

		# dimvec  = (nJ, ns, nz, ny, np, ntau, na, nh, nJ, nt-1 )
		dimvec  = (p.nJ, p.ns, p.nz, p.ny, p.np, p.ntau,  p.na, p.nh,p.nJ, p.nt-1 )
		dimvecH = (p.nh, p.nJ, p.ns, p.nz, p.ny, p.np, p.ntau,  p.na, p.nh,p.nJ, p.nt-1 )
		dimvec2 = (p.ns, p.nz, p.ny, p.np, p.ntau,  p.na, p.nh,p.nJ, p.nt-1)

		v = fill(p.myNA,dimvec)
		vfeas = falses(prod(dimvecH))
		vh= fill(p.myNA,dimvecH)
		sh= fill(0.0,dimvecH)
		ch= fill(0.0,dimvecH)
		cash= falses(dimvecH)
		canbuy= fill(0.0,dimvec)
		rho = fill(0.0,dimvec)

		EVfinal = fill(p.myNA,(p.na,p.nh,p.np,p.ny,p.nJ))
		# EVfinal = fill(0.0,(p.na,p.nh,p.np,p.nJ))

		dh = fill(0,dimvec)

		EV = fill(p.myNA,dimvec2)
		vbar = fill(p.myNA,dimvec2)

		bounds = Dict{String,Tuple{Float64,Float64}}()
		# bounds["Y"]          = (0.5,1.5)


		# import data from R
		# ==================

		# if on my machine
		io = setPaths()

		# dbase = h5read(joinpath(io["indir"],"mig_db_in.h5"))
		# rhoy = h52df(joinpath(io["indir"],"mig_db_in.h5"),"rhoy/")	# function makes a dataframe from all cols in rhoy

		# scalar params
		par_df = DataFrame(FileIO.load(joinpath(io["indir"],"par_df.rda"))["par_df"])
		init_asset = LogNormal(par_df[par_df[:param].=="meanlog",:value][1],par_df[par_df[:param].=="sdlog",:value][1])

		# distance matrix
		distdf = DataFrame(FileIO.load(joinpath(io["indir"],"distance.rda"))["dist"])
		dist = convert(Array,distdf)

		# population weights
		popweights = DataFrame(FileIO.load(joinpath(io["indir"],"prop.rda"))["prop"])
		sort!(popweights,cols=1)

		# age distribution
		agedist = DataFrame(FileIO.load(joinpath(io["indir"],"agedist.rda"))["agedist"])


		# put all amenities into amenity vector
		amenities = vcat(p.amenity_ENC,
						p.amenity_ESC,
						p.amenity_MdA,
						p.amenity_Mnt,
						p.amenity_NwE,
						p.amenity_Pcf,
						p.amenity_StA,
						p.amenity_WNC,
						p.amenity_WSC)


		# Aggregate and Regional price and income dynamic
		# ===============================================

		# regional house price and income
		# what is relationship y_j ~ P + Y

		# aggregate house price and income
		# VAR P ~ LY + LP
		# VAR Y ~ LY + LP
		if p.policy == "noShocks"
			VAR_reg = DataFrame(FileIO.load(joinpath(io["indir"],"VAR_reg.rda"))["VAR_reg"])
			Regmods_YP = Dict{Int,Matrix{Float64}}()
			for j in 1:p.nJ
				Regmods_YP[j] = vcat(convert(Array,VAR_reg[j,[:y_Intercept,:y_Y,:y_P]]),convert(Array,VAR_reg[j,[:p_Intercept,:p_Y,:p_P]]))
			end
			VAR_agg = DataFrame(FileIO.load(joinpath(io["indir"],"VAR_agg.rda"))["VAR_agg"])
			regY = DataFrame(FileIO.load(joinpath(io["indir"],"regY.rda"))["regY"])
			regP = DataFrame(FileIO.load(joinpath(io["indir"],"regP.rda"))["regP"])
			PYdata = DataFrame(FileIO.load(joinpath(io["indir"],"PYdata_noShock.rda"))["PYdata"])
			pred_ydf = DataFrame(FileIO.load(joinpath(io["indir"],"pred_y_noShock.rda"))["pred_y"])
			pred_pdf = DataFrame(FileIO.load(joinpath(io["indir"],"pred_p_noShock.rda"))["pred_p"])
		elseif p.policy=="smallShocks"
			VAR_reg = DataFrame(FileIO.load(joinpath(io["indir"],"VAR_reg_small.rda"))["VAR_reg"])
			Regmods_YP = Dict{Int,Matrix{Float64}}()
			for j in 1:p.nJ
				Regmods_YP[j] = vcat(convert(Array,VAR_reg[j,[:y_Intercept,:y_Y,:y_P]]),convert(Array,VAR_reg[j,[:p_Intercept,:p_Y,:p_P]]))
			end
			VAR_agg = DataFrame(FileIO.load(joinpath(io["indir"],"VAR_agg.rda"))["VAR_agg"])
			sigma_agg = DataFrame(FileIO.load(joinpath(io["indir"],"sigma_agg.rda"))["sigma_agg"])
			YPsigma = zeros(2,2)
			YPsigma[1,1] = @where(sigma_agg,:row.=="Y")[:Y][1]
			YPsigma[1,2] = @where(sigma_agg,:row.=="Y")[:P][1]
			YPsigma[2,1] = @where(sigma_agg,:row.=="P")[:Y][1]
			YPsigma[2,2] = @where(sigma_agg,:row.=="P")[:P][1]
			PYdata = DataFrame(FileIO.load(joinpath(io["indir"],"PYdata.rda"))["PYdata"])
			pred_ydf = DataFrame(FileIO.load(joinpath(io["indir"],"pred_y_small.rda"))["pred_y"])
			pred_pdf = DataFrame(FileIO.load(joinpath(io["indir"],"pred_p_small.rda"))["pred_p"])
		else
			VAR_reg = DataFrame(FileIO.load(joinpath(io["indir"],"VAR_reg.rda"))["VAR_reg"])
			Regmods_YP = Dict{Int,Matrix{Float64}}()
			for j in 1:p.nJ
				Regmods_YP[j] = vcat(convert(Array,VAR_reg[j,[:y_Intercept,:y_Y,:y_P]]),convert(Array,VAR_reg[j,[:p_Intercept,:p_Y,:p_P]]))
			end
			VAR_agg = DataFrame(FileIO.load(joinpath(io["indir"],"VAR_agg.rda"))["VAR_agg"])
			sigma_agg = DataFrame(FileIO.load(joinpath(io["indir"],"sigma_agg.rda"))["sigma_agg"])
			sigma_reg_df = DataFrame(FileIO.load(joinpath(io["indir"],"sigma_reg.rda"))["sigma_reg"])
			YPsigma = zeros(2,2)
			YPsigma[1,1] = @where(sigma_agg,:row.=="Y")[:Y][1]
			YPsigma[1,2] = @where(sigma_agg,:row.=="Y")[:P][1]
			YPsigma[2,1] = @where(sigma_agg,:row.=="P")[:Y][1]
			YPsigma[2,2] = @where(sigma_agg,:row.=="P")[:P][1]
			PYdata = DataFrame(FileIO.load(joinpath(io["indir"],"PYdata.rda"))["PYdata"])
			pred_ydf = DataFrame(FileIO.load(joinpath(io["indir"],"pred_y.rda"))["pred_y"])
			pred_pdf = DataFrame(FileIO.load(joinpath(io["indir"],"pred_p.rda"))["pred_p"])
		end

		# get region names
		nms = convert(Array,popweights[:Division])
		# get regional cov matrics
		# and convert to correlation
		sigma_reg = Dict(k => cov2corr(reshape(convert(Array,sigma_reg_df[Symbol(k)]),2,2)) for k in nms)
		# convert to correlation matrices



		# fill into a matrix
		# Var(Y),cov(Y,P),cov(P,Y),Var(P)


		# P,Y data series.

		pred_y = convert(Array,pred_ydf[:,2:end])
		pred_p = convert(Array,pred_pdf[:,2:end])



		# population weights
		regnames = DataFrame(j=1:p.nJ,Division=CategoricalArray(popweights[1:p.nJ,:Division]),prop=popweights[1:p.nJ,:proportion])



		# individual income process parameters
		inc_coefs = DataFrame(FileIO.load(joinpath(io["indir"],"ztable.rda"))["z"])

		# manually override AR1 coefs
		# using numbers from french(2005)
		inc_coefs[:,:Lresid] = 0.96
		inc_coefs[:,:sigma_resid] = 0.118

		# kids transition matrix
		ktrans = DataFrame(FileIO.load(joinpath(io["indir"],"kidstrans.rda"))["kids_trans"])

		kmat = zeros(p.ns,p.ns,p.nt)
		for ir in eachrow(ktrans)
			if ir[:age] < p.maxAge && ir[:age] >= p.minAge
				kmat[ir[:kids]+1,ir[:kids2]+1,findin(p.ages,ir[:age])[1]] = ir[:Freq]
			end
		end

		# fill kappa with correct values
		for j in 1:p.nJ
			p.kappa[j] = popweights[j,:r2p]
			# p.kappa[j] = 0.02 	# override
		end


		ageprof = zeros(p.nt-1,p.nJ)
		for it in 1:p.nt-1
			for ij in 1:p.nJ
				ageprof[it,ij] = inc_coefs[ij,:Intercept] + p.ages[it] * inc_coefs[ij,:age] + p.ages[it]^2 * inc_coefs[ij,:age2] + p.ages[it]^3 * inc_coefs[ij,:age3]
			end
		end

		# [rho,sigma,lb,ub]
		inc_shocks = zeros(p.nJ,4)
		for j in 1:p.nJ
			inc_shocks[j,1] = inc_coefs[j,:Lresid]  
			inc_shocks[j,2] = inc_coefs[j,:sigma_resid]
			ybar = log(VAR_reg[j,:mean_y])
			inc_shocks[j,3] = log(inc_coefs[j,:q05]) - ybar
			inc_shocks[j,4] = log(inc_coefs[j,:q95]) - ybar
		end


		# XD grids
		# =========

		Ygrid = linspace(@where(VAR_agg,:param.=="min_Y")[:value][1],@where(VAR_agg,:param.=="max_Y")[:value][1],p.np)
		Pgrid = linspace(@where(VAR_agg,:param.=="min_P")[:value][1],@where(VAR_agg,:param.=="max_P")[:value][1],p.ny)

		# p and y grids
		# -------------

		ygrid = zeros(p.ny,p.np,p.nJ)
		pgrid = zeros(p.ny,p.np,p.nJ)

		if p.policy=="noShocks"
			for j in 1:p.nJ
				for iP in 1:p.np
					for iY in 1:p.ny
						ygrid[iY,iP,j] = regY[1,1] + regY[2,1] * Ygrid[iY] + (j>1)*regY[2+j-1,1]
						pgrid[iY,iP,j] = regP[1,1] + regP[2,1] * Pgrid[iY] + (j>1)*regP[2+j-1,1]
					end
				end
			end
		elseif (p.policy=="all_j" || p.policy=="highMC_all_j")
			for j in 1:p.nJ
				for iP in 1:p.np
					for iY in 1:p.ny
						ygrid[iY,iP,j] = DataFramesMeta.@with(VAR_reg[p.shockReg,:], :y_Intercept + :y_P * Pgrid[iP] + :y_Y * Ygrid[iY])[1]
						pgrid[iY,iP,j] = DataFramesMeta.@with(VAR_reg[p.shockReg,:], :p_Intercept + :p_P * Pgrid[iP] + :p_Y * Ygrid[iY])[1]
					end
				end
			end
		else
			for j in 1:p.nJ
				for iP in 1:p.np
					for iY in 1:p.ny
						ygrid[iY,iP,j] = DataFramesMeta.@with(VAR_reg[j,:], :y_Intercept + :y_P * Pgrid[iP] + :y_Y * Ygrid[iY])[1]
						pgrid[iY,iP,j] = DataFramesMeta.@with(VAR_reg[j,:], :p_Intercept + :p_P * Pgrid[iP] + :p_Y * Ygrid[iY])[1]
					end
				end
			end
		end




		# if adjusting prices for GE effect after
		# mortgage subsidy is removed:
		if p.policy == "mortgageSubsidy_padjust"
			pgrid  = pgrid .* p.shockVal[1]
			pred_p = pred_p .* p.shockVal[1]
			# adjust price to rent ratio if doing GE adjustment
			p.kappa = p.kappa ./ p.shockVal[1]
		elseif p.policy == "noMove"
			pgrid  = pgrid .* p.shockVal_p[1]
			pred_p = pred_p .* p.shockVal_p[1]
			# adjust price to rent ratio if doing GE adjustment
			p.kappa = p.kappa ./ p.shockVal_p[1]
			# info("pshokc=$(p.shockVal_p[1])")
		end

		# grid for individual income (based on ygrid(Y,P))
		# -------------------------------------------

		# get supports for shocks and trans mat
		(zsupp,Gz) = rouwenhorst(inc_coefs,VAR_reg[:,:mean_y],p)

		# zgrid holds ln(y) for all states of Y,P,age and region
		zgrid = zeros(p.nz,p.ny,p.np,p.nt-1,p.nJ)
		for it in 1:p.nt-1
			for j in 1:p.nJ
				yshock = ((p.policy=="yshock" || p.policy=="ypshock" || p.policy=="ypshock3" || p.policy=="ypshock4" || p.policy=="ypshock5") && ((it >= p.shockAge) && (j==p.shockReg))) ? log(p.shockVal_y[it-p.shockAge+1]) : 0.0
				yshock = (p.policy=="noMove") ? log(p.shockVal_y[it]) : yshock
				# info("yshock = $yshock")
				for iy in 1:p.ny
					for ip in 1:p.np
						for iz in 1:p.nz
							
							zgrid[iz,iy,ip,it,j] = inc_coefs[j,:Intercept] + yshock + inc_coefs[j,:logCensusMedinc] * log(ygrid[iy,ip,j]) + inc_coefs[j,:age]*p.ages[it] + inc_coefs[j,:age2]*(p.ages[it])^2 + inc_coefs[j,:age3]*(p.ages[it])^3 + zsupp[iz,j]
						end
					end
				end
			end
		end

		# convert to levels
		zgrid = exp.(zgrid)

		# poterba & sinai average tax savings from mortgage subsidy
		# 2004 SCF data. adjust by value of cpi2012 in 2004: 0.818304 * 1000
		sinai = DataFrame(age=[34,50],inc_40K_minus=[208/(0.818304*1000),216/(0.818304*1000)],inc_40_75K=[592/(0.818304*1000),719/(0.818304*1000)],inc_75_125K=[1817/(0.818304*1000),1483/(0.818304*1000)],inc_125_250K=[3603/(0.818304*1000),3599/(0.818304*1000)],inc250K_plus=[7077/(0.818304*1000),5833/(0.818304*1000)])
		poterba_sinai = zeros(p.nz,p.ny,p.np,p.nt-1,p.nJ)
		for j in 1:p.nJ
			for iy in 1:p.ny
				for ip in 1:p.np
					for it in 1:p.nt-1
						if p.ages[it] < 34
							row = 1
						else
							row = 2
						end
						for iz in 1:p.nz
							if zgrid[iz,iy,ip,it,j] < 40.0
								col = 2
							elseif zgrid[iz,iy,ip,it,j] < 75.0
								col = 3
							elseif zgrid[iz,iy,ip,it,j] < 125.0
								col = 4
							elseif zgrid[iz,iy,ip,it,j] < 250.0
								col = 5
							else 
								col = 6
							end
							poterba_sinai[iz,iy,ip,it,j] = sinai[row,col]
						end
					end
				end
			end
		end

		# 1D grids
		# =========

		bounds["assets"] = (-maximum(pgrid)*0.9,0.9*maximum(pgrid))
		grids = Dict{String,Array{Float64,1}}()
		# x = grids["assets"] = scaleGrid(bounds["assets"][1],bounds["assets"][2],p.na,3,50.0,0.7)
		# x = grids["assets"] = scaleGrid(bounds["assets"][1],bounds["assets"][2],p.na,2,0.5)
		# center on zero
		# x = [linspace(bounds["assets"][1],50.0,p.na-6),linspace(100.0,bounds["assets"][2],5), 1000.0]
		x = [linspace(bounds["assets"][1],50.0,p.na-5);linspace(100.0,1000.0,5)]
		# x = [linspace(bounds["assets"][1],60.0,p.na-6),linspace(80.0,bounds["assets"][2],6)]
		# x = linspace(bounds["assets"][1],bounds["assets"][2],p.na)
		x = x .- x[ indmin(abs.(x)) ] 
		# println("assets = $x")
		grids["assets"]  = x
		aone  = findfirst(grids["assets"].>=0)
		grids["assets_rent"] = x[aone:end]
		grids["housing"] = linspace(0.0,1.0,p.nh)
		grids["W"]       = zeros(p.na)
		grids["tau"]     = linspace(0.0,p.tau,p.ntau)


		grids["Gtau"] = [(1.0-p.taudist), p.taudist]
		grids["P"] = Pgrid
		grids["Y"] = Ygrid

		

		# Aggregate Transition Matrix GYP
		# ===============================

		# the transition matrix for the VAR (Y,P) is
		# defined on the tensor product Pgrid * Ygrid
		# it's of dim (ny*np,ny*np)
		# each cell has the density of the event 
		# Pr(y(t+1) = y_i, p(t+1) = p_k | y(t),p(t) )
		# where the density if given by a joint normal with 
		#
		# mean: ( ymod(y(t),p(t),j), pmod(y(t),p(t)) )
		# Cov : sig
		# here ymod(y,p) and pmod(y,p) are the linear predictors
		# of the VAR models for y and p

		if p.policy== "noShocks"
			Gyp = convert(Matrix,Diagonal(ones(p.ny*p.np)))
		else
			Gyp = get_yp_transition(VAR_agg,p,YPsigma,Pgrid,Ygrid)
		end




		# moving cost function
		# ====================

		# scale age into [0,3]

		mc = zeros(p.nt,p.nJ,p.nJ,p.ntau,p.nh,p.ns)
		for it in 1:p.nt-1
			for ij in 1:p.nJ
				for ik in 1:p.nJ
					for itau in 1:p.ntau
						for ih in 0:1
							for is in 1:p.ns
								# if p.ages[it] < 40
									# mc[it,ij,ik,itau,ih+1,is] = (ij!=ik) * (grids["tau"][itau] + p.MC0*p.ages[it] + p.MC1*(p.ages[it])^2 + p.MC2*ih*p.ages[it] + p.MC3 * dist[ij,ik] + (is-1)*p.MC4 )
									mc[it,ij,ik,itau,ih+1,is] = (ij!=ik) * (grids["tau"][itau] + p.MC0 + p.MC1*p.ages[it] + p.MC2*(p.ages[it])^2 + p.MC3*ih + (is-1)*p.MC4 )
								# else
								# 	mc[it,ij,ik,itau,ih+1,is] = (ij!=ik) * (grids["tau"][itau] + p.MC0 + p.MC1*40 + p.MC2*(40)^2 + p.MC3*ih + (is-1)*p.MC4 )
								# end
							end
						end
					end
				end
			end
		end

		if p.policy == "halfMC"
			mc = mc .* 0.5
		elseif p.policy == "doubleMC"
			mc = mc .* 2.0
		elseif p.policy == "noMove"
			# completely shutdown moving
			mc[mc .> 0] = NOMOVE_PEN
		elseif p.policy == "tripleMC"
			mc = mc .* 3.0
		end

		zlength = [zsupp[end,1]-zsupp[1,1]]
        gridsXD = Dict("Gyp" => Gyp, "Gz"=> Gz,"p" => pgrid, "y" => ygrid, "z" => zgrid, "zsupp" => zsupp, "movecost" => mc ,"Gs" => kmat, "Poterba" => poterba_sinai, "zlength" => zlength )

		dimnames = DataFrame(dimension=["k", "s", "z", "y", "p", "tau", "a", "h", "j", "age" ],
			                  points = [p.nJ, p.ns, p.nz, p.ny, p.np, p.ntau,  p.na, p.nh, p.nJ, p.nt-1 ])




		# cohort settings
		# ===============

		c_yrs, c_idx, c_breaks, c_n, c_mem = cohortIdx(p)

		# random generator setup
		# ======================
		srand(12345)
		N = c_breaks[end]	# modified version of p.nsim
		zshock = rand(Normal(0.0,inc_coefs[1,:sigma_resid]),N*(p.nt-1))
		sshock = rand(N*(p.nt-1))
		mshock = rand(N*(p.nt-1))
		zshock0    = rand(Normal(0,0.1),N)
		eps_dist = Gumbel()
		eps_shock = Vector{Float64}[rand(eps_dist,p.nJ) for i in 1:N, j in 1:p.nt-1]


		# copula settings
		# ===============
		# the input numbers here are from function
		# migration::Sipp.wage_residual_copulas in 
		# in the associated R package mig-pkg in this same repository.
		zmarginal = collect(linspace(gridsXD["zsupp"][1,1],gridsXD["zsupp"][end,1],p.ncop))
		gridsXD["zmarginal"] = zmarginal
		gridsXD["zrange"] = extrema(zmarginal)
		copdf = rcopy(
			R"""
			library(copula)
			# load saved copula
			load($(joinpath(io["in"],"copula.RData")))
			qtl <- expand.grid(today=$zmarginal,tomorrow=$zmarginal)
			qtl$dens <- dMvdc(as.matrix(qtl),cop)
			qtl
			"""
			)
		cop = reshape(copdf[:dens],p.ncop,p.ncop)
		cop = cop ./ sum(cop,2)
		cop_cdf = cumsum(cop,2)


		cop_shock = rand(N*(p.nt-1))




        return new(v,vh,vfeas,sh,ch,cash,canbuy,rho,dh,EV,vbar,EVfinal,aone,amenities,grids,gridsXD,dimvec,dimvecH,dimvec2,popweights,dimnames,regnames,agedist,dist,inc_coefs,ageprof,inc_shocks,init_asset,Regmods_YP,sigma_reg,PYdata,pred_ydf,pred_pdf,pred_y,pred_p,extrema(zmarginal),c_yrs,c_idx,c_breaks,c_n,c_mem,zshock,sshock,mshock,zshock0,eps_dist,eps_shock,sinai,copdf,cop,cop_cdf,cop_shock)

	end
end


# functions for testing
# =====================

function setrand!(m::Model)
	m.v = reshape(rand(length(m.v)),size(m.v))
	m.vh = reshape(rand(length(m.vh)),size(m.vh))
	m.sh = reshape(rand(length(m.vh)),size(m.vh))
	m.ch = reshape(rand(length(m.vh)),size(m.vh))
	m.vbar = reshape(rand(length(m.vbar)),size(m.vbar))
	m.rho  = reshape(rand(length(m.rho)),size(m.rho))
	m.EVfinal = reshape(rand(length(m.EVfinal)),size(m.EVfinal))
	return nothing
end

function setones!(m::Model)
	m.v = reshape(ones(length(m.v)),size(m.v))
	m.vh = reshape(ones(length(m.vh)),size(m.vh))
	m.sh = reshape(ones(length(m.vh)),size(m.vh))
	m.ch = reshape(ones(length(m.vh)),size(m.vh))
	m.vbar = reshape(ones(length(m.vbar)),size(m.vbar))
	m.rho  = reshape(ones(length(m.rho)),size(m.rho))
	m.EVfinal = reshape(ones(length(m.EVfinal)),size(m.EVfinal))
	return nothing
end

function setincreasing!(m::Model)
	m.v = reshape(1.0:length(m.v),size(m.v))
	m.vh = reshape(1.0:length(m.vh),size(m.vh))
	m.sh = reshape(1.0:length(m.vh),size(m.vh))
	m.ch = reshape(1.0:length(m.vh),size(m.vh))
	m.vbar = reshape(1.0:length(m.vbar),size(m.vbar))
	m.rho  = reshape(1.0:length(m.rho),size(m.rho))
	m.EVfinal = reshape(1.0:length(m.EVfinal),size(m.EVfinal))
	return nothing
end



# cohort setup
# =============

function cohortIdx(p::Param)
	years = 1967:2012

	# which indices in "years" are relevant for
	# which cohort at which age

	cdict = Dict{Int,Range{Int}}()
	idict = Dict{Int,Range{Int}}()
	ndict = Dict{Int,Int}()
	mdict = Dict{Int,Range{Int}}()  # members
	for yr in 1:length(years)
		yr_born = years[yr]
		cdict[yr] = yr_born:(min(yr_born + p.nt-2,2012))
		idict[yr] = yr:(findin(years,min(yr_born + p.nt-2,2012))[1])
	end

	nc = length(cdict)
	ppc = round(Integer,p.nsim / (nc-1))
	pp = ppc
	breaks = Int[]
	push!(breaks,ppc)
	ndict[1]=ppc
	mdict[1]=1:ppc
	for i in 2:nc
		pp += ppc
		push!(breaks,pp)
		ndict[i] = ppc
		mdict[i] = mdict[i-1][end]+1:pp
	end
	return (cdict,idict,breaks,ndict,mdict)
end

# function logAssets(p::Param,x)

# 	out = zeros(length(x))
# 		off = 1	# offset for log(0) in case b[1] is positive
# 		out[1]            <- log(x[1] + off)
# 		out[end]            <- log(x[end] + off)
# 		out               <- linspace(out[1],out[end],round(p.na/2)
# 		out               <- exp( out ) .- off



# function makeTransition(n,rho)

# 	u = linspace(1/n, 1-1/n, n)
# 	u = [repmat(u,n,1) repmat(u,1,n)'[:] ]
	
# 	J = length(rho)

# 	if J==1
# 		G = zeros(n,n)
# 		Cop = NormalCopula(2,rho)
# 		G = reshape(dnormCopula(u,Cop),n,n)

# 		# normalize by row sums
# 		G = G./sum(G,2)
# 		return G

# 	else

# 		G = zeros(n,n,J)
# 		for i=1:J
# 			Cop = NormalCopula(2,rho[i])
# 			G[:,:,i] = reshape(dnormCopula(u,Cop),n,n)
# 		end

# 		# normalize by row sums
# 		G = G./sum(G,2)
# 		return G

# 	end

# end

function rouwenhorst(df::DataFrame,ygrid::Array,p::Param)

	# 3D version for rho varying by region
	# P = zeros(p.nz,p.nz,p.nJ)
	P = zeros(p.nz,p.nz)
	z = zeros(p.nz,p.nJ)

	for j in 1:p.nJ
		xz,xp = rouwenh(df[j,:Lresid][1],0.0,df[j,:sigma_resid][1],p.nz)
		# P[:,:,j] = xp
		P = xp
		# scale z into bounds
		ybar = log(ygrid[j])
		zlow = log(df[j,:q05]) - ybar
		zhigh = log(df[j,:q95]) - ybar
		# z[:,j] = linspace(zlow,zhigh,p.nz)
		z[:,j] = xz
	end
	return (z,P)
end




function rouwenh(rho::Float64,mu_eps,sigma_eps,n)
	q = (rho+1)/2
	nu = ((n-1)/(1-rho^2))^(1/2) * sigma_eps
	P = reshape([q,1-q,1-q,q],2,2)

	for i=2:n-1

		P = q * vcat(hcat(P , zeros(i,1)),zeros(1,i+1)) .+ (1-q).* vcat( hcat(zeros(i,1),P), zeros(1,i+1)) .+ 
		(1-q) .* vcat(zeros(1,i+1),hcat(P,zeros(i,1))) .+ q .*vcat(zeros(1,i+1),hcat(zeros(i,1),P))
		P[2:i,:] = P[2:i,:] ./ 2

	end

	z = linspace(mu_eps/(1-rho)-nu,mu_eps/(1-rho)+nu,n);
	return (z,P)
end


function get_yp_transition(df::DataFrame,p::Param,sigs::Array,pgrid,ygrid)
	Gyp = zeros(p.ny*p.np, p.ny*p.np)
	ycoef = convert(Array, @where(df,(:param.=="Y_Intercept") .| (:param.== "Y_LY") .| (:param.== "Y_LP"))[:value]) 
	pcoef = convert(Array, @where(df,(:param.=="P_Intercept") .| (:param.== "P_LY") .| (:param.== "P_LP"))[:value]) 
	for ip in 1:p.np
		for iy in 1:p.ny

		# TODO.
		# this should be the other way around?
		# i.e. define MvNormal on MvNormal([y,p]*coef,C)
		# but maybe this is actually equivalent.
			# setup MvNormal on that state
			C = PDMat(sigs)
			mvn = MvNormal([ygrid[iy],pgrid[ip]],C)

			for ip1 in 1:p.np
				for iy1 in 1:p.ny
					# get points to evaluate at
					xdata = hcat(1.0,ygrid[iy1],pgrid[ip1])
					new_y  = xdata * ycoef 
					new_p  = xdata * pcoef 
					# println("iy=$iy,ip=$ip,iy1=$iy1,ip1=$ip1")
					# println("y=$(ygrid[iy]),p=$(pgrid[ip])")
					# println("new_y = $(round(new_y,2)), new_p = $(round(new_p,2))")
					Gyp[iy + p.ny*(ip-1),iy1 + p.ny*(ip1-1)] = pdf(mvn,[new_y;new_p])
				end
			end
		end
	end
	# normalize for rows to sum to 1
	Gyp = Gyp ./ sum(Gyp,2)
	return Gyp
end

function Gyp_indices(p::Param,show=false)

	idx = zeros(Int,p.np*p.ny,3)
	for ip in 1:p.np
		for iy in 1:p.ny

			idx[iy + p.ny*(ip-1),1] = iy
			idx[iy + p.ny*(ip-1),2] = ip
			idx[iy + p.ny*(ip-1),3] = iy + p.ny*(ip-1)

			if show
				for ip1 in 1:p.np
					for iy1 in 1:p.ny
						println("iy  = $iy, ip  = $ip || iy1 = $iy1, ip1 = $ip1 || (row,col) = ($(iy + p.ny*(ip-1)), $(iy1 + p.ny*(ip1-1)))")
					end
				end
			end
		end
	end
	return idx
end


function show(io::IO, M::Model)
	r = sizeof(M.v)+sizeof(M.vh)+
		        sizeof(M.vfeas)+
		        sizeof(M.ch)+
		        sizeof(M.sh)+
		        sizeof(M.dh)+
		        sizeof(M.gridsXD["movecost"])+
		        sizeof(M.gridsXD["Gyp"])+
		        sizeof(M.gridsXD["Gz"])+
		        # sizeof(M.gridsXD["GzM"])+
		        sizeof(M.gridsXD["Gs"])+
		        sizeof(M.gridsXD["p"])+
		        sizeof(M.gridsXD["y"])+
		        sizeof(M.rho)+
		        sizeof(M.vbar)+
		        sizeof(M.EVfinal)+
		        sizeof(M.EV) 

	mb = round(r/1.049e+6,1)
	gb = round(r/1.074e+9,1)

	print(io, "size of model arrays:\n")
	print(io, "               in Mb: $(mb)\n")
	print(io, "               in Gb: $(gb)\n")
	print(io, "objects in model:\n")
	print(io, fieldnames(M))
end