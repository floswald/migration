

# setting up a model


type Model 

	# values and policies conditional on moving to k
	# dimvec  = (nJ, ns, ny, np, nP, nz, na, nh, ntau,  nJ, nt-1 )
	v   :: Array{Float64,11}
	s   :: Array{Int,11}
	c   :: Array{Float64,11}
	rho :: Array{Float64,11}

	# top-level value maxed over housing and location
	# dimvec2 = (ns, ny, np, nP, nz, na, nh, ntau,  nJ, nt-1 )
	EV   :: Array{Float64,10}
	EVMove   :: Array{Float64,10}
	vbar :: Array{Float64,10}
	dh   :: Array{Int,10}

	# expected final period value
	# dimensions: a,h,P,j,pj
	EVfinal :: Array{Float64,5}

	# index of the first asset element > 0
	aone :: Int

	# grids
	grids   :: Dict{ASCIIString,Array{Float64,1}}
	grids2D :: Dict{ASCIIString,Array{Float64,2}}
	gridsXD :: Dict{ASCIIString,Array{Float64}}

	dimvec ::(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) # total number of dimensions
	dimvec2::(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) # total - housing
	# dimnames::Array{ASCIIString}
	dimnames::DataFrame
	regnames::DataFrame
	distance::Array{Any,2}

	# constructor
	function Model(p::Param;dropbox=false)

		dimvec  = (p.nJ, p.ns, p.ny, p.np, p.nP, p.nz, p.na, p.nh, p.ntau,  p.nJ, p.nt-1 )
		dimvec2 = (p.ns, p.ny, p.np, p.nP, p.nz, p.na, p.nh, p.ntau,  p.nJ, p.nt-1)

		v= reshape(rand(prod((dimvec))),dimvec)
		s= fill(0,dimvec)
		c= fill(0.0,dimvec)
		rho = reshape(rand(prod((dimvec))),dimvec)

		EVfinal = reshape(rand(prod((p.na,p.nh,p.nP,p.np,p.nJ))),(p.na,p.nh,p.nP,p.np,p.nJ))

		dh = fill(0,dimvec2)

		EV = reshape(rand(prod((dimvec2))),dimvec2)
		EVMove = reshape(rand(prod((dimvec2))),dimvec2)
		vbar = reshape(rand(prod((dimvec2))),dimvec2)

		bounds = Dict{ASCIIString,(Float64,Float64)}()
		bounds["assets"] = (-2.0,2.0)
		bounds["tau"]        = (0.0,0.1)
		# bounds["Y"]          = (0.5,1.5)


		# import data from R
		# ==================

		# if on my machine
		if Sys.OS_NAME == :Darwin
			indir = joinpath(ENV["HOME"],"Dropbox/mobility/output/model/data_repo/in_data_jl")
		else
			if dropbox
				indir = joinpath(ENV["HOME"],"data_repo/mig")
				run(`dropbox_uploader download mobility/output/model/data_repo/in_data_jl/ $indir`)
				indir = joinpath(indir,"in_data_jl")
			else
				indir = joinpath(ENV["HOME"],"data_repo/mig/in_data_jl")
			end
		end

		# dbase = h5read(joinpath(indir,"mig_db_in.h5"))
		# rhoy = h52df(joinpath(indir,"mig_db_in.h5"),"rhoy/")	# function makes a dataframe from all cols in rhoy

		# distance matrix
		distdf = DataFrame(read_rda(joinpath(indir,"distance.rda"))["df"])
		dist = array(distdf)

		# population weights
		popweights = DataFrame(read_rda(joinpath(indir,"prop.rda"))["prop"])
		sort!(popweights,cols=1)

		# AR1 coefficients of regional price/income deviations from national index
		rhoy = DataFrame(read_rda(joinpath(indir,"rho-income.rda"))["rhoincome"])
		rhop = DataFrame(read_rda(joinpath(indir,"rho-price.rda"))["rhoprice"])

		# bounds on price and income deviations
		divinc       = DataFrame(read_rda(joinpath(indir,"divincome.rda"))["divincome"])
		divprice     = DataFrame(read_rda(joinpath(indir,"divprice.rda"))["divprice"])
		pbounds      = Dict{ASCIIString,DataFrame}()
		pbounds["y"] = divinc[1:p.nJ,[:Division,:mindev,:maxdev]]
		pbounds["p"] = divprice[1:p.nJ,[:Division,:mindev,:maxdev]]

		medinc = DataFrame(read_rda(joinpath(indir,"normalize.rda"))["normalize"])

		regnames = DataFrame(j=1:p.nJ,Division=PooledDataArray(divinc[1:p.nJ,:Division]),prop=popweights[:proportion])

		# price to income ratio: gives bounds on aggregate P
		p2y          = DataFrame(read_rda(joinpath(indir,"p2y.rda"))["p2y"])
		p2y          = p2y[p2y[:year].>1995,:]
		bounds["P"]  = (minimum(p2y[:,:p2y]),maximum(p2y[:,:p2y]))

		# Transition matrices of idiosyncratic term of income: z
		# get z supports and transition matrics (in long form)
		zsupp       = DataFrame(read_rda(joinpath(indir,"zsupp_n$(p.nz).rda"))["zsupp"])
		trans_z     = DataFrame(read_rda(joinpath(indir,"ztrans_n$(p.nz).rda"))["ztrans"])
		# transMove_z = DataFrame(read_rda(joinpath(indir,"transMove_n$(p.nz).rda"))["longtransMove"])

		# kids transition matrix
		ktrans = DataFrame(read_rda(joinpath(indir,"kidstrans.rda"))["kids_trans"])

		kmat = zeros(p.ns,p.ns,p.nt)
		for ir in eachrow(ktrans)
			if ir[:age] < p.maxAge && ir[:age] >= p.minAge
				kmat[ir[:kids]+1,ir[:kids2]+1,findin(p.ages,ir[:age])[1]] = ir[:Freq]
			end
		end



		# 1D grids
		# =========

		# grids = (ASCIIString => Array{Float64,1})["asset_own" => linspace(p.bounds["asset_own"][1],p.bounds["asset_own"][2],p.na)]
		x = sinh(linspace(asinh(bounds["assets"][1]),asinh(bounds["assets"][2]),p.na))
		# center on zero
		x = x .- x[ indmin(abs(x)) ] 
		grids = Dict{ASCIIString,Array{Float64,1}}()
		# x = [-4.0,-3.0,-2.0,-1.0,linspace(0.0,0.5,5),0.6,0.7,1.0,2.0,3.0,4.0]
		grids["assets"] = x

		grids["saving"] = deepcopy(x)
		grids["saving"][x.<0]  = grids["saving"][x.<0] ./ p.Rm
		grids["saving"][x.>=0]  = grids["saving"][x.>=0] ./ p.R
		# grids["asset_rent"] = linspace(bounds["asset_rent"][1],bounds["asset_rent"][2],p.na)
		grids["housing"]    = linspace(0.0,1.0,p.nh)
		# grids["P"]          = linspace(bounds["P"][1],bounds["P"][2],p.nP)
		grids["P"]          = linspace(1.0,1.0,p.nP)
		grids["W"]          = zeros(p.na)
		grids["tau"]        = linspace(0.0,1.0,p.ntau)

		aone  = findfirst(grids["assets"].>=0)

		# 2D grids
		# =========


		# national transition matrices
		# ============================

		# GY = makeTransition(p.nY,p.rhoY)
		GP = makeTransition(p.nP,p.rhoP)

		grids2D = (ASCIIString => Array{Float64,2})["GP" => GP]

		# 3D grids
		# =========

		# regional prices
		# 3D array (national_price,regional_price,region_id)
		# these are the percentage deviations from trend
		ygrid = zeros(Float64,p.ny,p.nJ)
		pgrid = zeros(Float64,p.np,p.nJ)
		for i = 1:p.nJ
			pgrid[:,i] = 1.0 .+ linspace(pbounds["p"][i,:mindev], pbounds["p"][i,:maxdev], p.np)   # (1 + %-deviation)
		    # ygrid[:,i] = 1.0 .+ linspace(pbounds["y"][i,:mindev], pbounds["y"][i,:maxdev], p.ny)
		    ygrid[:,i] = 1.0 .+ linspace(0.5,0.5 , p.ny)
		end

		# rebuild as 3D array
		# pgrid[AggState,LocalState,Location]
		# pgrid = Float64[grids["P"][i] .* pgrid[j,k] for i=1:p.nP, j=1:p.np, k=1:p.nJ]
		pgrid = Float64[3.0 for i=1:p.nP, j=1:p.np, k=1:p.nJ]
		# ygrid = [grids["Y"][i] .+ ygrid[j,k] for i=1:p.nY, j=1:p.ny, k=1:p.nJ]

		# supports of regional idiosyncratic income shocks z
		zgrid = zeros(Float64,p.nz,p.nJ,p.nt-1)
		for sdf in groupby(zsupp,[:Division,:age])
			jj = regnames[regnames[:Division] .== sdf[1,:Division],:j]
			it = findin(p.ages,sdf[:age])
			if length(it) > 0 && it[1] < p.nt
				for iz in 1:p.nz
					zgrid[iz,jj,it] = sdf[1,2+iz]
				end
			end
		end
		# convert to levels normalized by median income in 1000's of dollars
		zgrid = exp(zgrid) ./ (medinc[1] / 1000)


		# regional transition matrices
		# ============================

		# [LocalPrice(t),LocalPrice(t+1),Location]
		Gy  = makeTransition(p.ny,array(rhoy[:Ldev]))
		Gp  = makeTransition(p.np,array(rhop[:Ldev]))
		Gz  = zeros(p.nz,p.nz,p.nJ)
		# GzM = zeros(p.nz,p.nz,p.nJ)

		# [z(t),z(t+1),(move or stay in) region]
		for sdf in groupby(trans_z,:Division)
			jj = regnames[regnames[:Division] .== sdf[1,:Division],:j]
			Gz[:,:,jj] = array(sdf[:, 2:ncol(trans_z)])
		end

		# moving cost function
		# ====================

		mc = zeros(p.nt-1,p.nJ,p.nJ,p.nh,p.ns)
		for it in 1:p.nt-1
			for ij in 1:p.nJ
				for ik in 1:p.nJ
					for ih in 0:1
						for is in 0:(p.ns-1)
							mc[it,ij,ik,ih+1,is+1] = (ij!=ik) * (p.MC1*ih + p.MC2 * dist[ij,ik] + p.MC3 * it + is*p.MC4 )
						end
					end
				end
			end
		end


		gridsXD = (ASCIIString => Array{Float64})["Gy" => Gy, "Gp" => Gp, "Gz"=> Gz,"p" => pgrid, "y" => ygrid, "z" => zgrid, "movecost" => mc ,"Gs" => kmat]

		dimnames = DataFrame(dimension=["k", "s", "y", "p", "P", "z", "a", "h", "tau", "j", "age" ],
			                  points = [p.nJ, p.ns, p.ny, p.np, p.nP, p.nz, p.na, p.nh, p.ntau,  p.nJ, p.nt-1 ])


		return new(v,s,c,rho,EV,EVMove,vbar,dh,EVfinal,aone,grids,grids2D,gridsXD,dimvec,dimvec2,dimnames,regnames,dist)

	end



end


# function logAssets(p::Param,x)

# 	out = zeros(length(x))
# 		off = 1	# offset for log(0) in case b[1] is positive
# 		out[1]            <- log(x[1] + off)
# 		out[end]            <- log(x[end] + off)
# 		out               <- linspace(out[1],out[end],round(p.na/2)
# 		out               <- exp( out ) .- off



function makeTransition(n,rho)

	u = linspace(1/n, 1-1/n, n)
	u = [repmat(u,n,1) repmat(u,1,n)'[:] ]
	
	J = length(rho)

	if J==1
		G = zeros(n,n)
		Cop = NormalCopula(2,rho)
		G = reshape(dnormCopula(u,Cop),n,n)

		# normalize by row sums
		G = G./sum(G,2)
		return G

	else

		G = zeros(n,n,J)
		for i=1:J
			Cop = NormalCopula(2,rho[i])
			G[:,:,i] = reshape(dnormCopula(u,Cop),n,n)
		end

		# normalize by row sums
		G = G./sum(G,2)
		return G

	end

end



# function(ff::HDF5File,path)
# 	fid = h5open(ff,"r")
# 	for obj in fid[path] 



function show(io::IO, M::Model)
	r = sizeof(M.v)+
		        sizeof(M.c)+
		        sizeof(M.s)+
		        sizeof(M.grids2D["GP"])+
		        sizeof(M.gridsXD["movecost"])+
		        sizeof(M.gridsXD["Gy"])+
		        sizeof(M.gridsXD["Gp"])+
		        sizeof(M.gridsXD["Gz"])+
		        # sizeof(M.gridsXD["GzM"])+
		        sizeof(M.gridsXD["Gs"])+
		        sizeof(M.gridsXD["p"])+
		        sizeof(M.gridsXD["y"])+
		        sizeof(M.dh)+
		        sizeof(M.rho)+
		        sizeof(M.vbar)+
		        sizeof(M.EVfinal)+
		        sizeof(M.EV) +
		        sizeof(M.EVMove)

	mb = round(r/1.049e+6,1)
	gb = round(r/1.074e+9,1)

	print(io, "size of model arrays:\n")
	print(io, "               in Mb: $(mb)\n")
	print(io, "               in Gb: $(gb)\n")
	print(io, "objects in model:\n")
	print(io, names(M))
end