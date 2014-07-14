

# setting up a model


type Model2

	# values and policies conditional on housing
	# dimvec  = (nz, na, nh, nt-1 )
	v   :: Array{Float64,4}
	s   :: Array{Float64,4}
	c   :: Array{Float64,4}

	# top-level value maxed over housing
	# dimvec2 = (nz, na, nh, nt-1 )
	EV   :: Array{Float64,4}
	dh   :: Array{Int,4}

	# expected final period value
	# dimensions: a,h
	EVfinal :: Array{Float64,2}

	# index of the first asset element > 0
	aone :: Int
	aonemax :: Int

	# grids
	grids   :: Dict{ASCIIString,Array{Float64,1}}
	gridsXD :: Dict{ASCIIString,Array{Float64}}

	dimvec ::(Int,Int,Int,Int) # total number of dimensions
	# dimnames::Array{ASCIIString}
	dimnames::DataFrame
	price :: Float64

	# constructor
	function Model2(p::Param;dropbox=false)

		dimvec  = (p.nz, p.na, p.nh,p.nt-1 )

		v= reshape(rand(prod((dimvec))),dimvec)
		s= fill(NaN,dimvec)
		c= fill(NaN,dimvec)

		EVfinal = reshape(rand(prod((p.na,p.nh))),(p.na,p.nh))

		dh = fill(0,dimvec)

		EV = reshape(rand(prod((dimvec))),dimvec)

		bounds = Dict{ASCIIString,(Float64,Float64)}()
		bounds["assets"] = (-2.0,2.0)


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

		
		# Transition matrices of idiosyncratic term of income: z
		# get z supports and transition matrics (in long form)
		zsupp       = DataFrame(read_rda(joinpath(indir,"zsupp_n$(p.nz).rda"))["zsupp"])
		trans_z     = DataFrame(read_rda(joinpath(indir,"ztrans_n$(p.nz).rda"))["ztrans"])
		# transMove_z = DataFrame(read_rda(joinpath(indir,"transMove_n$(p.nz).rda"))["longtransMove"])



		# 1D grids
		# =========

		# grids = (ASCIIString => Array{Float64,1})["asset_own" => linspace(p.bounds["asset_own"][1],p.bounds["asset_own"][2],p.na)]

		# asset grid
		x = sinh(linspace(asinh(bounds["assets"][1]),asinh(bounds["assets"][2]),p.na))
		# center on zero
		x = x .- x[ indmin(abs(x)) ] 
		grids = Dict{ASCIIString,Array{Float64,1}}()
		# x = [-4.0,-3.0,-2.0,-1.0,linspace(0.0,0.5,5),0.6,0.7,1.0,2.0,3.0,4.0]
		grids["assets"] = deepcopy(x)
		aone  = findfirst(grids["assets"].>=0)

		# savings grid
		x = sinh(linspace(asinh(bounds["assets"][1]),asinh(bounds["assets"][2]),p.namax))
		# center on zero
		x = x .- x[ indmin(abs(x)) ] 
		grids["saving0"] = deepcopy(x)
		grids["saving1"] = deepcopy(x)

		# adjust today's savings by inverse interest rate
		grids["saving0"][x.<0]  = grids["saving0"][x.<0] ./ p.Rm
		grids["saving0"][x.>=0]  = grids["saving0"][x.>=0] ./ p.R
		aonemax  = findfirst(grids["saving0"].>=0)

		# grids["asset_rent"] = linspace(bounds["asset_rent"][1],bounds["asset_rent"][2],p.na)
		grids["housing"]    = linspace(0.0,1.0,p.nh)
		# grids["P"]          = linspace(bounds["P"][1],bounds["P"][2],p.nP)


		# 2D grids
		# =========


		# national transition matrices
		# ============================

		# GY = makeTransition(p.nY,p.rhoY)


		# 3D grids
		# =========

		# rebuild as 3D array
		# pgrid[AggState,LocalState,Location]
		# pgrid = Float64[grids["P"][i] .* pgrid[j,k] for i=1:p.nP, j=1:p.np, k=1:p.nJ]
		# ygrid = [grids["Y"][i] .+ ygrid[j,k] for i=1:p.nY, j=1:p.ny, k=1:p.nJ]

		# supports of regional idiosyncratic income shocks z
		zgrid = zeros(Float64,p.nz,p.nt-1)
		sdf = zsupp[zsupp[:Division].=="ENC",:]
		for sdf in groupby(zsupp,[:age])
			it = findin(p.ages,sdf[:age])
			if length(it) > 0 && it[1] < p.nt
				for iz in 1:p.nz
					zgrid[iz,it] = sdf[1,2+iz]
				end
			end
		end
		# convert to levels normalized by median income in 1000's of dollars
		zgrid = exp(zgrid) ./ (36000 / 1000)


		# transition matrices
		# ===================

		
		Gz  = zeros(p.nz,p.nz)
		# GzM = zeros(p.nz,p.nz,p.nJ)

		# [z(t),z(t+1),(move or stay in) region]
		sdf = trans_z[trans_z[:Division].=="ENC",:]
		Gz = array(sdf[:, 2:ncol(trans_z)])



		gridsXD = (ASCIIString => Array{Float64})["Gz"=> Gz,"z" => zgrid]

		dimnames = DataFrame(dimension=["z", "a", "h", "age" ],
			                  points = [p.nz, p.na, p.nh, p.nt-1 ])

		price = 2.0

		return new(v,s,c,EV,dh,EVfinal,aone,aonemax,grids,gridsXD,dimvec,dimnames,price)

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



function show(io::IO, M::Model2)
	r = sizeof(M.v)+
		        sizeof(M.c)+
		        sizeof(M.s)+
		        sizeof(M.gridsXD["Gz"])+
		        # sizeof(M.gridsXD["GzM"])+
		        sizeof(M.dh)+
		        sizeof(M.EVfinal)+
		        sizeof(M.EV) 

	mb = round(r/1.049e+6,1)
	gb = round(r/1.074e+9,1)

	print(io, "size of model arrays:\n")
	print(io, "               in Mb: $(mb)\n")
	print(io, "               in Gb: $(gb)\n")
	print(io, "objects in model:\n")
	print(io, names(M))
end