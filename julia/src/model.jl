

# setting up a model


type Model 

	# values and policies conditional on moving to k
	# dimvec  = (nJ, ny, np, nP, nz, na, nh, ntau,  nJ, nt-1 )
	v   :: Array{Float64,10}
	s   :: Array{Int,10}
	c   :: Array{Float64,10}
	rho :: Array{Float64,10}

	# top-level value maxed over housing and location
	# dimvec2 = (ny, np, nP, nz, na, nh, ntau,  nJ, nt-1 )
	EV   :: Array{Float64,9}
	vbar :: Array{Float64,9}
	dh   :: Array{Int,9}

	# expected final period value
	# dimensions: a,h,P,j,pj
	EVfinal :: Array{Float64,5}

	# index of the first asset element > 0
	aone :: Int

	# grids
	grids   :: Dict{ASCIIString,Array{Float64,1}}
	grids2D :: Dict{ASCIIString,Array{Float64,2}}
	gridsXD :: Dict{ASCIIString,Array{Float64}}

	dimvec ::(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) # total number of dimensions
	dimvec2::(Int,Int,Int,Int,Int,Int,Int,Int,Int) # total - housing
	# dimnames::Array{ASCIIString}
	dimnames::DataFrame
	regnames::DataFrame
	distance::Array{Any,2}

	# constructor
	function Model(p::Param)

		dimvec  = (p.nJ, p.ny, p.np, p.nP, p.nz, p.na, p.nh, p.ntau,  p.nJ, p.nt-1 )
		dimvec2 = (p.ny, p.np, p.nP, p.nz, p.na, p.nh, p.ntau,  p.nJ, p.nt-1)

		v= reshape(rand(prod((dimvec))),dimvec)
		s= fill(0,dimvec)
		c= fill(0.0,dimvec)
		rho = reshape(rand(prod((dimvec))),dimvec)

		EVfinal = reshape(rand(prod((p.na,p.nh,p.nP,p.np,p.nJ))),(p.na,p.nh,p.nP,p.np,p.nJ))

		dh = fill(0,dimvec2)

		EV = reshape(rand(prod((dimvec2))),dimvec2)
		vbar = reshape(rand(prod((dimvec2))),dimvec2)

		bounds = Dict{ASCIIString,(Float64,Float64)}()
		bounds["asset_own"] = (-5.0,10.0)
		bounds["asset_rent"] = (0.01,10.0)
		bounds["tau"]        = (0.0,0.1)
		# bounds["Y"]          = (0.5,1.5)


		# import data from R
		# ==================

		indir = joinpath(ENV["HOME"],"Dropbox/mobility/output/model/R2Julia")

		medinc = DataFrame(read_rda(joinpath(indir,"normalize.rda"))["normalize"])
		divinc = DataFrame(read_rda(joinpath(indir,"divincome.rda"))["divincome"])


		pbounds = Dict{ASCIIString,DataFrame}()
		pbounds["y"] = divinc[1:p.nJ,[:Division,:mindev,:maxdev]]
		regnames = DataFrame(j=1:p.nJ,Division=divinc[1:p.nJ,:Division])

		divprice = DataFrame(read_rda(joinpath(indir,"divprice.rda"))["divprice"])
		pbounds["p"] = divprice[1:p.nJ,[:Division,:mindev,:maxdev]]

		distdf = DataFrame(read_rda(joinpath(indir,"distance.rda"))["df"])
		dist = array(distdf)

		p2y = DataFrame(read_rda(joinpath(indir,"p2y.rda"))["p2y"])
		p2y = p2y[p2y[:year].>1995,:]
		bounds["P"]  = (minimum(p2y[:,:p2y]),maximum(p2y[:,:p2y]))

		


		# 1D grids
		# =========

		# grids = (ASCIIString => Array{Float64,1})["asset_own" => linspace(p.bounds["asset_own"][1],p.bounds["asset_own"][2],p.na)]
		x = sinh(linspace(asinh(bounds["asset_own"][1]),asinh(bounds["asset_own"][2]),p.na))
		# center on zero
		x = x .- x[ indmin(abs(x)) ] 
		grids = Dict{ASCIIString,Array{Float64,1}}()
		grids["asset_own"] = x
		grids["asset_rent"] = linspace(bounds["asset_rent"][1],bounds["asset_rent"][2],p.na)
		grids["housing"]    = linspace(0.0,1.0,p.nh)
		grids["P"]          = linspace(bounds["P"][1],bounds["P"][2],p.nP)
		grids["W"]          = zeros(p.na)
		grids["tau"]        = linspace(0.0,1.0,p.ntau)

		aone  = findfirst(grids["asset_own"].>=0)

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
		    ygrid[:,i] = 1.0 .+ linspace(pbounds["y"][i,:mindev], pbounds["y"][i,:maxdev], p.ny)
		end

		# rebuild as 3D array
		# pgrid[AggState,LocalState,Location]
		pgrid = Float64[grids["P"][i] .* pgrid[j,k] for i=1:p.nP, j=1:p.np, k=1:p.nJ]
		# ygrid = [grids["Y"][i] .+ ygrid[j,k] for i=1:p.nY, j=1:p.ny, k=1:p.nJ]

		# get z supports
		if p.nz==3
			zsupp = readtable("/Users/florianoswald/Dropbox/mobility/output/model/R2julia/zsupp_n3.csv")	
		elseif p.nz==4
			zsupp = readtable("/Users/florianoswald/Dropbox/mobility/output/model/R2julia/zsupp_n4.csv")	
		elseif p.nz==5
			zsupp = readtable("/Users/florianoswald/Dropbox/mobility/output/model/R2julia/zsupp_n5.csv")
		else
			error("have prepared only zsupport 3,4,5")
		end

		# subset to min/max age
		zsupp = zsupp[(zsupp[:age] .>= p.minAge) & (zsupp[:age] .<= p.maxAge), 2:end]

		zgrid = zeros(Float64,p.nz,p.nJ,p.nt-1)
		for iz = 1:p.nz
			for ij=1:p.nJ
				for it =1:(p.nt-1)
					zgrid[iz,ij,it] = 0.01 * zsupp[it + (p.nt-1)*(ij-1) ,2+iz]
				end
			end
		end

		# regional transition matrices
		# ============================

		# [LocalPrice(t),LocalPrice(t+1),Location]
		Gy = makeTransition(p.ny,p.rhoy)
		Gp = makeTransition(p.np,p.rhop)
		rhoz = [0.4 for i in 1:p.nJ]
		Gz = makeTransition(p.nz,rhoz)

		# moving cost function
		# ====================

		mc = zeros(p.nt-1,p.nJ,p.nJ,p.nh,p.ntau)
		for it in 1:p.nt-1
			for ij in 1:p.nJ
				for ik in 1:p.nJ
					for ih in 0:1
						for itau in 1:p.ntau
							mc[it,ij,ik,ih+1,itau] = (ij!=ik) * (grids["tau"][itau] + p.MC1*ih + p.MC2 * dist[ij,ik] + p.MC3 * it )
						end
					end
				end
			end
		end


		gridsXD = (ASCIIString => Array{Float64})["Gy" => Gy, "Gp" => Gp, "Gz"=> Gz, "p" => pgrid, "y" => ygrid, "z" => zgrid, "movecost" => mc ]

		dimnames = DataFrame(dimension=["k", "y", "p", "P", "z", "a", "h", "tau", "j", "age" ],
			                  points = [p.nJ, p.ny, p.np, p.nP, p.nz, p.na, p.nh, p.ntau,  p.nJ, p.nt-1 ])


		return new(v,s,c,rho,EV,vbar,dh,EVfinal,aone,grids,grids2D,gridsXD,dimvec,dimvec2,dimnames,regnames,dist)

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




function show(io::IO, M::Model)
	r = sizeof(M.v)+
		        sizeof(M.c)+
		        sizeof(M.s)+
		        # sizeof(M.grids2D["GY"])+
		        sizeof(M.grids2D["GP"])+
		        # sizeof(M.grids2D["ageprof"])+
		        sizeof(M.gridsXD["movecost"])+
		        sizeof(M.gridsXD["Gy"])+
		        sizeof(M.gridsXD["Gp"])+
		        sizeof(M.gridsXD["p"])+
		        sizeof(M.gridsXD["y"])+
		        sizeof(M.dh)+
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
	print(io, names(M))
end