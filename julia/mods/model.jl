

# setting up a model


type TModel

	v :: Array{Float64,12}
	function TModel(p::Param)
		v= fill(p.myNA,p.dimvec)
		return new(v)
	end
end




type Model 

	# values and policies conditional on moving to k
	# dimvec  = (nJ, nh, nJ, nh, ny, np, nY ,nP, nz, ntau,  na, nt-1 )
	v :: Array{Float64,12}
	s :: Array{Int,12}
	c :: Array{Float64,12}

	# location-optimal value and index array (maximized over housing in k)
	# dimvec2 = (nJ	, na, nh, ny, np, nY ,nP, nz, ntau,  nJ, nt-1 )
	vh  :: Array{Float64,11}
	rho :: Array{Float64,11}
	dh  :: Array{Int,11}

	# top-level value maxed over housing and location
	# dimvec3 = (na, nh, ny, np, nY ,nP, nz, ntau,  nJ, nt-1 )
	EV   :: Array{Float64,10}
	vbar :: Array{Float64,10}

	# expected final period value
	# dimensions: a,h,P,j,pj
	EVfinal :: Array{Float64,5}

	# grids
	grids   :: Dict{ASCIIString,Array{Float64,1}}
	grids2D :: Dict{ASCIIString,Array{Float64,2}}
	gridsXD :: Dict{ASCIIString,Array{Float64}}

	# constructor
	function Model(p::Param)

		v= fill(p.myNA,p.dimvec)
		s= fill(0,p.dimvec)
		c= fill(p.myNA,p.dimvec)

		EVfinal = reshape(rand(prod((p.na,p.nh,p.nP,p.np,p.nJ))),(p.na,p.nh,p.nP,p.np,p.nJ))

		vh = reshape(rand(prod((p.dimvec2))),p.dimvec2)
		rho = fill(p.myNA,p.dimvec2)
		dh = fill(0,p.dimvec2)

		EV = reshape(rand(prod((p.dimvec3))),p.dimvec3)
		vbar = fill(p.myNA,p.dimvec3)

		

		# 1D grids
		# =========

		grids = (ASCIIString => Array{Float64,1})["asset_own" => linspace(p.bounds["asset_own"][1],p.bounds["asset_own"][2],p.na)]
		grids["asset_rent"] = linspace(p.bounds["asset_rent"][1],p.bounds["asset_rent"][2],p.na)
		grids["housing"]    = linspace(0.0,1.0,p.nh)
		grids["P"]          = linspace(p.bounds["P"][1],p.bounds["P"][2],p.nP)
		grids["Y"]          = linspace(p.bounds["Y"][1],p.bounds["Y"][2],p.nY)
		grids["W"]          = zeros(p.na)
		grids["z"]          = zeros(p.nz)

		# 2D grids
		# =========


		# national transition matrices
		# ============================

		GY = makeTransition(p.nY,p.rhoY)
		GP = makeTransition(p.nP,p.rhoP)

		# Distance matrix
		# ===============

		dist = rand(p.nJ,p.nJ) * 1000.0
		dist[ diagind(dist,0) ] = 0
		Base.LinAlg.copytri!(dist,'U')

		# age profile by region
		# =====================

		agemat = hcat(ones(p.nt),p.minAge:(p.minAge+p.nt-1))
		agemat = hcat(agemat,agemat[:,2].^2)
		AgeP = JSON.parsefile("/Users/florianoswald/Dropbox/mobility/output/model/BBL/inc-process/Div-REcoefs.json")
		agep = zeros(p.nt,length(AgeP))

		i = 0
		for j in keys(AgeP)
			i = i+1
			agep[:,i] = agemat * convert(Array{Float64,1},AgeP[j]["fixed"])
		end

		# cut ageprofile to number of regions currently running:
		ageprofile = agep[:,1:p.nJ]

		grids2D = (ASCIIString => Array{Float64,2})["GY"=> GY, "GP" => GP, "dist" => dist, "ageprof" => ageprofile]

		# 3D grids
		# =========

		# regional prices
		# 3D array (national_price,regional_price,region_id)
		ygrid = zeros(p.ny,p.nJ)
		pgrid = zeros(p.np,p.nJ)
		for i = 1:p.nJ
			pgrid[:,i] = linspace(p.pbounds["p"][i][1], p.pbounds["p"][i][2], p.np)
		    ygrid[:,i] = linspace(p.pbounds["y"][i][1], p.pbounds["y"][i][2], p.ny)
		end

		# rebuild as 3D array
		# pgrid[AggState,LocalState,Location]
		pgrid = [grids["P"][i] .+ pgrid[j,k] for i=1:p.nP, j=1:p.np, k=1:p.nJ]
		ygrid = [grids["Y"][i] .+ ygrid[j,k] for i=1:p.nY, j=1:p.ny, k=1:p.nJ]

		# regional transition matrices
		# ============================

		# [LocalPrice(t),LocalPrice(t+1),Location]
		Gy = makeTransition(p.ny,p.rhoy)
		Gp = makeTransition(p.np,p.rhop)
		Gz = makeTransition(p.nz,p.rhoz)

		# moving cost function
		# ====================

		mc = zeros(p.nt-1,p.nJ,p.nJ,p.nh,p.ntau)
		for it in 1:p.nt-1
			for ij in 1:p.nJ
				for ik in 1:p.nJ
					for ih in 0:1
						for itau in 1:p.ntau
							mc[it,ij,ik,ih+1,itau] = (ij!=ik) * (p.tau[itau] + p.MC[1]*ih + p.MC[2] * dist[ij,ik] + p.MC[3] * it )
						end
					end
				end
			end
		end


		gridsXD = (ASCIIString => Array{Float64})["Gy" => Gy, "Gp" => Gp, "Gz"=> Gz, "p" => pgrid, "y" => ygrid, "movecost" => mc ]



		return new(v,s,c,vh,rho,dh,EV,vbar,EVfinal,grids,grids2D,gridsXD)

	end



end





function makeTransition(n,rho)

	u = linspace(1/n, 1-1/n, n)
	u =[repmat(u,n,1) repmat(u,1,n)'[:] ]
	
	J = length(rho)

	if J==1
		G = zeros(n,n)
		Cop = Copmod.Copula(2,rho)
		G = reshape(Copmod.dnormCopula(u,Cop),n,n)

		# normalize by row sums
		G = G./sum(G,2)
		return G

	else

		G = zeros(n,n,J)
		for i=1:J
			Cop = Copmod.Copula(2,rho[i])
			G[:,:,i] = reshape(Copmod.dnormCopula(u,Cop),n,n)
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
		        sizeof(M.grids2D["GY"])+
		        sizeof(M.grids2D["GP"])+
		        sizeof(M.grids2D["dist"])+
		        sizeof(M.grids2D["ageprof"])+
		        sizeof(M.gridsXD["movecost"])+
		        sizeof(M.gridsXD["Gy"])+
		        sizeof(M.gridsXD["Gp"])+
		        sizeof(M.gridsXD["p"])+
		        sizeof(M.gridsXD["y"])+
		        sizeof(M.vh)+
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