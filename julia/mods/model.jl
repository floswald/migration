

# setting up a model



type Model 

	# values conditional on moving to k
	# dimensions: a,z,psi,t,P,Y,j,k,pj,yj
	vsell :: Array{Float64,11}
	vrent :: Array{Float64,11}

	# values conditional on staying in j
	# dimensions: a,z,psi,t,P,Y,j,pj,yj
	vstay :: Array{Float64,10}
	vbuy  :: Array{Float64,10}

	# expected final period value
	# dimensions: a,h,P,j,pj
	EVfinal :: Array{Float64,5}

	# values unconditional on moving to k
	# dimensions: a,z,psi,t,P,Y,j,k,pj,yj
	Vown    :: Array{Float64,10}
	Vrent   :: Array{Float64,10}
	EVown   :: Array{Float64,10}
	EVrent  :: Array{Float64,10}


	# grids
	grids   :: Dict{ASCIIString,Array{Float64,1}}
	grids2D :: Dict{ASCIIString,Array{Float64,2}}
	grids3D :: Dict{ASCIIString,Array{Float64,3}}

	# constructor
	function Model(p::Param)

		vsell   = fill(0.0,p.dimvec)
		vrent   = fill(0.0,p.dimvec)
		vstay   = fill(0.0,p.dimvec2)
		vbuy    = fill(0.0,p.dimvec2)

		EVfinal = fill(0.0,(p.na,p.nh,p.nP,p.nJ,p.np))

		Vown    = fill(0.0,p.dimvec2)
		Vrent   = fill(0.0,p.dimvec2)

		EVown   = fill(0.0,p.dimvec2)
		EVrent  = fill(0.0,p.dimvec2)

		

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

		agemat = hcat(ones(p.nt),30:(30+p.nt-1))
		agemat = hcat(agemat,agemat[:,2].^2)
		AgeP = JSON.parsefile("/Users/florianoswald/Dropbox/mobility/output/model/BBL/inc-process/Div-REcoefs.json")
		ageprofile = zeros(p.nt,p.nJ)

		i = 0
		for j in keys(AgeP)
			i = i+1
			ageprofile[:,i] = agemat * convert(Array{Float64,1},AgeP[j]["fixed"])
		end


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
		pgrid = [grids["P"][i] .+ pgrid[j,k] for i=1:p.nP, j=1:p.np, k=1:p.nJ]
		ygrid = [grids["Y"][i] .+ ygrid[j,k] for i=1:p.nY, j=1:p.ny, k=1:p.nJ]

		# national transition matrices
		# ============================

		Gy = makeTransition(p.ny,p.rhoy)
		Gp = makeTransition(p.np,p.rhop)
		Gz = makeTransition(p.nz,p.rhoz)

		grids3D = (ASCIIString => Array{Float64,3})["Gy" => Gy, "Gp" => Gp, "Gz"=> Gz, "p" => pgrid, "y" => ygrid ]

		return new(vsell,vrent,vstay,vbuy,EVfinal,Vown,Vrent,EVown,EVrent,grids,grids2D,grids3D)

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
	r = round( (sizeof(M.vstay)+
		        sizeof(M.vsell)+
		        sizeof(M.vbuy)+
		        sizeof(M.vrent)+
		        sizeof(M.EVfinal)+
		        sizeof(M.Vown)+
		        sizeof(M.Vrent)+
		        sizeof(M.EVown)+
		        sizeof(M.EVrent))
		        /8388608, 3)
		        
	print(io, "size of Model in Mb: $(r)")
end