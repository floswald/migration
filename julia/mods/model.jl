

# setting up a model



type Model 

	# values conditional on moving to k
	# dimensions: a,z,psi,t,P,Y,j,k,pj,yj
	vstay :: Array{Float64,11}
	vsell :: Array{Float64,11}
	vbuy  :: Array{Float64,11}
	vrent :: Array{Float64,11}

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

		vstay   = fill(0.0,p.dimvec)
		vsell   = fill(0.0,p.dimvec)
		vbuy    = fill(0.0,p.dimvec)
		vrent   = fill(0.0,p.dimvec)

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

		# 2D grids
		# =========

		# price and incomes by region
		xp = fill(0.0,p.np,p.nJ)
		xy = fill(0.0,p.ny,p.nJ)
		for i = 1:p.nJ
			xp[:,i] = linspace(p.pbounds["p"][i][1], p.pbounds["p"][i][2], p.np)
			xy[:,i] = linspace(p.pbounds["y"][i][1], p.pbounds["y"][i][2], p.ny)
		end

		# national transition matrices
		GY = rand(p.nY,p.nY)
		GP = rand(p.nY,p.nY)
		GY = GY./sum(GY,2)
		GP = GP./sum(GP,2)

		dist = rand(p.nJ,p.nJ) * 1000.0
		dist[ diagind(dist,0) ] = 0
		Base.LinAlg.copytri!(dist,'U')

		grids2D = (ASCIIString => Array{Float64,2})["p" => xp, "y" => xy, "GY"=> GY, "GP" => GP, "dist" => dist]

		# 3D grids
		# =========

		Gy = [rand(p.ny,p.ny).+z for z=linspace(0.0,1.0,n.nJ)]	# transition matrices for each region
		Gp = [rand(p.np,p.np).+z for z=linspace(1.0,5.0,n.nJ)]	# transition matrices for each region
		Gz = [rand(p.nz,p.nz).+z for z=linspace(-0.1,0.1,n.nJ)]	# transition matrices for each region

		Gy = Gy./sum(Gy,2)
		Gp = Gp./sum(Gp,2)
		Gz = Gz./sum(Gz,2)

		grids3D = (ASCIIString => Array{Float64,3})["Gy" => Gy, "Gp" => Gp, "Gz"=> Gz]

		return new(vstay,vsell,vbuy,vrent,EVfinal,Vown,Vrent,EVown,EVrent,grids,grids2D,grids3D)

	end



end


function show(io::IO, M::Model)
	d = 0
	for i = 1:9
		d = d+sizeof(M.Gy[i]) + sizeof(M.Gp[i]) + sizeof(M.Gz[i])
	end
	r = round( (sizeof(M.vstay)+
		        sizeof(M.vsell)+
		        sizeof(M.vbuy)+
		        sizeof(M.vrent)+
		        sizeof(M.EVfinal)+
		        sizeof(M.Vown)+
		        sizeof(M.Vrent)+
		        sizeof(M.EVown)+
		        sizeof(M.EVrent)+
		        sizeof(M.GP)+
		        sizeof(M.GY)+d)/8388608, 3)
		        
	print(io, "size of Model in Mb: $(r)")
end