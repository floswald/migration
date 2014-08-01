
# setting up a model


type Model 

	# values and policies conditional on moving to k
	# dimvec  = (nJ, ns, nz, ny, np, na, nh, ntau,  nJ, nt-1 )
	v   :: Array{Float64,10}
	vh  :: Array{Float64,11}	# v of stay cond on hh choice: (nh, nJ, ns, ny, np, nz, na, nh, ntau,  nJ, nt-1 )
	vfeas  :: Array{Bool,1}	# feasibility map
	sh  :: Array{Float64,11}
	ch  :: Array{Float64,11}
	cash  :: Array{Float64,11}
	rho :: Array{Float64,10}
	dh   :: Array{Int,10}

	# top-level value maxed over housing and location
	# dimvec2 = (ns, ny, np, nz, na, nh, ntau,  nJ, nt-1 )
	EV   :: Array{Float64,9}
	vbar :: Array{Float64,9}

	# expected final period value
	# dimensions: a,h,j,pj
	EVfinal :: Array{Float64,4}

	# index of the first asset element > 0
	aone :: Int

	# grids
	grids   :: Dict{ASCIIString,Array{Float64,1}}
	gridsXD :: Dict{ASCIIString,Array{Float64}}

	dimvec  ::(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) # total number of dimensions
	dimvecH ::(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) # dimvec conditional on H choice
	dimvec2::(Int,Int,Int,Int,Int,Int,Int,Int,Int) # total - housing
	# dimnames::Array{ASCIIString}
	dimnames::DataFrame
	regnames::DataFrame
	distance::Array{Any,2}

	# spline approximation settings
	nknots :: Dict{ASCIIString,Integer}
	degs   :: Dict{ASCIIString,Integer}
	knots  :: Dict{ASCIIString,Vector}

	# constructor
	function Model(p::Param;dropbox=false)

		# dimvec  = (nJ, ns, nz, ny, np, na, nh, ntau,  nJ, nt-1 )
		dimvec  = (p.nJ, p.ns, p.nz, p.ny, p.np, p.na, p.nh, p.ntau,  p.nJ, p.nt-1 )
		dimvecH = (p.nh, p.nJ, p.ns, p.nz, p.ny, p.np, p.na, p.nh, p.ntau,  p.nJ, p.nt-1 )
		dimvec2 = (p.ns, p.nz, p.ny, p.np, p.na, p.nh, p.ntau,  p.nJ, p.nt-1)

		v = fill(p.myNA,dimvec)
		vfeas = falses(prod(dimvecH))
		vh= fill(p.myNA,dimvecH)
		sh= fill(0.0,dimvecH)
		ch= fill(0.0,dimvecH)
		cash= fill(0.0,dimvecH)
		rho = fill(0.0,dimvec)

		EVfinal = fill(p.myNA,(p.na,p.nh,p.np,p.nJ))
		# EVfinal = fill(0.0,(p.na,p.nh,p.np,p.nJ))

		dh = fill(0,dimvec)

		EV = fill(p.myNA,dimvec2)
		vbar = fill(p.myNA,dimvec2)

		bounds = Dict{ASCIIString,(Float64,Float64)}()
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

		# VAR(1) regional house price and income
		VAR_coef = DataFrame(read_rda(joinpath(indir,"region-coefs.rda"))["rcoefs"])
		VAR_sig  = DataFrame(read_rda(joinpath(indir,"region-sig.rda"))["sig"])

		# sigs as array
		sigs=Float64[]
        for e in eachrow(VAR_sig)
     	  for i in 1:4
     		  push!(sigs,e[i])
     	  end
        end
        sigs = reshape(sigs,2,2,p.nJ)

		# population weights
		regnames = DataFrame(j=1:p.nJ,Division=PooledDataArray(popweights[1:p.nJ,:Division]),prop=popweights[1:p.nJ,:proportion])

		# individual income process parameters
		inc_coefs = DataFrame(read_rda(joinpath(indir,"ztable.rda"))["z"])
		inc_rho   = DataFrame(read_rda(joinpath(indir,"rtable.rda"))["rho"])

		# TODO fix those estimates!
		# for now manually overwrite with higher rho values
		inc_rho[:,:Lresid] = [0.98 for i=1:p.nJ]

		# grids for regional prices and incomes


		# kids transition matrix
		ktrans = DataFrame(read_rda(joinpath(indir,"kidstrans.rda"))["kids_trans"])

		kmat = zeros(p.ns,p.ns,p.nt)
		for ir in eachrow(ktrans)
			if ir[:age] < p.maxAge && ir[:age] >= p.minAge
				kmat[ir[:kids]+1,ir[:kids2]+1,findin(p.ages,ir[:age])[1]] = ir[:Freq]
			end
		end

		# XD grids
		# =========

		# p and y grids
		# -------------

		pgrid = zeros(p.np,p.nJ)
		ygrid = zeros(p.ny,p.nJ)
		for j in 1:p.nJ
			ygrid[:,j] = linspace(VAR_coef[j,:lb_y][1],VAR_coef[j,:ub_y][1],p.ny)
			pgrid[:,j] = linspace(VAR_coef[j,:lb_p][1],VAR_coef[j,:ub_p][1],p.np)
		end

		# grid for individual income (based on ygrid)
		# -------------------------------------------

		# TODO rouwenhorst => array (nJ,nz) supports and array (nJ,nz,nz) transitions
		(zsupp,Gz) = rouwenhorst(inc_rho,inc_coefs,ygrid,p)

		zgrid = zeros(p.nz,p.ny,p.nt-1,p.nJ)
		for j in 1:p.nJ
			for iy in 1:p.ny
				for it in 1:p.nt-1
					for iz in 1:p.nz
						zgrid[iz,iy,it,j] = inc_coefs[j,:Intercept] + inc_coefs[j,:logCensusMedinc] * log(ygrid[iy,j]) + inc_coefs[j,:age]*p.ages[it] + inc_coefs[j,:age2]*(p.ages[it])^2 + inc_coefs[j,:age3]*(p.ages[it])^3 + zsupp[iz,j]
					end
				end
			end
		end
		# convert to levels
		zgrid = exp(zgrid)


		# 1D grids
		# =========

		bounds["assets"] = (-maximum(pgrid),maximum(zgrid))
		grids = Dict{ASCIIString,Array{Float64,1}}()
		# x = grids["assets"] = scaleGrid(bounds["assets"][1],bounds["assets"][2],p.na,3,50.0,0.7)
		# x = grids["assets"] = scaleGrid(bounds["assets"][1],bounds["assets"][2],p.na,2,0.5)
		# center on zero
		x = [linspace(bounds["assets"][1],50.0,p.na-5),linspace(70.0,bounds["assets"][2],5)]
		# x = [linspace(bounds["assets"][1],60.0,p.na-6),linspace(80.0,bounds["assets"][2],6)]
		# x = linspace(bounds["assets"][1],bounds["assets"][2],p.na)
		x = x .- x[ indmin(abs(x)) ] 
		# println("assets = $x")
		grids["assets"]  = x
		grids["housing"] = linspace(0.0,1.0,p.nh)
		grids["W"]       = zeros(p.na)
		grids["tau"]     = linspace(0.0,p.tau,p.ntau)

		aone  = findfirst(grids["assets"].>=0)

		

		# regional transition matrices
		# ============================

		# the transition matrix for the price VAR is
		# defined on the tensor product pgrid[j, ] * ygrid[j, ] for all j
		# it's of dim nJ,ny,np,ny,np
		# each cell has the density of the event 
		# Pr(y(t+1) = y_i, p(t+1) = p_k | y(t),p(t) )
		# where the density if given by a joint normal with 
		#
		# mean: ( ymod(y(t),p(t),j), pmod(y(t),p(t),j) )
		# Cov : sigs[:,:,j]
		#
		# here ymod(y,p,j) and pmod(y,p,j) are the linear predictors
		# of the VAR models for y and p
		Gyp = get_yp_transition(VAR_coef,p,sigs,pgrid,ygrid)




		# moving cost function
		# ====================

		mc = zeros(p.nt-1,p.nJ,p.nJ,p.nh,p.ns)
		for it in 1:p.nt-1
			for ij in 1:p.nJ
				for ik in 1:p.nJ
					for ih in 0:1
						for is in 1:p.ns
							mc[it,ij,ik,ih+1,is] = (ij!=ik) * (p.MC0 + p.MC1*ih + p.MC2 * dist[ij,ik] + p.MC3 * it + (is-1)*p.MC4 )
						end
					end
				end
			end
		end


		gridsXD = (ASCIIString => Array{Float64})["Gyp" => Gyp, "Gz"=> Gz,"p" => pgrid, "y" => ygrid, "z" => zgrid, "zsupp" => zsupp, "movecost" => mc ,"Gs" => kmat]

		dimnames = DataFrame(dimension=["k", "s", "z", "y", "p", "a", "h", "tau", "j", "age" ],
			                  points = [p.nJ, p.ns, p.nz, p.ny, p.np, p.na, p.nh, p.ntau,  p.nJ, p.nt-1 ])


		# spline settings
		# ===============

		degs   = Dict{ASCIIString,Int}()
		nknots = Dict{ASCIIString,Int}()
		knots  = Dict{ASCIIString,Vector}()

		degs["assets"] = 3
		degs["y"] = 1
		degs["p"] = 1
		degs["z"] = 1

		nknots["assets"] = p.na - degs["assets"] + 1
		nknots["p"] = p.np - degs["p"] + 1
		nknots["y"] = p.ny - degs["y"] + 1
		nknots["z"] = p.nz - degs["z"] + 1

		# making sure all knot spans are active
		knots["assets"] = [linspace(grids["assets"][1],grids["assets"][end-1]-1,p.na - degs["assets"] ) , grids["assets"][end]]


		return new(v,vh,vfeas,sh,ch,cash,rho,dh,EV,vbar,EVfinal,aone,grids,gridsXD,dimvec,dimvecH,dimvec2,dimnames,regnames,dist,nknots,degs,knots)

	end



end


# functions for testing
function setrand!(m::Model)
	m.v = reshape(rand(length(m.v)),size(m.v))
	m.vh = reshape(rand(length(m.vh)),size(m.vh))
	m.vbar = reshape(rand(length(m.vbar)),size(m.vbar))
	m.rho  = reshape(rand(length(m.rho)),size(m.rho))
	m.EVfinal = reshape(rand(length(m.EVfinal)),size(m.EVfinal))
	return nothing
end

function setincreasing!(m::Model)
	m.v = reshape(1.0:length(m.v),size(m.v))
	m.vh = reshape(1.0:length(m.vh),size(m.vh))
	m.vbar = reshape(1.0:length(m.vbar),size(m.vbar))
	m.rho  = reshape(1.0:length(m.rho),size(m.rho))
	m.EVfinal = reshape(1.0:length(m.EVfinal),size(m.EVfinal))
	return nothing
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

function rouwenhorst(df::DataFrame,bounds::DataFrame,ygrid::Matrix,p::Param)

	P = zeros(p.nz,p.nz,p.nJ)
	z = zeros(p.nz,p.nJ)

	for j in 1:p.nJ
		xz,xp = rouwenh(df[j,:Lresid][1],df[j,:Intercept][1],df[j,:sigma][1],p.nz)
		P[:,:,j] = xp
		# scale z into bounds
		ybar = mean(log(ygrid[:,j]))
		zlow = log(bounds[j,:q20]) - ybar
		zhigh = log(bounds[j,:q95]) - ybar
		z[:,j] = linspace(zlow,zhigh,p.nz)
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
	Gyp = zeros(p.ny*p.np, p.ny*p.np, p.nJ)
	for j in 1:p.nJ
		for ip in 1:p.np
			for iy in 1:p.ny

				# setup MvNormal on that state
				C = PDMat(sigs[:,:,j])
				mvn = MvNormal([ygrid[iy,j],pgrid[ip,j]],C)
				ycoef = array(df[j,[:y_Intercept, :y_Lp, :y_Ly]]) 
				pcoef = array(df[j,[:p_Intercept, :p_Lp, :p_Ly]])

				for ip1 in 1:p.np
					for iy1 in 1:p.ny
						# get points to evaluate at
						xdata = vcat(1.0,pgrid[ip1,j],ygrid[iy1,j])
						new_y  = ycoef * xdata
						new_p  = pcoef * xdata
						Gyp[iy + p.ny*(ip-1),iy1 + p.ny*(ip1-1),j] = pdf(mvn,[new_y,new_p])
						# Gyp[iy,ip,iy1,ip1,j] = pdf(mvn,[new_y,new_p])
					end
				end
			end
		end
	end
	# normalize for rows to sum to 1
	Gyp = Gyp ./ sum(Gyp,2)
	return Gyp
end

# function(ff::HDF5File,path)
# 	fid = h5open(ff,"r")
# 	for obj in fid[path] 

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
	print(io, names(M))
end