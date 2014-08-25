


function drawMarkov(G::Array{Float64,2},init::Array{Float64,1},N::Int,T::Int)

	out = zeros(Int,N,T)
	eps = rand(N,T)

	n = size(G,1)

	if length(init) != n
		error("length(init) must be size(G,1)")
	end

	g = Categorical(init)

	Gs = mapslices(cumsum,G,2)

	z = zeros(n)

	# initiate everybody on a random state
	# drawn from init distribution
	out[:,1] = rand(g,N)

	for i = 1:N
		for t = 2:T

			z = Gs[ out[i,t-1], : ]

			out[i,t] = findfirst(z.>eps[i,t])

		end
	end
	out
end




function forceBounds(x::Float64,lb::Float64,ub::Float64)
	if x < lb
		x = lb
	elseif x > ub
		x = ub
	end
 	return x
end

# # get one draw for y and p given Ly and Lp
# function draw_yp(m::Model,Ly::Float64,Lp::Float64,j::Int)


# 	ycoef = array(m.VAR_coefs[j,[:y_Intercept, :y_Lp, :y_Ly]]) 
# 	pcoef = array(m.VAR_coefs[j,[:p_Intercept, :p_Lp, :p_Ly]])

# 	x = vcat(1.0,Lp,Ly)

# 	shock = rand(m.VAR_shock[j]) 	# [shock_y, shock_p]
# 	yy = ycoef * x + shock[1]
# 	pp = pcoef * x + shock[2]

# 	# force inside of bounds
# 	y = forceBounds(yy[1],m.VAR_coefs[j,:lb_y],m.VAR_coefs[j,:ub_y])
# 	p = forceBounds(pp[1],m.VAR_coefs[j,:lb_p],m.VAR_coefs[j,:ub_p])
# 	(y,p)
# end

function draw_z(m::Model,Lz::Float64,j::Int)

	zz = m.Inc_shocks[j,1] * Lz + rand(Normal(0,m.Inc_shocks[j,2]))	#TODO
	z = forceBounds(zz,m.Inc_shocks[j,3],m.Inc_shocks[j,4])
	return z
end

function getIncome(m::Model,y::Float64,z::Float64,age::Int,j::Int)

	inc = exp(m.Inc_ageprofile[age,j] + m.Inc_coefs[j,:logCensusMedinc] * log(y) + z )
end








# simulator

function simulate(m::Model,p::Param)

	T = p.nt-1

	# set random seed
	srand(p.rseed)

	# grids
	agrid = m.grids["assets"]
	agrid_rent = m.grids["assets"][m.aone:end]
	ygrid = m.gridsXD["y"]
	pgrid = m.gridsXD["p"]
	zgrid = m.gridsXD["z"]
	ypidx = Gyp_indices(p)  # array with cols iy,ip,idx

	regnames = m.regnames[:Division]

	aone = m.aone

	# drawing the shocks 
	# ==================

	# initial distributions
	# TODO
	# all of those should be non-uniform probably
	G0tau = Categorical(m.grids["Gtau"])	# type distribution
	G0z   = Categorical([1/p.nz for i=1:p.nz])
	G0yp  = Categorical([1/(p.ny*p.np) for i=1:(p.ny*p.np)])
	G0j   = Categorical(array(m.regnames[:prop]))	# TODO popdist
	G0k   = Categorical([0.6,0.4])  # 40% of 21-year olds have kids in SIPP

	# prepare cumsum of probability matrices
	cumGz  = cumsum(m.gridsXD["Gz"],2)
	cumGyp = cumsum(m.gridsXD["Gyp"],2)
	cumGs  = cumsum(m.gridsXD["Gs"],2)

	# storage
	Dt       = zeros(Int,p.nsim*(T))	 # age
	Di       = zeros(Int,p.nsim*(T))	 # identity
	Dv       = zeros(p.nsim*(T))	     # value
	Dc       = zeros(p.nsim*(T))	     # consu
	Dcash    = zeros(p.nsim*(T))	     # cash after rent
	Drent    = zeros(p.nsim*(T))	     # rent
	Da       = zeros(p.nsim*(T))	     # asset value
	Dy       = zeros(p.nsim*(T))	     # region y value
	Dincome  = zeros(p.nsim*(T))	     # income value
	Dp       = zeros(p.nsim*(T))	     # house price value
	Dj       = zeros(Int,p.nsim*(T))	 # location index
	Dregname = ASCIIString[]             # location name
	Dh       = zeros(Int,p.nsim*(T))	 # housing state
	Dhh      = zeros(Int,p.nsim*(T))	 # housing choice
	Diyp     = zeros(Int,p.nsim*(T))	 # region yp joint index
	Dip      = zeros(Int,p.nsim*(T))	 # region p index
	Diy      = zeros(Int,p.nsim*(T))	 # region y index
	DS       = zeros(p.nsim*(T))	     # savings values
	Dz       = zeros(p.nsim*(T))	     # z value
	DM       = zeros(Int,p.nsim*(T))	 # move
	DMt      = zeros(Int,p.nsim*(T))	 # move
	Dkids    = zeros(Int,p.nsim*(T))	 # kids yes/no
	Ddist    = zeros(p.nsim*(T))
	Dtau     = zeros(Int,p.nsim*(T))
	Dcanbuy  = zeros(Int,p.nsim*(T))

	# temporary objects

	ktmp = zeros(Float64,p.nJ)
	ktmp_test = zeros(Float64,p.nJ)
	ktmp2 = zeros(p.nJ)

	azmat_v = zeros(p.na,p.nz)
	azmat_s = zeros(p.na,p.nz)
	azmat_c = zeros(p.na,p.nz)

	na_rent = length(agrid_rent)

	azmat_v_rent = zeros(na_rent,p.nz)
	azmat_s_rent = zeros(length(agrid_rent),p.nz)
	azmat_c_rent = zeros(length(agrid_rent),p.nz)
	v1tmp = 0.0
	v2tmp = 0.0

	ss = 0.0
	yy = 0.0

	zsupps = Dict{Int,Array{Float64,1}}()
	for j in 1:p.nJ
		zsupps[j] = m.gridsXD["zsupp"][:,j]
	end

	# begin simulation loop
	# =====================

	# @inbounds begin
	for i = 1:p.nsim

		is   = rand(G0k)
		iyp  = rand(G0yp)
		iy   = ypidx[iyp,1]
		ip   = ypidx[iyp,2]
		# iz   = rand(G0z)
		# iz   = convert(Int,floor(median([1:p.nz])))	#everybody gets median income
		z   = rand(Normal(0,0.1))
		# ia   = rand(G0a) + m.aone - 1
		# a   =  rand()
		a   =  0.0
		ih   = 0
		itau = rand(G0tau)
		ij   = rand(G0j)


		# tmp vars
		move = false	# move indidicator
		moveto = 0
	
		for age = 1:T


			if z < minimum(zsupps[ij])
				println("[age,id]=[$age,$i], ij=$ij, z=$z, minz = $(minimum(zsupps[ij]))")
				error()
			end

			# point = [a,y,p,z]

			# record beginning of period state
			# --------------------------------

			# yy = zgrid[ iz,iy,age,ij ]
			yy = getIncome(m,ygrid[iy,ij],z,age,ij)

			# flag for downpayment constraint
			canbuy = a + yy > p.chi * m.gridsXD["p"][ip,ij]

			Dj[age + T*(i-1)]      = ij
			push!(Dregname,regnames[ij])
			Dt[age + T*(i-1)]      = age
			Dtau[age + T*(i-1)]    = itau
			Di[age + T*(i-1)]      = i
			Dh[age + T*(i-1)]      = ih
			Da[age + T*(i-1)]      = a
			Dy[age + T*(i-1)]      = ygrid[iy,ij]	
			Dp[age + T*(i-1)]      = pgrid[ip,ij]
			Dincome[age + T*(i-1)] = yy
			Dz[age + T*(i-1)]      = z
			Diyp[age + T*(i-1)]    = iyp
			Dip[age + T*(i-1)]     = ip
			Diy[age + T*(i-1)]     = iy
			Dkids[age + T*(i-1)]   = is-1
			Dcanbuy[age + T*(i-1)] = canbuy


			# get probability of moving to each location

			# slow
			# for ik in 1:p.nJ

			# 	# interpolate rho function in asset and z dim
			# 	for iia in 1:p.na
			# 		for iz in 1:p.nz
			# 			azmat_v[iia + p.na*(iz-1)] = m.rho[idx10(ik,is,iz,iy,ip,itau,iia,ih+1,ij,age,p)] 	# TODO can to faster indexing here
			# 		end
			# 	end
			# 	ktmp[ik] = bilinearapprox(a,z,agrid,zsupps[ik],azmat_v)
			# end

			# fast
			fill_ktmp!(ktmp,azmat_v,a,z,is,iy,ip,itau,ih+1,ij,age,p,agrid,zsupps,m.rho)

			# check
			# if (sumabs(ktmp .- ktmp2) > eps())
			# 	println("ktmp=$ktmp")
			# 	println("ktmp2=$ktmp2")
			# 	error()
			# end


			# check quality of approximation off grid
			# ---------------------------

			# if abs(sum(ktmp_test) - 1.0) > 0.001
			# 	println("--------------------------------------")
			# 	println("sum(ktmp_off_grid) = $(sum(ktmp_test))")
			# 	println("ktmp_off_grid/sum(ktmp_off_grid) = $(ktmp_test/sum(ktmp_test))")
			# 	println("sum(ktmp_test/sum(ktmp_test)) = $(sum(ktmp_test/sum(ktmp_test)))")
			# 	println("----------------------------------------------------------------")
			# end------------

			# normalizing vector of moving probs: because of approximation error
			# sometimes this is not *exactly* summing to 1
			ktmp = ktmp ./ sum(ktmp)
			
			# get cumulative prob
			cumsum!(ktmp2,ktmp,1)
			# throw a k-dice 
			moveto = searchsortedfirst(ktmp2,rand())
			move = ij != moveto

			if moveto>p.nJ || moveto < 1
				println(ktmp2)
				error("probelm in moveto = $moveto")
			end

			if move

				ihh = 0
				fill_azmats!(azmat_v,azmat_c,azmat_s,ihh+1,moveto,is,iy,ip,itau,ih+1,ij,age,p,m)

				vcs = bilinearapprox(a,z,agrid,zsupps[ij],azmat_v,azmat_c,azmat_s)

				val               = vcs[1]
				Dc[age + T*(i-1)] = vcs[2]
				ss                = vcs[3]

			else # stay

				# you are current owner or you can buy
				if (ih==1 || (ih==0 && canbuy))

					# find housing choice
					# use azmat_c here for v2
					# for iia in 1:p.na
					# 	for iz in 1:p.nz
					# 		azmat_v[iia + p.na*(iz-1)] = m.vh[idx11(1,moveto,is,iz,iy,ip,itau,iia,ih+1,ij,age,p)]
					# 		azmat_c[iia + p.na*(iz-1)] = m.vh[idx11(2,moveto,is,iz,iy,ip,itau,iia,ih+1,ij,age,p)]
					# 	end
					# end
					fill_azmats_h!(azmat_v,azmat_c,moveto,is,iy,ip,itau,ih+1,ij,age,p,m)
					v1v2 = bilinearapprox(a,z,agrid,zsupps[ij],azmat_v,azmat_c)

					ihh = v1v2[1] > v1v2[2] ? 0 : 1
					val = v1v2[1] > v1v2[2] ? v1v2[1] : v1v2[2]

					# find corresponding consumption and savings
					# for iia in 1:p.na
					# 	for iz in 1:p.nz
					# 		azmat_c[iia + p.na*(iz-1)] = m.ch[idx11(ihh+1,moveto,is,iz,iy,ip,itau,iia,ih+1,ij,age,p)]
					# 		azmat_s[iia + p.na*(iz-1)] = m.sh[idx11(ihh+1,moveto,is,iz,iy,ip,itau,iia,ih+1,ij,age,p)]
					# 	end
					# end
					fill_azmats!(azmat_c,azmat_s,ihh+1,moveto,is,iy,ip,itau,ih+1,ij,age,p,m.ch,m.sh)
					cs                = bilinearapprox(a,z,agrid,zsupps[ij],azmat_c,azmat_s)
					Dc[age + T*(i-1)] = cs[1]
					ss                = cs[2]
					

				# current renter who cannot buy
				else
					ihh = 0
					if a < 0
						println("error: id=$i,age=$age,ih=$ih,canbuy=$canbuy,a=$a")
					end
					# for iia in aone:p.na
					# 	for iz in 1:p.nz
					# 		idx = mig.idx11(ihh+1,moveto,is,iz,iy,ip,itau,iia,ih+1,ij,age,p)
					# 		azmat_v_rent[iia-aone+1 + na_rent*(iz-1)] = m.vh[idx]
					# 		azmat_c_rent[iia-aone+1 + na_rent*(iz-1)] = m.ch[idx]
					# 		azmat_s_rent[iia-aone+1 + na_rent*(iz-1)] = m.sh[idx]
					# 	end
					# end
					fill_azmats_rent!(azmat_v_rent,azmat_c_rent,azmat_s_rent,ihh+1,moveto,is,iy,ip,itau,ih+1,ij,age,p,m)
					vcs = bilinearapprox(a,z,agrid_rent,zsupps[ij],azmat_v_rent,azmat_c_rent,azmat_s_rent)
					val               = vcs[1]
					Dc[age + T*(i-1)] = vcs[2]
					ss                = vcs[3]

				end
			end

			# record current choices
			# ----------------------

			# make sure savings is inside grid
			ss = forceBounds(ss,agrid[1],agrid[end])

			DS[age + T*(i-1)]    = ss
			Dcash[age + T*(i-1)] = cashFunction(a,yy,ih,ihh,m.gridsXD["p"][ip,moveto],move,moveto,p)
			Drent[age + T*(i-1)] = pifun(ih,ihh,m.gridsXD["p"][ip,moveto],move,moveto,p)

			Dv[age + T*(i-1)]      = val
			Ddist[age + T*(i-1)]   = m.distance[ij,moveto]
			
			DM[age + T*(i-1)]  = move
			DMt[age + T*(i-1)] = moveto
			Dhh[age + T*(i-1)] = ihh	


			# transition to new state
			# -----------------------

			a  = ss
			ij = moveto
			ih = ihh

			# draw new values for z,y and p
			iyp = searchsortedfirst( cumGyp[iy + p.np * (ip-1),:,moveto][:], rand())
			is  = searchsortedfirst( cumGs[is,:,age][:], rand() )

			# if move
			if move
				itau = rand(G0tau)
				z   = draw_z(m,z,ij) 	# TODO draw from copula,not from here!
			else
				z   = draw_z(m,z,ij)
			end

			iy   = ypidx[iyp,1]
			ip   = ypidx[iyp,2]

		end	# age t

	end	# individual i

	# end # inbounds

	# collect all data into a dataframe
	w = (Dp .* Dh) .+ Da

	df = DataFrame(id=Di,age=Dt,age2=Dt.^2,kids=PooledDataArray(convert(Array{Bool,1},Dkids)),tau=Dtau,j=Dj,Division=Dregname,a=Da,save=DS,c=Dc,cash=Dcash,rent=Drent,z=Dz,ip=Dip,iy=Diy,p=Dp,y=Dy,income=Dincome,move=DM,moveto=DMt,h=Dh,hh=Dhh,v=Dv,wealth=w,km_distance=Ddist,km_distance2=Ddist.^2,own=PooledDataArray(convert(Array{Bool,1},Dh)),canbuy=Dcanbuy)
	# df = join(df,m.regnames,on=:j)
	# sort!(df,cols=[1,2]	)

	return df
end



 # r = ik + p.nJ * (is-1 + p.ns * (iz-1 + p.nz * (iy-1 + p.ny * (ip-1 + p.np * (itau-1 + p.ntau * (ia-1 + p.na * (ih-1 + p.nh * (ij-1 + p.nJ * (age-1)))))))))


function fill_ktmp!{T<:Real}(kvec::Vector{T},azmat_v::Matrix{T},a::Float64,z::Float64,is::Int,iy::Int,ip::Int,itau::Int,ih::Int,ij::Int,age::Int,p::Param,agrid::Vector{T},zsupps::Dict,val::Array)

	# get parts of index that do not change
	offset_h_age_j = ih-1 + p.nh * (ij-1 + p.nJ * (age-1))

	for ik in 1:p.nJ
		for iia in 1:p.na
			offset_a = iia-1 + p.na*offset_h_age_j
			for iz in 1:p.nz
				offset_z = iz-1 + p.nz * (iy-1 + p.ny * (ip-1 + p.np * (itau-1 + p.ntau * offset_a)))

				idx = ik + p.nJ * (is-1 + p.ns * offset_z)
				# println("idx = $idx")
				# println("idx10 = $(idx10(ik,is,iz,iy,ip,itau,iia,ih,ij,age,p))")
				@inbounds azmat_v[iia + p.na*(iz-1)] = val[idx] 	
			end
		end
		kvec[ik] = bilinearapprox(a,z,agrid,zsupps[ij],azmat_v)
		# println("kvec[$ik] = $(kvec[ik])")
	end
end

function fill_azmats!{T<:Real}(azmatv::Matrix{T},azmatc::Matrix{T},azmats::Matrix{T},ihh::Int,ik::Int,is::Int,iy::Int,ip::Int,itau::Int,ih::Int,ij::Int,age::Int,p::Param,m::Model)

	# get parts of index that do not change
	offset_h_age_j = ih-1 + p.nh * (ij-1 + p.nJ * (age-1))

	for iia in 1:p.na
		offset_a = iia-1 + p.na*offset_h_age_j
		for iz in 1:p.nz
			offset_z = iz-1 + p.nz * (iy-1 + p.ny * (ip-1 + p.np * (itau-1 + p.ntau * offset_a)))

			idx = ihh + p.nh * (ik-1 + p.nJ * (is-1 + p.ns * offset_z))

			@inbounds azmatv[iia + p.na*(iz-1)] = m.vh[idx]
			@inbounds azmatc[iia + p.na*(iz-1)] = m.ch[idx]
			@inbounds azmats[iia + p.na*(iz-1)] = m.sh[idx]
			# println("idx = $idx")
			# println("idx10 = $(idx10(ik,is,iz,iy,ip,itau,iia,ih,ij,age,p))")
		end
	end
	return nothing
end
	


function fill_azmats!{T<:Real}(azmatv::Matrix{T},azmatc::Matrix{T},ihh::Int,ik::Int,is::Int,iy::Int,ip::Int,itau::Int,ih::Int,ij::Int,age::Int,p::Param,v1::Array,v2::Array)

	# get parts of index that do not change
	offset_h_age_j = ih-1 + p.nh * (ij-1 + p.nJ * (age-1))

	# @inbounds begin
	for iia in 1:p.na
		offset_a = iia-1 + p.na*offset_h_age_j
		for iz in 1:p.nz
			offset_z = iz-1 + p.nz * (iy-1 + p.ny * (ip-1 + p.np * (itau-1 + p.ntau * offset_a)))

			idx = ihh + p.nh * (ik-1 + p.nJ * (is-1 + p.ns * offset_z))

			@inbounds azmatv[iia + p.na*(iz-1)] = v1[idx]
			@inbounds azmatc[iia + p.na*(iz-1)] = v2[idx]
			# println("idx = $idx")
			# println("idx10 = $(idx10(ik,is,iz,iy,ip,itau,iia,ih,ij,age,p))")
		end
	end
	# end
	return nothing
end

# method for 2 value funcitons only: housing choice
function fill_azmats_h!{T<:Real}(azmatv1::Matrix{T},azmatv2::Matrix{T},ik::Int,is::Int,iy::Int,ip::Int,itau::Int,ih::Int,ij::Int,age::Int,p::Param,m::Model)

	# get parts of index that do not change
	offset_h_age_j = ih-1 + p.nh * (ij-1 + p.nJ * (age-1))

	for iia in 1:p.na
		offset_a = iia-1 + p.na*offset_h_age_j
		for iz in 1:p.nz
			offset_z = iz-1 + p.nz * (iy-1 + p.ny * (ip-1 + p.np * (itau-1 + p.ntau * offset_a)))

			idx1 = 1 + p.nh * (ik-1 + p.nJ * (is-1 + p.ns * offset_z))
			idx2 = 2 + p.nh * (ik-1 + p.nJ * (is-1 + p.ns * offset_z))

			@inbounds azmatv1[iia + p.na*(iz-1)] = m.vh[idx1]
			@inbounds azmatv2[iia + p.na*(iz-1)] = m.vh[idx2]
			# println("idx = $idx")
			# println("idx10 = $(idx10(ik,is,iz,iy,ip,itau,iia,ih,ij,age,p))")
		end
	end
	return nothing
end
	
function fill_azmats_rent!{T<:Real}(azmatv::Matrix{T},azmatc::Matrix{T},azmats::Matrix{T},ihh::Int,ik::Int,is::Int,iy::Int,ip::Int,itau::Int,ih::Int,ij::Int,age::Int,p::Param,m::Model)

	na_rent = size(azmatv,1)
	aone = m.aone

	# get parts of index that do not change
	offset_h_age_j = ih-1 + p.nh * (ij-1 + p.nJ * (age-1))

	for iia in m.aone:p.na
		offset_a = iia-1 + p.na*offset_h_age_j
		for iz in 1:p.nz
			offset_z = iz-1 + p.nz * (iy-1 + p.ny * (ip-1 + p.np * (itau-1 + p.ntau * offset_a)))

			idx = ihh + p.nh * (ik-1 + p.nJ * (is-1 + p.ns * offset_z))

			@inbounds azmatv[iia-aone+1 + na_rent*(iz-1)] = m.vh[idx]
			@inbounds azmatc[iia-aone+1 + na_rent*(iz-1)] = m.ch[idx]
			@inbounds azmats[iia-aone+1 + na_rent*(iz-1)] = m.sh[idx]
			# println("idx = $idx")
			# println("idx10 = $(idx10(ik,is,iz,iy,ip,itau,iia,ih,ij,age,p))")
		end
	end
	return nothing
end







# computing moments from simulation
function computeMoments(df::DataFrame,p::Param,m::Model)


	# partition df0 in groups by id
	# insert!(df0,3,floor(integer(df0[:id]/1000)),:idgroup)

	# dfs = DataFrame[]

	# df_g = groupby(df0, :idgroup)

	# for df in df_g

		# create a dataframe to push moments on to
		mom1 = DataFrame(moment="", model_value = 0.0, model_sd = 0.0)

		# grouped dfs
		g_div = groupby(df, :Division)
		g_kids = groupby(df, :kids)
		g_own = groupby(df, :h)

		# linear probability model of homeownership
		# =========================================

		if mean(df[:h]) == 1.0 || sum(df[:h]) == 0.0
			nm_h  = ["lm_h_(Intercept)","lm_h_age","lm_h_age2"]  
			coef_h = DataArray(Float64,3)
			std_h =  DataArray(Float64,3)
		else
			lm_h = fit(LinearModel, h ~ age + age2 ,df)
			cc_h  = coeftable(lm_h)
			nm_h  = ASCIIString["lm_h_" *  convert(ASCIIString,cc_h.rownms[i]) for i=1:size(cc_h.mat,1)] 
			coef_h = @data(coef(lm_h))
			std_h = @data(stderr(lm_h))
		end


		# own ~ Division
		for div in g_div
			push!(mom1,["mean_own_$(div[1,:Division])",mean(div[:h]),std(div[:h])])
		end

		# own ~ kids
		for div in g_kids
			kk = "$(div[1,:kids])"
			push!(mom1,["mean_own_kids$(uppercase(kk))",mean(div[:h]),std(div[:h])])
		end
		# TODO std error
		push!(mom1,["cov_own_kids",cov(df[:h],df[:kids]),1.0])


		# linear probability model of mobility
		# ====================================

		if sum(df[:move]) == 0.0
			nm_mv  = ["lm_mv_(Intercept)","lm_mv_age","lm_mv_age2"]  
			coef_mv = @data(zeros(3))
			std_mv =  @data(ones(3))
		else
			lm_mv = fit(LinearModel, move ~ age + age2 ,df)
			cc_mv = coeftable(lm_mv)
			nm_mv = ASCIIString["lm_mv_" * convert(ASCIIString,cc_mv.rownms[i]) for i=1:size(cc_mv.mat,1)] 
			coef_mv = @data(coef(lm_mv))
			std_mv = @data(stderr(lm_mv))
		end

		# move count
		# ----------

		movecount=by(df,:id,x -> sum(x[:move]))
		moved0 = mean(movecount[:x1].==0)
		moved1 = mean(movecount[:x1].==1)
		moved2 = mean(movecount[:x1].==2)

		# TODO std error
		push!(mom1,["mean_move",mean(df[:move]),std(df[:move])])	# unconditional mean
		push!(mom1,["moved0",moved0,1.0])
		push!(mom1,["moved1",moved1,1.0])
		push!(mom1,["moved2",moved2,1.0])


		# move ~ own
		# ----------

		if sum(df[:own]) == 0
			push!(mom1,["mean_move_ownTRUE",0.0,1.0])
			push!(mom1,["mean_move_ownFALSE",1.0,1.0])
		elseif mean(df[:own]) == 1.0
			push!(mom1,["mean_move_ownTRUE",1.0,1.0])
			push!(mom1,["mean_move_ownFALSE",0.0,1.0])
		else
			for idf in g_own
				kk = "$(idf[1,:own])"
				push!(mom1,["mean_move_own$(uppercase(kk))",mean(idf[:move]),std(idf[:move])])
			end

		end
		# TODO std error
		push!(mom1,["cov_move_h",cov(df[:h],df[:move]),1.0])


		# move ~ kids
		# ----------

		for idf in g_kids
			kk = "$(idf[1,:kids])"
			push!(mom1,["mean_move_kids$(uppercase(kk))",mean(idf[:move]),std(idf[:move])])
		end
		# TODO std error
		push!(mom1,["cov_move_kids",cov(df[:move],df[:kids]),1.0])


		# linear regression of total wealth
		# =================================

		if sum(df[:own]) == 1.0 || sum(df[:own]) == 0.0
			nm_w  = ["lm_w_(Intercept)","lm_w_age","lm_w_age2","lm_w_owntrue"]  
			coef_w = DataArray(Float64,4)
			std_w =  DataArray(Float64,4)
		else
			lm_w = fit(LinearModel, wealth ~ age + age2 + own,df )
			cc_w  = coeftable(lm_w)
			nm_w  = ASCIIString["lm_w_" *  convert(ASCIIString,cc_w.rownms[i]) for i=1:size(cc_w.mat,1)] 
			coef_w = @data(coef(lm_w))
			std_w = @data(stderr(lm_w))
		end

		# wealth ~ division
		# -----------------

		for idf in g_div
			push!(mom1,["mean_wealth_$(idf[1,:Division])",mean(idf[:wealth]),std(idf[:wealth])])
		end



		# collect estimates
		# =================


		nms = vcat(nm_mv,nm_h,nm_w)

		# get rid of parens and hyphens
		# TODO get R to export consitent names with julia output - i'm doing this side here often, not the other one
		for i in 1:length(nms)
			ss = replace(nms[i]," - ","")
			ss = replace(ss,")","")
			ss = replace(ss,"(","")
			# ss = replace(ss,"kidstrue","kidsTRUE")
			ss = replace(ss,"owntrue","ownTRUE")
			nms[i] = ss
		end

		mom2 = DataFrame(moment = nms, model_value = [coef_mv,coef_h,coef_w], model_sd = [std_mv,std_h,std_w])

		dfout = rbind(mom1,mom2)

	# 	push!(dfs,rbind(mom1,mom2))

	# end

	# # compute mean
	# dfout = dfs[1]
	# for irow in 1:nrow(dfout)
	# 	x = 0.0
	# 	sdx = 0.0
	# 	for i in 1:length(dfs)
	# 		x += dfs[i][irow,:model_value]
	# 		sdx += dfs[i][irow,:model_sd]
	# 	end
	# 	dfout[irow,:model_value] = x / length(dfs)
	# 	dfout[irow,:model_sd] = sdx / length(dfs)
	# end


	return dfout
end







# setting up the FSpace objects for simulation
if Sys.OS_NAME == :Darwin 
	using BSplines
end
function setupFSpaceXD(m::Model,p::Param)

	ndims = 4 # number of cont dimensions

	# ordering of continuous dims
	# 1) a
	# 2) y
	# 3) p
	# 4) z

	# the return object: a dict of collections of fspaces.
	fx = Dict{ASCIIString,Dict{Integer,FSpaceXD}}()
	fx["rho"] = Dict{Integer,FSpaceXD}()
	fx["vh"]  = Dict{Integer,FSpaceXD}()
	fx["ch"]  = Dict{Integer,FSpaceXD}()
	fx["sh"]  = Dict{Integer,FSpaceXD}()

	points = Dict{Int,Dict{Integer,Array}}()
	bounds = Dict{Int,Dict{Integer,Array}}()
	bsp = Dict{Int,Dict{Integer,BSpline}}()

	# full basis to compute inverses
	d = Dict{Int,Dict{Integer,Matrix}}()
	id = Dict{Int,Dict{Integer,Array{Float64,2}}}()
	# bsp[5] = BSpline(m.nknots["age"],m.degs["age"],bounds[5][1],bounds[5][2])

	for ij   = 1:p.nJ	

		points[ij] = Dict{Integer,Array}()
		bounds[ij] = Dict{Integer,Array}()
		bsp[ij] = Dict{Integer,BSpline}()

		# full basis to compute inverses
		d[ij] = Dict{Integer,Matrix}()
		id[ij] = Dict{Integer,Array{Float64,2}}()

		points[ij][1] = m.grids["assets"]
		bounds[ij][1] = [m.grids["assets"][1],m.grids["assets"][end]]
		# points[5] = linspace(1.0,p.nt-1,p.nt-1)
		# bounds[5] = [1.0,convert(Float64,p.nt-1)]

		# construct asset basis with custom knots
		# bsp[1] = BSpline(m.knots["assets"],m.degs["assets"])
		bsp[ij][1] = BSpline(m.nknots["assets"],m.degs["assets"],bounds[ij][1][1],bounds[ij][1][2])
		points[ij][2] = m.gridsXD["y"][:,ij]
		points[ij][3] = m.gridsXD["p"][:,ij]
		points[ij][4] = m.gridsXD["zsupp"][:,ij]

		bounds[ij][2] = [ points[ij][2][1],points[ij][2][end] ]
		bounds[ij][3] = [ points[ij][3][1],points[ij][3][end] ]
		bounds[ij][4] = [ points[ij][4][1],points[ij][4][end] ]

		bsp[ij][2] = BSpline(m.nknots["y"],m.degs["y"],bounds[ij][2][1],bounds[ij][2][2])
		bsp[ij][3] = BSpline(m.nknots["p"],m.degs["p"],bounds[ij][3][1],bounds[ij][3][2])
		bsp[ij][4] = BSpline(m.nknots["z"],m.degs["z"],bounds[ij][4][1],bounds[ij][4][2])


		# get full basis for inverses
		for i=1:ndims
			d[ij][i] = full(getBasis(points[ij][i],bsp[ij][i]))
		end
		for k in collect(keys(d[ij]))
			id[ij][k] = inv(d[ij][k])
		end


		for itau = 1:p.ntau		
		for ih   = 1:2
		for is   = 1:p.ns
		for ik   = 1:p.nJ
		for age  = 1:p.nt-1

			# get FSpace for rho
			# ------------------

			rhotmp = get_cont_vals(ik,is,ih,itau,ij,age,m.rho,p)
			mycoef1 = getTensorCoef(id[ij],rhotmp)
			rhoidx = fx_idx_rho(ik,is,ih,itau,ij,age,p)
			# info("at index $rhoidx")
			fx["rho"][rhoidx] = FSpaceXD(ndims,mycoef1,bsp[ij])


			for ihh  = 1:2


				# get FSpace for vh, ch and sh
				# ----------------------------

				# vh
				vtmp = get_cont_vals(ihh,ik,is,ih,itau,ij,age,m.vh,p)
				mycoef = getTensorCoef(id[ij],vtmp)

				idx = fx_idx(ihh,ik,is,ih,itau,ij,age,p)
				# info("doing state $idx")
				fx["vh"][idx] = FSpaceXD(ndims,mycoef,bsp[ij])

				# ch
				vtmp = get_cont_vals(ihh,ik,is,ih,itau,ij,age,m.ch,p)
				mycoef = getTensorCoef(id[ij],vtmp)

				idx = fx_idx(ihh,ik,is,ih,itau,ij,age,p)
				fx["ch"][idx] = FSpaceXD(ndims,mycoef,bsp[ij])

				# sh                 ihh,ik,is,ih,itau,ij,it,m.vh,p
				vtmp = get_cont_vals(ihh,ik,is,ih,itau,ij,age,m.sh,p)
				mycoef = getTensorCoef(id[ij],vtmp)

				idx = fx_idx(ihh,ik,is,ih,itau,ij,age,p)
				fx["sh"][idx] = FSpaceXD(ndims,mycoef,bsp[ij])

			end

		end
		end
		end
		end
		end

	end

	return fx

end


# FSpace Indexer functions
function fx_idx_rho(ik::Int,is::Int,ih::Int,itau::Int,ij::Int,age::Int,p::Param)
	ik + p.nJ * (is + p.ns * (ih + p.nh * (itau + p.ntau * (ij + p.nJ*(age-1)-1)-1)-1)-1)
end

function fx_idx(ihh::Int,ik::Int,is::Int,ih::Int,itau::Int,ij::Int,age::Int,p::Param)
	 
	ihh + p.nh * (ik + p.nJ * (is + p.ns * (ih + p.nh * (itau + p.ntau * (ij + p.nJ*(age-1)-1)-1)-1)-1)-1)
end

function fx_idx_cont(ia::Int,iy::Int,ip::Int,iz::Int,p::Param)
	ia + p.na * (iy + p.ny * (ip + p.np * (iz-1) -1) -1)
end



# get values in continuous dims at given discrete state
# method for idx11
function get_cont_vals(ihh::Int,ik::Int,is::Int,ih::Int,itau::Int,ij::Int,age::Int,v::Array,p::Param)
# function get_cont_vals(ihh::Int,ik::Int,is::Int,ih::Int,itau::Int,ij::Int,v::Array,p::Param)

	# vout = zeros(p.na*p.np*p.ny*p.nz*(p.nt-1))
	vout = zeros(p.na*p.np*p.ny*p.nz)

	for ia = 1:p.na
	for iy = 1:p.ny
	for ip = 1:p.np
	for iz = 1:p.nz
	# for it = 1:p.nt-1

		@inbounds vout[fx_idx_cont(ia,iy,ip,iz,p)] = v[idx11(ihh,ik,is,iz,iy,ip,ia,ih,itau,ij,age,p)]

	# end
	end
	end
	end
	end

	return vout
end


# method for idx10
function get_cont_vals(ik::Int,is::Int,ih::Int,itau::Int,ij::Int,age::Int,v::Array,p::Param)
# function get_cont_vals(ik::Int,is::Int,ih::Int,itau::Int,ij::Int,v::Array,p::Param)

	vout = zeros(p.na*p.np*p.ny*p.nz)

	for ia = 1:p.na
	for iy = 1:p.ny
	for ip = 1:p.np
	for iz = 1:p.nz
	# for it = 1:p.nt-1

		# vout[fx_idx_cont(ia,ip,iy,iz,it,p)] = v[idx10(ik,is,iz,iy,ip,ia,ih,itau,ij,it,p)]
		@inbounds vout[fx_idx_cont(ia,iy,ip,iz,p)] = v[idx10(ik,is,iz,iy,ip,ia,ih,itau,ij,age,p)]

	# end
	end
	end
	end
	end

	return vout
end

















	