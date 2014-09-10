


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


function getRegional(m::Model,Y::Float64,P::Float64,j::Int)

	m.Regmods[j] * vcat(Y,P)

end






# simulator

function simulate(m::Model,p::Param)
	srand(p.rseed)
	simulate(m,p,p.nsim)
end

function simulate_parts(m::Model,p::Param,parts::Int)

	srand(p.rseed)
	# loop over simulations
	# and combine moments in averages
	nsim = int(floor(p.nsim / parts))

	# collecting moments
	dfs = DataFrame[]

	for is in 1:parts

		s = simulate(m,p,nsim)
		push!(dfs,computeMoments(s,p,m))

	end

	# compute average over moments
	dfout = dfs[1]
	for irow in 1:nrow(dfout)
		x = 0.0
		sdx = 0.0
		for i in 1:parts
			x += dfs[i][irow,:model_value]
			sdx += dfs[i][irow,:model_sd]
		end
		dfout[irow,:model_value] = x / parts
		dfout[irow,:model_sd] = sdx / parts
	end
	return dfout
end

function simulate(m::Model,p::Param,nsim::Int)

	T = p.nt-1

	# set random seed

	# grids
	agrid = m.grids["assets"]
	agrid_rent = m.grids["assets"][m.aone:end]

	regnames = m.regnames[:Division]

	aone = m.aone

	# drawing the shocks 
	# ==================

	# initial distributions
	# TODO
	# all of those should be non-uniform probably
	Gtau  = Categorical(m.grids["Gtau"])	# type distribution
	G0z   = Categorical([1/p.nz for i=1:p.nz])
	G0yp  = Categorical([1/(p.ny*p.np) for i=1:(p.ny*p.np)])
	G0j   = Categorical(array(m.regnames[:prop]))	# TODO popdist
	G0k   = Categorical([0.6,0.4])  # 40% of 21-year olds have kids in SIPP

	# prepare cumsum of probability matrices
	cumGz  = cumsum(m.gridsXD["Gz"],2)
	cumGs  = cumsum(m.gridsXD["Gs"],2)

	# storage
	Dt       = zeros(Int,nsim*(T))	 # age
	Di       = zeros(Int,nsim*(T))	 # identity
	Dv       = zeros(nsim*(T))	     # value
	Dprob    = zeros(nsim*(T))	     # value
	Dc       = zeros(nsim*(T))	     # consu
	Dcash    = zeros(nsim*(T))	     # cash after rent
	Drent    = zeros(nsim*(T))	     # rent
	Da       = zeros(nsim*(T))	     # asset value
	Dy       = zeros(nsim*(T))	     # region y value
	Dp       = zeros(nsim*(T))	     # house price value
	DY       = zeros(nsim*(T))	     # aggregate
	DP       = zeros(nsim*(T))	     # aggregate
	Dincome  = zeros(nsim*(T))	     # income value
	Dj       = zeros(Int,nsim*(T))	 # location index
	Dregname = ASCIIString[]             # location name
	Dh       = zeros(Int,nsim*(T))	 # housing state
	Dhh      = zeros(Int,nsim*(T))	 # housing choice
	DS       = zeros(nsim*(T))	     # savings values
	Dz       = zeros(nsim*(T))	     # z value
	DM       = zeros(Int,nsim*(T))	 # move
	DMt      = zeros(Int,nsim*(T))	 # move
	Dkids    = zeros(Int,nsim*(T))	 # kids yes/no
	Ddist    = zeros(nsim*(T))
	Dtau     = zeros(Int,nsim*(T))
	Dcanbuy  = zeros(Int,nsim*(T))
	Dcohort  = zeros(Int,nsim*(T))

	# temporary objects

	ktmp = zeros(Float64,p.nJ)
	ktmp_test = zeros(Float64,p.nJ)
	ktmp2 = zeros(p.nJ)

	na_rent = length(agrid_rent)

	v1tmp = 0.0
	v2tmp = 0.0

	ss = 0.0
	yy = 0.0

	# z shock supports for each region
	zsupps = Dict{Int,Array{Float64,1}}()
	for j in 1:p.nJ
		zsupps[j] = m.gridsXD["zsupp"][:,j]
	end

	# linear interpolator objects
	# ===========================

	# grids
	# -----
	gs = Array{Float64,1}[]
	push!(gs,agrid)
	push!(gs,zsupps[1])
	push!(gs,ygrid)
	push!(gs,pgrid)

	gs_rent = Array{Float64,1}[]
	push!(gs_rent,agrid_rent)
	push!(gs_rent,zsupps[1])
	push!(gs_rent,ygrid)
	push!(gs_rent,pgrid)

	# value arrays 
	# ------------
	rho_arr = Array{Float64}[]
	for i in 1:p.nJ
		push!(rho_arr,zeros(p.na,p.nz,p.ny,p.np))
	end
	vcs_arr = Array{Float64}[]
	push!(vcs_arr,zeros(p.na,p.nz,p.ny,p.np),zeros(p.na,p.nz,p.ny,p.np),zeros(p.na,p.nz,p.ny,p.np))
	vcs_rent_arr = Array{Float64}[]
	push!(vcs_rent_arr,zeros(na_rent,p.nz,p.ny,p.np),zeros(na_rent,p.nz,p.ny,p.np),zeros(na_rent,p.nz,p.ny,p.np))
	vh_arr = Array{Float64}[]
	push!(vh_arr,zeros(p.na,p.nz,p.ny,p.np),zeros(p.na,p.nz,p.ny,p.np))
	cs_arr = Array{Float64}[]
	push!(cs_arr,zeros(p.na,p.nz,p.ny,p.np),zeros(p.na,p.nz,p.ny,p.np))

	# construct the interpolators
	# ---------------------------
	l_vcs      = lininterp(vsc_arr,gs)
	l_vcs_rent = lininterp(vsc_rent_arr,gs_rent)
	l_vh       = lininterp(vh_arr,gs)
	l_cs       = lininterp(cs_arr,gs)
	l_rho      = lininterp(rho_arr,gs)


	# begin simulation loop
	# =====================

	for i=1:nsim
		# find cohort index of i
		# assign i to a certain cohort.
		cohort   = yearidx = assignCohort(i,m)
		T_cohort = length(m.coh_idx[cohort])
		is       = rand(G0k)
		z        = rand(Normal(0,0.1))
		a        = forceBounds(rand(m.Init_asset),0.0,100.0)
		ih       = 0
		itau     = rand(Gtau)
		ij       = rand(G0j)

		# tmp vars
		move = false	# move indidicator
		moveto = 0

		# aggregate state in year 1
		P = PYdata[yearidx]
		Y = PYdata[yearidx]

		for age=1:T_cohort	# later cohorts can be cut off earlier

			# current continuous state is
			azYP = [a,z,Y,P]


			if z < minimum(zsupps[ij])
				println("[age,id]=[$age,$i], ij=$ij, z=$z, minz = $(minimum(zsupps[ij]))")
				error()
			end

			# point = [a,Y,P,z]

			# record beginning of period state
			# --------------------------------

			yp = getRegional(m,Y,P,ij) # yp[1] = y, yp[2] = p

			yy = getIncome(m,yp[1],z,age,ij)

			# flag for downpayment constraint
			canbuy = a + yy > p.chi * yp[2]

			Dj[age + T*(i-1)]      = ij
			Dt[age + T*(i-1)]      = age
			Dtau[age + T*(i-1)]    = itau
			Di[age + T*(i-1)]      = i
			Dh[age + T*(i-1)]      = ih
			Da[age + T*(i-1)]      = a
			DY[age + T*(i-1)]      = Y
			DP[age + T*(i-1)]      = P
			Dy[age + T*(i-1)]      = yp[1]
			Dp[age + T*(i-1)]      = yp[2]
			Dincome[age + T*(i-1)] = yy
			Dz[age + T*(i-1)]      = z
			Dkids[age + T*(i-1)]   = is-1
			Dcanbuy[age + T*(i-1)] = canbuy
			Dcohort[age + T*(i-1)] = cohort
			Dyear[age + T*(i-1)]   = yearidx
			push!(Dregname,regnames[ij])


			# change linear interpolators z grid to current region ij grid
			setGrid(l_rho,2,zsupps[ij])
			setGrid(l_vh,2,zsupps[ij])
			setGrid(l_vcs,2,zsupps[ij])
			setGrid(l_vcs_rent,2,zsupps[ij])
	
			# 4D interpolation: (a,z,Y,P)
			ktmp = get_rho_ktmp(l_rho,azYP,is,itau,ih+1,ij,age,m)
			# normalizing vector of moving probs: because of approximation error
			# sometimes this is not *exactly* summing to 1
			ktmp = ktmp ./ sum(ktmp)
			
			# get cumulative prob
			cumsum!(ktmp2,ktmp,1)
			# throw a k-dice 
			shock = rand()
			moveto = searchsortedfirst(ktmp2,shock)
			move = ij != moveto

			Dprob[age + T*(i-1)]      = ktmp[moveto]

			if moveto>p.nJ || moveto < 1
				println(ktmp2)
				error("probelm in moveto = $moveto")
			end

			if move
				ihh = 0
				vcs = get_vcs(l_vcs,azYP,ihh+1,moveto,is,itau,ih+1,ij,age,p,m)
				val               = vcs[1]
				Dc[age + T*(i-1)] = vcs[2]
				ss                = vcs[3]

			else # stay

				# you are current owner or you can buy
				if (ih==1 || (ih==0 && canbuy))

					# get housing choice
					v1v2 = get_v1v2(l_vh,azYP,moveto,is,iy,ip,itau,ih+1,ij,age,p,m)

					ihh = v1v2[1] > v1v2[2] ? 0 : 1
					val = v1v2[1] > v1v2[2] ? v1v2[1] : v1v2[2]

					# find corresponding consumption and savings
					cs = get_cs(l_cs,azYP,ihh+1,moveto,is,itau,ih+1,ij,age,p,m)
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
					vcs = get_vcs_rent(l_vcs_rent,azYP,ihh+1,moveto,is,itau,ih+1,ij,age,p,m)
					val               = vcs[1]
					Dc[age + T*(i-1)] = vcs[2]
					ss                = vcs[3]

				end
			end

			# record current choices
			# ======================

			# make sure savings is inside grid
			ss = forceBounds(ss,agrid[1],agrid[end])

			DS[age + T*(i-1)]    = ss
			Dcash[age + T*(i-1)] = cashFunction(a,yy,ih,ihh,yp[2],move,moveto,p)
			Drent[age + T*(i-1)] = pifun(ih,ihh,yp[2],move,moveto,p)

			Dv[age + T*(i-1)]      = val
			Ddist[age + T*(i-1)]   = m.distance[ij,moveto]
			
			DM[age + T*(i-1)]  = move
			DMt[age + T*(i-1)] = moveto
			Dhh[age + T*(i-1)] = ihh	


			# transition to new state
			# =======================

			a  = ss
			ij = moveto
			ih = ihh
			yearidx+=1
			P = PYdata[yearidx]
			Y = PYdata[yearidx]

			# draw new values child shock
			is  = searchsortedfirst( cumGs[is,:,age][:], rand() )

			# if move
			if move
				z   = draw_z(m,z,ij) 	# TODO draw from copula,not from here!
			else
				z   = draw_z(m,z,ij)
			end

			# iy   = ypidx[iyp,1]
			# ip   = ypidx[iyp,2]
		end 	# age

	end 	# ind

	# collect all data into a dataframe
	w = (Dp .* Dh) .+ Da

	df = DataFrame(id=Di,age=Dt,age2=Dt.^2,kids=PooledDataArray(convert(Array{Bool,1},Dkids)),tau=Dtau,j=Dj,Division=Dregname,a=Da,save=DS,c=Dc,cash=Dcash,rent=Drent,z=Dz,p=Dp,y=Dy,P=DP,Y=DY,income=Dincome,move=DM,moveto=DMt,h=Dh,hh=Dhh,v=Dv,prob=Dprob,wealth=w,km_distance=Ddist,own=PooledDataArray(convert(Array{Bool,1},Dh)),canbuy=Dcanbuy,cohort=Dcohort)
	# df = join(df,m.regnames,on=:j)
	# sort!(df,cols=[1,2]	)

	return df
end



 # r = ik + p.nJ * (is-1 + p.ns * (iz-1 + p.nz * (iy-1 + p.ny * (ip-1 + p.np * (itau-1 + p.ntau * (ia-1 + p.na * (ih-1 + p.nh * (ij-1 + p.nJ * (age-1)))))))))


function get_rho_ktmp{T<:Real}(l::lininterp,azYP::Array{T},is::Int,itau::Int,ih::Int,ij::Int,age::Int,p::Param,m::Model)

	# fill array azYP (na,nz,nY,nP) at states (is,itau,ih,ij,age)

	# get parts of index that do not change
	offset_h_age_j = ih-1 + p.nh * (ij-1 + p.nJ * (age-1))

	for ik in 1:p.nJ
		for iia in 1:p.na
			offset_a = iia-1 + p.na*offset_h_age_j
			for iP in 1:p.np
				offset_P = iP-1 + p.np * (itau-1 + p.ntau * offset_a)
				for iY in 1:p.ny 
					offset_Y = iY-1 + p.ny * offset_P
					for iz in 1:p.nz
						offset_z = iz-1 + p.nz * offset_Y

						idx = ik + p.nJ * (is-1 + p.ns * offset_z)
						# println("idx = $idx")
						# println("idx10 = $(idx10(ik,is,iz,iy,ip,itau,iia,ih,ij,age,p))")
						# directly access the lininterp internal value array!
						@inbounds l.vals[ik][iia + p.na*(iz-1 + p.nz *(iY-1 + p.ny *(iP-1)))] = m.rho[idx] 	
					end
				end
			end
		end
	end
	l.hitnow = false  # force refilling of vertices in eval4D, even if we are in same cache bracket
	# the cache should stay constant here throughout: (a,z,Y,P) don't change at all!
	getValue(l,azYP)
end

#' ..py:function:: get_vcs(l,azYP,ihh,ik,is,itau,ih,ij,age,p,m)
#' gets vcs (value, consumption, savings) at discrete state (ihh,ik,is,itau,ih,ij,age)
function get_vcs(l::lininterp,azYP::Vector{Float64},ihh::Int,ik::Int,is::Int,itau::Int,ih::Int,ij::Int,age::Int,p::Param,m::Model)

	# get parts of index that does not change
	offset_h_age_j = ih-1 + p.nh * (ij-1 + p.nJ * (age-1))

	for iia in 1:p.na
		offset_a = iia-1 + p.na*offset_h_age_j
		for iP in 1:p.np
			offset_P = iP-1 + p.np * (itau-1 + p.ntau * offset_a)
			for iY in 1:p.ny 
				offset_Y = iY-1 + p.ny * offset_P
				for iz in 1:p.nz
					offset_z = iz-1 + p.nz * offset_Y

					idx = ihh + p.nh * (ik-1 + p.nJ * (is-1 + p.ns * offset_z))
					# println("idx = $idx")
					# println("idx11 = $(idx11(ihh,ik,is,iz,iy,ip,itau,iia,ih,ij,age,p))")
					# directly access the lininterp internal value array!
					@inbounds l.vals[1][iia + p.na*(iz-1 + p.nz *(iY-1 + p.ny *(iP-1)))] = m.vh[idx] 	
					@inbounds l.vals[2][iia + p.na*(iz-1 + p.nz *(iY-1 + p.ny *(iP-1)))] = m.ch[idx] 	
					@inbounds l.vals[3][iia + p.na*(iz-1 + p.nz *(iY-1 + p.ny *(iP-1)))] = m.sh[idx] 	
				end
			end
		end
	end
	getValue(l,azYP)
end

#' ..py:function:: get_vcs(l,azYP,ihh,ik,is,itau,ih,ij,age,p,m)
#' gets vcs (value, consumption, savings) at discrete state (ihh,ik,is,itau,ih,ij,age)
function get_vcs_rent(l::lininterp,azYP::Array{Float64},ihh::Int,ik::Int,is::Int,itau::Int,ih::Int,ij::Int,age::Int,p::Param,m::Model)

	na_rent = l.d[1]
	aone = m.aone

	# get parts of index that does not change
	offset_h_age_j = ih-1 + p.nh * (ij-1 + p.nJ * (age-1))

	for iia in aone:p.na
		offset_a = iia-1 + p.na*offset_h_age_j
		for iP in 1:p.np
			offset_P = iP-1 + p.np * (itau-1 + p.ntau * offset_a)
			for iY in 1:p.ny 
				offset_Y = iY-1 + p.ny * offset_P
				for iz in 1:p.nz
					offset_z = iz-1 + p.nz * offset_Y

					idx = ihh + p.nh * (ik-1 + p.nJ * (is-1 + p.ns * offset_z))
					# println("idx = $idx")
					# println("idx11 = $(idx11(ihh,ik,is,iz,iy,ip,itau,iia,ih,ij,age,p))")
					# directly access the lininterp internal value array!
					@inbounds l.vals[1][iia-aone+1 + na_rent*(iz-1 + p.nz *(iY-1 + p.ny *(iP-1)))] = m.vh[idx] 	
					@inbounds l.vals[2][iia-aone+1 + na_rent*(iz-1 + p.nz *(iY-1 + p.ny *(iP-1)))] = m.ch[idx] 	
					@inbounds l.vals[3][iia-aone+1 + na_rent*(iz-1 + p.nz *(iY-1 + p.ny *(iP-1)))] = m.sh[idx] 	
				end
			end
		end
	end
	getValue(l,azYP)
end


#' ..py:function:: get_cs(l,azYP,ihh,ik,is,itau,ih,ij,age,p,m)
#' gets cs (consumption, savings) at discrete state (ihh,ik,is,itau,ih,ij,age)
function get_cs(l::lininterp,azYP::Array{Float64},ihh::Int,ik::Int,is::Int,itau::Int,ih::Int,ij::Int,age::Int,p::Param,m::Model)

	# get parts of index that does not change
	offset_h_age_j = ih-1 + p.nh * (ij-1 + p.nJ * (age-1))

	for iia in 1:p.na
		offset_a = iia-1 + p.na*offset_h_age_j
		for iP in 1:p.np
			offset_P = iP-1 + p.np * (itau-1 + p.ntau * offset_a)
			for iY in 1:p.ny 
				offset_Y = iY-1 + p.ny * offset_P
				for iz in 1:p.nz
					offset_z = iz-1 + p.nz * offset_Y

					idx = ihh + p.nh * (ik-1 + p.nJ * (is-1 + p.ns * offset_z))
					# println("idx = $idx")
					# println("idx11 = $(idx11(ihh,ik,is,iz,iy,ip,itau,iia,ih,ij,age,p))")
					# directly access the lininterp internal value array!
					@inbounds l.vals[1][iia + p.na*(iz-1 + p.nz *(iY-1 + p.ny *(iP-1)))] = m.vh[idx] 	
					@inbounds l.vals[2][iia + p.na*(iz-1 + p.nz *(iY-1 + p.ny *(iP-1)))] = m.ch[idx] 	
				end
			end
		end
	end
	eval4D(l,azYP)
end

function get_v1v2(l::lininterp,azYP::Array{Float64},ik::Int,is::Int,itau::Int,ih::Int,ij::Int,age::Int,p::Param,m::Model)

	# get parts of index that does not change
	offset_h_age_j = ih-1 + p.nh * (ij-1 + p.nJ * (age-1))

	for iia in 1:p.na
		offset_a = iia-1 + p.na*offset_h_age_j
		for iP in 1:p.np
			offset_P = iP-1 + p.np * (itau-1 + p.ntau * offset_a)
			for iY in 1:p.ny 
				offset_Y = iY-1 + p.ny * offset_P
				for iz in 1:p.nz
					offset_z = iz-1 + p.nz * offset_Y

					idx = ik + p.nJ * (is-1 + p.ns * offset_z)
					# println("idx = $idx")
					# println("idx10 = $(idx10(ik,is,iz,iy,ip,itau,iia,ih,ij,age,p))")
					# directly access the lininterp internal value array!
					@inbounds l.vals[1][iia + p.na*(iz-1 + p.nz *(iY-1 + p.ny *(iP-1)))] = m.vh[idx] 	
					@inbounds l.vals[2][iia + p.na*(iz-1 + p.nz *(iY-1 + p.ny *(iP-1)))] = m.ch[idx] 	
				end
			end
		end
	end
	eval4D(l,azYP)
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





function assignCohort(i::Int,m::Model)
	searchsortedlast(i,m.coh_breaks)
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

		ngroups = 6

		# grouped dfs
		g_div = groupby(df, :Division)
		g_kids = groupby(df, :kids)
		g_own = groupby(df, :h)
		df = @transform(df, agebin = cut(p.ages[:age],int(quantile(p.ages[:age],[1 : ngroups - 1] / ngroups))))  # cut age into 6 bins
		g_abin = groupby(df,:agebin)

		# moments relating to homeownership
		# =================================

		noown = sum(df[:h]) == 0.0

		# linear probability model of homeownership
		if mean(df[:h]) == 1.0 || noown
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
		# ----------

		for div in g_div
			push!(mom1,["mean_own_$(div[1,:Division])",mean(div[:h]),std(div[:h])/sqrt(size(div,1))])
		end

		# own ~ kids
		# ----------

		for div in g_kids
			kk = "$(div[1,:kids])"
			push!(mom1,["mean_own_kids$(uppercase(kk))",mean(div[:h]),std(div[:h])/sqrt(size(div,1))])
		end
		# TODO std error
		push!(mom1,["cov_own_kids",cov(df[:h],df[:kids]),1.0])


		# moments relating to mobility
		# ============================

		nomove = false

		# linear probability model of mobility
		if sum(df[:move]) == 0.0
			nomove = true
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

		if noown
			push!(mom1,["mean_move_ownTRUE",0.0,1.0])
			push!(mom1,["mean_move_ownFALSE",1.0,1.0])
		elseif mean(df[:own]) == 1.0
			push!(mom1,["mean_move_ownTRUE",1.0,1.0])
			push!(mom1,["mean_move_ownFALSE",0.0,1.0])
		else
			for idf in g_own
				kk = "$(idf[1,:own])"
				push!(mom1,["mean_move_own$(uppercase(kk))",mean(idf[:move]),std(idf[:move])/sqrt(size(idf,1))])
			end

		end
		# TODO std error
		push!(mom1,["cov_move_h",cov(df[:h],df[:move]),1.0])


		# move ~ kids
		# ----------

		for idf in g_kids
			kk = "$(idf[1,:kids])"
			push!(mom1,["mean_move_kids$(uppercase(kk))",mean(idf[:move]),std(idf[:move])/sqrt(size(idf,1))])
		end
		# TODO std error
		push!(mom1,["cov_move_kids",cov(df[:move],df[:kids]),1.0])

		# move ~ distance 
		# ---------------

		if nomove
			push!(mom1,["q25_move_distance",0.0,1.0])
			push!(mom1,["q50_move_distance",0.0,1.0])
			push!(mom1,["q75_move_distance",0.0,1.0])
		else
			qts = quantile(df[df[:move].==true,:km_distance],[0.25,0.5,0.75])
			push!(mom1,["q25_move_distance",qts[1],1.0])
			push!(mom1,["q50_move_distance",qts[2],1.0])
			push!(mom1,["q75_move_distance",qts[3],1.0])
		end

		# move | negative equity
		# ----------------------

		# if noown || nomove
			push!(mom1,["move_neg_equity",0.0,1.0])
		# else
		# 	neq = df[(df[:move].==true) & (df[:own].==true),:equity] .< 0.0
		# 	push!(mom1,["move_neg_equity",0.0,1.0])




		# moments relating to total wealth
		# ================================

		# linear regression of total wealth
		# if sum(df[:own]) == 1.0 || noown
		# 	nm_w  = ["lm_w_(Intercept)","lm_w_age","lm_w_age2"]  
		# 	coef_w = DataArray(Float64,3)
		# 	std_w =  DataArray(Float64,3)
		# else
		# 	lm_w = fit(LinearModel, wealth ~ age + age2,df )
		# 	cc_w  = coeftable(lm_w)
		# 	nm_w  = ASCIIString["lm_w_" *  convert(ASCIIString,cc_w.rownms[i]) for i=1:size(cc_w.mat,1)] 
		# 	coef_w = @data(coef(lm_w))
		# 	std_w = @data(stderr(lm_w))
		# end

		# wealth ~ age
		# ------------
		for idf in g_abin
			push!(mom1,["mean_wealth_$(idf[1,:agebin])",mean(idf[:wealth]),std(idf[:wealth])/sqrt(size(idf,1))])
		end

		# wealth ~ division
		# -----------------

		for idf in g_div
			push!(mom1,["mean_wealth_$(idf[1,:Division])",mean(idf[:wealth]),std(idf[:wealth])/sqrt(size(idf,1))])
		end

		# wealth ~ own
		# ------------

		if noown

			push!(mom1,["mean_wealth_ownTRUE",0.0,0.0])
			push!(mom1,["mean_wealth_ownFALSE",mean(df[:wealth]),std(df[:wealth]) / sqrt(size(df,1))])
		else
			for idf in g_own
				kk = "$(idf[1,:own])"
				push!(mom1,["mean_wealth_own$(uppercase(kk))",mean(idf[:wealth]),std(idf[:wealth]) / sqrt(size(idf,1))])
			end
		end


		# collect estimates
		# =================


		# nms = vcat(nm_mv,nm_h,nm_w)
		nms = vcat(nm_mv,nm_h)

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

		mom2 = DataFrame(moment = nms, model_value = [coef_mv,coef_h], model_sd = [std_mv,std_h])

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



# select inds from sim dataframe
function getID(df::DataFrame,id::Array{Int,1})
	df[findin(df[:id],id),:] 
end






# setting up the FSpace objects for simulation
if Sys.OS_NAME == :Darwin 
	using ApproXD
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




# function simulate_cohorts(m::Model,p::Param,nsim::Int)

# 	T = p.nt-1

# 	nc = m.ncohorts
# 	npc = iround(nsim / nc) 	# number per cohort

# 	# storage
# 	# ========

# 	Dt = zeros(Int,nc*npc*T)	 # age

# 	for coh in 1:nc
# 		for age in ages[coh]
# 			# get macro variables for this cohort in that year
# 			#P = PYdata[PYindex[cohort,age]]
# 			#Y = PYdata[PYindex[cohort,age]]

# 			# fill all data arrays here:
# 			# vh(1,k,:,:,:,:,1,1,age)

# 			# find groups of i with the same discrete state
# 			# is,ih,ij,itau,age
# 			# and find moving decision for all of those
# 			for i in 1:npc

# 				idx = coh + nc*(age-1 + T *(i-1))

# 				# ... do sim
# 				# get current state

# 				Dj[idx] = ij

# 			end
# 		end
# 	end
# end
















	