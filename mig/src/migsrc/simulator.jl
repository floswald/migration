


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

function forceBounds(x::Vector{Float64},lb::Float64,ub::Float64)
	for i in 1:length(x)
		x[i] = forceBounds(x[i],lb,ub)
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


# function getRegional(m::Model,Y::Float64,P::Float64,j::Int)

# 	m.Regmods_YP[j] * vcat(1,Y,P)

# end


function simulate(m::Model,p::Param)

	srand(12345)

	T     = p.nt-1
	nsim  = m.coh_breaks[end]	# total number of individuals
	N_coh = length(m.coh_breaks)  # number of cohorts

	# get grids
	agrid      = m.grids["assets"]
	agrid_rent = m.grids["assets"][m.aone:end]
	na_rent    = length(agrid_rent)
	ygrid      = m.grids["Y"]
	pgrid      = m.grids["P"]
	regnames   = m.regnames[:Division]

	# temporary objects
	ktmp  = zeros(Float64,p.nJ)
	ktmp2 = zeros(p.nJ)
	v1tmp = 0.0
	v2tmp = 0.0

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

	# value arrays 
	# ------------
	rho_arr = Array{Float64}[]
	for i in 1:p.nJ
		push!(rho_arr,zeros(p.na,p.nz,p.ny,p.np))
	end
	vcs_arr = Array{Float64}[]
	for vcs in 1:3  # value, cons and savings
		for j in 1:p.nJ  # at each loc choice
			for ihh in 1:p.nh 	# at each housing choice
				push!(vcs_arr,zeros(p.na,p.nz,p.ny,p.np))
			end
		end
	end

	# construct the interpolators
	# ---------------------------
	L = Dict{ASCIIString,lininterp}()
	L["l_vcs"] = lininterp(vcs_arr,gs)
	L["l_rho"] = lininterp(rho_arr,gs)

	# prepare initial distributions to draw from
	cumGz = cumsum(m.gridsXD["Gz"],2)
	cumGs = cumsum(m.gridsXD["Gs"],2)
	Gtau  = Categorical(m.grids["Gtau"])	# type distribution
	G0j   = Categorical(array(m.regnames[:prop]))	
	G0k   = Categorical([0.6,0.4])  # 40% of 21-year olds have kids in SIPP
	G0z   = Normal(0,0.1)

	# individual specific variables
	id     = 0
	a      = 0.0
	z      = 0.0
	yy     = 0.0   # individual income
	cons   = 0.0
	canbuy = false
	ihh    = 0
	ih     = 0
	move   = false
	moveto = 0
	val    = 0.0
	cons   = 0.0
	ss     = 0.0
	prob   = 0.0
	Y      = 0.0
	P      = 0.0

	# Storage Arrays: all of size nsim*T
	Dv       = zeros(nsim*T)
	Dcons    = zeros(nsim*T)
	Dsave    = zeros(nsim*T)
	Dincome  = zeros(nsim*T)
	Dprob    = zeros(nsim*T)
	Dwealth  = zeros(nsim*T)
	Ddist    = zeros(nsim*T)
	Dcash    = zeros(nsim*T)
	Drent    = zeros(nsim*T)
	Da       = zeros(nsim*T)
	Dz       = zeros(nsim*T)
	Dy       = zeros(nsim*T)
	Dp       = zeros(nsim*T)
	DY       = zeros(nsim*T)
	DP       = zeros(nsim*T)
	DM       = falses(nsim*T)
	Dcanbuy  = falses(nsim*T)
	Di       = repeat([1:nsim],inner=[T],outer=[1])
	Dage     = repeat([1:T],inner=[1],outer=[nsim])
	Drealage = repeat([p.ages[1:T]],inner=[1],outer=[nsim])
	Dyear    = DataArray(Int,nsim*T)
	Dcohort  = DataArray(Int,nsim*T) # allocates as NA
	Dhh      = zeros(Int,nsim*T)
	Dh       = zeros(Int,nsim*T)
	Dj       = zeros(Int,nsim*T)
	DMt      = zeros(Int,nsim*T)
	Dis      = zeros(Int,nsim*T)
	Dtau     = zeros(Int,nsim*T)
	Dregname = ASCIIString["" for i = 1:(nsim*T)]

	# fill in aggregate prices faced by each cohort
	# also draw invariant type tau here
	for i in 1:nsim
		coh = mig.assignCohort(i,m)
		yearidx = coh
		tau = rand(Gtau)
		for it in 1:length(m.coh_yrs[coh])
			Dtau[it + T*(i-1)] = tau
			Dcohort[it + T*(i-1)] = coh
			DY[it + T*(i-1)] = m.PYdata[yearidx,:Y]
			DP[it + T*(i-1)] = m.PYdata[yearidx,:P]
			yearidx += 1
		end
	end

	# wherever isna(Dcohort), now simulation took place, so throw those rows away in the end	.

	g_count = 0


	# populate age=1 with initial conditions
	# those are independent of cohort, so just draw for each 
	# age = 1 individual
	idxvec = Dage .== 1
	Dis[idxvec]  = rand(G0k,nsim)
	Dj[idxvec]   = rand(G0j,nsim)
	Dz[idxvec]   = rand(G0z,nsim)
	Da[idxvec]   = forceBounds(rand(m.Init_asset,nsim),0.0,100.0)

	for age = 1:T

		# who is around at this age?
		maxcohort = N_coh - (age-1)   # all cohorts > maxcohort don't have to be simulated at that age

		# individuals to simulate at this age
		inds   = 1:m.coh_breaks[maxcohort]
		idxvec = (Dage.==age) & (Di .<= inds[end])

		# dataframe with discrete states for each id
		g = DataFrame(id = inds,j = Dj[idxvec], is = Dis[idxvec], h=Dh[idxvec], tau=Dtau[idxvec])
		# groups with identical discrete state
		gg = groupby(g,[:j,:is,:h,:tau])

		for ig in gg

			g_count += 1

			# get group-specific discrete vars
			is   = ig[1,:is] 	# {1,2}
			ih   = ig[1,:h] 	# {0,1}
			itau = ig[1,:tau]	# {1,2}
			ij   = ig[1,:j]    # {1,...,9}


			# setup the interpolators on this function space
			# ----------------------------------------------
			fill_interp_arrays!(L,is,ih+1,itau,ij,age,p,m)
			setGrid!(L["l_rho"],2,zsupps[ij])
			setGrid!(L["l_vcs"],2,zsupps[ij])

			# reset caches
			resetCache!(L["l_rho"])
			resetCache!(L["l_vcs"])

			# list of group individuals	
			ginds = ig[:id]

			# loop over individuals
			# notice that inds here are from several distinct cohorts. 
			# that's fine, because the cohort effect (aggregate prices) is predetermined.
			for i in ginds
				# this individuals index in arrays at current age
				i_idx = age + T*(i-1)
				@assert i == Di[i_idx]
				@assert age == Dage[i_idx]
				# this individuals index in arrays at next age
				i_idx_next = age+1 + T*(i-1)

				coh = Dcohort[i_idx]

				# get current state
				Y = DY[i_idx]
				P = DP[i_idx]
				a = Da[i_idx]
				z = Dz[i_idx]
				azYP = [a,z,Y,P]
				price_j = m.pred_p[m.coh_idx[coh][age],ij]


				# get moving choice given current state
				# =====================================
		
				# 4D interpolation on (a,z,Y,P)
				ktmp = get_rho_ktmp(L["l_rho"],azYP,p)
				# normalizing vector of moving probs: because of approximation error
				# sometimes this is not *exactly* summing to 1
				ktmp = ktmp ./ sum(ktmp)
				
				# get cumulative prob
				cumsum!(ktmp2,ktmp,1)
				# throw a k-sided dice 
				shock  = rand()
				moveto = searchsortedfirst(ktmp2,shock)
				move   = ij != moveto
				prob   = ktmp[moveto]

				if moveto>p.nJ || moveto < 1
					println(ktmp2)
					error("problem in moveto = $moveto")
				end


				# from here on you are assumed to be in 
				# new region "moveto"
				# =====================================

				price = m.pred_p[m.coh_idx[coh][age],moveto]
				y     = m.pred_y[m.coh_idx[coh][age],moveto]

				# get regional price and income for that individual
				# TODO precompute yp for all cohorts and regions
				# yp = getRegional(m,Y,P,ij) # yp[1] = y, yp[2] = p
				Dy[i_idx] = y
				Dp[i_idx] = price
				Dyear[i_idx] = m.coh_yrs[coh][age]

				# get individual specific state
				yy = getIncome(m,y,z,age,moveto)

				# flag for downpayment constraint
				canbuy = a + yy > p.chi * price

				# get value, consumption and savings given moving choice
				# ======================================================

				# you are current owner or you can buy
				if (ih==1 || (ih==0 && canbuy))

					# get housing choice
					v1v2 = get_v1v2(L["l_vcs"],azYP,moveto,p)

					ihh = v1v2[1] > v1v2[2] ? 0 : 1
					val = v1v2[1] > v1v2[2] ? v1v2[1] : v1v2[2]

					# find corresponding consumption and savings
					cs = get_cs(L["l_vcs"],azYP,ihh+1,moveto,p)
					cons       = cs[1]
					ss         = cs[2]

				# else you are current renter who cannot buy
				else
					ihh = 0
					if a < 0
						println("error: id=$id,age=$age,ih=$ih,canbuy=$canbuy,a=$a")
					end
					# for iia in aone:p.na
					# 	for iz in 1:p.nz
					# 		idx = mig.idx11(ihh+1,moveto,is,iz,iy,ip,itau,iia,ih+1,ij,age,p)
					# 		azmat_v_rent[iia-aone+1 + na_rent*(iz-1)] = m.vh[idx]
					# 		azmat_c_rent[iia-aone+1 + na_rent*(iz-1)] = m.ch[idx]
					# 		azmat_s_rent[iia-aone+1 + na_rent*(iz-1)] = m.sh[idx]
					# 	end
					# end
					vcs = get_vcs(L["l_vcs"],azYP,ihh+1,moveto,p)
					val  = vcs[1]
					cons = vcs[2]
					ss   = vcs[3]

				end

				# make sure savings is inside grid
				# (although interpolator forces that anyway)
				ss = forceBounds(ss,agrid[1],agrid[end])

				# storing choices and values
				Dv[i_idx]       = val
				Dcons[i_idx]    = cons
				Dsave[i_idx]    = ss
				Dincome[i_idx]  = yy
				Dhh[i_idx]      = ihh
				Dprob[i_idx]    = prob
				DM[i_idx]       = move
				DMt[i_idx]      = moveto
				Dregname[i_idx] = regnames[ij]
				Dcanbuy[i_idx]  = canbuy
				Dwealth[i_idx]  = (price * ih) + a
				Ddist[i_idx]    = m.distance[ij,moveto]
				Dcash[i_idx]    = cashFunction(a,yy,ih,ihh,price_j,price,move,ij,p)
				Drent[i_idx]    = pifun(ih,ihh,price_j,price,move,ij,p)


				# storing transition
				if age < T
					Dh[i_idx_next] = ihh
					Da[i_idx_next] = ss
					Dj[i_idx_next] = moveto
					Dz[i_idx_next] = draw_z(m,z,moveto) # TODO drawign this from the right distribution depending on whether move or not!
					Dis[i_idx_next] = searchsortedfirst( cumGs[is,:,age][:], rand() )
				end
			end # individual

			if p.verbose>0
				if mod(g_count,400) == 0
					println("this is group $g_count")
					println("for group age=$age,is=$is,ih=$ih,itau=$itau,ij=$ij:")
					println("interpolator l_vcs:")
					println(L["l_vcs"])
					println("interpolator l_rho:")
					println(L["l_rho"])
				end
			end
		end # groups
	end  # age

	# pack into a dataframe
	# kids=PooledDataArray(convert(Array{Bool,1},Dis))
	df = DataFrame(id=Di,age=Dage,realage=Drealage,year=Dyear,kids=PooledDataArray(convert(Array{Bool,1},Dis.-ones(length(Dis)))),tau=Dtau,j=Dj,Division=Dregname,a=Da,save=Dsave,cons=Dcons,cash=Dcash,rent=Drent,z=Dz,p=Dp,y=Dy,P=DP,Y=DY,income=Dincome,move=DM,moveto=DMt,h=Dh,hh=Dhh,v=Dv,prob=Dprob,wealth=Dwealth,km_distance=Ddist,own=PooledDataArray(convert(Array{Bool,1},Dh)),canbuy=Dcanbuy,cohort=Dcohort)

	return df
end




# fills arrays with corresponding values at 
# discrete state (is,ih,itau,ij,age)
# fills l_vcs with all possible combinations of (ihh,ik)
function fill_interp_arrays!(L::Dict{ASCIIString,lininterp},is::Int,ih::Int,itau::Int,ij::Int,age::Int,p::Param,m::Model)

	# L contains:
	# 1. l_vcs
	# 2. l_rho  

	# get part of index that does not change
	offset_h_age_j = ih-1 + p.nh * (ij-1 + p.nJ * (age-1))

	for iia in 1:p.na
		offset_a = iia-1 + p.na*offset_h_age_j
		for iP in 1:p.np
			offset_P = iP-1 + p.np * (itau-1 + p.ntau * offset_a)
			for iY in 1:p.ny 
				offset_Y = iY-1 + p.ny * offset_P
				for iz in 1:p.nz
					offset_z = iz-1 + p.nz * offset_Y

					arr_idx = iia + p.na*(iz-1 + p.nz *(iY-1 + p.ny *(iP-1)))

					for ik in 1:p.nJ
						rho_idx = ik + p.nJ * (is-1 + p.ns * offset_z)
						@inbounds L["l_rho"].vals[ik][arr_idx] = m.rho[rho_idx] 	

						for ihh in 1:p.nh

							vh_idx = ihh + p.nh * (ik-1 + p.nJ * (is-1 + p.ns * offset_z))
							# index to get i in {(v,c,s) x (1,2,3,...,9) x (1,2)}
							l_idx  = 3*(ik-1 + p.nJ*(ihh-1))
							@inbounds L["l_vcs"].vals[1+l_idx][arr_idx] = m.vh[vh_idx] 	
							@inbounds L["l_vcs"].vals[2+l_idx][arr_idx] = m.ch[vh_idx] 	
							@inbounds L["l_vcs"].vals[3+l_idx][arr_idx] = m.sh[vh_idx] 	


						end
					end
				end
			end
		end
	end


end



 # r = ik + p.nJ * (is-1 + p.ns * (iz-1 + p.nz * (iy-1 + p.ny * (ip-1 + p.np * (itau-1 + p.ntau * (ia-1 + p.na * (ih-1 + p.nh * (ij-1 + p.nJ * (age-1)))))))))


function get_rho_ktmp(l::lininterp,azYP::Vector{Float64},p::Param)
	out = zeros(p.nJ)

	getValue!(out,l,azYP,[1:p.nJ])
	return out
end

#' ..py:function:: get_vcs(l,azYP,ihh,ik,is,itau,ih,ij,age,p,m)
#' gets vcs (value, consumption, savings) at discrete state (ihh,ik,is,itau,ih,ij,age)
function get_vcs(l::lininterp,azYP::Vector{Float64},ihh::Int,ik::Int,p::Param)

	out = zeros(3)
	l_idx  = 3*(ik-1 + p.nJ*(ihh-1))

	getValue!(out,l,azYP,[1+l_idx,2+l_idx,3+l_idx])
	# out[2] = getValue(l,azYP,2+l_idx)
	# out[3] = getValue(l,azYP,3+l_idx)
	return out
end

#' ..py:function:: get_vcs(l,azYP,ihh,ik,is,itau,ih,ij,age,p,m)
#' gets vcs (value, consumption, savings) at discrete state (ihh,ik,is,itau,ih,ij,age)
function get_v1v2(l::lininterp,azYP::Vector{Float64},ik::Int,p::Param)

	out = zeros(2)
	l_idx1  = 3*(ik-1 + p.nJ*(1-1))
	l_idx2  = 3*(ik-1 + p.nJ*(2-1))

	getValue!(out,l,azYP,[1+l_idx1,1+l_idx2])
	return out
end

#' ..py:function:: get_vcs(l,azYP,ihh,ik,is,itau,ih,ij,age,p,m)
#' gets vcs (value, consumption, savings) at discrete state (ihh,ik,is,itau,ih,ij,age)
function get_cs(l::lininterp,azYP::Vector{Float64},ihh::Int,ik::Int,p::Param)

	out = zeros(2)
	l_idx  = 3*(ik-1 + p.nJ*(ihh-1))

	getValue!(out,l,azYP,[2+l_idx,3+l_idx])	# 2 is index for cons, 3 is index for save
	return out
end



function assignCohort(i::Int,m::Model)
	searchsortedfirst(m.coh_breaks,i)
end



# computing moments from simulation
function computeMoments(df::DataFrame,p::Param,m::Model)

	# keep only relevant years
	# and drop NAs
	df = @where(df,(:year.>1997) & (!isna(:cohort)))


	# transformations, adding columns
	# df = @transform(df, agebin = cut(p.ages[:age],int(quantile(p.ages[:age],[1 : ngroups - 1] / ngroups))), age2 = :age.^2)  # cut age into 6 bins, and add age squared
	ngroups = 6
	df = @transform(df, agebin = cut(:realage,int(quantile(:realage,[1 : ngroups - 1] / ngroups))), age2 = :age.^2, sell = (:h.==1) & (:hh.==0) )  # cut age into 6 bins, and add age squared

	df = join(df,m.agedist,on=:realage)
	fullw = WeightVec(array(df[:density]))


	# create a dataframe to push moments on to
	mom1 = DataFrame(moment="", model_value = 0.0, model_sd = 0.0)


	# grouped dfs
	g_div = groupby(df, :Division)
	g_kids = groupby(df, :kids)
	g_own = groupby(df, :h)
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
	
	xx = @> begin
		   df 
		   @where(:age.==30) 
		   @select(moment="mean_sell_50",model_value=mean(:sell),model_sd=std(:sell)/sqrt(size(df,1)))
		end
	append!(mom1,xx)


	# own ~ Division
	# ----------

	for div in g_div
		w = WeightVec(array(div[:density]))
		push!(mom1,["mean_own_$(div[1,:Division])",mean(convert(Array{Float64},div[:h]),w),std(convert(Array{Float64},div[:h]),w)/sqrt(size(div,1))])
	end

	# own ~ kids
	# ----------

	for div in g_kids
		kk = "$(div[1,:kids])"
		w = WeightVec(array(div[:density]))
		push!(mom1,["mean_own_kids$(uppercase(kk))",mean(convert(Array{Float64},div[:h]),w),std(convert(Array{Float64},div[:h]),w)/sqrt(size(div,1))])
	end
	# TODO std error
	covar = cov(hcat(convert(Array{Float64},df[:h]),df[:kids]),fullw)
	push!(mom1,["cov_own_kids",covar[1,2],1.0])


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
	push!(mom1,["mean_move",mean(convert(Array{Float64},df[:move]),fullw),std(convert(Array{Float64},df[:move]),fullw)])	# unconditional mean
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
			w = WeightVec(array(idf[:density]))
			push!(mom1,["mean_move_own$(uppercase(kk))",mean(convert(Array{Float64},idf[:move]),w),std(convert(Array{Float64},idf[:move]),w)/sqrt(size(idf,1))])
		end

	end
	# TODO std error
	covar = cov(hcat(convert(Array{Float64},df[:h]),df[:move]),fullw)
	push!(mom1,["cov_move_h",covar[1,2],1.0])


	# move ~ kids
	# ----------

	for idf in g_kids
		kk = "$(idf[1,:kids])"
		w = WeightVec(array(idf[:density]))
		push!(mom1,["mean_move_kids$(uppercase(kk))",mean(convert(Array{Float64},idf[:move]),w),std(convert(Array{Float64},idf[:move]),w)/sqrt(size(idf,1))])
	end
	# TODO std error
	covar = cov(hcat(convert(Array{Float64},df[:move]),df[:kids]),fullw)
	push!(mom1,["cov_move_kids",covar[1,2],1.0])

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
		w = WeightVec(array(idf[:density]))
		push!(mom1,["mean_wealth_$(idf[1,:Division])",mean(array(idf[:wealth]),w),std(array(idf[:wealth]),w)/sqrt(size(idf,1))])
	end

	# wealth ~ own
	# ------------

	if noown

		push!(mom1,["mean_wealth_ownTRUE",0.0,0.0])
		push!(mom1,["mean_wealth_ownFALSE",mean(array(df[:wealth]),fullw),std(array(df[:wealth]),fullw) / sqrt(size(df,1))])
	else
		for idf in g_own
			w = WeightVec(array(idf[:density]))
			kk = "$(idf[1,:own])"
			push!(mom1,["mean_wealth_own$(uppercase(kk))",mean(array(idf[:wealth]),w),std(array(idf[:wealth]),w) / sqrt(size(idf,1))])
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
















	