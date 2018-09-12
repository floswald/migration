

function clamp(x::Number,a::Number,b::Number)
	if x < a
		a
	elseif x > b
		b
	else
		x
	end
end


"""
	draw_z(m::Model,Lz::Float64,idx::Int)

Given a last draw `Lz`, return the next random draw ``z_{t+1}`` from
```math 
z_{t+1} = \rho z_{t}  + \varepsilon_t
```
"""
function draw_z(m::Model,Lz::Float64,idx::Int)
	zz = m.Inc_shocks[1,1] * Lz + m.zshock[idx]
	return clamp(zz,m.zrange[1],m.zrange[2])
end

"compute the level of income, including age profile and regional effects"
function getIncome(m::Model,y::Float64,z::Float64,age::Int,j::Int)
	inc = exp(m.Inc_ageprofile[age,j] + m.Inc_coefs[j,:logq] * log(y) + z )
end



function simulate(m::Model,p::Param)

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
	zmarginal  = m.gridsXD["zmarginal"]

	# temporary objects
	ktmp  = zeros(Float64,p.nJ)
	# vtmp  = similar(ktmp)
	ktmp2 = zeros(p.nJ)
	v1tmp = 0.0
	v2tmp = 0.0

	movecost = m.gridsXD["movecost"]
	realized_shock = 0.0

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
	# EV_arr = Array{Float64}[]
	# push!(EV_arr,zeros(p.na,p.nz,p.ny,p.np))

	# construct the interpolators
	# ---------------------------
	L = Dict{String,Lininterp}()
	L["l_vcs"] = Lininterp(vcs_arr,gs)
	L["l_rho"] = Lininterp(rho_arr,gs)
	# L_EV  = Lininterp(EV_arr,gs)

	# pdebug("setup Lininterps")

	srand(54321)

	# prepare initial distributions to draw from
	cumGz = cumsum(m.gridsXD["Gz"],2)
	cumGs = cumsum(m.gridsXD["Gs"],2)
	Gtau  = Categorical(m.grids["Gtau"])	# type distribution
	G0j   = Categorical(m.regnames[:prop])
	G0k   = Categorical([0.6,0.4])  # 40% of 21-year olds have kids in SIPP
	G0h   = Categorical([0.8,0.2])  # 20% of 21-year olds have kids a house in SIPP

	# Distribution of idiosyncratic utility shocks is Gumbel(0,1)

	# individual specific variables
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
	cum_moveprob = 0.0
	eps_shock = 0.0   # "winning" idiosyncratic utility shock
	consta = 0.0  # utility shifter
	utility = 0.0 # utuility

	# Storage Arrays: all of size nsim*T
	Dv       = zeros(nsim*T)
	Dmaxv    = zeros(nsim*T)
	Dutility = zeros(nsim*T)
	Dcons    = zeros(nsim*T)
	Dsave    = zeros(nsim*T)
	Dincome  = zeros(nsim*T)
	Dprob    = zeros(nsim*T)
	Dshock   = zeros(nsim*T)	# value of idiosyncratic shock of the chosen discrete choice, eps_shock
	Dcumprob = zeros(nsim*T)
	Dwealth  = zeros(nsim*T)
	Dwealth2  = zeros(nsim*T)
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
	DDrop    = falses(nsim*T)
	Di       = repeat(collect(1:nsim),inner = [T],outer = [1])
	Dage     = repeat(collect(1:T),inner = [1],outer = [nsim])
	Drealage = repeat(collect(p.ages[1:T]),inner = [1],outer = [nsim])
	# Dyear    = DataArray(Int,nsim*T)
	Dyear    = Union{Int64, Missings.Missing}[missing for i in 1:nsim*T]
	Dcohort  = Union{Int64, Missings.Missing}[missing for i in 1:nsim*T] # allocates as missing
	Dhh      = zeros(Int,nsim*T)
	Dh       = zeros(Int,nsim*T)
	Dj       = zeros(Int,nsim*T)
	DMt      = zeros(Int,nsim*T)
	Dis      = zeros(Int,nsim*T)
	Dtau     = zeros(Int,nsim*T)
	Dregname = String["" for i = 1:(nsim*T)]
	Dtoname  = String["" for i = 1:(nsim*T)]
	Dsubsidy = zeros(nsim*T)

	# pdebug("initialized sim arrays")

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
	Dz[idxvec]   = m.zshock0	# get initial z shock
	Dh[idxvec]   = rand(G0h,nsim) .- 1

	# we want stable proportions within each cohort. i.e. each cohort should have the
	# the same amount of people in state j, type tau, etct
	Dj[idxvec] = repeat(rand(G0j,m.coh_n[1]),inner=[1],outer=[N_coh])

	# policies
	# ========

	mortgageSub  = false
	noBuying     = false
	highMC       = false

	if p.policy == "mortgageSubsidy" || p.policy == "mortgageSubsidy_padjust"
		mortgageSub  = true
	end

	pshock = false
	yshock = false
	nomove_yshock = false
	shockmyy = false
	if p.policy == "pshock"
		pshock = true
	end
	if p.policy == "yshock"
		yshock = true
	end
	if p.policy == "pshock_noBuying"
		pshock = true
		noBuying = true
	end
	if p.policy == "noBuying"
		noBuying = true
	end
	if p.policy == "highMC"
		highMC = true
	end
	if p.policy == "noMove"
		highMC = true
		nomove_yshock = pshock = true
	end


	if p.policy == "pshock_highMC"
		pshock = true
		highMC = true
	end

	if ((length(p.policy)>=7) && (p.policy[1:7] == "ypshock"))
		pshock = true
		yshock = true
	end

	# pdebug("set policy switches")
	# println("policy = $(p.policy) and pshock = $(pshock) and yshock=$(yshock)")

	for age = 1:T

		# pdebug("simulating age=$age")

		# who is around at this age?
		maxcohort = N_coh - (age-1)   # all cohorts > maxcohort don't have to be simulated at that age

		# individuals to simulate at this age
		inds   = 1:m.coh_breaks[maxcohort]
		idxvec = (Dage.==age) .& (Di .<= inds[end])

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

			# start to build utility
			if is==1
				consta = ih*p.xi1
			else
				consta = ih*p.xi2
			end


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
				Dyear[i_idx] = m.coh_yrs[coh][age]

				# get current state determined from aggregates
				Y         = DY[i_idx]
				P         = DP[i_idx]
				a         = Da[i_idx]
				z         = Dz[i_idx]
				azYP      = [a,z,Y,P]
				price_j   = m.pred_p[m.coh_idx[coh][age],ij]
				y         = m.pred_y[m.coh_idx[coh][age],ij]

				# apply price policy adjustment here
				if pshock && ((age >= p.shockAge) && (ij == p.shockReg))
					price_j *= p.shockVal_p[age-p.shockAge+1]
				end

				# apply price policy adjustment here
				shockmyy = yshock && ((age >= p.shockAge) && (ij == p.shockReg))
				# if yshock && ((age >= p.shockAge) && (ij == p.shockReg))
				# 	y *= p.shockVal_y[age-p.shockAge+1]
				# end


				# record regional prices
				Dy[i_idx] = y
				Dp[i_idx] = price_j

				# get individual specific income at current state
				# this is baseline income, i.e. before any policy induced adjustments
				# if shockmyy
					# println("income = $(getIncome(m,y,z,age,ij))")
					# println("shockincome = $(getIncome(m,y,z + log(p.shockVal_y[age-p.shockAge+1]),age,ij))")
				# end
				yy = shockmyy ? getIncome(m,y,z + log(p.shockVal_y[age-p.shockAge+1]) ,age,ij) : getIncome(m,y,z,age,ij)
				yy = nomove_yshock ? getIncome(m,y,z + log(p.shockVal_y[age]) ,age,ij) : yy

				# CAUTION: this if-clause should not be necessary!
				# if moving is not allowed from your region...
				# if highMC && (p.shockReg==ij)

				# 	moveto = ij
				# 	move = false

				# else

					# get moving choice given current state
					# =====================================

					# V(state) = max_k {v(state,k) + eps(k)}	--> maximal discrete choice value
					# D(state) = indmax_k {v(state,k) + eps(k)}	 --> maximal discrete choice (index)
					# the solution computes v(state,k), i.e. the value net of eps(k)
					# 1) interpolate v(state,k) ∀ k
					# 2) draw eps(k) ∀ k
					# 3) find indmax_k {v(state,k) + eps(k)}

					# 1) interpolate v(state,k) ∀ k
					ktmp = get_rho_ktmp(L["l_rho"],azYP,p)
					if ktmp[ij] == p.myNA
						DDrop[i_idx] = true
							# warn("indi $i on an infeasible state")
					end

					# 2) draw eps(k) ∀ k
					# eps = rand(G_eps,p.nJ)   done in model
					# vtmp[:] = ktmp .+ convert(Vector{Float64},m.eps_shock[i_idx])

					# 3) find indmax_k {v(state,k) + eps(k)}
					maxval, moveto = findmax(ktmp + m.eps_shock[i_idx])
					realized_shock = m.eps_shock[i_idx][moveto]

					# 4) find implied probabilities of stay and move
					mmx = ktmp - maximum(ktmp)
					stayprob = exp(mmx[ij]) / sum(exp.(mmx))
					cum_moveprob = 1.0 - stayprob

					move   = ij != moveto

					if moveto>p.nJ || moveto < 1
						println("ktmp=$ktmp")
						println("eps=$eps")
						error("problem in moveto = $moveto")
					end
					if move && highMC
						# println(ktmp)
						# println(m.eps_shock[i_idx])
						if ktmp[ij] == p.myNA
							DDrop[i_idx] = true
							# warn("indi $i on an infeasible state")
						end
						# println("movecost = $(movecost[idxMC(age,ij,moveto,itau,ih+1,is,p)])")
						# warn("indi $i is moving from $ij to $moveto")
					end
				# end

				# house price in new region "moveto"
				# ==================================

				price_k = m.pred_p[m.coh_idx[coh][age],moveto]
				# apply price policy adjustment here
				if pshock && ((age >= p.shockAge) && (moveto == p.shockReg))
					price_k *= p.shockVal_p[age-p.shockAge+1]
				end


				# policy adjustments to baseline net income
				# =========================================

				if mortgageSub
					# policy is on:
					# 1) substract subsidy from owners
					if ih == 1
						subsidize = findSubsidy(yy,age,p,m)
						yy -= subsidize
					end

					# 2) redistribute according to policy
					yy += p.redistribute[age]

				else
					# mortgage subsidy policy is off: record amount paid to owners
					if ih == 1
						Dsubsidy[i_idx] = findSubsidy(yy,age,p,m)
					end
				end



				# flag for downpayment constraint
				if ih==0
					canbuy = a + yy > p.chi * price_k
				else
					canbuy = a + yy + (1-p.phi)*price_j > p.chi * price_k
				end

				if noBuying
					canbuy = false
				end

				# get value, consumption and savings given moving choice
				# ======================================================

				# if
				# 1) you are current owner who stays or
				# 2) you are current owner who moves and can buy
				# 3) you are current renter who can buy

				if ((ih==1 && (!move)) || (ih==1 && move && canbuy) || (ih==0 && canbuy))

					# get housing choice
					v1v2 = get_v1v2(L["l_vcs"],azYP,moveto,p)

					ihh = v1v2[1] > v1v2[2] ? 0 : 1
					val = v1v2[1] > v1v2[2] ? v1v2[1] : v1v2[2]

					# find corresponding consumption and savings
					cs = get_cs(L["l_vcs"],azYP,ihh+1,moveto,p)
					cons       = cs[1]
					ss         = cs[2]

				# else you cannot buy
				else
					ihh = 0
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

				# get flow utility
				# this is utility net of continuation value
				mc = movecost[idxMC(age,ij,moveto,itau,ih+1,is,p)]
				constant = consta - mc + m.amenities[moveto]
				utility = ufun(cons,0.0,p) + constant + realized_shock

				# println("")
				# println("mc= $mc")
				# println("iconsta = $constant")
				# println("utility = $utility")
				# println("maxval = $maxval")
				# println("shock = $realized_shock")
				# make sure savings is inside grid
				# (although interpolator forces that anyway)
				clamp(ss,agrid[1],agrid[end])

				# if move
				# 	# find alternative value based on average moving cost for a mover
				# 	# v = u(cons) + ihh*p.xi + p.beta * EV(ss,ihh+1,ij,age+1) - log(stayprob)/(1-log(stayprob))

				# 	# 1. find EV(ss,ihh+1,ij,age+1)
				# 	setGrid!(L_EV,2,zsupps[ij])
				# 	resetCache!(L_EV)
				# 	fill_interp_EV!(L_EV,is,ihh+1,itau,ij,age+1,p,m)

				# 	szYP      = [ss,z,Y,P]
				# 	ev = getValue(L_EV,szYP)

				# 	# 2. put value together
				# 	if is==1
				# 		val2 = ufun(cons,ev,p) + ihh*p.xi1 - log(stayprob)/(1-log(stayprob))
				# 	else
				# 		val2 = ufun(cons,ev,p) + ihh*p.xi2 - log(stayprob)/(1-log(stayprob))
				# 	end
				# else
				# 	val2 = 0.0
				# end


				# storing choices and values
				Dshock[i_idx]   = realized_shock
				Dutility[i_idx] = utility
				Dmaxv[i_idx]    = maxval
				Dv[i_idx]       = val
				Dcons[i_idx]    = cons
				Dsave[i_idx]    = ss
				Dincome[i_idx]  = yy
				Dhh[i_idx]      = ihh
				Dprob[i_idx]    = prob
				Dcumprob[i_idx] = cum_moveprob
				DM[i_idx]       = move
				DMt[i_idx]      = moveto
				Dregname[i_idx] = regnames[ij]
				Dtoname[i_idx] = regnames[moveto]
				Dcanbuy[i_idx]  = canbuy
				Dwealth[i_idx]  = (price_k * ih) + a
				Dwealth2[i_idx]  = (price_j * ih) + a 	# TODO
				Ddist[i_idx]    = m.distance[ij,moveto]
				Dcash[i_idx]    = cashFunction(a,yy,ih,ihh,price_j,price_k,move,ij,p)
				Drent[i_idx]    = pifun(ih,ihh,price_j,price_k,move,ij,p)


				# storing transition
				if age < T
					Dh[i_idx_next] = ihh
					Da[i_idx_next] = ss
					Dj[i_idx_next] = moveto
					if move 
						iz = searchsortedfirst(zmarginal,z)  # rank of your current z
						Dz[i_idx_next] = zmarginal[searchsortedfirst( m.cop_cdf[iz,:], m.cop_shock[i_idx])]
					else
						Dz[i_idx_next] = draw_z(m,z,i_idx)
					end
					Dis[i_idx_next] = searchsortedfirst( cumGs[is,:,age][:], m.sshock[i_idx] )
				end
				if highMC && (p.shockReg==ij) && move
					error("ind=$id, cohort = $coh moved even though it's ruled out.")
				end
			end # individual

			if p.verbose>0
				if mod(g_count,400) == 0
					info("this is group $g_count")
					info("for group age=$age,is=$is,ih=$ih,itau=$itau,ij=$ij:")
					info("interpolator l_vcs:")
					info(L["l_vcs"])
					info("interpolator l_rho:")
					info(L["l_rho"])
				end
			end
		end # groups
	end  # age

	sdrop = sum(DDrop)
	if sdrop > 0
		warn("Had to drop $sdrop inds (out of $(length(DDrop))) because on infeasible state")
	end

	# pack into a dataframe
	# kids=CategoricalArray(convert(Array{Bool,1},Dis))

	# note: some individuals will not get simulated at certain ages because they fall out of the sampling frame
	# i.e. if you are the last cohort, i will only observe you at age 1
	# therefore, for all other ages, that cohort will have garbage values in the arrays.

	# convert children indicator into a boolean:
	# Dis[Dis.>0] = Dis[Dis.>0] .-ones(length(Dis[Dis.>0]))

	# df = DataFrame(id=Di,age=Dage,realage=Drealage,income=Dincome,cons=Dcons,cash=Dcash,a=Da,save=Dsave,kids=CategoricalArray(convert(Array{Bool},Dis)),tau=Dtau,j=Dj,Division=Dregname,Division_to=Dtoname,rent=Drent,z=Dz,p=Dp,y=Dy,P=DP,Y=DY,move=CategoricalArray(convert(Array{Bool},DM)),moveto=DMt,h=Dh,hh=Dhh,v=Dv,utility=Dutility,maxv = Dmaxv,prob=Dprob,eps_shock=Dshock,cumprob=Dcumprob,wealth=Dwealth,wealth2=Dwealth2,km_distance=Ddist,own=CategoricalArray(convert(Array{Bool},Dh)),canbuy=Dcanbuy,cohort=Dcohort,year=Dyear,subsidy=Dsubsidy,own_30=CategoricalArray(convert(Array{Bool},Dh.*(Drealage.==30))),rent_30=CategoricalArray(convert(Array{Bool},(Dh.==0).*(Drealage.==30))))
	df = DataFrame(id=Di,age=Dage,realage=Drealage,income=Dincome,cons=Dcons,cash=Dcash,a=Da,save=Dsave,kids=Dis.>1,tau=Dtau,j=Dj,Division=CategoricalArray(Dregname),Division_to=Dtoname,rent=Drent,z=Dz,p=Dp,y=Dy,P=DP,Y=DY,move=DM,moveto=DMt,h=Dh.==1,hh=Dhh.==1,v=Dv,utility=Dutility,maxv = Dmaxv,prob=Dprob,eps_shock=Dshock,cumprob=Dcumprob,wealth=Dwealth,wealth2=Dwealth2,km_distance=Ddist,own=Dh.==1,canbuy=Dcanbuy,cohort=Dcohort,year=Dyear,subsidy=Dsubsidy,own_30=(Dh.==1).*(Drealage.==30),rent_30=(Dh.==0).*(Drealage.==30),drop=DDrop)
	meanz = @by(df,:id,mean_z=mean(:z))
	df = join(df,meanz,on=:id)
	df[:mean_zcat] = cut(df[:mean_z],5,labels=["20","40","60","80","100"])   # period by period z quantile

	# some transformations before exit
	# --------------------------------

	df = join(df,m.agedist,on=:realage)
	wgt = m.regnames[[:j,:prop]]
	wgt[:pop_wgt] = Weights(wgt[:prop])
	@assert isa(wgt[:pop_wgt],AbstractWeights)
	df = join(df,wgt[[:j,:pop_wgt]],on=:j)
	df[:p2y] = df[:p] ./ df[:y]
	df[:p2w] = df[:p] ./ df[:wealth]
	# df = @transform(df,p2y = :p ./ :y, p2w = :p ./ :wealth)
	gc()
	return df
end


# mortgage policy
# subsidy finder
function findSubsidy(y::Float64,age::Int,p::Param,m::Model)
	ret = 0.0
	row = 0
	col = 0

	if p.ages[age] < 34
		row = 1
	else
		row = 2
	end

	if y < 40.0
		col = 2
	elseif y < 75.0
		col = 3
	elseif y < 125.0
		col = 4
	elseif y < 250.0
		col = 5
	else
		col = 6
	end

	ret = m.sinai[row,col]
	return ret
end




# fills arrays with corresponding values at
# discrete state (is,ih,itau,ij,age)
# fills l_vcs with all possible combinations of (ihh,ik)
function fill_interp_arrays!(L::Dict{String,Lininterp},is::Int,ih::Int,itau::Int,ij::Int,age::Int,p::Param,m::Model)

	# L contains:
	# 1. l_vcs
	# 2. l_rho

# # dimvec  = (p.nJ, p.ns, p.nz, p.ny, p.np, p.ntau,  p.na, p.nh,p.nJ, p.nt-1 )
# function idx10(ik::Int,is::Int,iz::Int,iy::Int,ip::Int,itau::Int,ia::Int,ih::Int,ij::Int,age::Int,p::Param)
# 	 r = ik + p.nJ * (is-1 + p.ns * (iz-1 + p.nz * (iy-1 + p.ny * (ip-1 + p.np * (itau-1 + p.ntau * (ia-1 + p.na * (ih-1 + p.nh * (ij-1 + p.nJ * (age-1)))))))))
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
						# CAUTION: we are no longer interpolating the probability function rho but the underlying value function v
						# @inbounds L["l_rho"].vals[ik][arr_idx] = m.rho[rho_idx]
						@inbounds L["l_rho"].vals[ik][arr_idx] = m.v[rho_idx]

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

# fills interp array for expected value function at
# discrete state (is,ihh,itau,ij,age)
# must pass age+1 !
function fill_interp_EV!(L::Lininterp,is::Int,ih::Int,itau::Int,ij::Int,age::Int,p::Param,m::Model)

	# L contains:
	# 1. l_EV

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
					@inbounds L.vals[1][arr_idx] = m.EV[arr_idx]

				end
			end
		end
	end
end


 # r = ik + p.nJ * (is-1 + p.ns * (iz-1 + p.nz * (iy-1 + p.ny * (ip-1 + p.np * (itau-1 + p.ntau * (ia-1 + p.na * (ih-1 + p.nh * (ij-1 + p.nJ * (age-1)))))))))


function get_rho_ktmp(l::Lininterp,azYP::Vector{Float64},p::Param)
	out = zeros(p.nJ)

	getValue!(out,l,azYP,collect(1:p.nJ))
	return out
end

#' ..py:function:: get_vcs(l,azYP,ihh,ik,is,itau,ih,ij,age,p,m)
#' gets vcs (value, consumption, savings) at discrete state (ihh,ik,is,itau,ih,ij,age)
function get_vcs(l::Lininterp,azYP::Vector{Float64},ihh::Int,ik::Int,p::Param)

	out = zeros(3)
	l_idx  = 3*(ik-1 + p.nJ*(ihh-1))

	getValue!(out,l,azYP,[1+l_idx;2+l_idx;3+l_idx])
	# out[2] = getValue(l,azYP,2+l_idx)
	# out[3] = getValue(l,azYP,3+l_idx)
	return out
end

#' ..py:function:: get_vcs(l,azYP,ihh,ik,is,itau,ih,ij,age,p,m)
#' gets vcs (value, consumption, savings) at discrete state (ihh,ik,is,itau,ih,ij,age)
function get_v1v2(l::Lininterp,azYP::Vector{Float64},ik::Int,p::Param)

	out = zeros(2)
	l_idx1  = 3*(ik-1 + p.nJ*(1-1))
	l_idx2  = 3*(ik-1 + p.nJ*(2-1))

	getValue!(out,l,azYP,[1+l_idx1;1+l_idx2])
	return out
end

#' ..py:function:: get_vcs(l,azYP,ihh,ik,is,itau,ih,ij,age,p,m)
#' gets vcs (value, consumption, savings) at discrete state (ihh,ik,is,itau,ih,ij,age)
function get_cs(l::Lininterp,azYP::Vector{Float64},ihh::Int,ik::Int,p::Param)

	out = zeros(2)
	l_idx  = 3*(ik-1 + p.nJ*(ihh-1))

	getValue!(out,l,azYP,[2+l_idx;3+l_idx])	# 2 is index for cons, 3 is index for save
	return out
end



function assignCohort(i::Int,m::Model)
	searchsortedfirst(m.coh_breaks,i)
end



# computing moments from simulation
# takes df: simulation output
function computeMoments(df::DataFrame,p::Param)

	# pdebug("entered computeMoments")

	# keep only relevant years
	# and drop NAs / missing
	df = @where(df,(:year.>1996) .& (.!ismissing.(:cohort)))


	# transformations, adding columns
	# df = @transform(df, agebin = cut(p.ages[:age],int(quantile(p.ages[:age],[1 : ngroups - 1] / ngroups))), age2 = :age.^2)  # cut age into 3 bins, and add age squared
	ngroups = 3
	# df = @transform(df, agebin = cut(:realage,round(Int64,quantile(:realage,collect(1 : ngroups - 1)/ ngroups))), age2 = :age.^2, sell = (:h.==1) & (:hh.==0) )  # cut age into 3 bins, and add age squared
	df[:agebin] = CategoricalArrays.cut(df[:realage],round.(Int64,quantile(df[:realage],collect(1 : ngroups - 1)/ ngroups)),extend=true)
	df[:age2] = df[:age].^2
	df[:sell] = (df[:h].==1) .& (df[:hh].==0)   # cut age into 3 bins, and add age squared

	# df = join(df,m.agedist,on=:realage)
	fullw = Weights(convert(Array,df[:density]))


	# grouped dfs
	g_div = groupby(df, :Division)
	g_kids = groupby(df, :kids)
	g_own = groupby(df, :h)
	g_abin = groupby(df,:agebin)
	g_tau = groupby(df, :tau)  # mover type
	g_tau_own = groupby(df, [:tau,:h])  # mover type

	# pdebug("defined groups")

	# moments relating to homeownership
	# =================================

	noown = sum(df[:h]) == 0.0

	# linear probability model of homeownership
	if mean(df[:h]) == 1.0 || noown
		nm_h  = ["lm_h_(Intercept)","lm_h_age","lm_h_age2"]
		coef_h = missings(Float64,3)
		std_h =  missings(Float64,3)
	else
		try
			lm_h = glm(@formula(h ~ age + age2) ,df,Normal(),IdentityLink())
			cc_h  = coeftable(lm_h)
			nm_h  = String["lm_h_" *  convert(String,cc_h.rownms[i]) for i=1:length(cc_h.rownms)]
			coef_h = coef(lm_h)
			std_h =  stderr(lm_h)
		catch
			nm_h  = ["lm_h_(Intercept)","lm_h_age","lm_h_age2"]
			coef_h = missings(Float64,3)
			std_h =  missings(Float64,3)
		end
	end

	xx = @linq df |>
		   @where(:age.==31) |>
		   @select(moment="mean_sell_50",model_value=mean(:sell))
	mom1 = deepcopy(xx)

	# unconditional mean of owning
	# ----------------------------

	push!(mom1,["mean_own",mean(df[:h],fullw)])
	covar = mean(df[:h],fullw)

	# own ~ Type
	# ----------

	d_tau = Dict()
	for tau in g_tau
		w = Weights(tau[:density])
		d_tau[Symbol("mean_own_$(tau[1,:tau])")] = mean(tau[:h],w)
	end

	# own ~ Division
	# ----------

	for div in g_div
		w = Weights(div[:density])
		push!(mom1,["mean_own_$(div[1,:Division])",mean(div[:h],w)])
	end

	# own ~ kids
	# ----------

	for div in g_kids
		kk = "$(div[1,:kids])"
		w = mig.Weights(div[:density])
		push!(mom1,["mean_own_kids$(uppercase(kk))",mean(div[:h],w)])
		# println("mean_own_kids$(uppercase(kk)) = $(mean(div[:h],w))")
	end
	# TODO std error
	covar = cov(hcat(df[:h],df[:kids]),fullw,corrected=false)
	push!(mom1,["cov_own_kids",covar[1,2]])

	gc()

	# moments relating to mobility
	# ============================

	nomove = false

	# linear probability model of mobility
	if sum(df[:move]) == 0.0
		nomove = true
		nm_mv  = ["lm_mv_(Intercept)","lm_mv_age","lm_mv_age2"]
		coef_mv = missings(zeros(3))
		std_mv =  missings(ones(3))
	else
		lm_mv = glm( @formula(move ~ age + age2),df,Normal(),IdentityLink())
		cc_mv = coeftable(lm_mv)
		nm_mv = String["lm_mv_" * convert(String,cc_mv.rownms[i]) for i=1:length(cc_mv.rownms)]
		coef_mv = coef(lm_mv)
		std_mv = stderr(lm_mv)
	end

	# move count
	# ----------

	movecount=by(df,:id,x -> sum(x[:move]))
	moved0 = mean(movecount[:x1].==0)
	moved1 = mean(movecount[:x1].==1)
	moved2plus = mean(movecount[:x1].>=2)

	# TODO std error
	push!(mom1,["mean_move",mean(df[:move],fullw)])	# unconditional mean
	push!(mom1,["moved0",moved0])
	push!(mom1,["moved1",moved1])
	push!(mom1,["moved2plus",moved2plus])

	xx = @linq df |>
		   @where(:age.==31) |>
		   @select(moment="mean_move_50",model_value=mean(:move))
	append!(mom1,xx)

	# 

	# flows of moves: where do moves go to?
	# -------------------------------------
	xx = StatsBase.proportionmap(df[ df[:move],:Division_to] )
	zz = DataFrame(moment=map(string,map(x->"flow_move_to_$x",collect(keys(xx)))), model_value = collect(values(xx)))
	append!(mom1,zz)

	gc()

	# move ~ type

	for tau in g_tau
		w = Weights(tau[:density])
		d_tau[Symbol("mean_move_$(tau[1,:tau])")] = mean(tau[:move],w)
	end
	for tau in g_tau_own
		kk = "$(tau[1,:own])"
		w = Weights(tau[:density])
		d_tau[Symbol("mean_move_own$(uppercase(kk))_$(tau[1,:tau])")] = mean(tau[:move],w)
	end

	# move ~ own
	# ----------

	if noown
		push!(mom1,["mean_move_ownTRUE",0.0])
		push!(mom1,["mean_move_ownFALSE",1.0])
	elseif mean(df[:own]) == 1.0
		push!(mom1,["mean_move_ownTRUE",1.0])
		push!(mom1,["mean_move_ownFALSE",0.0])
	else
		for idf in g_own
			kk = "$(idf[1,:own])"
			w = Weights(idf[:density])
			push!(mom1,["mean_move_own$(uppercase(kk))",mean(idf[:move],w)])
		end

	end
	# TODO std error
	covar = cov(hcat(df[:h],df[:move]),fullw,corrected=false)
	push!(mom1,["cov_move_h",covar[1,2]])



	# move ~ kids
	# ----------

	for idf in g_kids
		kk = "$(idf[1,:kids])"
		w = Weights(idf[:density])
		push!(mom1,["mean_move_kids$(uppercase(kk))",mean(idf[:move],w)])
	end
	# TODO std error
	covar = cov(hcat(df[:move],df[:kids]),fullw,corrected=false)
	push!(mom1,["cov_move_kids",covar[1,2]])

	gc()
	# move ~ distance
	# ---------------

	if nomove
		push!(mom1,["q25_move_distance",0.0])
		push!(mom1,["q50_move_distance",0.0])
		push!(mom1,["q75_move_distance",0.0])
	else
		qts = quantile(df[df[:move].==true,:km_distance],[0.25,0.5,0.75])
		push!(mom1,["q25_move_distance",qts[1]])
		push!(mom1,["q50_move_distance",qts[2]])
		push!(mom1,["q75_move_distance",qts[3]])
	end

	# move | negative equity
	# ----------------------

	# if noown || nomove
		push!(mom1,["move_neg_equity",0.0])
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
	# 	nm_w  = String["lm_w_" *  convert(String,cc_w.rownms[i]) for i=1:size(cc_w.rownms,1)]
	# 	coef_w = @data(coef(lm_w))
	# 	std_w = @data(stderr(lm_w))
	# end

	# wealth ~ age
	# ------------
	for idf in g_abin
		push!(mom1,["mean_wealth_$(idf[1,:agebin])",mean(idf[:wealth])])
	end

	# wealth ~ division
	# -----------------

	for idf in g_div
		w = Weights(idf[:density])
		push!(mom1,["mean_wealth_$(idf[1,:Division])",mean(idf[:wealth],w)])
	end

	# wealth ~ own
	# ------------

	if noown

		push!(mom1,["mean_wealth_ownTRUE",0.0,0.0])
		push!(mom1,["mean_wealth_ownFALSE",mean(df[:wealth],fullw)])
	else
		for idf in g_own
			w = Weights(idf[:density])
			kk = "$(idf[1,:own])"
			push!(mom1,["mean_wealth_own$(uppercase(kk))",mean(idf[:wealth],w)])
		end
	end
	gc()

	# pdebug("finisehd moments")

	# collect estimates
	# =================


	# nms = vcat(nm_mv,nm_h,nm_w)
	nms = vcat(nm_mv,nm_h)

	# get rid of parens and hyphens
	# TODO get R to export consitent names with julia output - i'm doing this side here often, not the other one
	# for i in 1:length(nms)
	# 	ss = replace(nms[i]," - ","")
	# 	ss = replace(ss,")","")
	# 	ss = replace(ss,"(","")
	# 	ss = replace(ss,",","_")
	# 	# ss = replace(ss,"kidstrue","kidsTRUE")
	# 	ss = replace(ss,"owntrue","ownTRUE")
	# 	nms[i] = ss
	# end

	mom2 = DataFrame(moment = nms, model_value = [coef_mv;coef_h])

	dfout = vcat(mom1,mom2)
	nms = dfout[:moment]

	for i in 1:length(nms)
		ss = replace(nms[i]," - ","")
		ss = replace(ss,"]","")
		ss = replace(ss,"[","")
		ss = replace(ss,")","")
		ss = replace(ss,"(","")
		ss = replace(ss,",","_")
		# ss = replace(ss,"kidstrue","kidsTRUE")
		ss = replace(ss,"owntrue","ownTRUE")
		nms[i] = ss
	end
	dfout[:moment] = nms

	# compute estimates by year
	yearly = @by(df,[:year; :Division],meany = mean(:income), meanc = mean(:cons), means = mean(:save),meanw = mean(:wealth))

	# 	push!(dfs,vcat(mom1,mom2))

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


	return Dict("moments" => dfout, "yearly" => yearly, "tau" => d_tau)
end



# select inds from sim dataframe
function getID(df::DataFrame,id::Array{Int,1})
	df[findin(df[:id],id),:]
end






# setting up the FSpace objects for simulation
# if Sys.OS_NAME == :Darwin
# 	using ApproXD
# end
# function setupFSpaceXD(m::Model,p::Param)

# 	ndims = 4 # number of cont dimensions

# 	# ordering of continuous dims
# 	# 1) a
# 	# 2) y
# 	# 3) p
# 	# 4) z

# 	# the return object: a dict of collections of fspaces.
# 	fx = Dict{String,Dict{Integer,FSpaceXD}}()
# 	fx["rho"] = Dict{Integer,FSpaceXD}()
# 	fx["vh"]  = Dict{Integer,FSpaceXD}()
# 	fx["ch"]  = Dict{Integer,FSpaceXD}()
# 	fx["sh"]  = Dict{Integer,FSpaceXD}()

# 	points = Dict{Int,Dict{Integer,Array}}()
# 	bounds = Dict{Int,Dict{Integer,Array}}()
# 	bsp = Dict{Int,Dict{Integer,BSpline}}()

# 	# full basis to compute inverses
# 	d = Dict{Int,Dict{Integer,Matrix}}()
# 	id = Dict{Int,Dict{Integer,Array{Float64,2}}}()
# 	# bsp[5] = BSpline(m.nknots["age"],m.degs["age"],bounds[5][1],bounds[5][2])

# 	for ij   = 1:p.nJ

# 		points[ij] = Dict{Integer,Array}()
# 		bounds[ij] = Dict{Integer,Array}()
# 		bsp[ij] = Dict{Integer,BSpline}()

# 		# full basis to compute inverses
# 		d[ij] = Dict{Integer,Matrix}()
# 		id[ij] = Dict{Integer,Array{Float64,2}}()

# 		points[ij][1] = m.grids["assets"]
# 		bounds[ij][1] = [m.grids["assets"][1],m.grids["assets"][end]]
# 		# points[5] = linspace(1.0,p.nt-1,p.nt-1)
# 		# bounds[5] = [1.0,convert(Float64,p.nt-1)]

# 		# construct asset basis with custom knots
# 		# bsp[1] = BSpline(m.knots["assets"],m.degs["assets"])
# 		bsp[ij][1] = BSpline(m.nknots["assets"],m.degs["assets"],bounds[ij][1][1],bounds[ij][1][2])
# 		points[ij][2] = m.gridsXD["y"][:,ij]
# 		points[ij][3] = m.gridsXD["p"][:,ij]
# 		points[ij][4] = m.gridsXD["zsupp"][:,ij]

# 		bounds[ij][2] = [ points[ij][2][1],points[ij][2][end] ]
# 		bounds[ij][3] = [ points[ij][3][1],points[ij][3][end] ]
# 		bounds[ij][4] = [ points[ij][4][1],points[ij][4][end] ]

# 		bsp[ij][2] = BSpline(m.nknots["y"],m.degs["y"],bounds[ij][2][1],bounds[ij][2][2])
# 		bsp[ij][3] = BSpline(m.nknots["p"],m.degs["p"],bounds[ij][3][1],bounds[ij][3][2])
# 		bsp[ij][4] = BSpline(m.nknots["z"],m.degs["z"],bounds[ij][4][1],bounds[ij][4][2])


# 		# get full basis for inverses
# 		for i=1:ndims
# 			d[ij][i] = full(getBasis(points[ij][i],bsp[ij][i]))
# 		end
# 		for k in collect(keys(d[ij]))
# 			id[ij][k] = inv(d[ij][k])
# 		end


# 		for itau = 1:p.ntau
# 		for ih   = 1:2
# 		for is   = 1:p.ns
# 		for ik   = 1:p.nJ
# 		for age  = 1:p.nt-1

# 			# get FSpace for rho
# 			# ------------------

# 			rhotmp = get_cont_vals(ik,is,ih,itau,ij,age,m.rho,p)
# 			mycoef1 = getTensorCoef(id[ij],rhotmp)
# 			rhoidx = fx_idx_rho(ik,is,ih,itau,ij,age,p)
# 			# pdebug("at index $rhoidx")
# 			fx["rho"][rhoidx] = FSpaceXD(ndims,mycoef1,bsp[ij])


# 			for ihh  = 1:2


# 				# get FSpace for vh, ch and sh
# 				# ----------------------------

# 				# vh
# 				vtmp = get_cont_vals(ihh,ik,is,ih,itau,ij,age,m.vh,p)
# 				mycoef = getTensorCoef(id[ij],vtmp)

# 				idx = fx_idx(ihh,ik,is,ih,itau,ij,age,p)
# 				# pdebug("doing state $idx")
# 				fx["vh"][idx] = FSpaceXD(ndims,mycoef,bsp[ij])

# 				# ch
# 				vtmp = get_cont_vals(ihh,ik,is,ih,itau,ij,age,m.ch,p)
# 				mycoef = getTensorCoef(id[ij],vtmp)

# 				idx = fx_idx(ihh,ik,is,ih,itau,ij,age,p)
# 				fx["ch"][idx] = FSpaceXD(ndims,mycoef,bsp[ij])

# 				# sh                 ihh,ik,is,ih,itau,ij,it,m.vh,p
# 				vtmp = get_cont_vals(ihh,ik,is,ih,itau,ij,age,m.sh,p)
# 				mycoef = getTensorCoef(id[ij],vtmp)

# 				idx = fx_idx(ihh,ik,is,ih,itau,ij,age,p)
# 				fx["sh"][idx] = FSpaceXD(ndims,mycoef,bsp[ij])

# 			end

# 		end
# 		end
# 		end
# 		end
# 		end

# 	end

# 	return fx

# end


# # FSpace Indexer functions
# function fx_idx_rho(ik::Int,is::Int,ih::Int,itau::Int,ij::Int,age::Int,p::Param)
# 	ik + p.nJ * (is + p.ns * (ih + p.nh * (itau + p.ntau * (ij + p.nJ*(age-1)-1)-1)-1)-1)
# end

# function fx_idx(ihh::Int,ik::Int,is::Int,ih::Int,itau::Int,ij::Int,age::Int,p::Param)

# 	ihh + p.nh * (ik + p.nJ * (is + p.ns * (ih + p.nh * (itau + p.ntau * (ij + p.nJ*(age-1)-1)-1)-1)-1)-1)
# end

# function fx_idx_cont(ia::Int,iy::Int,ip::Int,iz::Int,p::Param)
# 	ia + p.na * (iy + p.ny * (ip + p.np * (iz-1) -1) -1)
# end



# # get values in continuous dims at given discrete state
# # method for idx11
# function get_cont_vals(ihh::Int,ik::Int,is::Int,ih::Int,itau::Int,ij::Int,age::Int,v::Array,p::Param)
# # function get_cont_vals(ihh::Int,ik::Int,is::Int,ih::Int,itau::Int,ij::Int,v::Array,p::Param)

# 	# vout = zeros(p.na*p.np*p.ny*p.nz*(p.nt-1))
# 	vout = zeros(p.na*p.np*p.ny*p.nz)

# 	for ia = 1:p.na
# 	for iy = 1:p.ny
# 	for ip = 1:p.np
# 	for iz = 1:p.nz
# 	# for it = 1:p.nt-1

# 		@inbounds vout[fx_idx_cont(ia,iy,ip,iz,p)] = v[idx11(ihh,ik,is,iz,iy,ip,ia,ih,itau,ij,age,p)]

# 	# end
# 	end
# 	end
# 	end
# 	end

# 	return vout
# end


# # method for idx10
# function get_cont_vals(ik::Int,is::Int,ih::Int,itau::Int,ij::Int,age::Int,v::Array,p::Param)
# # function get_cont_vals(ik::Int,is::Int,ih::Int,itau::Int,ij::Int,v::Array,p::Param)

# 	vout = zeros(p.na*p.np*p.ny*p.nz)

# 	for ia = 1:p.na
# 	for iy = 1:p.ny
# 	for ip = 1:p.np
# 	for iz = 1:p.nz
# 	# for it = 1:p.nt-1

# 		# vout[fx_idx_cont(ia,ip,iy,iz,it,p)] = v[idx10(ik,is,iz,iy,ip,ia,ih,itau,ij,it,p)]
# 		@inbounds vout[fx_idx_cont(ia,iy,ip,iz,p)] = v[idx10(ik,is,iz,iy,ip,ia,ih,itau,ij,age,p)]

# 	# end
# 	end
# 	end
# 	end
# 	end

# 	return vout
# end




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
