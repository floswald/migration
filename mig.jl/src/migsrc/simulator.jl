


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









# simulator

function simulate(m::Model,p::Param)

	T = p.nt-1

	# set random seed
	srand(p.rseed)

	# compute approximating coefficients from
	# *) m.vh
	# *) m.rho
	# *) m.sh
	# *) m.ch
	# at each discrete index (age,is,ij,ij,tau,ih,ihh)
	# 
	# continuous variables are (a,z,y,p). for those compute a constant basis function
	# on nx evaluation points each. get inverse of those and store in ibm dict
	#
	# then for each i in prod(age,is,ij,ij,tau,ih,ihh), there is a different section of the function you want to approximate and therefore a different approximation coefficient vector
	# for any function f you want to approx:
	# coeff_mat_f = zeros(n_cont_coefs,n_discrete_states)
	# for i in discrete_states
	# 	  ftemp = get_cont_vals(f,i)
	#     coeff_mat_f[:,i] = getTensorCoef(ibm,ftemp)
	# end
	#
	# in simulation
	# =============
	#
	# you enter the period on state idx in {age,is,ij,ij,tau,ih,ihh}
	# for each cont fuction f_j there is an FspaceXD object
	# this contains the coeff_mat_f_j and the 4 univariate spline objects
	
	# 2 methods
	# 1. setindex(fx::FSpaceXD,idx::Int) set the current index
	# 2. getValue(point, fx::FSpaceXD). computes each individual basis_function[i] at point[i], using the corresponding coefficient vector coeff_mat_f[:,idx]
	# and then puts it into evalTensor4 to obtain the function value.

	# grids
	agrid = m.grids["assets"]
	ygrid = m.gridsXD["y"]
	pgrid = m.gridsXD["p"]
	zgrid = m.gridsXD["z"]

	# drawing the shocks 
	# ==================

	# initial distributions
	# TODO
	# all of those should be non-uniform probably
	G0tau = Categorical([1-p.taudist, p.taudist])	# type distribution
	G0z   = Categorical([1/p.nz for i=1:p.nz])
	G0y   = Categorical([1/p.ny for i=1:p.ny])
	G0p   = Categorical([1/p.np for i=1:p.np])
	# G0P   = Categorical([1/p.nP for i=1:p.nP])
	x     = [1/i^2 for i=1:length(m.aone:p.na)]
	x     = x / sum(x)
	G0a   = Categorical(x)
	G0j   = Categorical(array(m.regnames[:prop]))	# TODO popdist
	G0k   = Categorical([0.6,0.4])  # 40% of 21-year olds have kids in SIPP

	# prepare cumsum of probability matrices
	# cumrho = cumsum(m.rho,1)	# get cumulative prob of moving along dim k
	# cumGP  = cumsum(m.grids2D["GP"],2)	# transition matrices cumulate over dim 2
	cumGp  = cumsum(m.gridsXD["Gy"],2)
	cumGy  = cumsum(m.gridsXD["Gp"],2)
	cumGz  = cumsum(m.gridsXD["Gz"],2)
	# cumGzM = cumsum(m.gridsXD["GzM"],2)
	cumGs  = cumsum(m.gridsXD["Gs"],2)

	# storage
	Dt      = zeros(Int,p.nsim*(T))	# age
	Di      = zeros(Int,p.nsim*(T))	# identity
	Dv      = zeros(p.nsim*(T))	# value
	Dc      = zeros(p.nsim*(T))	# consu
	Dcash   = zeros(p.nsim*(T))	# consu
	Da      = zeros(p.nsim*(T))	# asset value
	Dy      = zeros(p.nsim*(T))	# region y value
	Dz      = zeros(p.nsim*(T))	# income shock value
	Dincome = zeros(p.nsim*(T))	# income value
	Dp      = zeros(p.nsim*(T))	# house price value
	Dj      = zeros(Int,p.nsim*(T))	# location index
	Dh      = zeros(Int,p.nsim*(T))	# housing state
	Dhh     = zeros(Int,p.nsim*(T))	# housing choice
	# DiP     = zeros(Int,p.nsim*(T))	# macro P index
	Dip     = zeros(Int,p.nsim*(T))	# region p index
	Diy     = zeros(Int,p.nsim*(T))	# region y index
	# DiS     = zeros(Int,p.nsim*(T))	# savings index
	DS     = zeros(p.nsim*(T))	# savings values
	Diz     = zeros(Int,p.nsim*(T))	# z index
	DM      = zeros(Int,p.nsim*(T))	# move
	DMt     = zeros(Int,p.nsim*(T))	# move
	Dkids   = zeros(Int,p.nsim*(T))	# kids yes/no
	Ddist   = zeros(p.nsim*(T))
	Dtau    = zeros(Int,p.nsim*(T))
	Dcanbuy   = zeros(Int,p.nsim*(T))

	ktmp = zeros(Float64,p.nJ)
	ktmp_test = zeros(Float64,p.nJ)
	ktmp2 = zeros(p.nJ)

	avec = zeros(p.na)
	avec2= zeros(p.na)
	fvec = falses(p.na)
	fvec2= falses(p.na)
	fvecs = (falses(p.na),falses(p.na))
	asum = 0.0

	v1tmp = 0.0
	v2tmp = 0.0

	ss = 0.0
	yy = 0.0

	# begin simulation loop
	# =====================

	# @inbounds begin
	for i = 1:p.nsim

		is   = rand(G0k)
		iy   = rand(G0y)
		# ip   = rand(G0p)
		ip   = 2
		# iP   = rand(G0P)
		# iz   = rand(G0z)
		iz   = convert(Int,floor(median([1:p.nz])))	#everybody gets median income
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

			# record beginning of period state
			# --------------------------------

			yy = income(zgrid[ iz,ij,age ], ygrid[iy,ij])

			# flag for downpayment constraint
			canbuy = a + yy > p.chi * m.gridsXD["p"][ip,ij]

			Dj[age + T*(i-1)]      = ij
			Dt[age + T*(i-1)]      = age
			Dtau[age + T*(i-1)]    = itau
			Di[age + T*(i-1)]      = i
			Dh[age + T*(i-1)]      = ih
			Da[age + T*(i-1)]      = a
			Dy[age + T*(i-1)]      = ygrid[iy,ij]	# SLOW
			Dz[age + T*(i-1)]      = zgrid[iz,ij,age]
			Dp[age + T*(i-1)]      = pgrid[ip,ij]
			Dincome[age + T*(i-1)] = yy
			Diz[age + T*(i-1)]     = iz
			Dip[age + T*(i-1)]     = ip
			Diy[age + T*(i-1)]     = iy
			Dkids[age + T*(i-1)]   = is-1
			Dcanbuy[age + T*(i-1)]   = canbuy


			# get value for each location
			for ik in 1:p.nJ

				# interpolate rho function in asset dim
				for iia in 1:p.na
					avec[iia] = m.rho[idx10(ik,is,iy,ip,iz,iia,ih+1,itau,ij,age,p)]
				end

				# there is an approximation issue here:
				# sometimes the approximated vector of probabilities does not sum to 1
				# this happens because rho is poorly approximated
				# I know that the true rho (on the grid) does sum to 1
				# workaround: if I find a vector whose sum deviates significantly from one
				# i just rescale it, assuming that the relative weights are in line with rho
			
				# evaluate at "a"
				# debug code
				# -----------
				# at grid point closest to a
				# imya = find(abs(m.grids["assets"] .- a) .== minabs(m.grids["assets"] .- a))[1]
				# mya = m.grids["assets"][imya]
				# ktmp[ik] = m.rho[idx10(ik,is,iy,ip,iz,imya,ih+1,itau,ij,age,p)]

				# approximate rho at "a" off grid
				ktmp[ik] = linearapprox(agrid,avec,a,1,p.na)[1]
			end

			# check quality of approximation off grid
			# ---------------------------

			# if abs(sum(ktmp_test) - 1.0) > 0.001
			# 	println("--------------------------------------")
			# 	println("sum(ktmp_off_grid) = $(sum(ktmp_test))")
			# 	println("ktmp_off_grid/sum(ktmp_off_grid) = $(ktmp_test/sum(ktmp_test))")
			# 	println("sum(ktmp_test/sum(ktmp_test)) = $(sum(ktmp_test/sum(ktmp_test)))")
			# 	println("----------------------------------------------------------------")
			# end------------

			# normalizing vector of moving probs
			if abs(sum(ktmp) - 1.0) > 0.001
				ktmp = ktmp ./ sum(ktmp)
			end
			
			# get cumulative prob
			cumsum!(ktmp2,ktmp,1)
			# throw a k-dice 
			moveto = searchsortedfirst(ktmp2,rand())
			move = ij != moveto

			# debugging
			# if moveto < 1 || moveto > p.nJ
			# 	println("ik=$moveto,iy=$iy,ip=$ip,iz=$iz,ih=$ih,ihh=$ihh,itau=$itau,ij=$ij,age=$age,id=$i,move=$move")
			# 	println("id = $i")
			# 	println("a = $a")
			# 	println("mya = $mya")
			# 	println("ij= $ij")
			# 	println("itau= $itau")
			# 	println(ktmp)
			# 	println(ktmp2)
			# 	println(m.rho[:,is,iy,ip,iz,imya,ih+1,itau,ij,age][:])
			# 	println(size(m.rho[:,is,iy,ip,iz,:,ih+1,itau,ij,age]))
			# 	println(reshape(m.rho[:,is,iy,ip,iz,:,ih+1,itau,ij,age],p.nJ,p.na))
			# 	for iage in 1:age
			# 		println(Da[iage + T*(i-1)])
			# 	end
			# end

			if move

				ihh = 0
				for iia in 1:p.na
					avec[iia] = m.vh[idx11(1,moveto,is,iy,ip,iz,iia,ih+1,itau,ij,age,p)]
					fvec[iia] = m.vfeas[idx11(1,moveto,is,iy,ip,iz,iia,ih+1,itau,ij,age,p)]
				end
				val = linearapprox(agrid,avec,a,fvec,p)[1]
				# find consumption and savings
				for iia in 1:p.na
					avec[iia]  = m.sh[idx11(ihh+1,moveto,is,iy,ip,iz,iia,ih+1,itau,ij,age,p)]
					avec2[iia] = m.ch[idx11(ihh+1,moveto,is,iy,ip,iz,iia,ih+1,itau,ij,age,p)]
				end

				ss                   = linearapprox(agrid,avec,a,fvec,p)[1] 
				Dc[age + T*(i-1)]    = linearapprox(agrid,avec2,a,fvec,p)[1]
					if (i==1) & (age==8)
						println("you are in MOVE")
						println("fvec = $fvec")
						println("avec = $avec")
						println("avec2 = $avec2")
						println("val = $val")
						println("ss = $ss")
						println("a = $a")
					end
			
			else # stay

				# you are current owner or you can buy
				if (ih==1 || (ih==0 && canbuy))

					# find housing choice
					for iia in 1:p.na
						avec[iia]  = m.vh[idx11(1,moveto,is,iy,ip,iz,iia,ih+1,itau,ij,age,p)]
						avec2[iia] = m.vh[idx11(2,moveto,is,iy,ip,iz,iia,ih+1,itau,ij,age,p)]
						fvec[iia]  = m.vfeas[idx11(1,moveto,is,iy,ip,iz,iia,ih+1,itau,ij,age,p)]
						fvec2[iia] = m.vfeas[idx11(2,moveto,is,iy,ip,iz,iia,ih+1,itau,ij,age,p)]
					end
					# if either of a[low] and a[high] are NA, approximation must return NA
					v1tmp = linearapprox(agrid,avec,a,fvec,p)[1]
					v2tmp = linearapprox(agrid,avec2,a,fvec2,p)[1]
					ihh = v1tmp > v2tmp ? 0 : 1
					val = v1tmp > v2tmp ? v1tmp : v2tmp
					# find consumption and savings
					for iia in 1:p.na
						avec[iia]  = m.sh[idx11(ihh+1,moveto,is,iy,ip,iz,iia,ih+1,itau,ij,age,p)]
						avec2[iia] = m.ch[idx11(ihh+1,moveto,is,iy,ip,iz,iia,ih+1,itau,ij,age,p)]
					end

					fvecs = (fvec,fvec2)

					ss                   = linearapprox(agrid,avec,a,fvecs[ihh+1],p)[1] 	# false here for NOT linearly extrapolating
					Dc[age + T*(i-1)]    = linearapprox(agrid,avec2,a,fvecs[ihh+1],p)[1]
					if (i==1) & (age==8)
						println("you are in STAY and can buy")
						println("fvec = $fvec")
						println("avec = $avec")
						println("avec2 = $avec2")
						println("val = $val")
						println("ss = $ss")
						println("a = $a")
					end

				# current renter who cannot buy
				else
					ihh = 0
					for iia in 1:p.na
						avec[iia]  = m.vh   [idx11(ihh+1,moveto,is,iy,ip,iz,iia,ih+1,itau,ij,age,p)]
						fvec[iia]  = m.vfeas[idx11(ihh+1,moveto,is,iy,ip,iz,iia,ih+1,itau,ij,age,p)]
					end
					val = linearapprox(agrid,avec,a,fvec,p)[1]
					# find consumption and savings
					for iia in 1:p.na
						avec[iia]  = m.sh[idx11(ihh+1,moveto,is,iy,ip,iz,iia,ih+1,itau,ij,age,p)]
						avec2[iia] = m.ch[idx11(ihh+1,moveto,is,iy,ip,iz,iia,ih+1,itau,ij,age,p)]
					end

					ss                   = linearapprox(agrid,avec, a,fvec,p)[1] 	# false here for NOT linearly extrapolating
					Dc[age + T*(i-1)]    = linearapprox(agrid,avec2,a,fvec,p)[1]
					if (i==1) & (age==8)
						println("you are in STAY and canNOT buy")
						println("fvec = $fvec")
						println("avec = $avec")
						println("avec2 = $avec2")
						println("val = $val")
						println("ss = $ss")
						println("a = $a")
					end
				end
			end

			# record current choices
			# ----------------------

			DS[age + T*(i-1)]    = ss
			Dcash[age + T*(i-1)] = cashFunction(a,income(zgrid[ iz,ij,age ], ygrid[iy,ij]),ih,ihh,m.gridsXD["p"][ip,moveto],move,moveto,p)

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

			# iP = searchsortedfirst( cumGP[iP,:][:] , rand() ) 	# SLOW, all of them
			ip = searchsortedfirst( cumGp[ip,:,moveto][:], rand() )
			iy = searchsortedfirst( cumGy[iy,:,moveto][:], rand() )
			iz = searchsortedfirst( cumGz[iz,:,moveto][:], rand() )
			is = searchsortedfirst( cumGs[is,:,age][:], rand() )

		end	# age t

	end	# individual i

	# end # inbounds

	# collect all data into a dataframe
	w = (Dp .* Dh) .+ Da

	df = DataFrame(id=Di,age=Dt,age2=Dt.^2,kids=PooledDataArray(convert(Array{Bool,1},Dkids)),j=Dj,a=Da,save=DS,c=Dc,cash=Dcash,iz=Diz,ip=Dip,iy=Diy,p=Dp,y=Dy,income=Dincome,move=DM,moveto=DMt,h=Dh,hh=Dhh,v=Dv,wealth=w,km_distance=Ddist,km_distance2=Ddist.^2,own=PooledDataArray(convert(Array{Bool,1},Dh)),canbuy=Dcanbuy)
	df = join(df,m.regnames,on=:j)
	sort!(df,cols=[1,2]	)

	return df
end



# computing moments from simulation
function computeMoments(df::DataFrame,p::Param,m::Model)

	# moved0
	# moved1
	# moved2

	# linear probability model of mobility
	# ====================================
	# move ~ age + age^2 + dist + dist2 + own + kids
	lm_mv = fit(LinearModel, move ~ age + age2 + km_distance + km_distance2 + kids + own,df)


	# linear probability model of homeownership
	# =========================================
	lm_h = fit(LinearModel, h ~ age + age2 + Division + kids,df)


	# linear regression of total wealth
	# =================================
	lm_w = fit(LinearModel, wealth ~ age + age2 + own + Division,df )

	# collect estimates
	# =================
	cc_mv = coeftable(lm_mv)
	cc_h  = coeftable(lm_h)
	cc_w  = coeftable(lm_w)

	nm_mv = ASCIIString["lm_mv_" * convert(ASCIIString,cc_mv.rownms[i]) for i=1:size(cc_mv.mat,1)] 
	nm_h  = ASCIIString["lm_h_" *  convert(ASCIIString,cc_h.rownms[i]) for i=1:size(cc_h.mat,1)] 
	nm_w  = ASCIIString["lm_w_" *  convert(ASCIIString,cc_w.rownms[i]) for i=1:size(cc_w.mat,1)] 

	nms = vcat(nm_mv,nm_h,nm_w)

	# get rid of parens and hyphens
	# TODO get R to export consitent names with julia output - i'm doing this side here often, not the other one
	for i in 1:length(nms)
		ss = replace(nms[i]," - ","")
		ss = replace(ss,")","")
		ss = replace(ss,"(","")
		ss = replace(ss,"kidstrue","kidsTRUE")
		ss = replace(ss,"owntrue","ownTRUE")
		nms[i] = ss
	end


	dfout = DataFrame(moment = nms, model_value = DataArray([coef(lm_mv),coef(lm_h),coef(lm_w)]), model_sd = DataArray([stderr(lm_mv),stderr(lm_h),stderr(lm_w)]))

	return dfout



	# # overall ownership rate
	# own = mean(df[: ,:h])

	# # ownership by age?
	# ownage = by(df, :age, d -> mean(d[:h]))

	# # find all movers, i.e. people who move at least once
	# movers = unique(df[df[:,:move].==true, :id])
	# # fraction of people who never move
	# nomove = 1 - length(movers) / p.nsim

	# # fraction of people who move twice/more
	# x = by(df[df[:move].==true,:], :id, d -> DataFrame(N=size(d,1)))    
	# nummoves = by(x,:N,d -> DataFrame(Num=size(d,1))) 

	# # moving rate by ownership
	# movebyh = DataFrames.by(df, :h, d -> DataFrames.DataFrame(moverate=mean(d[:move])))

	# # moving rate by age
	# movebyage = by(df, :age, d -> DataFrame(moverate=mean(d[:move])))

	# # assets by ownership and age
	# # aggregate in 5-year bins
	
	# # assets_hage = by(df, [:age, :h], d -> DataFrame(assets=mean(d[:,:a])))


	# assets_h = by(df,  :h, d -> DataFrame(assets=mean(d[:a])))
	# assets_age = by(df,  :age, d -> DataFrame(assets=mean(d[:a])))
	# assets_hage = by(df, [ :h, :age] , d -> DataFrame(assets=mean(d[:a])))

	# # # autocorrelation of income by region
	# # # make a lagged income for each id
	# # ly = df[:,[:j, :id ,:age, :y]]
	# # ly = hcat(ly,DataFrame(Ly=@data([0.0 for i in 1:nrow(ly)])))
	# # for i in 1:nrow(ly)
	# # 	if ly[i,:age] == 1 
	# # 		ly[i,:Ly] = NA
	# # 	else
	# # 		ly[i,:Ly] = ly[i-1,:y]
	# # 	end
	# # end

	# # rhos = by( ly[ly[:age].!=1,:], :j, d -> DataFrame(rho = cor(d[:y],d[:Ly])))

	# # equity
	# equity = by(df,:age,d->DataFrame(equity=mean(d[:,:eq])))

	# # fraction of people who move with neg equity

	# out = ["own" => own, "ownage" => ownage, "nomove" => nomove, "nummoves" => nummoves, "movebyh" => movebyh, "movebyage" => movebyage, "assets_h" => assets_h, "assets_age" => assets_age, "assets_hage" => assets_hage,"rhos" => rhos, "equity" => equity]

	# return out

end





























	