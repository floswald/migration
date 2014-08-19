

# solving the model at the current 
# parameter vector

# main loop
function solve!(m::Model, p::Param)

	# make sure gamma got updated
	p.mgamma  = 1.0-p.gamma
	p.imgamma = 1.0/p.mgamma
	# set v to na
	fill!(m.v,p.myNA)

	# final period
	solveFinal!(m,p)
	# loop over time
	for age=(p.nt-1):-1:1

		if p.verbose > 0 
			info("solving period $age")
		end

		# 	# compute current period values
		solvePeriod!(age,m,p)

	end

	return nothing

end


# auxiliary functions
# ===================


function solveFinal!(m::Model,p::Param)

	# extract grids for faster lookup
	agrid = m.grids["assets"]
	hgrid = m.grids["housing"]
	# loop over all states
	for ih = 1:p.nh
	for ij = 1:p.nJ
	for ip = 1:p.np 

	for ia = m.aone:p.na

		if ia == m.aone
			tmp1 = p.omega1 + p.omega2 * log(agrid[ia+1] + hgrid[ih] * (m.gridsXD["p"][ip,ij] ) )
			tmp2 = p.omega1 + p.omega2 * log(agrid[ia+2] + hgrid[ih] * (m.gridsXD["p"][ip,ij] ) )

			m.EVfinal[ia,ih,ip,ij] = tmp1 + (tmp2-tmp1) * (agrid[ia] - agrid[ia+1]) / agrid[ia+2] - agrid[ia+1]
	
		elseif ia > m.aone

			m.EVfinal[ia,ih,ip,ij] = p.omega1 + p.omega2 * log(agrid[ia] + hgrid[ih] * (m.gridsXD["p"][ip,ij] ) )
		end
	end

	# linear approximation below 0 or p.NA?
	# truth is that this section of assets can never be chosen anyway
	# y1 = m.EVfinal[m.aone,ih,ip,ij]
	# y2 = m.EVfinal[m.aone+1,ih,ip,ij]
	for ia = 1:(m.aone-1)
		# m.EVfinal[ia,ih,ip,ij] = y1 + (y2 - y1) * (agrid[ia] - agrid[m.aone]) / (agrid[m.aone+1] - agrid[m.aone])
		m.EVfinal[ia,ih,ip,ij] = p.myNA
	end
		
	end
	end
	end

	# integrate
	# m.EVfinal = E_tensors.T_Final(m.grids2D["GP"],m.gridsXD["p"],m.EVfinal)

	return nothing
end



# period loop
# @debug function solvePeriod!(age::Int,m::Model,p::Param)
function solvePeriod!(age::Int,m::Model,p::Param)

	# initialise some objects

	vstay = zeros(2)

	w = zeros(p.namax)
	EV = zeros(p.na)

	# return value for findmax: tuple (value,index)
	r = (0.0,0.0,0.0)
	rh = (0.0,0.0,0.0)

	first = 1

	canbuy = false

	movecost = m.gridsXD["movecost"]

	Gz = m.gridsXD["Gz"]
	Gs = squeeze(m.gridsXD["Gs"][:,:,age],3)
	Gyp = m.gridsXD["Gyp"]
	Gtau = m.grids["Gtau"]

	vtmp = zeros(p.nJ) 
	feasible_k = falses(p.nJ)
	vbartmp = (0.0,0.0)

	jidx = 0
	# indexes the state when moving to k: k, s, z, y, p, tau,  a, h, current loc, age
	# dimvecH = (p.nh, p.nJ, p.ns, p.nz, p.ny, p.np, p.ntau,  p.na, p.nh, p.nJ, p.nt-1 )
	hidx = 0

	# ================
	# loop over states
	# ================

	# dimvec  = (nJ, ns, nz, ny, np,ntau,  na, nh,  nJ, nt-1 )

	agrid  = m.grids["assets"]

	for ij=1:p.nJ				# current location
		for ih=0:1
			first = ih + (1-ih)*m.aone	# first admissible asset index
			for ia=first:p.na
				a = agrid[ia]

				# start loop over stochastic states
				# ---------------------------------

				for itau=1:p.ntau			# local taste type 
					tau = m.grids["tau"][itau]
					for ip=1:p.np 				# regional price deviation
						price = m.gridsXD["p"][ip,ij]
						# given h, price and a, figure out if in neg equtiy
						def=false

						for iy=1:p.ny 				# regional income deviation
							for iz=1:p.nz				# individual income shock
								
								z = m.gridsXD["z"][iz,iy,age,ij] 	# z is dollar income

								canbuy = a + z > p.chi * price

								for is=1:p.ns

									# now you know the index of the
									# current state
									jidx = idx9(is,iz,iy,ip,itau,ia,ih+1,ij,age,p)

									# you can compute here
									# edx = EVidx(is,iz,iy,ip,itau,age+1)
									# for
									# all k and for all h


									# =================
									# loop over choices
									# =================

									# fill!(vtmp,0.0)
									# fill!(expv,0.0)
									expv = Float64[]
									fill!(feasible_k,false)

									# TODO
									# get a temporary copy of EV[possible.choices|all.states]
									# fillTempEV!(tempEV,jidx)

									# location choice
									for ik=1:p.nJ

										# now you know the index of the 
										# state when moving to k
										kidx = idx10(ik,is,iz,iy,ip,itau,ia,ih+1,ij,age,p)

										# you stay and you have a housing choice
										if ij==ik && (ih==1 || (ih==0 && canbuy))

											# compute vstay(ij,ih,ihh)

											# you have a housing choice
											# if ih==1 || (ih==0 && canbuy)

											def = false

											# you have a housing choice
											# if you are
											# 1) a current owner or
											# 2) a renter who can buy

											fill!(vstay,p.myNA)

											# optimal housing choice
											for ihh in 0:1

												hidx = idx11(ihh+1,ik,is,iz,iy,ip,itau,ia,ih+1,ij,age,p)

												blim = age < p.nt-1 ? (-ihh) * (1-p.chi) * price : 0.0

												# reset w vector
												fill!(EV,p.myNA)
												fill!(w,p.myNA)

												cash = cashFunction(a,z,ih,ihh,price,ij!=ik,ik,p)
												m.cash[hidx] = cash

												# find moving cost
												mc = 0.0

												# find relevant future value:
												EVfunChooser!(EV,is,iz,ihh+1,itau,ip,iy,ij,ik,age,m,p)

												# optimal savings choice
												rh = maxvalue(cash,is,itau,p,agrid,w,ihh,mc,def,EV,blim,age)

												# put into vfun, savings and cons policies
												# keep vstay for discrete housing choice function and envelope
												vstay[ihh+1] = rh[1]

												# store save and cons in h-conditional arrays
												if rh[1] > p.myNA
													# m.vfeas[hidx] = true
													m.vh[hidx] = rh[1] 
													m.sh[hidx] = rh[2] 
													m.ch[hidx] = rh[3]
												else
													m.sh[hidx] = p.myNA
													m.ch[hidx] = 0.0
												end

												if cash < 0 && ihh==0 && ih==0
													println("state: j=$ij,tau=$itau,h=$ih,a=$(round(a)),z=$(round(z)),p=$price,s=$is,k=$ik")
													println("cash at ihh=$ihh is $cash")
													println("maxvalue = $(r[1])")
												end

											end

											# find optimal housing choice
											r = findmax(vstay)
											# and store value, discrete choice idx, savings idx and consumption

											# checking for feasible choices
											if r[1] > p.myNA
												m.v[kidx]  = r[1]
												m.dh[kidx] = r[2] - 1
												else
												# infeasible
												m.v[kidx]  = r[1]
												m.dh[kidx] = 0
											end

										else   # you move or you're a renter who cannot buy


											# compute vmove(k,j,h)
											# TODO
											# moving with a < 0 means default = true
											def = (ih*(ia<m.aone) == true)

											ihh = 0

											hidx = idx11(ihh+1,ik,is,iz,iy,ip,itau,ia,ih+1,ij,age,p)

											blim = 0.0

											# reset w vector
											fill!(EV,p.myNA)	
											fill!(w,p.myNA)

											cash = cashFunction(a,z,ih,ihh,price,ij!=ik,ik,p)
											m.cash[hidx] = cash

											# find moving cost
											mc = movecost[age,ij,ik,ih+1,is]

											# find relevant future value:
											EVfunChooser!(EV,is,iz,ihh+1,itau,ip,iy,ij,ik,age,m,p)

											# TODO
											# you could just fix (ip,iy) at middle index here
											# to get "unconditional distribution"
											# EVfunChooser!(EV,iz,ihh+1,itau,iP,ip,iy,ik,age,m,p)

											# TODO
											# EVfunChooser should depend on whether you move or not?
											# if you move, must select the EV that corresponds to movers, 
											# i.e. reversion to the mean of the shock? (for example)


											# optimal savings choice
											r = maxvalue(cash,is,itau,p,agrid,w,ihh,mc,def,EV,blim,age)
											# if ij==1 && is==1 && itau==1 && age==1 && ia == m.aone && ih==0
											# 	println("                                        ")
											# 	println("========================================")
											# 	println("age = $age")
											# 	println("ih = $ih")
											# 	println("ihh = $ihh")
											# 	println("ij = $ij")
											# 	println("ik = $ik")
											# 	println("cash = $cash")
											# 	println("EV = $EV")
											# 	println("blim = $(ihh * blim)")
											# 	println("mc= $mc")
											# 	println("maxvalue = $r")
											# 	println("========================================")
											# 	println("                                        ")
											# end

											# checking for infeasible choices
											if r[1] > p.myNA
												# m.vfeas[hidx] = true
												m.vh[hidx]   = r[1]
												m.sh[hidx]   = r[2] 
												m.ch[hidx]   = r[3] 
												m.v[kidx]    = r[1]
												m.dh[kidx] = 0
											else
												m.vh[hidx]   = p.myNA
												m.sh[hidx]   = p.myNA
												m.ch[hidx]   = 0.0
												m.v[kidx]  = p.myNA
												m.dh[kidx] = 0
											end

											if cash < 0 && ih==0
												println("state: j=$ij,tau=$itau,h=$ih,a=$(round(a)),z=$(round(z)),p=$price,s=$is,k=$ik")
												println("cash at ihh=$ihh is $cash")
												println("maxvalue = $(r[1])")
												println("maxindex = $(r[2])")
											end

										end  # end if stay and houseing choice

										# store optimal value in tmp vector
										# used in vbar calculation
										if r[1] > p.myNA
											vtmp[ik] = r[1]
											# expv[ik]  = exp(r[1])
											push!(expv,exp(r[1]))
											feasible_k[ik] = true
										else
											vtmp[ik] = r[1]
											# expv[ik]  = 0.0
										end

									end	# choice: location 

									# compute vbar and rho

									# TODO was faster before
									if any(feasible_k)
										logsum = log( sum(expv) )
										m.vbar[jidx] = p.euler + logsum
									else
										# TODO that will pull the integration down a lot
										m.vbar[jidx] = p.myNA
									end
									# println(vtmp)
									# println(logsum)


									# compute rho: probability of moving to k given j
									for ik in 1:p.nJ
										if feasible_k[ik]
											# TODO this has numerical underflow problems
											# need to make sure that vtmp[ik] - logsum is in a numerically stable range
											# of the exp function!
											@inbounds m.rho[idx10(ik,is,iz,iy,ip,itau,ia,ih+1,ij,age,p)] = exp( (p.myNA + vtmp[ik]) -  (p.myNA + logsum))
										else
											m.rho[idx10(ik,is,iz,iy,ip,itau,ia,ih+1,ij,age,p)] = 0.0
										end
									end


								end # household size
							end	# local y-level
						end	# local p-level 
					end # individual z
				end	# individual tau
				# integrate vbar to get EV 
				@profile integrateVbar!(ia,ih+1,ij,age,Gz,Gyp,Gs,Gtau,m,p)
				# m.EVMove[jidx] = vbartmp[2]
			end	# assets
		end	# housing
	end	# current location

	return nothing

end

function integrateVbar!(ia::Int,ih::Int,ij::Int,age::Int,Gz::Array{Float64,3},Gyp::Array{Float64,3},Gs::Array{Float64,2},Gtau::Array{Float64,1},m::Model,p::Param)

	# offsets
	o_tau = 0
	o_p = 0
	o_y = 0
	o_z = 0
	o_s = 0

	# factors
	f_tau = 0.0
	f_py = 0.0
	f_z = 0.0
	f_s = 0.0
		
	@inbounds begin

	# loop over conditioning states
	for itau = 1:p.ntau 		 # current tau
	for ip   = 1:p.np 			 # current p
	for iy   = 1:p.ny 			 # current y
	for iz   = 1:p.nz			 # current z 		
	for is   = 1:p.ns 			 # current HHsize

		# set value
		tmp = 0.0

		# compute current index
		idx0 = idx9(is,iz,iy,ip,itau,ia,ih,ij,age,p)

		idx1  = idx_tau(ia,ih,ij,age,p)

		# loop over future states
		for itau1 = 1:p.ntau 		# future tau
			o_tau = (idx1 + (itau1-1)) * p.np
			f_tau = Gtau[itau1]
		for ip1   = 1:p.np 			
			o_p   = (o_tau + (ip1-1)) * p.ny
		for iy1   = 1:p.ny 				# future y
			o_y   = (o_p + (iy1-1)) * p.nz
			f_py  = f_tau * Gyp[iy + p.ny * ((ip-1) + p.np * ((iy1-1) + p.ny * ((ip1-1) + p.np * (ij-1)))) ]
		for iz1   = 1:p.nz			# future z 		
			o_z   = (o_y + (iz1-1)) * p.ns
			f_z   = f_py * Gz[iz + p.nz * (iz1 + p.nz * (ij-1)-1)]
		for is1   = 1:p.ns 				# future HHsize

			# compute future index 
     	    # idx1 = idx9(is1,iz1,iy1,ip1,itau1,ia,ih,ij,age,p)

     	    # construct sum
			# @inbounds tmp += m.vbar[idx1] * Gz[iz + p.nz * (iz1 + p.nz * (ij-1)-1)] * Gyp[iy + p.ny * ((ip-1) + p.np * ((iy1-1) + p.ny * ((ip1-1) + p.np * (ij-1)))) ] * Gs[is + p.ns * (is1-1)] * Gtau[itau1]
			tmp += m.vbar[o_z + is1] * Gs[is + p.ns * (is1-1)] * f_z

			# mover's future value: mover specific transitions of z
			# tmp2 += m.vbar[idx] * GzM[iz + p.nz * (iz1 + p.nz * (ij-1)-1)] * Gp[ip + p.np * (ip1 + p.np * (ij-1)-1)] * Gy[iy + p.ny * (iy1 + p.ny * (ij-1)-1)] * GP[iP + p.nP * (iP1-1)] * Gs[is + p.ns * (is1-1)]

		end
		end			
		end			
		end			
		end		

		# assign to current index
		m.EV[idx0] = tmp

	end
	end			
	end			
	end			
	end		

	end #inbounds
	return nothing
end





# linear index functions
# ======================

# dimvecH  = (nh, nJ, ns, nz, ny, np, na, nh, ntau,  nJ, nt-1 )
function idx11(ihh::Int,ik::Int,is::Int,iz::Int,iy::Int,ip::Int,itau::Int,ia::Int,ih::Int,ij::Int,age::Int,p::Param)

	 r = ihh + p.nh * (ik + p.nJ * (is + p.ns * (iz + p.nz * (iy + p.ny * (ip + p.np * (itau + p.ntau * (ia + p.na * (ih + p.nh * (ij + p.nJ * (age-1)-1)-1)-1)-1)-1)-1)-1)-1)-1)
	return r
end

# dimvec  = (nJ, ns, nz, ny, np, na, nh, ntau,  nJ, nt-1 )
function idx10(ik::Int,is::Int,iz::Int,iy::Int,ip::Int,itau::Int,ia::Int,ih::Int,ij::Int,age::Int,p::Param)

	 r = ik + p.nJ * (is + p.ns * (iz + p.nz * (iy + p.ny * (ip + p.np * (itau + p.ntau * (ia + p.na * (ih + p.nh * (ij + p.nJ * (age-1)-1)-1)-1)-1)-1)-1)-1)-1)
	return r
end

# dimvec2 = (ns, nz, ny, np, ntau, na, nh,nJ, nt-1 )
function idx9(is::Int,iz::Int,iy::Int,ip::Int,itau::Int,ia::Int,ih::Int,ij::Int,age::Int,p::Param)

	 r = is + p.ns * (iz + p.nz * (iy + p.ny * (ip + p.np * (itau + p.ntau * (ia + p.na * (ih + p.nh * (ij + p.nJ * (age-1)-1)-1)-1)-1)-1)-1)-1)
	return r
end

function idx_tau(ia::Int,ih::Int,ij::Int,age::Int,p::Param)

	 r = p.ntau * (ia + p.na * (ih + p.nh * (ij + p.nJ * (age-1)-1)-1)-1)
	return r
end

# p.na,p.nh,p.nP,p.np,p.nJ
function idxFinal(ia::Int,ih::Int,ip::Int,ij::Int,p::Param)

	r = ia + p.na * (ih-1 + p.nh* (ip-1 + p.np* (ij-1)))
    return r
end




# finds optimal value and 
# index of optimal savings choice
# on a given state
# discrete maximization
function maxvalue(cash::Float64,is::Int,itau::Int,p::Param,a::Array{Float64,1},w::Array{Float64,1},own::Int,mc::Float64,def::Bool,EV::Array{Float64,1},lb::Float64,age::Int)

	# if your current cash debt is lower than 
	# maximum borrowing, infeasible
	if (lb < 0) && (cash < lb / p.Rm)
		return (p.myNA,0.0,0.0)
	else
		# compute value of all savings choices

		x = 0.0
		# grid for next period assets
		s = linspace(lb,cash-0.01,p.namax)
		cons = zeros(p.namax)
		# fix upper bound of search grid
		# ub = minimum([cash-0.0001,a[end]])
		# ub = cash-0.0001 < a[end] ? cash-0.0001 : a[end]
		ub = a[end]

		# w[i] = u(cash - s[i]/(1+r)) + beta EV(s[i],h=1,j,t+1)
		vsavings!(w,a,EV,s,cons,cash,ub,is,own,itau,mc,def,p)	

		r = findmax(w)

	# println("own=$own, cash=$cash, is=$is, itau=$itau, mc=$mc, def=$def,lb=$lb")
	# println("V = $(r[1]), save = $(s[r[2]]), cons = $(cash-x)")
	# println("------------------------------------------------")
	# end
		return (r[1],s[r[2]],cons[r[2]])	# (value,saving,consumption)
	end
end


function vsavings!(w::Array{Float64,1},a::Array{Float64,1},EV::Array{Float64,1},s::Array{Float64,1},cons_arr::Array{Float64,1},cash::Float64,ub::Float64,is::Int,own::Int,itau::Int,mc::Float64,def::Bool,p::Param)
	n = p.namax
	x = 0.0
	cons = 0.0
	jinf = 1
	jsup = searchsortedfirst(a,ub)
	
	# check which hh size
	size2 = is == 1 ? false : true

	if size2
		consta =  own*p.xi2 - def*p.lambda - mc + (itau-1)*p.tau
	else
		consta =  own*p.xi1 - def*p.lambda - mc + (itau-1)*p.tau
	end


	# compute consumption at each potential savings choice
	# consumption is cash - x, where x is s/(1+interest)
	# and interest depends on whether borrow or save

	 for i=1:n
		lin = linearapprox(a,EV,s[i],jinf,jsup)
		# println("s=$(s[i]), i=$i")
		# println("lin = $lin")
		if s[i] < 0
			x = s[i] / p.Rm
		else
			x = s[i] / p.R
		end

		# w[i] = ufun(cash-x,is,own,itau,mc,def,p) + p.beta * lin[1]
		# note: cons^(1-gamma) = exp( (1-gamma)*log(cons) )
		if size2
			cons = (cash-x)*p.sscale
		else
			cons = cash-x
		end
		if cons < 0
			w[i] = p.myNA				
		else
			w[i] = ufun(cons,lin[1],p)
			# w[i] = p.imgamma * myexp(p.mgamma * mylog(cons) ) + p.beta * lin[1]
			w[i] += consta
		end
		jinf = lin[2]

		# println("jsup = $jsup")
		# println("jinf = $jinf")
		cons_arr[i] = cons
	end

	return 
end

function ufun(c::Float64,ev::Float64,p::Param)
	p.imgamma * myexp2(p.mgamma * mylog2(c) ) + p.beta * ev
end

# housing payment function
function pifun(ih::Int,ihh::Int,price::Float64,move::Bool,ik::Int,p::Param)
	r = 0.0

	if (move * ihh) == 1
		error("you cannot move and be an owner")
	end

	if ih==0
		# if you came into period as a renter:
		# choose whether to buy. if choose to move,
		# can only rent
		r = (1-ihh)*p.kappa[ik]*price + (!move) * ihh * price
	else 
		# if you sell (don't move and sell: !move * (1-ihh))
		# or if you move (then you are forced to sell)
		r = -( (!move)*(1-ihh) + (move) )*(1-p.phi-p.kappa[ik])*price 
	end
end


function grossSaving(x::Float64,p::Param)
	if x>0
		return x * p.R
	else
		return x * p.Rm
	end
end


function income(muy::Float64,shock::Float64)
	y = muy+shock
end


# cashfunction
# computes cash on hand given a value of the
# state vector and a value of the discrete choices
function cashFunction(a::Float64, y::Float64, ih::Int, ihh::Int,price::Float64,move::Bool,ik::Int,p::Param)
	a + y - pifun(ih,ihh,price,move,ik,p)
end


# agridChooser
# chooses the appropriate asset grid
function agridChooser( own::Int ,m::Model)
	if (own==1)
		return m.grids["asset_own"]
	else
		return m.grids["asset_rent"]
	end
end




# TODO slow
# EV selector
# given current state and discrete choice, which portion of
# EV is relevant for current choice?
function EVfunChooser!(ev::Array{Float64,1},is::Int,iz::Int,ihh::Int, itau::Int, ip::Int,iy::Int, ij::Int, ik::Int,age::Int,m::Model,p::Param)
	if age==p.nt-1
		for ia in 1:p.na
			ev[ia] = m.EVfinal[ia,ihh,ip,ik]
		end
	else 
		if ik==ij
			 for ia in 1:p.na
				ev[ia] = m.EV[idx9(is,iz,iy,ip,itau,ia,ihh,ik,age+1,p)]
			end
		else
			 for ia in 1:p.na
				ev[ia] = m.EV[idx9(is,iz,iy,ip,itau,ia,ihh,ik,age+1,p)]
				# ev[ia] = m.EVMove[idx10(is,iy,ip,iP,iz,ia,ihh,itau,ik,age+1,p)]
			end
		end
	end

	return nothing
end



	# for (Eigen::SparseVector<double>::InnerIterator it0(a.at(0)); it0; ++it0) {
	# 		int offset1    = it0.index();
	# 		offset1       *= a.at(1).size();
	# 		double factor1 = it0.value();

	# 		for (Eigen::SparseVector<double>::InnerIterator it1(a.at(1)); it1; ++it1) {
	# 			int offset2    = offset1 + it1.index();
	# 			offset2       *= a.at(2).size();
	# 			double factor2 = factor1 * it1.value();

	# 			for (Eigen::SparseVector<double>::InnerIterator it2(a.at(2)); it2; ++it2) {
	# 				int offset3    = offset2 + it2.index();
	# 				offset3       *= a.at(3).size();
	# 				double factor3 = factor2 * it2.value();

	# 				for (Eigen::SparseVector<double>::InnerIterator it3(a.at(3)); it3; ++it3) {
	# 					int offset4    = offset3 + it3.index();
	# 					offset4       *= a.at(4).size();
	# 					double factor4 = factor3 * it3.value();

	# 					for (Eigen::SparseVector<double>::InnerIterator it4(a.at(4)); it4; ++it4) {

	# 						sum += factor4 * it4.value() * this->d_vecCoefs( offset4 + it4.index() );
	# 					}
	# 				}
	# 			}
	# 		}
	# }
	# return sum;

function linearapprox(x::Array{Float64,1},y::Array{Float64,1},xi::Float64,lo::Int,hi::Int)
	n = length(y)
	r = 0.0

	# determining bounds 
	if xi == x[1]
		r = y[1] 
		return (r,1)
	elseif xi < x[1]
		# get linear approx below
		r = y[1] + (y[2] - y[1]) * (xi - x[1])  / (x[2] - x[1])
		return (r,1)
	end
	if xi == x[n]
		r = y[n] 
		return (r,n)
	elseif xi > x[n]
		# get linear approx above
		r = y[n] + (y[n] - y[n-1]) * (xi - x[n])  / (x[n] - x[n-1])
		return (r,n)
	end

	# if have to find interval
	if hi - lo > 1
		jinf = searchsortedlast(x,xi,lo,hi,Base.Forward)
	# if not, lo is jinf
	else
		jinf = lo
	end
	r = (y[jinf] * (x[jinf+1] - xi) + y[jinf+1] * (xi - x[jinf]) ) / (x[jinf+1] - x[jinf])
	return (r,jinf)
end

function linearapprox(x::Array{Float64,1},y::Array{Float64,1},xi::Float64,feasible::BitArray{1},p::Param)
	n = length(y)
	lo = 1
	hi = n
	r = 0.0
	# behaviour on bounds 
	if xi <= x[1]
		if feasible[1]
			r = y[1] 
		else 
			r = p.myNA
		end
		return (r,1)

	elseif xi>= x[n]
		if feasible[n]
			r = y[n] 
		else
			r = p.myNA
		end
		return (r,n)

	# if have to find interval
	elseif hi - lo > 1
		jinf = searchsortedlast(x,xi,lo,hi,Base.Forward)
	# if not, lo is jinf
	else
		jinf = lo
	end
	# only if both points are feasible do we consider them
	if feasible[jinf] && feasible[jinf+1]
		r = (y[jinf] * (x[jinf+1] - xi) + y[jinf+1] * (xi - x[jinf]) ) / (x[jinf+1] - x[jinf])
	else 
		r = p.myNA
	end
	return (r,jinf)
end

function linearapprox(x::Array{Float64,1},y::Array{Float64,1},xi::Float64,p::Param)
	n = length(y)
	lo = 1
	hi = n
	r = 0.0
	# behaviour on bounds 
	if xi <= x[1]
		r = y[1] 
		return (r,1)

	elseif xi>= x[n]
		r = y[n] 
		return (r,n)

	# if have to find interval
	elseif hi - lo > 1
		jinf = searchsortedlast(x,xi,lo,hi,Base.Forward)
		# if not, lo is jinf
	else
		jinf = lo
	end
	r = (y[jinf] * (x[jinf+1] - xi) + y[jinf+1] * (xi - x[jinf]) ) / (x[jinf+1] - x[jinf])
	return (r,jinf)
end

function integrateFinal!(m::Model)

	m.EVfinal = T_Final(m.EVfinal,m.gridsXD)
	return nothing
end







