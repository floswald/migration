

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
	for ia = 1:p.na
	for ih = 1:p.nh
	for ij = 1:p.nJ
	for ip = 1:p.np 

		if ia == m.aone
			tmp1 = p.omega1 + p.omega2 * log(agrid[ia+1] + hgrid[ih] * (m.gridsXD["p"][ip,ij] ) )
			tmp2 = p.omega1 + p.omega2 * log(agrid[ia+2] + hgrid[ih] * (m.gridsXD["p"][ip,ij] ) )

			m.EVfinal[ia,ih,ip,ij] = tmp1 + (tmp2-tmp1) * (agrid[ia] - agrid[ia+1]) / agrid[ia+2] - agrid[ia+1]
	
		elseif ia > m.aone

			m.EVfinal[ia,ih,ip,ij] = p.omega1 + p.omega2 * log(agrid[ia] + hgrid[ih] * (m.gridsXD["p"][ip,ij] ) )
		else
			m.EVfinal[ia,ih,ip,ij] = p.myNA
		end

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
	sstay = zeros(2)
	cstay = zeros(2)

	w = zeros(p.namax)
	EV = zeros(p.na)

	# return value for findmax: tuple (value,index)
	r = (0.0,0.0,0.0)

	first = 1

	canbuy = false

	movecost = m.gridsXD["movecost"]

	Gz = m.gridsXD["Gz"]
	Gs = squeeze(m.gridsXD["Gs"][:,:,age],3)
	Gy = m.gridsXD["Gy"]
	Gp = m.gridsXD["Gp"]

	vtmp = zeros(p.nJ) 
	expv = zeros(p.nJ) 
	vbartmp = (0.0,0.0)

	# indexes the current state: s, y, p, z, a, h, tau, current loc, age
	jidx = 0
	# indexes the state when moving to k: k, s, y, p, z, a, h, tau, current loc, age
	kidx = 0

	# ================
	# loop over states
	# ================

	# dimvec  = (nJ, ns, ny, np, nz, na, nh, ntau,  nJ, nt-1 )
	# V[s,y,p,z,a,h,tau,j,age]

	agrid  = m.grids["assets"]
	sgrid0 = m.grids["saving0"]
	sgrid1 = m.grids["saving1"]

	for ij=1:p.nJ				# current location
		for itau=1:p.ntau			# type TODO this must be random if you move!
			tau = m.grids["tau"][itau]
			for ih=0:1
				# choose asset grid for owner/renter
				# agrid = agridChooser(ih,m)
				first = ih + (1-ih)*m.aone	# first admissible asset index
				for ia=first:p.na
					a = agrid[ia]

					# start loop over stochastic states
					# ---------------------------------

					for iz=1:p.nz				# individual income shock
						z = m.gridsXD["z"][iz,ij,age]
						for ip=1:p.np 				# regional price deviation
							price = m.gridsXD["p"][ip,ij]
							# given h, price and a, figure out if in neg equtiy
							def=false

							for iy=1:p.ny 				# regional income deviation

								y     = m.gridsXD["y"][iy,ij]
								yy    = z

								canbuy = a + yy > p.chi * price
								blim = (-1) * (1-p.chi) * price

								for is=1:p.ns

									# now you know the index of the
									# current state
									jidx = idx9(is,iy,ip,iz,ia,ih+1,itau,ij,age,p)


									# =================
									# loop over choices
									# =================

									# fill!(vtmp,0.0)
									# fill!(expv,0.0)

									# TODO
									# get a temporary copy of EV[possible.choices|all.states]
									# fillTempEV!(tempEV,jidx)

									# location choice
									for ik=1:p.nJ

										# now you know the index of the 
										# state when moving to k
										kidx = idx10(ik,is,iy,ip,iz,ia,ih+1,itau,ij,age,p)

										# you stay
										if ij==ik && (ih==1 || (ih==0 && canbuy))

											def = false

											# you have a housing choice
											# if you are
											# 1) a current owner or
											# 2) a renter who can buy

											fill!(vstay,p.myNA)
											fill!(sstay,0.0)
											fill!(cstay,0.0)

											# optimal housing choice
											for ihh in 0:1

												# reset w vector
												fill!(EV,p.myNA)
												fill!(w,p.myNA)

												cash = cashFunction(a,yy,is,ih,ihh,price,ij!=ik,ik,p)
												if ihh==0
													m.cash0[kidx] = cash
												else
													m.cash1[kidx] = cash
												end

												# find moving cost
												mc = 0.0

												# find relevant future value:
												EVfunChooser!(EV,is,iz,ihh+1,itau,ip,iy,ij,ik,age,m,p)

												# optimal savings choice
												r = maxvalue(cash,is,itau,p,agrid,w,ihh,mc,def,EV,ihh * blim,age)

												# put into vfun, savings and cons policies
												vstay[ihh+1] = r[1]
												sstay[ihh+1] = r[2] 
												cstay[ihh+1] = r[3]

												if cash < 0 && ihh==0 && ih==0
													println("state: j=$ij,tau=$itau,h=$ih,a=$(round(a)),z=$(round(z)),P=$iP,p=$price,y=$(round(y)),s=$is,k=$ik")
													println("cash at ihh=$ihh is $cash")
													println("maxvalue = $(r[1])")
												end

											end

											# find optimal housing choice
											# TODO is that slow?
											r = findmax(vstay)
											# and store value, discrete choice idx, savings idx and consumption

											# checking for feasible choices
											if r[1] > p.myNA
												m.v[kidx]  = r[1]
												m.dh[kidx] = r[2] - 1
												m.s[kidx]  = sstay[r[2]] 
												m.c[kidx]  = cstay[r[2]]								
												else
												# infeasible
												m.v[kidx]  = r[1]
												m.dh[kidx] = 0
												m.s[kidx] = 0
												m.c[kidx] = 0
											end


										# you either move or you are a 
										# current renter who cannot buy
										else

											# TODO
											# moving with a < 0 means default = true
											def = (ih*(ia<m.aone) == true)

											ihh = 0

											# reset w vector
											fill!(EV,p.myNA)	
											fill!(w,p.myNA)

											# cashfunction(a,y,ageeffect,z,ih,ihh)
											cash = cashFunction(a,yy,is,ih,ihh,price,ij!=ik,ik,p)
											m.cash0[kidx] = cash

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
											r = maxvalue(cash,is,itau,p,agrid,w,ihh,mc,def,EV,ihh * blim,age)

											# checking for infeasible choices
											if r[1] > p.myNA
												m.v[kidx]   = r[1]
												m.dh[kidx] = 0
												m.s[kidx] = r[2] 
												m.c[kidx] = r[3] 
											else
												m.v[kidx]  = p.myNA
												m.dh[kidx] = 0
												m.s[kidx] = 0
												m.c[kidx] = 0
											end

												if cash < 0 && ih==0
													println("state: j=$ij,tau=$itau,h=$ih,a=$(round(a)),z=$(round(z)),P=$iP,p=$price,y=$(round(y)),s=$is,k=$ik")
													println("cash at ihh=$ihh is $cash")
													println("maxvalue = $(r[1])")
													println("maxindex = $(r[2])")
												end

										end

										# store optimal value in tmp vector
										# used in vbar calculation
										if r[1] > p.myNA
											vtmp[ik] = r[1]
											expv[ik]  = exp(r[1])
										else
											vtmp[ik] = r[1]
											expv[ik]  = 0.0
										end

									end	# choice: location 

									# compute vbar and rho
									logsum = log( sum(expv) )
									if !isfinite(logsum)
										m.vbar[jidx] = p.myNA
									else
										m.vbar[jidx] = p.euler + logsum
									end
									# println(vtmp)
									# println(logsum)


									# compute rho: probability of moving to k given j
									for ik in 1:p.nJ
										m.rho[idx10(ik,is,iy,ip,iz,ia,ih+1,itau,ij,age,p)] = exp( vtmp[ik] - logsum)
									end

									# integrate vbar to get EV and EVbar
									m.EV[jidx] = integrateVbar(ia,is,ih+1,iy,ip,iz,itau,ij,age,Gz,Gy,Gp,Gs,m,p)
									# m.EVMove[jidx] = vbartmp[2]

								end # household size
							end	# local y-level
						end	# local p-level 
					end # individual z
				end	# assets
			end	# housing
		end	# individual tau
	end	# current location

	return nothing

end

function integrateVbar(ia::Int,is::Int,ih::Int,iy::Int,ip::Int,iz::Int,itau::Int,ij::Int,age::Int,Gz::Array{Float64,3},Gy::Array{Float64,3},Gp::Array{Float64,3},Gs::Array{Float64,2},m::Model,p::Param)
	# set index
	idx = 0
	# set value
	tmp = 0.0
	# tmp2 = 0.0
		
	for iz1 = 1:p.nz			# future z 		
		for ip1 = 1:p.np 			# future p
			for iy1=1:p.ny 				# future y
				for is1=1:p.ns 				# future HHsize

					# compute index in integrand: uses ix1 indices!
	         	     idx = idx9(is1,iy1,ip1,iz1,ia,ih,itau,ij,age,p)
	         	     # idx = is1 + p.ns * (iy1 + p.ny * (ip1 + p.np * (iz1 + p.nz * (ia + p.na * (ih + p.nh * (itau + p.ntau * (ij + p.nJ * (age-1)-1)-1)-1)-1)-1)-1)-1)

	         	    # construct sum
					 tmp += m.vbar[idx] * Gz[iz + p.nz * (iz1 + p.nz * (ij-1)-1)] * Gp[ip + p.np * (ip1 + p.np * (ij-1)-1)] * Gy[iy + p.ny * (iy1 + p.ny * (ij-1)-1)] * Gs[is + p.ns * (is1-1)]

					# mover's future value: mover specific transitions of z
					# tmp2 += m.vbar[idx] * GzM[iz + p.nz * (iz1 + p.nz * (ij-1)-1)] * Gp[ip + p.np * (ip1 + p.np * (ij-1)-1)] * Gy[iy + p.ny * (iy1 + p.ny * (ij-1)-1)] * GP[iP + p.nP * (iP1-1)] * Gs[is + p.ns * (is1-1)]
				end
			end
		end
	end
	# 
	return tmp
end


# linear index functions
# ======================


# dimvec  = (nJ, ns, ny, np, nP, nz, na, nh, ntau,  nJ, nt-1 )
function idx10(ik::Int,is::Int,iy::Int,ip::Int,iz::Int,ia::Int,ih::Int,itau::Int,ij::Int,age::Int,p::Param)

	 r = ik + p.nJ * (is + p.ns * (iy + p.ny * (ip + p.np * (iz + p.nz * (ia + p.na * (ih + p.nh * (itau + p.ntau * (ij + p.nJ * (age-1)-1)-1)-1)-1)-1)-1)-1)-1)
	return r
end

# dimvec2 = (ns, ny, np, nP, nz, na, nh, ntau,  nJ, nt-1 )
function idx9(is::Int,iy::Int,ip::Int,iz::Int,ia::Int,ih::Int,itau::Int,ij::Int,age::Int,p::Param)

	 r = is + p.ns * (iy + p.ny * (ip + p.np * (iz + p.nz * (ia + p.na * (ih + p.nh * (itau + p.ntau * (ij + p.nJ * (age-1)-1)-1)-1)-1)-1)-1)-1)
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
	size2 = false
	if is ==1
		consta =  own*p.xi1 - def*p.lambda - mc + (itau-1)*p.tau
	else
		size2 = true
		consta =  own*p.xi2 - def*p.lambda - mc + (itau-1)*p.tau
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
			if cons < 0
				w[i] = p.myNA				
			else
				w[i] = p.imgamma * myexp2(p.mgamma * mylog2(cons) ) + p.beta * lin[1]
				# w[i] = p.imgamma * myexp(p.mgamma * mylog(cons) ) + p.beta * lin[1]
				w[i] += consta
			end
		else
			cons = cash-x
			if cons < 0
				w[i] = p.myNA
			else
				# w[i] = p.imgamma * myexp(p.mgamma * mylog(cons) ) + p.beta * lin[1]
				w[i] = p.imgamma * myexp2(p.mgamma * mylog2(cons) ) + p.beta * lin[1]
				w[i] += consta
			end
		end
		jinf = lin[2]

		# println("jsup = $jsup")
		# println("jinf = $jinf")
		cons_arr[i] = cons
	end

	return 
end


function ufun(x::Float64,is::Int,own::Int,itau::Int,mc::Float64,def::Bool,p::Param)
	if is==1
		r = p.imgamma * x^p.mgamma + own*p.xi1 - def*p.lambda - mc + (itau-1)*p.tau
	else 
		r = p.imgamma * (x*p.sscale)^p.mgamma + own*p.xi2 - def*p.lambda - mc + (itau-1)*p.tau
	end
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
	y = muy*shock
end


# cashfunction
# computes cash on hand given a value of the
# state vector and a value of the discrete choices
function cashFunction(a::Float64, y::Float64, is::Int, ih::Int, ihh::Int,price::Float64,move::Bool,ik::Int,p::Param)
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
				ev[ia] = m.EV[idx9(is,iy,ip,iz,ia,ihh,itau,ik,age+1,p)]
			end
		else
			 for ia in 1:p.na
				ev[ia] = m.EV[idx9(is,iy,ip,iz,ia,ihh,itau,ik,age+1,p)]
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
	# determining bounds 
	if xi <= x[1]
		# get linear approx below
		r = y[1] + (y[2] - y[1]) * (xi - x[1])  / (x[2] - x[1])
		return (r,1)
	elseif xi>= x[n]
		# get linear approx above
		r = y[n] + (y[n] - y[n-1]) * (xi - x[n])  / (x[n] - x[n-1])
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







