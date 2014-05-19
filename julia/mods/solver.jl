

# solving the model at the current 
# parameter vector

# main loop
function solve!(m::Model, p::Param)

	# final period
	solveFinal!(m,p)

	# loop over time
	for age=(p.nt-1):-1:1

		info("solving period $age")

		# 	# compute current period values
		solvePeriod!(age,m,p)

	end

	return nothing

end


# auxiliary functions
# ===================


function solveFinal!(m::Model,p::Param)

	# extract grids for faster lookup
	agrid = m.grids["asset_rent"]
	hgrid = m.grids["housing"]
	# loop over all states
	for ia = 1:p.na
	for ih = 1:p.nh
	for iP = 1:p.nP
	for ij = 1:p.nJ
	for ip = 1:p.np 

		m.EVfinal[ia,ih,iP,ip,ij] = p.omega[1] + p.omega[2] * log(agrid[ia] + hgrid[ih] * (m.gridsXD["p"][iP,ip,ij] ) )

	end
	end
	end
	end
	end

	# integrate
	m.EVfinal = E_tensors.T_Final(m.grids2D["GP"],m.gridsXD["p"],m.EVfinal)

	return nothing
end



# period loop
# @debug function solvePeriod!(age::Int,m::Model,p::Param)
function solvePeriod!(age::Int,m::Model,p::Param)

	# initialise some objects

	vstay = zeros(2)
	sstay = [0, 0]
	cstay = zeros(2)

	w = zeros(p.na)
	EV = zeros(p.na)

	# return value for findmax: tuple (value,index)
	r = (0.0,0)

	canbuy = false

	movecost = m.gridsXD["movecost"]

	Gz = m.gridsXD["Gz"]
	Gy = m.gridsXD["Gy"]
	Gp = m.gridsXD["Gp"]
	GP = m.grids2D["GP"]

	vtmp = zeros(p.nJ) 
	expv = zeros(p.nJ) 
	vbartmp = 0.0

	# indexes the current state: y, p, P, z, a, h, tau, current loc, age
	jidx = 0
	# indexes the state when moving to k: k, y, p, P, z, a, h, tau, current loc, age
	kidx = 0

	# ================
	# loop over states
	# ================

	# dimvec  = (nJ, ny, np, nP, nz, na, nh, ntau,  nJ, nt-1 )
	# V[y,p,P,z,a,h,tau,j,age]

	@inbounds begin
	for ij=1:p.nJ				# current location
		ageeffect = m.grids2D["ageprof"][age,ij]
		for itau=1:p.ntau			# type
			tau = p.tau[itau]
			for ih=0:1
				# choose asset grid for owner/renter
				agrid = agridChooser(ih,m)
				for ia=1:p.na
					a = agrid[ia]
					for iz=1:p.nz				# individual income shock
						z = m.grids["z"][iz]
						for ip=1:p.np 				# regional price deviation
							for iP=1:p.nP 				# national price index
								price = m.gridsXD["p"][iP,ip,ij]
								# given h, price and a, figure out if in neg equtiy
								def=false

								for iy=1:p.ny 				# regional income deviation

									y     = m.gridsXD["y"][iy,ij]
									yy    = income(y,ageeffect,z)

									canbuy = grossSaving(a,p) + yy > p.chi * price

									# now you know the index of the
									# current state
									jidx = idx9(iy,ip,iP,iz,ia,ih+1,itau,ij,age,p)


									# =================
									# loop over choices
									# =================

									fill!(vtmp,0.0)
									fill!(expv,0.0)
									vbartmp = 0.0

									# location choice
									for ik=1:p.nJ

										# now you know the index of the 
										# state when moving to k
										kidx = idx10(ik,iy,ip,iP,iz,ia,ih+1,itau,ij,age,p)

										# you stay: therefore you have got a housing choice
										if ij==ik


											# vstay is a triple (value, optimal index, consumption)
											fill!(vstay,p.myNA)
											fill!(sstay,0)
											fill!(cstay,0.0)

											# optimal housing choice
											for ihh in 0:1

												# you can only buy if you have enough cash
												if ((1-ih)*ihh == 1) && canbuy

													# you can buy
													# choose relevant savings grid
													sgrid = agridChooser(ihh,m)
													# reset w vector
													fill!(w,p.myNA)
													fill!(EV,p.myNA)

													# cashfunction(a,y,ageeffect,z,ih,ihh)
													cash = cashFunction(a,yy,ih,ihh,price,ij!=ik,ik,p)

													# find moving cost
													mc = 0.0

													# find relevant future value:
													EVfunChooser!(EV,iz,ihh+1,itau,iP,ip,iy,ik,age,m,p)

													# optimal savings choice
													r = maxvalue(cash,p,sgrid,w,ihh,mc,def,EV)

													# put into vfun, savings and cons policies
													vstay[ihh+1] = r[1]
													sstay[ihh+1] = r[2]
													cstay[ihh+1] = cash - sgrid[ r[2] ]

												else
													# you are a renter staying renter
													# you are an owner staying owner
													# you are an owner becoming renter

													# choose relevant savings grid
													sgrid = agridChooser(ihh,m)
													# reset w vector
													fill!(w,p.myNA)
													fill!(EV,p.myNA)

													# cashfunction(a,y,ageeffect,z,ih,ihh)
													cash = cashFunction(a,yy,ih,ihh,price,ij!=ik,ik,p)

													# find moving cost
													mc = 0.0

													# find relevant future value:
													EVfunChooser!(EV,iz,ihh+1,itau,iP,ip,iy,ik,age,m,p)

													# optimal savings choice
													r = maxvalue(cash,p,sgrid,w,ihh,mc,def,EV)

													# put into vfun, savings and cons policies
													vstay[ihh+1] = r[1]
													sstay[ihh+1] = r[2]
													cstay[ihh+1] = cash - sgrid[ r[2] ]

												end

											end

											# find optimal housing choice
											r = findmax(vstay)
											# and store value, discrete choice idx, savings idx and consumption

											# checking for feasible choices
											if r[1] > p.myNA
												m.v[kidx]  = r[1]
												m.dh[jidx] = r[2]
												m.s[kidx]  = sstay[r[2]] 
												m.c[kidx]  = cstay[r[2]]								
											else
												# infeasible
												m.v[kidx]  = r[1]
												m.dh[jidx] = 0
												m.s[kidx] = 0
												m.c[kidx] = 0
											end


										# you move locations: you must sell
										else
											ihh = 0
											# choose relevant savings grid
											sgrid = agridChooser(ihh,m)

											# reset w vector
											fill!(w,p.myNA)
											fill!(EV,p.myNA)	

											# cashfunction(a,y,ageeffect,z,ih,ihh)
											cash = cashFunction(a,yy,ih,ihh,price,ij!=ik,ik,p)

											# find moving cost
											mc = movecost[age,ij,ik,ih+1,itau]

											# find relevant future value:
											EVfunChooser!(EV,iz,ihh+1,itau,iP,ip,iy,ik,age,m,p)

											# TODO
											# you could just fix (ip,iy) at middle index here
											# to get "unconditional distribution"
											# EVfunChooser!(EV,iz,ihh+1,itau,iP,ip,iy,ik,age,m,p)

											# optimal savings choice
											r = maxvalue(cash,p,sgrid,w,ihh,mc,def,EV)

											# put into vfun, savings and cons policies
											m.v[kidx] = r[1]

											# checking for infeasible choices
											if r[1] > p.myNA
												m.s[kidx] = r[2]
												m.c[kidx] = cash - sgrid[ r[2] ]
											else
												m.s[kidx] = 0
												m.c[kidx] = 0
											end

										end

										# store optimal value in tmp vector
										# used in vbar calculation
										vtmp[ik] = r[1]
										expv[ik]  = exp(r[1])

									end	# choice: location 

									# compute vbar and rho
									vbartmp = p.euler + log(sum(expv))
									m.vbar[jidx] = vbartmp

									# compute rho: probability of moving to k given j
									for ik in 1:p.nJ
									m.rho[idx10(ik,iy,ip,iP,iz,ia,ih+1,itau,ij,age,p)] = exp( p.euler - vbartmp + vtmp[ik] )
									end

									# integrate vbar to get EV
									m.EV[jidx] = integrateVbar(ia,ih+1,iy,ip,iP,iz,itau,ij,age,p,Gz,Gy,Gp,GP,m)

								end	# local y-level
							end	# aggregate p-level 
							# independent of that: expectation over prices in k
							# integrateVbark()

						end	# local P-level 
					end # individual z
				end	# assets
			end	# housing
		end	# individual tau
	end	# current location

	end # inbounds

	return nothing

end

function integrateVbar(ia::Int,ih::Int,iy::Int,ip::Int,iP::Int,iz::Int,itau::Int,ij::Int,age::Int,p::Param,Gz::Array{Float64,3},Gy::Array{Float64,3},Gp::Array{Float64,3},GP::Array{Float64,2},m::Model)
	# set index
	idx = 0
	# set value
	tmp = 0.0
	# dimvec2 = (ny, np, nP, nz, na, nh, ntau,  nJ, nt-1 )
	# for iz=1:p.nz				# current z
	# 	for iP=1:p.nP 				# current P
	# 		for ip=1:p.np 				# current p
	# 			for iy=1:p.ny 				# current y
	# ===========================================================
					for iz1 = 1:p.nz			# future z
						for iP1 = 1:p.nP 			# future P
							for ip1 = 1:p.np 			# future p
								for iy1=1:p.ny 				# future y
					# ===========================================================

									# compute index in integrand: uses ix1 indices!
					         	    idx = iy1 + p.ny * (ip1 + p.np * (iP1 + p.nP * (iz1 + p.nz * (ia + p.na * (ih + p.nh * (itau + p.ntau * (ij + p.nJ * (age-1)-1)-1)-1)-1)-1)-1)-1)

					         	    # construct sum
									tmp += m.vbar[idx] * Gz[iz + p.nz * (iz1 + p.nz * (ij-1)-1)] * Gp[ip + p.np * (ip1 + p.np * (ij-1)-1)] * Gy[iy + p.ny * (iy1 + p.ny * (ij-1)-1)] * GP[iP + p.nP * (iP1-1)]

								end
							end
						end
					end
					# ===========================================================
	# 			end
	# 		end
	# 	end
	# end
	return tmp
end



# linear index functions
# ======================


# dimvec  = (nJ, ny, np, nP, nz, na, nh, ntau,  nJ, nt-1 )
function idx10(ik::Int,iy::Int,ip::Int,iP::Int,iz::Int,ia::Int,ih::Int,itau::Int,ij::Int,age::Int,p::Param)

	r = ik + p.nJ * (iy + p.ny * (ip + p.np * (iP + p.nP * (iz + p.nz * (ia + p.na * (ih + p.nh * (itau + p.ntau * (ij + p.nJ * (age-1)-1)-1)-1)-1)-1)-1)-1)-1)
	return r
end

# dimvec2 = (ny, np, nP, nz, na, nh, ntau,  nJ, nt-1 )
function idx9(iy::Int,ip::Int,iP::Int,iz::Int,ia::Int,ih::Int,itau::Int,ij::Int,age::Int,p::Param)

	r = iy + p.ny * (ip + p.np * (iP + p.nP * (iz + p.nz * (ia + p.na * (ih + p.nh * (itau + p.ntau * (ij + p.nJ * (age-1)-1)-1)-1)-1)-1)-1)-1)
	return r
end

# p.na,p.nh,p.nP,p.np,p.nJ
function idxFinal(ia::Int,ih::Int,iP::Int,ip::Int,ij::Int,p::Param)

	r = ia + p.na * (ih-1 + p.nh* (iP-1 + p.nP* (ip-1 + p.np* (ij-1))))
    return r
end




# finds optimal value and 
# index of optimal savings choice
# on a given state
# discrete maximization
function maxvalue(x::Float64,p::Param,s::Array{Float64,1},w::Array{Float64,1},own::Int,mc::Float64,def::Bool,EV::Array{Float64,1})

	@assert length(s) == length(w)
	@assert length(s) == length(EV)

	# v = max u(cash - s) + beta EV(s,h=1,j,t+1)

	for i=1:p.na
		if x-s[i] > 0
			w[i] = ufun(x-s[i],own,mc,def,p) + p.beta * EV[i]
		end
	end

	r = findmax(w)
	return r
end



function ufun(x::Float64,own::Int,mc::Float64,def::Bool,p::Param)
	r = p.imgamma * x^p.mgamma + own*p.xi - def*p.lambda - mc
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


function income(muy::Float64,ageeff::Float64,shock::Float64)
	# y = muy + f(i,j,t) + shock
	y = muy + ageeff + shock
end


# cashfunction
# computes cash on hand given a value of the
# state vector and a value of the discrete choices
function cashFunction(a::Float64, y::Float64, ih::Int, ihh::Int,price::Float64,move::Bool,ik::Int,p::Param)

	r = grossSaving(a,p) + y - pifun(ih,ihh,price,move,ik,p)

end


# sgridChooser
# chooses the appropriate savings grid
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
function EVfunChooser!(ev::Array{Float64,1},iz::Int,ihh::Int, itau::Int, iP::Int,ip::Int,iy::Int, ik::Int,age::Int,m::Model,p::Param)


	if age==p.nt-1
		for ia in 1:p.na
			ev[ia] = m.EVfinal[ia,ihh,iP,ip,ik]
		end
	else 
		for ia in 1:p.na
			ev[ia] = m.EV[idx9(iy,ip,iP,iz,ia,ihh,itau,ik,age+1,p)]
		end
		
	end

	return nothing
end







function integrateFinal!(m::Model)

	m.EVfinal = T_Final(m.EVfinal,m.gridsXD)
	return nothing
end







