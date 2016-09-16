

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

			# info("solving period $age")

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
	cash = 0.0
	for ih = 1:p.nh
	for ij = 1:p.nJ
	for ip = 1:p.np 
	for iy = 1:p.ny 

	for ia = 1:p.na

		if agrid[ia] + (ih-1) * (m.gridsXD["p"][iy,ip,ij] ) > 0
			m.EVfinal[idxFinal(ia,ih,ip,iy,ij,p)] = p.omega1 * p.imgamma * myexp2(p.mgamma * mylog2(agrid[ia] + (ih-1) * (m.gridsXD["p"][iy,ip,ij] )) ) + p.omega2 * (ih-1) 
		else
			m.EVfinal[ia,ih,ip,iy,ij] = p.myNA
		end

	end
	end
	end
	end
	end


		# if ia == m.aone
		# 	tmp1 = p.omega1 + p.omega2 * log(agrid[ia+1] + p.omega3 * hgrid[ih] * (m.gridsXD["p"][ip,ij] ) )
		# 	tmp2 = p.omega1 + p.omega2 * log(agrid[ia+2] + p.omega3 * hgrid[ih] * (m.gridsXD["p"][ip,ij] ) )

		# 	m.EVfinal[ia,ih,ip,ij] = tmp1 + (tmp2-tmp1) * (agrid[ia] - agrid[ia+1]) / agrid[ia+2] - agrid[ia+1]
	
		# elseif ia > m.aone

		# 	m.EVfinal[ia,ih,ip,ij] = p.omega1 + p.omega2 * log(agrid[ia] + hgrid[ih] * (m.gridsXD["p"][ip,ij] ) )
		# end
	
	# for ia = m.aone:p.na

	# 	if ia == m.aone
	# 		tmp1 = p.omega1 + p.omega2 * log(agrid[ia+1] + p.omega3 * hgrid[ih] * (m.gridsXD["p"][ip,ij] ) )
	# 		tmp2 = p.omega1 + p.omega2 * log(agrid[ia+2] + p.omega3 * hgrid[ih] * (m.gridsXD["p"][ip,ij] ) )

	# 		m.EVfinal[ia,ih,ip,ij] = tmp1 + (tmp2-tmp1) * (agrid[ia] - agrid[ia+1]) / agrid[ia+2] - agrid[ia+1]
	
	# 	elseif ia > m.aone

	# 		m.EVfinal[ia,ih,ip,ij] = p.omega1 + p.omega2 * log(agrid[ia] + hgrid[ih] * (m.gridsXD["p"][ip,ij] ) )
	# 	end
	# end

	# linear approximation below 0 or p.NA?
	# truth is that this section of assets can never be chosen anyway
	# y1 = m.EVfinal[m.aone,ih,ip,ij]
	# y2 = m.EVfinal[m.aone+1,ih,ip,ij]
	# for ia = 1:(m.aone-1)
	# 	# m.EVfinal[ia,ih,ip,ij] = y1 + (y2 - y1) * (agrid[ia] - agrid[m.aone]) / (agrid[m.aone+1] - agrid[m.aone])
	# 	m.EVfinal[ia,ih,ip,ij] = p.myNA
	# end
		
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
	consta = 0.0	# utility shifter

	first = 1

	canbuy = false
	move = false

	movecost = m.gridsXD["movecost"]

	Gz = m.gridsXD["Gz"]
	Gs = zeros(p.ns,p.ns)
	Gs = m.gridsXD["Gs"][:,:,age]
	Gyp = m.gridsXD["Gyp"]

	vtmp = zeros(p.nJ) 
	feasible_k = falses(p.nJ)
	vbartmp = (0.0,0.0)

	jidx = 0
	# indexes the state when moving to k: k, s, z, y, p, tau,  a, h, current loc, age
	# dimvecH = (p.nh, p.nJ, p.ns, p.nz, p.ny, p.np, p.ntau,  p.na, p.nh, p.nJ, p.nt-1 )
	hidx = 0

	agrid  = m.grids["assets"]
	a = 0.0

	# setup interpolation accelerator
	acc = Accelerator(0)



	# ===============
	# policy switches
	# ===============

	mortgageSub = false
	moneyMC     = false
	noSaving    = false
	noBuying    = false
	highMC      = false
	all_j       =  false
	pshock      = false

	Poterba = m.gridsXD["Poterba"]
	if p.policy == "mortgageSubsidy" || p.policy == "mortgageSubsidy_padjust"
		mortgageSub  = true
		@assert length(p.redistribute) == p.nt-1
		if p.verbose > 0
			println("policy is on: $mortgageSub")
		end
	end

	if p.policy=="moneyMC"
		if age == p.shockAge
			moneyMC = true
			# if now is age where you want to measure MC,
			# switch cost on
			setfield!(p,:noMC,false)
		else
			moneyMC = true
			# if not, switch cost off as in baseline
			setfield!(p,:noMC,true)
		end
	end

	if p.policy=="noSaving"
		noSaving = true
	end

	if p.policy=="noBuying"
		noBuying = true
	end

	if p.policy=="highMC" || p.policy == "yshock_highMC"
		highMC = true
	end

	if p.policy=="highMC_all_j"
		highMC = true
		all_j  = true
	end

	if p.policy=="all_j"
		all_j = true
	end
	if ((p.policy == "pshock") && (age >= p.shockAge))
		pshock = true
	end

	if p.policy == "pshock_highMC"
		# you can never move?
		# or you cannot move as soon as shock hits?
		highMC = true
		if age >= p.shockAge
			pshock = true
		end
	end

	if p.policy == "pshock_noSaving"
		noSaving = true
		if age >= p.shockAge
			pshock = true
		end
	end

	if p.policy == "pshock_noBuying"
		noBuying = true
		if age >= p.shockAge
			pshock = true
		end
	end

	if ((length(p.policy)>=7) && (p.policy[1:7] == "ypshock"))
		if age >= p.shockAge
			pshock = true
		end
	end

	# state dependent stochastic states 
	for iz=1:p.nz
		for iy=1:p.ny
			for ip=1:p.np
				for is=1:p.ns
					# compute expected future values given stochastic state
					# (iz,iy,ip,is)
					if age < p.nt-1
						integrateVbar!(iz,iy,ip,is,age+1,Gz,Gyp,Gs,m,p)
					end
					for itau=1:p.ntau
						tau = m.grids["tau"][itau]
						for ij=1:p.nJ
							# z = m.gridsXD["z"][iz,iy,ip,age,ij] 	# z is dollar income
							z = m.gridsXD["z"][iz+p.nz*(iy-1 + p.ny*(ip-1 + p.np*(age-1 + (p.nt-1)*(ij-1))))] 	# z is dollar income

							# for owner-mover case: get current region price
							if pshock && (p.shockReg == ij)
								# price_j = m.gridsXD["p"][iy,ip,ij]*p.shockVal
								price_j = m.gridsXD["p"][iy,ip,ij]*p.shockVal_p[age-p.shockAge+1]
							else
								price_j = m.gridsXD["p"][iy,ip,ij]
							end

							if all_j
								price_j = m.gridsXD["p"][iy,ip,p.shockReg]
							end

							for ih=0:1
	
								newz = 0.0

								# if mortgage subsidy policy is on: 
								if mortgageSub

									# 1) take away subsidy from owners
									if ih == 1 
										newz = z - Poterba[iz+p.nz*(iy-1 + p.ny*(ip-1 + p.np*(age-1 + (p.nt-1)*(ij-1))))]
									else
										newz = z
									end

									# 2) redistribute according to selected rule
									newz += p.redistribute[age]

								else
									# no policy
									newz = z
								end


								first = ih + (1-ih)*m.aone	# first admissible asset index
								for ia=first:p.na
									a_0 = agrid[ia]


									jidx = idx9(is,iz,iy,ip,itau,ia,ih+1,ij,age,p)

									# notice the ih-1+1 here!
									offset_1 = is-1 + p.ns * (iz-1 + p.nz * (iy-1 + p.ny * (ip-1 + p.np * (itau-1 + p.ntau * (ia-1 + p.na * (ih + p.nh * (ij-1 + p.nJ * (age-1))))))))
									@assert jidx == offset_1 + 1

									# =================
									# loop over choices
									# =================
									expv = Float64[]
									fill!(feasible_k,false)
										
									for ik=1:p.nJ
									
										move = ij != ik

										# find moving cost
										mc = movecost[idxMC(age,ij,ik,itau,ih+1,is,p)]
										if p.noMC 
											mc = 0.0
										end

										# if shut down moving from region ij
										if highMC && (p.shockReg==ij) && (ij!=ik)
											mc = 10000.0
										end

										offset_k = ik-1 + p.nJ * offset_1

										if pshock && (p.shockReg == ik)
											# price_j = m.gridsXD["p"][iy,ip,ij]*p.shockVal
											if ((age-p.shockAge+1 ==0) )
											# if ((age-p.shockAge+1 ==0) || ((age-p.shockAge+1)>length(p.shockVal_p)))
												println("requiresting index = $(age-p.shockAge+1)")
												println("age = $(age)")
												println("p.shockAge = $(p.shockAge)")
												println("of length $(length(p.shockVal_p))) array")
												error("stop")
											end
											price_k = m.gridsXD["p"][iy,ip,ik]*p.shockVal_p[age-p.shockAge+1]
										else
											price_k = m.gridsXD["p"][iy + p.ny * (ip-1 + p.np * (ik-1))]
										end

										if all_j
											price_k = m.gridsXD["p"][iy,ip,p.shockReg]
										end

										# kidx = idx10(ik,is,iz,iy,ip,itau,ia,ih+1,ij,age,p)
										# @assert offset_k+1 == kidx
										kidx = offset_k + 1

										# add money to assets of movers if required

										# if in moneyMC experiment and
										# if this is the period where we measure, ie there IS a MC (!p.noMC) and
										# if this is the value of someone who is moving
										if moneyMC && (!p.noMC) && (ij!=ik)
											a = a_0 + p.shockVal[1]
										else
											a = a_0
										end


										if ih==0
											canbuy = a + newz > p.chi * price_k
										else
											canbuy = a + newz + (1-p.phi)*price_j > p.chi * price_k
										end

										if noBuying
											canbuy = false
										end

										## constant utility shifter
										if is==1
											consta = ih*p.xi1 - mc + m.amenities[ik]
										else
											consta = ih*p.xi2 - mc + m.amenities[ik]
										end


										m.canbuy[kidx] = canbuy


										# you have a housing choice
										# if
										# 1) you are current owner who stays, or 
										# 2) you are current owner who moves and can buy, or
										# 3) you are current renter who can buy

										if ((ih==1 && (!move)) || (ih==1 && move && canbuy) || (ih==0 && canbuy))

											fill!(vstay,p.myNA)

											for ihh in 0:1

												hidx = ihh+1 + p.nh * offset_k
												# @assert hidx == idx11(ihh+1,ik,is,iz,iy,ip,itau,ia,ih+1,ij,age,p)

												# hidx = idx11(ihh+1,ik,is,iz,iy,ip,itau,ia,ih+1,ij,age,p)

												pp = (-ihh) * (1-p.chi) * price_k
												# borrowing limit for owner who stays can be lower than current margin call
												# if ih*ihh==1 && ik==ij
												# 	pp = pp < a ? pp : a
												# end

												blim = age < p.nt-1 ? pp : 0.0

												# reset w vector
												fill!(EV,p.myNA)
												fill!(w,p.myNA)


												cash = cashFunction(a,newz,ih,ihh,price_j,price_k,ij!=ik,ik,p)
												m.cash[hidx] = cash


												# find relevant future value:
												EVfunChooser!(EV,is,iz,ihh+1,itau,ip,iy,ij,ik,age,m,p)

												# # assign to interpolator
												# setVals

												# optimal savings choice
												rh = maxvalue(cash,is,p,agrid,w,ihh,mc,EV,blim,age,acc,noSaving,consta)

												vstay[ihh+1] = rh[1]

												# store save and cons in h-conditional arrays
												if rh[1] > p.myNA
													# m.vfeas[hidx] = true
													m.vh[hidx] = rh[1] 
													m.sh[hidx] = rh[2] 
													m.ch[hidx] = rh[3]
												else
													m.sh[hidx] = 0.0
													m.ch[hidx] = 0.0
												end

												if cash < 0 && ihh==0 && ih==0
													# println("state: j=$ij,tau=$itau,h=$ih,a=$(round(a)),z=$(round(z)),pj=$price_j,pk=$price_k,s=$is,k=$ik")
													# println("cash at ihh=$ihh is $cash")
													# println("maxvalue = $(r[1])")
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

										else # you cannot buy

											ihh = 0

											# hidx = idx11(ihh+1,ik,is,iz,iy,ip,itau,ia,ih+1,ij,age,p)
											# @assert hidx == ihh+1 + p.nh * offset_k
											hidx = ihh+1 + p.nh * offset_k

											blim = 0.0

											# reset w vector
											fill!(EV,p.myNA)	
											fill!(w,p.myNA)

											cash = cashFunction(a,newz,ih,ihh,price_j,price_k,ij!=ik,ij,p)
											m.cash[hidx] = cash

											# find relevant future value:
											EVfunChooser!(EV,is,iz,ihh+1,itau,ip,iy,ij,ik,age,m,p)

											# optimal savings choice
											r = maxvalue(cash,is,p,agrid,w,ihh,mc,EV,blim,age,acc,noSaving,consta)

											# checking for infeasible choices
											if r[1] > p.myNA
												# m.vfeas[hidx] = true
												m.vh[hidx]   = r[1]
												m.sh[hidx]   = r[2] 
												m.ch[hidx]   = r[3] 
												m.v[kidx]    = r[1] 	# for renter optimal housing choice is easy: value of renting.
												m.dh[kidx] = 0
											else
												m.vh[hidx]   = p.myNA
												m.sh[hidx]   = 0.0
												m.ch[hidx]   = 0.0
												m.v[kidx]  = p.myNA
												m.dh[kidx] = 0
											end

											if cash < 0 && ih==0 && ip<p.np
												# println("state: j=$ij,tau=$itau,h=$ih,a=$(round(a)),z=$(round(z)),p=$price,s=$is,k=$ik")
												# println("aggregate: P=$ip,Y=$iy")
												# println("cash at ihh=$ihh is $cash")
												# println("maxvalue = $(r[1])")
												# println("maxindex = $(r[2])")
											end

										end # end if stay and houseing choice
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
									end 	# end location choice

									if any(feasible_k)
										logsum = log( sum(expv) )
										m.vbar[jidx] = p.euler + logsum
									else
										m.vbar[jidx] = p.myNA
									end

									# compute rho: probability of moving to k given j
									for ik in 1:p.nJ
										if feasible_k[ik]
											# this has numerical underflow problems
											# need to make sure that vtmp[ik] - logsum is in a numerically stable range
											# of the exp function!

											# @assert idx10(ik,is,iz,iy,ip,itau,ia,ih+1,ij,age,p) == ik + p.nJ * offset_1
											@inbounds m.rho[ik + p.nJ * offset_1] = exp( (p.myNA + vtmp[ik]) - (p.myNA + logsum))
										else
											m.rho[ik + p.nJ * offset_1] = 0.0
										end
									end
								end 	#assets
							end 	# h
						end 	# location
					end 	#tau
				end		# s
			end 	#p
		end 	#y
	end 	#z
	return nothing

end

function integrateVbar!(iz::Int,iy::Int,ip::Int,is::Int,age::Int,Gz::Array{Float64,2},Gyp::Array{Float64},Gs::Array{Float64,2},m::Model,p::Param)

	# offsets
	# o_tau = 0
	o_p = 0
	o_y = 0
	o_z = 0
	o_s = 0

	# factors
	# f_tau = 0.0
	f_py = 0.0
	f_z = 0.0
	f_s = 0.0
		
	# @inbounds begin

	# dimvec  = (nhh, nJ, na, nh, nJ, ntau, ns, np, ny, nz, nt-1 )
	# r = ia-1 + p.na*(ih-1 + p.nh*(ij-1 + p.nJ*(itau-1 + p.ntau *(is-1 + p.ns*(ip-1 + p.np *(iy-1 + p.ny*(iz-1+p.nz*(age-1))))))))

	# tmp = 0.0

	# # build up future index for innermost to outermost
	# for iz1 = 1:p.nz
	# 	o_z = iz1-1 + p.nz*(age-1)
	# 	f_z = Gz[iz + p.nz * (iz1-1)]
	# 	for iy1 = 1:p.ny
	# 		o_y = iy1-1 + p.ny*o_z
	# 		for ip1 = 1:p.np
	# 			o_p = ip1-1 + p.np*o_y
	# 			@inbounds f_py = f_z * Gyp[iy + p.ny * ((ip-1) + p.np * ((iy1-1) + p.ny * (ip1-1))) ]
	# 			for is1 = 1:p.ns
	# 				o_s = is1-1 + p.ns*o_p
	# 				@inbounds f_s = f_py * Gs[is + p.ns * (is1-1)]
	# 				tmp = 0.0
	# 				m.EV[idx0] = tmp

	# 			end
	# 		end
	# 	end
	# end



	# looping over states which do not influence integration
	for ia = 1:p.na
	for ih = 1:p.nh
	for ij = 1:p.nJ
	for itau = 1:p.ntau

		# set value
		tmp = 0.0

		# compute current index
		idx0 = idx9(is,iz,iy,ip,itau,ia,ih,ij,age,p)

		idx1  = idx_p(ia,ih,ij,itau,age,p)

		# loop over future states
		# for itau1 = 1:p.ntau 		# future tau
		# 	o_tau = (idx1 + (itau1-1)) * p.np
		# 	f_tau = Gtau[itau1]
			for ip1 = 1:p.np 			
				o_p = (idx1 + (ip1-1)) * p.ny
				for iy1  = 1:p.ny 				# future y
					o_y  = (o_p + (iy1-1)) * p.nz
					# f_py = f_tau * Gyp[iy + p.ny * ((ip-1) + p.np * ((iy1-1) + p.ny * ((ip1-1) + p.np * (ij-1)))) ]
					@inbounds f_py = Gyp[iy + p.ny * ((ip-1) + p.np * ((iy1-1) + p.ny * (ip1-1))) ]
					for iz1 = 1:p.nz			# future z 		
						o_z = (o_y + (iz1-1)) * p.ns
						f_z = f_py * Gz[iz + p.nz * (iz1-1)]
						for is1   = 1:p.ns 				# future HHsize

							# compute future index 
				     	    # idx1 = idx9(is1,iz1,iy1,ip1,itau1,ia,ih,ij,age,p)

				     	    # construct sum
				     	    # TODO
							# mover's future value: mover specific transitions of z
				     	    # tmp0 = m.vbar[o_z + is1]
							@inbounds tmp += m.vbar[o_z + is1] * Gs[is + p.ns * (is1-1)] * f_z
							# tmp2 += tmp0 * Gs[is + p.ns * (is1-1)] * f_z2


						end
					end
				end
			end
		# end

		m.EV[idx0] = tmp
	end			
	end			
	end			
	end		


	# end #inbounds
	return nothing
end





# linear index functions
# ======================

# dimvecH  = (nh, nJ, ns, nz, ny, np, ntau,  nJ, nt-1 )
function idx11(ihh::Int,ik::Int,is::Int,iz::Int,iy::Int,ip::Int,itau::Int,ia::Int,ih::Int,ij::Int,age::Int,p::Param)

	 r = ihh + p.nh * (ik + p.nJ * (is + p.ns * (iz + p.nz * (iy + p.ny * (ip + p.np * (itau + p.ntau * (ia + p.na * (ih + p.nh * (ij + p.nJ * (age-1)-1)-1)-1)-1)-1)-1)-1)-1)-1)
	return r
end

# dimvec  = (nJ, ns, nz, ny, np, na, nh, ntau,  nJ, nt-1 )
function idx10(ik::Int,is::Int,iz::Int,iy::Int,ip::Int,itau::Int,ia::Int,ih::Int,ij::Int,age::Int,p::Param)

	 # r = ik + p.nJ * (is + p.ns * (iz + p.nz * (iy + p.ny * (ip + p.np * (itau + p.ntau * (ia + p.na * (ih + p.nh * (ij + p.nJ * (age-1)-1)-1)-1)-1)-1)-1)-1)-1)
	 r = ik + p.nJ * (is-1 + p.ns * (iz-1 + p.nz * (iy-1 + p.ny * (ip-1 + p.np * (itau-1 + p.ntau * (ia-1 + p.na * (ih-1 + p.nh * (ij-1 + p.nJ * (age-1)))))))))
	return r
end

# function idx10_azk(ik::Int,is::Int,iz::Int,iy::Int,ip::Int,itau::Int,ih::Int,ij::Int,age::Int,p::Param)

# 	r = zeros(Int,p.na)

# 	r = ik + p.nJ * (is-1 + p.ns * (iz-1 + p.nz * (iy-1 + p.ny * (ip-1 + p.np * (itau-1 + p.ntau * (ia-1 + p.na * (ih-1 + p.nh * (ij-1 + p.nJ * (age-1)))))))))
# 	r = ik + p.nJ * (is-1 + p.ns * (iz-1 + p.nz * (iy-1 + p.ny * (ip-1 + p.np * (itau-1 + p.ntau * (ia-1 + p.na * (ih-1 + p.nh * (ij-1 + p.nJ * (age-1)))))))))

# 	before = ik + p.nJ * ((is-1) + p.ns * ((iz-1) + p.nz * ((iy-1) + p.ny * ((ip-1) + p.np * ((itau-1) + p.ntau )))))
# 	after  = (ih-1 + p.nh * (ij-1 + p.nJ * (age-1)))

# 	for ia in 1:p.na
# 		r[ia] = before * ((ia-1) + p.na * after)
# 	end
# 	return r
# end
# dimvec2 = (ns, nz, ny, np, ntau, na, nh,nJ, nt-1 )
# dimvec2 = (na, nh, nJ, ntau, ns, np, ny,nz, nt-1 )
function idx9(is::Int,iz::Int,iy::Int,ip::Int,itau::Int,ia::Int,ih::Int,ij::Int,age::Int,p::Param)

	 r = is + p.ns * (iz + p.nz * (iy + p.ny * (ip + p.np * (itau + p.ntau * (ia + p.na * (ih + p.nh * (ij + p.nJ * (age-1)-1)-1)-1)-1)-1)-1)-1)
	 # r = ia + p.na * (ih + p.nh * (ij + p.nJ * (itau + p.ntau * (is + p.ns * (ip + p.np * (iy + p.ny * (iz + p.nz * (age-1)-1)-1)-1)-1)-1)-1)-1)
	return r
end

function idx_tau(ia::Int,ih::Int,ij::Int,age::Int,p::Param)

	 r = p.ntau * (ia + p.na * (ih + p.nh * (ij + p.nJ * (age-1)-1)-1)-1)
	return r
end

function idx_p(ia::Int,ih::Int,ij::Int,itau::Int,age::Int,p::Param)

	 r = p.np * (itau + p.ntau * (ia + p.na * (ih + p.nh * (ij + p.nJ * (age-1)-1)-1)-1)-1)
	return r
end

# p.na,p.nh,p.nP,p.np,p.nJ
function idxFinal(ia::Int,ih::Int,ip::Int,iy::Int,ij::Int,p::Param)

	r = ia + p.na * (ih-1 + p.nh* (ip-1 + p.np* (iy-1 + p.ny* (ij-1))))
    return r
end



function idxMC(it::Int,ij::Int,ik::Int,itau::Int,ih::Int,is::Int,p::Param)
	r = it + p.nt * (ij-1 + p.nJ * (ik-1 + p.nJ * (itau-1 + p.ntau * (ih-1 + p.nh * (is-1 )))))
	return r

end


# finds optimal value and 
# index of optimal savings choice
# on a given state
# discrete maximization
function maxvalue(cash::Float64,is::Int,p::Param,a::Array{Float64,1},w::Array{Float64,1},own::Int,mc::Float64,EV::Array{Float64,1},lb::Float64,age::Int,acc::Accelerator,noSaving::Bool,constant::Float64)

	# if your current cash debt is lower than 
	# maximum borrowing, infeasible
	if (lb < 0) && (cash < lb / p.Rm)
		return (p.myNA,0.0,0.0)
	else
		# compute value of all savings choices

		x = 0.0
		# grid for next period assets
		ub = noSaving ?  0.0 : cash-0.01 
		s = zeros(p.namax)
		# s = collect(linspace(lb,ub,p.namax))	# this implies you can save a lot if you have a lot of cash
		mylinspace!(s,lb,ub)	# this implies you can save a lot if you have a lot of cash
		# however in the interpolation you don't allow s > a[end]
		cons = zeros(p.namax)
		# fix upper bound of search grid
		# ub = minimum([cash-0.0001,a[end]])
		# ub = cash-0.0001 < a[end] ? cash-0.0001 : a[end]

		# w[i] = u(cash - s[i]/(1+r)) + beta EV(s[i],h=1,j,t+1)
		if p.ctax == 1.0
			vsavings!(w,a,EV,s,cons,cash,is,own,mc,p,acc,constant)	
		else
			vsavings!(w,a,EV,s,cons,cash,is,own,mc,p,acc,true,constant)	
		end

		r = findmax(w)

	# println("own=$own, cash=$cash, is=$is, itau=$itau, mc=$mc, def=$def,lb=$lb")
	# println("V = $(r[1]), save = $(s[r[2]]), cons = $(cash-x)")
	# println("------------------------------------------------")
	# end
		return (r[1],s[r[2]],cons[r[2]])	# (value,saving,consumption)
	end
end


function mylinspace!(s::Vector{Float64},lb::Float64,ub::Float64)
	n = length(s)
	r = (ub-lb) / (n-1)
	for i in 0:(n-1)
		s[i+1] = lb + i * r
	end
end


function vsavings!(w::Array{Float64,1},a::Array{Float64,1},EV::Array{Float64,1},s::Array{Float64,1},cons_arr::Array{Float64,1},cash::Float64,is::Int,own::Int,mc::Float64,p::Param,acc::Accelerator,constant::Float64)
	n = p.namax
	x = 0.0
	cons = 0.0
	# jinf = 1
	# jsup = p.na
	
	# compute consumption at each potential savings choice
	# consumption is cash - x, where x is s/(1+interest)
	# and interest depends on whether borrow or save

	# very careful here!
	# innermost loop punishes a lot!

	setAccel!(acc,1)

	 for i=1:n
		lin = linearapprox(a,EV,s[i],p.na,acc)
		# println("s=$(s[i]), i=$i")
		# println("lin = $lin")
		if s[i] < 0
			x = s[i] / p.Rm
		else
			x = s[i] / p.R
		end

		# w[i] = ufun(cash-x,is,own,itau,mc,def,p) + p.beta * lin[1]
		# note: cons^(1-gamma) = exp( (1-gamma)*log(cons) )
		cons = (cash-x)*p.sscale[is]
		if cons < 0
			w[i] = p.myNA				
		else
			w[i] = ufun(cons,lin,p)
			# w[i] = p.imgamma * myexp(p.mgamma * mylog(cons) ) + p.beta * lin[1]
			w[i] += constant
		end
		# jinf = lin[2]

		# println("jsup = $jsup")
		# println("jinf = $jinf")
		cons_arr[i] = cons
	end

	return 
end

function vsavings!(w::Array{Float64,1},a::Array{Float64,1},EV::Array{Float64,1},s::Array{Float64,1},cons_arr::Array{Float64,1},cash::Float64,is::Int,own::Int,mc::Float64,p::Param,acc::Accelerator,ctax_adjust::Bool,constant::Float64)
	n = p.namax
	x = 0.0
	cons = 0.0
	# jinf = 1
	# jsup = p.na
	
	# compute consumption at each potential savings choice
	# consumption is cash - x, where x is s/(1+interest)
	# and interest depends on whether borrow or save

	# very careful here!
	# innermost loop punishes a lot!

	setAccel!(acc,1)

	for i=1:n
		lin = linearapprox(a,EV,s[i],p.na,acc)
		# println("s=$(s[i]), i=$i")
		# println("lin = $lin")
		if s[i] < 0
			x = s[i] / p.Rm
		else
			x = s[i] / p.R
		end

		# w[i] = ufun(cash-x,is,own,itau,mc,def,p) + p.beta * lin[1]
		# note: cons^(1-gamma) = exp( (1-gamma)*log(cons) )
		cons = (cash-x)*p.sscale[is]
		if cons < 0
			w[i] = p.myNA		
		else
			if ctax_adjust
			# adjust consumption with scale factor tau. tau = 1 in baseline
				cons *= p.ctax
				w[i] = ufun(cons,lin,p)
				# w[i] = p.imgamma * myexp(p.mgamma * mylog(cons) ) + p.beta * lin[1]
				w[i] += constant
			else
				error("you are running the ctax-method of vsavings without doing the adjustment")
			end
		end
		# jinf = lin[2]

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
function pifun(ih::Int,ihh::Int,price_j::Float64,price_k::Float64,move::Bool,ik::Int,p::Param)
	r = 0.0

	if ih==0
		# if you came into period as a renter:
		# choose whether to buy.  	
		# in this case not necessarily price_k == price_j (you could be moving)
		r = -(1-ihh)*p.kappa[ik]*price_k - ihh * price_k
	else
		if move
			#  must take two different prices into account
			r = (1-p.phi)*price_j - ((1-ihh)*p.kappa[ik] + ihh)*price_k
		else
			# you only pay anyting if you sell and rent
			# in this case price_k == price_j
			r = (1-ihh)*(1-p.phi-p.kappa[ik])*price_k 
		end
	end
	return r
end

function income(muy::Float64,shock::Float64)
	y = muy+shock
end


# cashfunction
# computes cash on hand given a value of the
# state vector and a value of the discrete choices
function cashFunction(a::Float64, y::Float64, ih::Int, ihh::Int,price_j::Float64,price_k::Float64,move::Bool,ik::Int,p::Param)
	a + y + pifun(ih,ihh,price_j,price_k,move,ik,p)
end


# EV selector
# given current state and discrete choice, which portion of
# EV is relevant for current choice?
function EVfunChooser!(ev::Array{Float64,1},is::Int,iz::Int,ihh::Int, itau::Int, ip::Int,iy::Int, ij::Int, ik::Int,age::Int,m::Model,p::Param)
	if age==p.nt-1
		for ia in 1:p.na
			ev[ia] = m.EVfinal[idxFinal(ia,ihh,ip,iy,ik,p)]
		end
	else 

		offset_after = ihh-1 + p.nh * (ik-1 + p.nJ * (age))	# need age+1-1 here!
		# if ik==ij
		# 	 for ia in 1:p.na
		# 		ev[ia] = m.EV[idx9(is,iz,iy,ip,itau,ia,ihh,ik,age+1,p)]
		# 	end
		# else
			 for ia in 1:p.na
			 	idx = is + p.ns * (iz-1 + p.nz * (iy-1 + p.ny * (ip-1 + p.np * (itau-1 + p.ntau * (ia-1 + p.na*offset_after)))))
			 	# @assert idx == idx9(is,iz,iy,ip,itau,ia,ihh,ik,age+1,p)
				ev[ia] = m.EV[idx]
				# ev[ia] = m.EVMove[idx10(is,iy,ip,iP,iz,ia,ihh,itau,ik,age+1,p)]
			end
		# end
	end

	return nothing
end





function linearapprox(x::Array{Float64,1},y::Array{Float64,1},xi::Float64,lo::Int,hi::Int)
	n = length(y)	# get rid
	@assert (length(x) == n)
	r = 0.0

	# determining bounds 
	if xi == x[1]
		r = y[1] 
		return (r,1)
	elseif xi < x[1]
		# get linear approx below
		@inbounds r = y[1] + (y[2] - y[1]) * (xi - x[1])  / (x[2] - x[1])
		return (r,1)
	end
	if xi == x[n]
		r = y[n] 
		return (r,n)
	elseif xi > x[n]
		# get linear approx above
		@inbounds r = y[n] + (y[n] - y[n-1]) * (xi - x[n])  / (x[n] - x[n-1])
		return (r,n)
	end

	# if have to find interval
	if hi - lo > 1
		jinf = searchsortedlast(x,xi,lo,hi,Base.Forward)	# get rid
	# if not, lo is jinf
	else
		jinf = lo
	end
	@inbounds r = (y[jinf] * (x[jinf+1] - xi) + y[jinf+1] * (xi - x[jinf]) ) / (x[jinf+1] - x[jinf])
	return (r,jinf)
end


function linearapprox(x::Array{Float64,1},y::Array{Float64,1},xi::Float64,feasible::BitArray{1},p::Param)
	n = length(y)
	@assert (length(x) == n)
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
	@inbounds 	r = (y[jinf] * (x[jinf+1] - xi) + y[jinf+1] * (xi - x[jinf]) ) / (x[jinf+1] - x[jinf])
	else 
		r = p.myNA
	end
	return (r,jinf)
end

function linearapprox(x::Array{Float64,1},y::Array{Float64,1},xi::Float64)
	n = length(y)
	@assert (length(x) == n)
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
	@inbounds r = (y[jinf] * (x[jinf+1] - xi) + y[jinf+1] * (xi - x[jinf]) ) / (x[jinf+1] - x[jinf])
	return (r,jinf)
end

function integrateFinal!(m::Model)

	m.EVfinal = T_Final(m.EVfinal,m.gridsXD)
	return nothing
end



# 2D linear approximation
# =======================

# function bilinearapprox{T<:Real}(x::T,y::T,xgrid::Vector{T},ygrid::Vector{T},zmat::Matrix{T})

# 	# zmat[i,j] = f(xgrid[i],ygrid[j])
# 	n = length(xgrid)
# 	m = length(ygrid)

# 	if length(zmat) != n*m
# 		throw(ArgumentError("zmat must be (length(x),length(y))"))
# 	end


# 	# find last grid point in xgrid smaller or equal to x
# 	if x < xgrid[1]
# 		ix = 1
# 		warn("x=$x < $(xgrid[1]). setting x = xgrid[1]")
# 		x = xgrid[1]
# 	elseif x > xgrid[n]
# 		ix = n-1
# 		warn("x=$x > $(xgrid[n]). setting x = xgrid[n]")
# 		x = xgrid[n]
# 	else
# 		ix = searchsortedlast(xgrid,x,1,n-1,Base.Forward)
# 	end

# 	if y < ygrid[1]
# 		iy = 1
# 		warn("y=$y < $(ygrid[1]). setting y = ygrid[1]")
# 		y = ygrid[1]
# 	elseif y > ygrid[m]
# 		iy = m-1
# 		warn("y=$y > $(ygrid[m]). setting y = ygrid[m]")
# 		y = ygrid[m]
# 	else
# 		iy = searchsortedlast(ygrid,y,1,m-1,Base.Forward)
# 	end

# 	@inbounds begin
# 	# get boxes around x and y
# 	xmin = xgrid[ix]
# 	xmax = xgrid[ix+1]
# 	ymin = ygrid[iy]
# 	ymax = ygrid[iy+1]

# 	# get value of z at 4 vertices
# 	zxminymin = zmat[ix + n*(iy-1)]
# 	zxmaxymin = zmat[ix+1 + n*(iy-1)]
# 	zxminymax = zmat[ix + n*iy ]
# 	zxmaxymax = zmat[ix+1 + n*iy]
# 	end

# 	# length of brackets
# 	dx = xmax - xmin
# 	dy = ymax - ymin

# 	# relative position of x in bracket
# 	t = (x - xmin)/dx
# 	u = (y - ymin)/dy

# 	# linear combination of locations
# 	z = (1.0-t)*(1.0-u)*zxminymin + t*(1.0-u)*zxmaxymin + (1.0-t)*u*zxminymax + t*u*zxmaxymax
# end


# # same for 2 functions in zmat and zmat2 defined on same space
# function bilinearapprox{T<:Real}(x::T,y::T,xgrid::Vector{T},ygrid::Vector{T},zmat::Matrix{T},zmat2::Matrix{T})

# 	# zmat[i,j] = f(xgrid[i],ygrid[j])
# 	n = length(xgrid)
# 	m = length(ygrid)

# 	if length(zmat) != n*m
# 		throw(ArgumentError("zmat must be (length(x),length(y))"))
# 	end


# 	# find last grid point in xgrid smaller or equal to x
# 	# find last grid point in xgrid smaller or equal to x
# 	if x < xgrid[1]
# 		ix = 1
# 		warn("x=$x < $(xgrid[1]). setting x = xgrid[1]")
# 		x = xgrid[1]
# 	elseif x > xgrid[n]
# 		ix = n-1
# 		warn("x=$x > $(xgrid[n]). setting x = xgrid[n]")
# 		x = xgrid[n]
# 	else
# 		ix = searchsortedlast(xgrid,x,1,n-1,Base.Forward)
# 	end

# 	if y < ygrid[1]
# 		iy = 1
# 		warn("y=$y < $(ygrid[1]). setting y = ygrid[1]")
# 		y = ygrid[1]
# 	elseif y > ygrid[m]
# 		iy = m-1
# 		warn("y=$y > $(ygrid[m]). setting y = ygrid[m]")
# 		y = ygrid[m]
# 	else
# 		iy = searchsortedlast(ygrid,y,1,m-1,Base.Forward)
# 	end

# 	@inbounds begin
# 	# get boxes around x and y
# 	xmin = xgrid[ix]
# 	xmax = xgrid[ix+1]
# 	ymin = ygrid[iy]
# 	ymax = ygrid[iy+1]

# 	# get value of z at 4 vertices of zmat 1
# 	zxminymin = zmat[ix + n*(iy-1)]
# 	zxminymax = zmat[ix + n*iy ]
# 	zxmaxymin = zmat[ix+1 + n*(iy-1)]
# 	zxmaxymax = zmat[ix+1 + n*iy]

# 	# get value of z at 4 vertices of zmat 2
# 	z2xminymin = zmat2[ix + n*(iy-1)]
# 	z2xminymax = zmat2[ix + n*iy ]
# 	z2xmaxymin = zmat2[ix+1 + n*(iy-1)]
# 	z2xmaxymax = zmat2[ix+1 + n*iy]
# 	end

# 	# length of brackets
# 	dx = xmax - xmin
# 	dy = ymax - ymin

# 	# relative position of x in bracket
# 	t = (x - xmin)/dx
# 	u = (y - ymin)/dy

# 	# linear combination of locations
# 	z = (1.0-t)*(1.0-u)*zxminymin + t*(1.0-u)*zxmaxymin + (1.0-t)*u*zxminymax + t*u*zxmaxymax
# 	z2 = (1.0-t)*(1.0-u)*z2xminymin + t*(1.0-u)*z2xmaxymin + (1.0-t)*u*z2xminymax + t*u*z2xmaxymax
# 	(z,z2)
# end


# # same for 3 
# function bilinearapprox{T<:Real}(x::T,y::T,xgrid::Vector{T},ygrid::Vector{T},zmat::Matrix{T},zmat2::Matrix{T},zmat3::Matrix{T})

# 	# zmat[i,j] = f(xgrid[i],ygrid[j])
# 	n = length(xgrid)
# 	m = length(ygrid)

# 	if length(zmat) != n*m
# 		throw(ArgumentError("zmat must be (length(x),length(y))"))
# 	end


# 	# find last grid point in xgrid smaller or equal to x
# 	if x < xgrid[1]
# 		ix = 1
# 		warn("x=$x < $(xgrid[1]). setting x = xgrid[1]")
# 		x = xgrid[1]
# 	elseif x > xgrid[n]
# 		ix = n-1
# 		warn("x=$x > $(xgrid[n]). setting x = xgrid[n]")
# 		x = xgrid[n]
# 	else
# 		ix = searchsortedlast(xgrid,x,1,n-1,Base.Forward)
# 	end

# 	if y < ygrid[1]
# 		iy = 1
# 		warn("y=$y < $(ygrid[1]). setting y = ygrid[1]")
# 		y = ygrid[1]
# 	elseif y > ygrid[m]
# 		iy = m-1
# 		warn("y=$y > $(ygrid[m]). setting y = ygrid[m]")
# 		y = ygrid[m]
# 	else
# 		iy = searchsortedlast(ygrid,y,1,m-1,Base.Forward)
# 	end

# 	@inbounds begin
# 	# get boxes around x and y
# 	xmin = xgrid[ix]
# 	xmax = xgrid[ix+1]
# 	ymin = ygrid[iy]
# 	ymax = ygrid[iy+1]

# 	# get value of z at 4 vertices of zmat 1
# 	zxminymin = zmat[ix + n*(iy-1)]
# 	zxminymax = zmat[ix + n*iy ]
# 	zxmaxymin = zmat[ix+1 + n*(iy-1)]
# 	zxmaxymax = zmat[ix+1 + n*iy]

# 	# get value of z at 4 vertices of zmat 2
# 	z2xminymin = zmat2[ix + n*(iy-1)]
# 	z2xminymax = zmat2[ix + n*iy ]
# 	z2xmaxymin = zmat2[ix+1 + n*(iy-1)]
# 	z2xmaxymax = zmat2[ix+1 + n*iy]

# 	# get value of z at 4 vertices of zmat 2
# 	z3xminymin = zmat3[ix + n*(iy-1)]
# 	z3xminymax = zmat3[ix + n*iy ]
# 	z3xmaxymin = zmat3[ix+1 + n*(iy-1)]
# 	z3xmaxymax = zmat3[ix+1 + n*iy]

# 	end

# 	# length of brackets
# 	dx = xmax - xmin
# 	dy = ymax - ymin

# 	# relative position of x in bracket
# 	t = (x - xmin)/dx
# 	u = (y - ymin)/dy

# 	# linear combination of locations
# 	z  = (1.0-t)*(1.0-u)*zxminymin  + t*(1.0-u)*zxmaxymin  + (1.0-t)*u*zxminymax  + t*u*zxmaxymax
# 	z2 = (1.0-t)*(1.0-u)*z2xminymin + t*(1.0-u)*z2xmaxymin + (1.0-t)*u*z2xminymax + t*u*z2xmaxymax
# 	z3 = (1.0-t)*(1.0-u)*z3xminymin + t*(1.0-u)*z3xmaxymin + (1.0-t)*u*z3xminymax + t*u*z3xmaxymax
# 	(z,z2,z3)
# end





