

# solving the model at the current 
# parameter vector

# main loop
function solve!(m::Model, p::Param)

	# final period
	solveFinal!(m,p)

	# loop over time
	for age=p.nt-1:1

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
# function solvePeriod!(age::Int,m::Model,p::Param)


# 	# res = zeros(size(m.v))
# 	res = zeros(p.na,p.nz,p.nh,p.ntau,p.nP,p.nY,p.np,p.ny,p.nJ,p.nt,p.nh,p.nJ)
# 	# ================
# 	# loop over states
# 	# ================

# 	v = squeeze(m.v[:,:,:,:,:,:,:,:,:,age,:,:],10)

# 	for ik=1:p.nJ
# 		for ihh in 0:1
# 			for ij=1:p.nJ				# current location
# 				for iy=1:p.ny 				# regional income deviation
# 					for ip=1:p.np 				# regional price deviation
# 						for iY=1:p.nY 				# national income lebel
# 							for iP=1:p.nP 				# national price index
# 								for itau=1:p.ntau			# type
# 									for ih=0:1
# 										for iz=1:p.nz				# individual income shock
# 											for ia=1:p.na

# 												v[ia,iz,ih+1,itau,iP,iY,ip,iy,ij,ihh+1,ik] = rand();

# 											end # choice: housing
# 										end	# choice: location 
# 									end # state: assets
# 								end	# state: housing
# 							end	# state: local y-level
# 						end	# state: local p-level
# 					end	# state: aggregate Y-level
# 				end	# state: aggregate P-level
# 			end	# state: individual z
# 		end	# state: individual tau
# 	end	# state: location

# 		# @bp
# 	return v

# end


# period loop
# @debug function solvePeriod!(age::Int,m::Model,p::Param)
function solvePeriod!(age::Int,m::Model,p::Param)

	w = zeros(p.na)
	r = (0.0,0)
	movecost = m.gridsXD["movecost"]

	vhtmp = zeros(p.nJ) 
	expvh = zeros(p.nJ) 
	vbartmp = 0.0

	v = squeeze(m.v[:,:,:,:,:,:,:,:,:,:,:,age],12)
	c = squeeze(m.c[:,:,:,:,:,:,:,:,:,:,:,age],12)
	s = squeeze(m.s[:,:,:,:,:,:,:,:,:,:,:,age],12)

	vh  = squeeze(m.vh[:,:,:,:,:,:,:,:,:,:,age],11)
	dh  = squeeze(m.dh[:,:,:,:,:,:,:,:,:,:,age],11)
	rho = squeeze(m.rho[:,:,:,:,:,:,:,:,:,:,age],11)

	vbar = squeeze(m.vbar[:,:,:,:,:,:,:,:,:,age],10)



	# ================
	# loop over states
	# ================

	# dimvec  = (nh, nJ, na, nh, ny, np, nY ,nP, nz, ntau,  nJ, nt-1 )

	for ij=1:p.nJ				# current location
		ageeffect = m.grids2D["ageprof"][age,ij]
		for itau=1:p.ntau			# type
			tau = p.tau[itau]
			for iz=1:p.nz				# individual income shock
				z = m.grids["z"][iz]
				for iP=1:p.nP 				# national price index
					for iY=1:p.nY 				# national income lebel
						for ip=1:p.np 				# regional price deviation
							for iy=1:p.ny 				# regional income deviation
								price = m.gridsXD["p"][iP,ip,ij]
								y     = m.gridsXD["y"][iY,iy,ij]
								for ih=0:1

									# choose asset grid for owner/renter
									agrid = agridChooser(ih,m)

									# loop over asset levels
									for ia=1:p.na

										a = agrid[ia]

										# given h, price and a, figure out if in neg equtiy
										def=false

										# =================
										# loop over choices
										# =================

										fill!(vhtmp,0.0)
										fill!(expvh,0.0)

										# location choice
										for ik=1:p.nJ

											# housing choice
											for ihh in 0:1

												# println("loop: ij=$(ij), itau=$itau, iz=$iz,iP=$iP, iY=$iY, ip=$ip, iy=$iy, price=$price, y=$y,ih=$ih,ia=$ia,ik=$ik,ihh=$ihh")

												# choose relevant savings grid
												sgrid = agridChooser(ihh,m)

												if ihh*(ij!=ik)==1
												
													# ruled out: do nothing
													# cannot be choose h=1 and move

												else

													# reset w vector
													fill!(w,p.myNA)

													# cashfunction(a,y,ageeffect,z,ih,ihh)
													cash = cashFunction(a,income(y,ageeffect,z),ih,ihh,price,ij!=ik,ik,p)

													# find moving cost
													mc = movecost[age,ij,ik,ih+1,itau]

													# find relevant future value:
													EV = EVfunChooser(iz,ihh,itau,iP,iY,ip,iy,ik,age,m,p)

													# optimal savings choice
													r = maxvalue(cash,p,sgrid,w,ihh,mc,def,EV)

													# put into vfun, savings and cons policies
													v[ihh+1,ik,ia,ih+1,iy,ip,iY,iP,iz,itau,ij] = r[1]
													s[ihh+1,ik,ia,ih+1,iy,ip,iY,iP,iz,itau,ij] = r[2]
													c[ihh+1,ik,ia,ih+1,iy,ip,iY,iP,iz,itau,ij] = cash - sgrid[ r[2] ]

												end # if owner wants to move
											end # choice: housing

											# get housing discrete choice
											r = findmax(v[:,ik,ia,ih+1,iy,ip,iY,iP,iz,itau,ij])

											vh[ik,ia,ih+1,iy,ip,iY,iP,iz,itau,ij] = r[1]
											dh[ik,ia,ih+1,iy,ip,iY,iP,iz,itau,ij] = r[2]

											vhtmp[ik] = r[1]
											expvh[ik] = exp(r[1])

										end	# choice: location 

										# compute vbar and rho
										vbartmp = p.euler + log(sum(expvh))
										vbar[ia,ih+1,iy,ip,iY,iP,iz,itau,ij] = vbartmp

										for ik in 1:p.nJ
											rho[ik,ia,ih+1,iy,ip,iY,iP,iz,itau,ij] = exp( p.euler - vbartmp + vhtmp[ik])
										end

									end # state: assets
								end	# state: housing
							end	# state: local y-level
						end	# state: local p-level
					end	# state: aggregate Y-level
				end	# state: aggregate P-level
			end	# state: individual z
		end	# state: individual tau
	end	# state: location



	# repack into m
	m.v[:,:,:,:,:,:,:,:,:,:,:,age] = v;
	m.s[:,:,:,:,:,:,:,:,:,:,:,age] = s;
	m.c[:,:,:,:,:,:,:,:,:,:,:,age] = c;

	m.vh[:,:,:,:,:,:,:,:,:,:,age] = vh;
	m.dh[:,:,:,:,:,:,:,:,:,:,age] = dh;
	m.rho[:,:,:,:,:,:,:,:,:,:,age] = rho;
	
	# vbar is redundant: get rid of this later on
	m.vbar[:,:,:,:,:,:,:,:,:,age] = vbar;

	integrateVbar!(vbar,m)

	return nothing

end


function integrateVbar!(vb::Array{Float64,9},m::Model)

# function T_Evbar(GP,GY,Gp,Gy,Gz,V)
	m.EV[:,:,:,:,:,:,:,:,:,age] = T_Evbar(m.grids2D["GP"],m.grids2D["GY"],m.gridsXD["Gp"],m.gridsXD["Gy"],m.gridsXD["Gz"],vb)
	return nothing

end



function tfun1()
	A = rand(1000,1000,2)
	B = zeros(1000,1000)
	C = zeros(Int,(1000,1000))
	for i=1:1000
		for j=1:1000
			r = findmax(A[i,j,:])
			B[i,j] = r[1]
			C[i,j] = r[2]
		end
	end
	return nothing
end

function tfun2()
	A = rand(1000,1000,2)
	x = mapslices(findmax,A,3)
	return nothing
end








function computeRhoVbar!(age::Int,m::Model,p::Param)

	# dimvec3 = (na, nz, nh, ntau, nP, nY, np ,ny, nJ, (nt-1))  
	m.vbar[:,:,:,:,:,:,:,:,:,age] = p.euler .+ log( sum( exp( m.vh ), 11 ) )

	# dimvec2 = (na, nz, nh, ntau, nP, nY, np ,ny, nJ, (nt-1),  nJ)
	m.rho [:,:,:,:,:,:,:,:,:,age,:] = exp( p.euler .+ m.vh[:,:,:,:,:,:,:,:,:,age,:] .- m.vbar[:,:,:,:,:,:,:,:,age] ) 
	return nothing

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


# EV selector
# given current state and discrete choice, which portion of
# EV is relevant for current choice?
function EVfunChooser(iz::Int,ihh::Int, itau::Int, iP::Int,iY::Int,ip::Int,iy::Int, ik::Int,age::Int,m::Model,p::Param)

	if age==p.nt-1
		m.EVfinal[:,ihh+1,iP,ip,ik]
	else 
		m.EV[:,ihh+1,iy,ip,iY,iP,iz,itau,ik,age+1]
	end
end


# housing discrete choice
function computeHousingDchoice!(age::Int,m::Model)

	# dimvec  = (na, nz, nh, ntau, nP, nY, np ,ny, nJ, (nt-1),  nh, nJ)
	# r = ismaxfun(squeeze(m.v[:,:,:,:,:,:,:,:,:,age,:,:],10), 10)  # max over nh
	r = mapslices(findmax,m.v[:,:,:,:,:,:,:,:,:,age,:,:], 11)  # max over nh

	# dimvec2 = (na, nz, nh, ntau, nP, nY, np ,ny, nJ, (nt-1),  nJ)
	# get the first elt of each tuple: the value
	m.vh[:,:,:,:,:,:,:,:,:,age,:] = map(x->x[1],r)
	# get the second elt of each tuple: the index
	m.dh[:,:,:,:,:,:,:,:,:,age,:] = map(x->x[2],r)

	return nothing
end



function integrateVbar!(age::Int,m::Model)

	vbar = squeeze(m.vbar[:,:,:,:,:,:,:,:,:,age],10)
	# dimvec3 = (na, nz, nh, ntau, nP, nY, np ,ny, nJ, (nt-1)) 
	m.EV[:,:,:,:,:,:,:,:,:,age] = T_Evbar(vbar,m.gridsXD["Gz"],m.gridsXD["Gp"],m.gridsXD["Gy"],m.grids2D["GP"],m.grids2D["GY"])
	return nothing

end






function integrateFinal!(m::Model)

	m.EVfinal = T_Final(m.EVfinal,m.gridsXD)
	return nothing
end







