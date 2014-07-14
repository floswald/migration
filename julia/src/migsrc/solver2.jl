

# solving the model at the current 
# parameter vector

# main loop
function solve!(m::Model2, p::Param)

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


function solveFinal!(m::Model2,p::Param)

	# extract grids for faster lookup
	agrid = m.grids["assets"]
	hgrid = m.grids["housing"]
	# loop over all states
	for ia = 1:p.na
	for ih = 1:p.nh

		if ia == m.aone
			tmp1 = p.omega1 + p.omega2 * log(agrid[ia+1] + hgrid[ih]  )
			tmp2 = p.omega1 + p.omega2 * log(agrid[ia+2] + hgrid[ih]  )

			m.EVfinal[ia,ih] = tmp1 + (tmp2-tmp1) * (agrid[ia] - agrid[ia+1]) / agrid[ia+2] - agrid[ia+1]
	
		elseif ia > m.aone

			m.EVfinal[ia,ih] = p.omega1 + p.omega2 * log(agrid[ia] + hgrid[ih] )
		else
			m.EVfinal[ia,ih] = p.myNA
		end

	end
	end

	# integrate
	# m.EVfinal = E_tensors.T_Final(m.grids2D["GP"],m.gridsXD["p"],m.EVfinal)

	return nothing
end



# period loop
# @debug function solvePeriod!(age::Int,m::Model,p::Param)
function solvePeriod!(age::Int,m::Model2,p::Param)

	# initialise some objects

	vstay = zeros(2)
	sstay = zeros(2)
	cstay = zeros(2)

	w = zeros(p.namax)
	EV = zeros(p.na)

	# return value for findmax: tuple (value,index)
	r = (0.0,0)

	first = 1

	canbuy = false


	Gz = m.gridsXD["Gz"]
	# GzM = m.gridsXD["GzM"]

	vtmp = zeros(p.nJ) 

	# ================
	# loop over states
	# ================

	# dimvec  = (nz, na, nh, nt-1 )
	# V[z,a,h,j,age]

	agrid = m.grids["assets"]
	sgrid0 = m.grids["saving0"]
	sgrid1 = m.grids["saving1"]

	for ih=0:1
		# choose asset grid for owner/renter
		# agrid = agridChooser(ih,m)
		first = ih + (1-ih)*m.aone	# first admissible asset index
		for ia=first:p.na
			a = agrid[ia]

			# start loop over stochastic states
			# ---------------------------------

			for iz=1:p.nz				# individual income shock
				z = m.gridsXD["z"][iz,age]
				price = m.price
				# given h, price and a, figure out if in neg equtiy

				canbuy = a + z > p.chi * price

				# =================
				# loop over choices
				# =================

				# fill!(vtmp,0.0)
				# fill!(expv,0.0)

				# TODO
				# get a temporary copy of EV[possible.choices|all.states]
				# fillTempEV!(tempEV,jidx)


				# now you know the index of the 
				# state when moving to k
				# kidx = idx4(iz,ia,ih+1,age,p)

				# you have h choice
				if ih==1 || (ih==0 && canbuy)

					fill!(vstay,p.myNA)
					fill!(sstay,0)
					fill!(cstay,0.0)

					# optimal housing choice
					for ihh in 0:1

						# reset w vector
						fill!(EV,p.myNA)
						fill!(w,p.myNA)

						cash = cashFunction(a,z,ih,ihh,price,p)

						# find relevant future value:
						EVfunChooser!(EV,iz,ihh+1,age,m,p)

						# optimal savings choice
						r = maxvalue(cash,p,agrid,sgrid0,sgrid1,w,ihh,EV,m.aonemax)

						# put into vfun, savings and cons policies
						vstay[ihh+1] = r[1]
						sstay[ihh+1] = sgrid0[ r[2] ]
						cstay[ihh+1] = cash - sgrid0[ r[2] ]

					end

					# find optimal housing choice
					# TODO is that slow?
					r = findmax(vstay)
					# and store value, discrete choice idx, savings idx and consumption

					# checking for feasible choices
					if r[1] > p.myNA
						m.v[iz,ia,ih+1,age]  = r[1]
						m.dh[iz,ia,ih+1,age] = r[2] - 1
						m.s[iz,ia,ih+1,age]  = sstay[r[2]] 
						m.c[iz,ia,ih+1,age]  = cstay[r[2]]								
					else
						# infeasible
						m.v[iz,ia,ih+1,age]  = r[1]
						m.dh[iz,ia,ih+1,age] = 0
						m.s[iz,ia,ih+1,age]  = 0
						m.c[iz,ia,ih+1,age]  = 0
					end

				# current renter who cannot buy
				else

					ihh = 0

					# reset w vector
					fill!(EV,p.myNA)	
					fill!(w,p.myNA)

					# cashfunction(a,y,ageeffect,z,ih,ihh)
					cash = cashFunction(a,z,ih,ihh,price,p)

					# find relevant future value:
					EVfunChooser!(EV,iz,ihh+1,age,m,p)

					# optimal savings choice
					r = maxvalue(cash,p,agrid,sgrid0,sgrid1,w,ihh,EV,m.aonemax)

					# put into vfun, savings and cons policies
					m.v[iz,ia,ih,age] = r[1]

					# checking for infeasible choices
					if r[1] > p.myNA
						m.s[iz,ia,ih+1,age] = sgrid0[ r[2] ]
						m.c[iz,ia,ih+1,age] = cash - sgrid0[ r[2] ]
					else
						m.s[iz,ia,ih+1,age] = 0
						m.c[iz,ia,ih+1,age] = 0
					end

				end

	# V[z,a,h,age]
				m.EV[iz,ia,ih+1,age] = integrateV(ia,ih+1,iz,age,p,Gz,m)

			end # individual z
		end	# assets
	end	# housing

	return nothing

end

function integrateV(ia::Int,ih::Int,iz::Int,age::Int,p::Param,Gz::Array{Float64,2},m::Model2)
	# set index
	idx = 0
	# set value
	tmp = 0.0
	# tmp2 = 0.0
	for iz1 = 1:p.nz			# future z 		

		# compute index in integrand: uses ix1 indices!
 	    idx = iz1 + p.nz * (ia + p.na * (ih + p.nh * (age-1)-1)-1)

 	    # construct sum
		tmp += m.v[idx] * Gz[iz + p.nz * (iz1-1)] 
	end
	# 
	return tmp
end




# finds optimal value and 
# index of optimal savings choice
# on a given state
# discrete maximization
function maxvalue(x::Float64,p::Param,a::Array{Float64,1},s0::Array{Float64,1},s1::Array{Float64,1},w::Array{Float64,1},own::Int,EV::Array{Float64,1},aone::Int)

	@assert length(s0) == length(w)

	# v = max u(cash - s/(1+r)) + beta EV(s,h=1,j,t+1)

	first = 1

	if own==0
		first = aone
	end


	for i=first:p.namax
		if x-s0[i] > 0
			w[i] = ufun(x-s0[i],own,p) + p.beta * linearapprox(a,EV,s1[i])
		end
	end
	
	# println(w)

	r = findmax(w)
	return r
end



function ufun(x::Float64,own::Int,p::Param)
	r = p.imgamma * x^p.mgamma + own*p.xi1 
end


# housing payment function
function pifun(ih::Int,ihh::Int,price::Float64,p::Param)
	r = 0.0
	if ih==0
		# if you came into period as a renter:
		# choose whether to buy. if choose to move,
		# can only rent
		r = (1-ihh)*p.kappa[1]*price + ihh * price
	else 
		r = -( 1-ihh )*(1-p.phi-p.kappa[1])*price 
	end
end



# cashfunction
# computes cash on hand given a value of the
# state vector and a value of the discrete choices
function cashFunction(a::Float64, y::Float64, ih::Int, ihh::Int,price::Float64,p::Param)
	a + y - pifun(ih,ihh,price,p)
end



# TODO slow
# EV selector
# given current state and discrete choice, which portion of
# EV is relevant for current choice?
function EVfunChooser!(ev::Array{Float64,1},iz::Int,ihh::Int, age::Int,m::Model2,p::Param)
	if age==p.nt-1
		for ia in 1:p.na
			ev[ia] = m.EVfinal[ia,ihh]
		end
	else 
		for ia in 1:p.na
			ev[ia] = m.EV[iz,ia,ihh,age+1]
		end
		
	end

	return nothing
end




function linearapprox(x::Array{Float64,1},y::Array{Float64,1},xi::Float64)
	n = length(y)
	if xi < x[1]
		return y[1]
	elseif xi> x[n]
		return y[n]
	else
		jinf = searchsortedlast(x,xi)
	end
	return (y[jinf] * (x[jinf+1] - xi) + y[jinf+1] * (xi - x[jinf]) ) / (x[jinf+1] - x[jinf])
end


































