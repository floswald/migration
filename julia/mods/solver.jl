

# functions to solve model 

function solve(m::Model, p::Param)

	# final period
	solveFinal!(m,p)

	# loop over time
	# for age=p.nt-1:1

	# 	# compute current period values

	# 	# optimal savings conditional on 
	# 	# each housing and loc choice:
	# 	m.vstay = computeStay!(m,p,age)
	# 	m.vsell = computeSell!(m,p,age)
	# 	m.vbuy  = computeBuy!(m,p,age)
	# 	m.vrent = computeRent!(m,p,age)

	# 	# optimal housing choice in each loc
	# 	m = computeTenureChoice!(m,p,age)	

	# 	# compute vbar

	# 	# compute CCPs

	# 	# compute expectations	 
	# 	m = computeExpectations!(m,p,age)

	# end

	return m

end



function maxvalue(x::Float64,p::Param,s::Array{Float64,1},w::Array{Float64,1},own::Float64,mc::Float64,def::Float64,EV::Array{Float64,1})

	@assert length(s) == length(w)

	# v = max u(cash - s) + beta EV(s,h=1,j,t+1)

	for i=1:na
		if x-s[i] > 0
			w[i] = ufun(x-s[i],own,mc,def,p) + p.beta * EV[i]
		end
	end

	r = findmax(w)
	return r
end



function ufun(x::Float64,own::Float64,mc::Float64,def::Float64,p::Param)
	r = p.imgamma * x^p.mgamma + own*p.xi - def*p.lambda - mc
end


# housing payment function
function pifun(ih::Int,ihh::Int,price::Float64,ij::Int,ik::Int,p::Param)
	r = 0.0

	if ih==0
		r = (1-ihh)*p.kappa[ij]*price + ihh * price
	else 
		r = -( (ij==ik)*(1-ihh) + (ij!=ik) )*(1-p.phi-p.kappa[ij])*price 
	end
	return r
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

# scaffold for main loop
# ======================	
		dimvec  = ((nt-1), na, nz, nh, npsi, nP, nY, nJ ,np ,ny, (nJ-1))

function solvePeriod(age::Int,m::Model,p::Param)

	w = zeros(p.na)
	r = (0.0,0)

	for ij=1:p.nJ				# current location
		ageeffect = m.grids["ageprof"][age,ij]
	for ipsi=1:p.npsi			# type
		psi = p.psi[ipsi]
	for iz=1:p.nz				# individual income shock
		z = m.grids["z"][iz]
	for iP=1:p.nP 				# national price index
		P = m.grids["P"][iP]	
	for iY=1:p.nY 				# national income lebel
	for ip=1:p.np 				# regional price deviation
	for iy=1:p.ny 				# regional income deviation
		price = m.grids3D["p"][iP,ip,ij]
		y = m.grids3D["y"][iY,iy,ij]
	for ih=1:p.nh
	for ia=1:p.na

		if ih==0
			# enter period as renter
			a = m.grids["asset_rent"][ia]

		else
			# enter period as owner

			a = m.grids["asset_own"][ia]

			# choices
			# =======

			# housing choice
			for ihh in m.grids["housing"]

				if ihh==0.0
					# sell the house:
					# you are allowed to move

					# next period you are a renter:
					# that's your savings grid
					s = m.grids["asset_rent"]


					# location choice
					for ik=1:p.nJ

						# reset w vector
						fill!(w,p.myNA)

						# cashfunction(a,y,ageeffect,z,ih,ihh)
						cash = grossSaving(a) + income(y,ageeffect,z) - pifun*(ih,ihh,price,ij,ik,p)


						# find moving cost
						mc = movecost[age,ij,ik,ih,ipsi]

						# relevant future value:
						# EV[:,iz,ik,...,age+1]

						# savings choice given you move to k
						# at the end of the period

						r = maxvalue(cash,p,s,w,ihh,mc,def,EV)

						m.v[age,ia,iz,ih,ipsi,iP,iY,ij,ip,iy,ik] = r[1]
						m.s[age,ia,iz,ih,ipsi,iP,iY,ij,ip,iy,ik] = r[2]
						m.c[age,ia,iz,ih,ipsi,iP,iY,ij,ip,iy,ik] = cash - s[ r[2] ]

					end

				else
					# keep the house:
					# no move

				end

		end

	end
	end
	end
	end
	end
	end
	end
	end
	end


end






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

		m.EVfinal[ia,ih,iP,ij,ip] = p.omega[1] + p.omega[2] * log(agrid[ia] + hgrid[ih] * (m.grids3D["p"][iP,ip,ij] ) )

	end
	end
	end
	end
	end

	# return the part we changed: final value
	# m.EVfinal = integrateFinal(m)
	return m
end

function integrateFinal(m::Model)

	m.EVfinal = T_Final(m.EVfinal,m.grids3D)
end





