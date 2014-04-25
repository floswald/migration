

# functions to solve model 

function solve(m::Model, p::Param)

	# final period
	m.EVfinal = solveFinal!(m,p)

	# loop over time
	for age=p.nt-1:1

		# compute current period values

		# optimal savings conditional on 
		# each housing and loc choice:
		m.vstay = computeStay!(m,p,age)
		m.vsell = computeSell!(m,p,age)
		m.vbuy  = computeBuy!(m,p,age)
		m.vrent = computeRent!(m,p,age)

		# optimal housing choice in each loc
		m = computeTenureChoice!(m,p,age)	

		# compute vbar

		# compute CCPs

		# compute expectations	 
		m = computeExpectations!(m,p,age)

	end

	return m

end


for ij=1:p.nJ
for ipsi=1:p.npsi
	psi = p.psi[ipsi]
for iz=1:p.nz
	z = m.grids["z"][iz]
for iP=1:p.nP
	P = m.grids["P"][iP]
for iY=1:p.nY
for ip=1:p.np
for iy=1:p.ny
	# can define p and y here
for ih=1:p.nh
for ia=1:p.na

	if ih==0
		# enter period as renter
		a = m.grids["asset_rent"][ia]

	else
		# enter period as owner

		# housing choice
		for ihh=1:p.nh

			if ihh==0
				# sell the house:
				# you are allowed to move

				# location choice
				for ik=1:p.nJ

					# savings choice given you move to k
					# at the end of the period

					cash = a*p.R + y[iz] + p*(1-phi-kappa)

					# loop over savings
					for is=1:p.na
						cons[is] = cash - a[is]
						W[is] = u(cons[is]) + beta*R*m.EVrent[is,...,ik,age]
					end

					# find maximal level
					findmax(W)

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






# compute value of owner who 
# stays in the house
# notice that this value is only defined
# for each home location
# (not for all potential destinations!!)
function computeStay!(m::Model,p::Param)







function solveFinal!(m::Model,p::Param)

	# extract grids for faster lookup
	agrid = m.grids["asset_rent"]
	hgrid = m.grids["housing"]
	Pgrid = m.grids["P"]
	pgrid = m.grids2D["p"]

	# loop over all states
	for ia = 1:p.na
	for ih = 1:p.nh
	for iP = 1:p.nP
	for ij = 1:p.nJ
	for ip = 1:p.np 

		m.EVfinal[ia,ih,iP,ij,ip] = p.omega[1] + p.omega[2] * log(agrid[ia] + hgrid[ih] * (Pgrid[iP] + pgrid[ip,ij] ) )

	end
	end
	end
	end
	end

	# return the part we changed: final value
	m.EVfinal = integrateFinal(m)
	return m
end

function integrateFinal(m::Model)

	m.EVfinal = T_Final(m.EVfinal,m.grids3D)
end





