

# functions to solve model 

function solve(m::Model, p::Param)



	# final period


	m.EVfinal = solveFinal(m,p)


	# loop over time

	# compute.period
	# a function that computes current period value,
	# as a function of m, p, period, and the future value
	# nested loop over all states except period.

	# compute.expectations
	# function that computes EVown and EVrent
	# tensor


	# end loop over time

	# return model
	return m

end






function solveFinal(m::Model,p::Param)

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
	return m.EVfinal
end

function integrateFinal(m::Model)

	m.EVfinal = T_Final(m.EVfinal,m.grids3D)
end





