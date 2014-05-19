

module test_Tensors

using FactCheck
using mig

facts("testing the final period tensors") do

	p = mig.Param(1)
	m = mig.Model(p)

	context("testing final period Tensor") do


		agrid = m.grids["asset_rent"]
		hgrid = m.grids["housing"]

		# compute tensor
		tens = p.omega[1] .+ p.omega[2] .* mig.E_tensors.T_FinalV(agrid,hgrid,m.gridsXD["p"])

		# loop over all states
		for ia = 1:p.na
		for ih = 1:p.nh
		for iP = 1:p.nP
		for ij = 1:p.nJ
		for ip = 1:p.np 

			tval = p.omega[1] + p.omega[2] * log(agrid[ia] + hgrid[ih] * (m.gridsXD["p"][iP,ip,ij] ) )

			@fact tens[ia,ih,iP,ij,ip] == tval => true

		end
		end
		end
		end
		end
	end

end




end	# module