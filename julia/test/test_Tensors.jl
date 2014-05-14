

module test_Tensors

using FactCheck
using mig

facts("testing the tensors") do

	p = mig.Param()
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

	context("testing EVbar integration tensor: integrate up to one?") do

		# function T_Evbar(GP,GY,Gp,Gy,Gz,V)
		GP = zeros(p.nP,p.nP)
		GY = zeros(p.nY,p.nY)
		Gy = zeros(p.ny,p.ny,p.nJ)
		Gp = zeros(p.np,p.np,p.nJ)
		Gz = zeros(p.nz,p.nz,p.nJ)
		fill!(GP,1/p.nP )
		fill!(GY,1/ p.nY)
		fill!(Gy,1/ (p.ny*p.nJ))
		fill!(Gp,1/ (p.np*p.nJ))
		fill!(Gz,1/ (p.nz*p.nJ))

		V = ones(p.dimvec3[1:9])
		EV = mig.E_tensors.T_Evbar(GP,GY,Gp,Gy,Gz,V)

		@fact V[:] == EV[:] => true

	end



	

end







end	# module