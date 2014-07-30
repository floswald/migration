

module test_module


using FactCheck

include("../src/mig.jl")

facts("testing the model module") do

	p = mig.Param(2)
	m = mig.Model(p)

	context("Transition arrays sum to 1 over dimension 2") do

		# @fact sum(m.grids2D["GP"],2)[:] .-1.0 => roughly(zeros(p.nP)[:],atol=0.00001)
		# @fact sum(m.grids2D["GY"],2)[:] .-1.0 => roughly(zeros(p.nY)[:],atol=0.00001)

		@fact sum(m.gridsXD["Gz"],2)[:] .- 1.0 => roughly(zeros(p.nz*p.nJ)[:],atol=0.00001)
		@fact sum(m.gridsXD["Gs"],2)[:] .- 1.0 => roughly(zeros(p.ns*p.nt)[:],atol=0.00001)
		@fact sum(m.gridsXD["Gyp"],2)[:] .- 1.0 => roughly(zeros(p.np*p.ny*p.nJ)[:],atol=0.00001)
	 	end


 	context("moving cost is zero for j==k") do

		for it in 1:p.nt-1
			for ij in 1:p.nJ
				for ik in 1:p.nJ
					for ih in 0:1
						for is in 1:p.ns
							if ij==ik
								@fact m.gridsXD["movecost"][it,ij,ik,ih+1,is] == 0.0 => true
							end
						end
					end
				end
			end
		end

	end



end


	



end