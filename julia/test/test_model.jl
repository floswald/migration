

module test_module

using FactCheck
using mig

facts("testing the model module") do

	p = mig.Param()
	m = mig.Model(p)

	context("Dictionaries have all required elements") do

		b = ["asset_own","asset_rent","housing","P","Y","W"]
		@fact length(collect(keys(m.grids))) == 6 => true

		for i in b
			@fact haskey(m.grids,i) => true
		end

		b = ["GY","GP","dist"]
		@fact length(collect(keys(m.grids2D))) == 3 => true
		for i in b
			@fact haskey(m.grids2D,i) => true
		end

		b = ["Gy","Gp","Gz","p","y"]
		@fact length(collect(keys(m.grids3D))) == 5 => true
		for i in b
			@fact haskey(m.grids3D,i) => true
		end


	end

	context("price grids have correct dimenions (p,P,J)") do

		@fact size(m.grids3D["p"]) == (p.nP,p.np,p.nJ) => true
		@fact size(m.grids3D["y"]) == (p.nY,p.ny,p.nJ) => true

	end



	context("Transition arrays sum to 1 over dimension 2") do

		@fact sum(m.grids2D["GP"],2)[:] .-1.0 => roughly(zeros(p.nP)[:],atol=0.00001)
		@fact sum(m.grids2D["GY"],2)[:] .-1.0 => roughly(zeros(p.nY)[:],atol=0.00001)

		@fact sum(m.grids3D["Gp"],2)[:] .- 1.0 => roughly(zeros(p.np*p.nJ)[:],atol=0.00001)
		@fact sum(m.grids3D["Gz"],2)[:] .- 1.0 => roughly(zeros(p.nz*p.nJ)[:],atol=0.00001)
		@fact sum(m.grids3D["Gp"],2)[:] .- 1.0 => roughly(zeros(p.np*p.nJ)[:],atol=0.00001)
		# for p.np==2 that is actually symmetric!
		# @fact all(sum(m.grids3D["Gp"],1)[:] .- 1.0 != zeros(p.np*p.nJ)) => true
		# @fact all(sum(m.grids3D["Gz"],1)[:] .- 1.0 != zeros(p.nz*p.nJ)) => true
		# @fact all(sum(m.grids3D["Gp"],1)[:] .- 1.0 != zeros(p.np*p.nJ)) => true
 	end

end



end