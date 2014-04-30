

module test_module

using FactCheck
using mig

facts("testing the model module") do

	p = mig.Param()
	m = mig.Model(p)

	context("Dictionaries have all required elements") do

		b = ["asset_own","asset_rent","housing","P","Y","W","z"]
		@fact length(setdiff(b, collect(keys(m.grids)))) => 0

		b = ["GY","GP","dist","ageprof"]
		@fact length(setdiff(b, collect(keys(m.grids2D)))) => 0

		b = ["Gy","Gp","Gz","p","y","movecost"]
		@fact length(setdiff(b, collect(keys(m.gridsXD)))) => 0

	end

	context("price grids have correct dimenions (p,P,J)") do

		@fact size(m.gridsXD["p"]) == (p.nP,p.np,p.nJ) => true
		@fact size(m.gridsXD["y"]) == (p.nY,p.ny,p.nJ) => true

	end



	context("Transition arrays sum to 1 over dimension 2") do

		@fact sum(m.grids2D["GP"],2)[:] .-1.0 => roughly(zeros(p.nP)[:],atol=0.00001)
		@fact sum(m.grids2D["GY"],2)[:] .-1.0 => roughly(zeros(p.nY)[:],atol=0.00001)

		@fact sum(m.gridsXD["Gp"],2)[:] .- 1.0 => roughly(zeros(p.np*p.nJ)[:],atol=0.00001)
		@fact sum(m.gridsXD["Gz"],2)[:] .- 1.0 => roughly(zeros(p.nz*p.nJ)[:],atol=0.00001)
		@fact sum(m.gridsXD["Gp"],2)[:] .- 1.0 => roughly(zeros(p.np*p.nJ)[:],atol=0.00001)
		# for p.np==2 that is actually symmetric!
		# @fact all(sum(m.gridsXD["Gp"],1)[:] .- 1.0 != zeros(p.np*p.nJ)) => true
		# @fact all(sum(m.gridsXD["Gz"],1)[:] .- 1.0 != zeros(p.nz*p.nJ)) => true
		# @fact all(sum(m.gridsXD["Gp"],1)[:] .- 1.0 != zeros(p.np*p.nJ)) => true
 	end


 	context("price grids have correct values for each region") do

 		for i in 1:p.nP
			for k in 1:p.nJ
				x =linspace(p.pbounds["p"][k][1], p.pbounds["p"][k][2], p.np) 
	 			for j in 1:p.np
 					@fact m.gridsXD["p"][i,j,k] - m.grids["P"][i] - x[j] => roughly(0.0,atol=0.000001)
 				end
 			end
 		end
 	end

 	context("moving cost is zero for j==k") do

		for it in 1:p.nt-1
			for ij in 1:p.nJ
				for ik in 1:p.nJ
					for ih in 0:1
						for itau in 1:p.ntau
							if ij==ik
								@fact m.gridsXD["movecost"][it,ij,ik,ih+1,itau] == 0.0 => true
							end
						end
					end
				end
			end
		end

	end



end



end