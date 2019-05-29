


@testset "testing the model module" begin

	p = mig.Param(2)
	m = mig.Model(p)

	@testset "Transition arrays sum to 1 over dimension 2" begin

		# @test sum(m.grids2D["GP"],2)[:] .-1.0 => roughly(zeros(p.nP)[:],atol=0.00001)
		# @test sum(m.grids2D["GY"],2)[:] .-1.0 => roughly(zeros(p.nY)[:],atol=0.00001)
		mz = size(m.gridsXD["Gz"],2)
		ms = size(m.gridsXD["Gs"],2)
		my = size(m.gridsXD["Gyp"],2)

		println(size(sum(m.gridsXD["Gs"],2)[:]))
		println(size(ones(ms)))

		@test isapprox(sum(m.gridsXD["Gz"],2)[:] ,ones(mz)  ,atol=0.00001)
		@test all(abs.(sum(m.gridsXD["Gs"],2)[:] - 1.0) .< 0.00001)
		@test isapprox(sum(m.gridsXD["Gyp"],2)[:], ones(my) ,atol=0.00001)
 	end


 	@testset "moving cost is zero for j==k" begin

		for it in 1:p.nt-1
			for ij in 1:p.nJ
				for ik in 1:p.nJ
					for ih in 0:1
						for is in 1:p.ns
							if ij==ik
								@test m.gridsXD["movecost"][it,ij,ik,ih+1,is] == 0.0
							end
						end
					end
				end
			end
		end

	end

	@testset "covariance matrices of regional VARS" begin
		@test isa(m.sigma_reg["WNC"],Matrix) 
		@test length(m.sigma_reg) == 9 
		for (k,v) in m.sigma_reg
			@test issymmetric(v)
			e = extrema(v)
			@test e[1] >= -1.0
			@test e[2] <=  1.0
		end

	end



	@testset "Transition arrays sum to 1 over dimension 2 with changed rho/sigma" begin
		p = mig.Param(2,opts=Dict(:rho=>0.5,:sigma => 0.1))
		m = mig.Model(p)

		# @test sum(m.grids2D["GP"],2)[:] .-1.0 => roughly(zeros(p.nP)[:],atol=0.00001)
		# @test sum(m.grids2D["GY"],2)[:] .-1.0 => roughly(zeros(p.nY)[:],atol=0.00001)
		mz = size(m.gridsXD["Gz"],2)
		ms = size(m.gridsXD["Gs"],2)
		my = size(m.gridsXD["Gyp"],2)

		println(size(sum(m.gridsXD["Gs"],2)[:]))
		println(size(ones(ms)))

		@test isapprox(sum(m.gridsXD["Gz"],2)[:] ,ones(mz)  ,atol=0.00001)
		@test all(abs.(sum(m.gridsXD["Gs"],2)[:] - 1.0) .< 0.00001)
		@test isapprox(sum(m.gridsXD["Gyp"],2)[:], ones(my) ,atol=0.00001)
 	end
end


	



