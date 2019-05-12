



@testset "checking some properties of the solution." begin

	p = Param(2)
	m = Model(p)
	println("solving model")
	mig.solve!(m,p)
	println("done solving model. now testing.")

	@testset "test the range of outputs from final value" begin

		mval = p.omega1 * p.imgamma * exp( p.mgamma * log(maximum(m.grids["assets"]) + maximum(m.gridsXD["p"]) ) ) + p.omega2
		
		@test minimum(m.EVfinal) == p.myNA
		@test isapprox(maximum(m.EVfinal), mval)

	end

	@testset "value function of high cost type movers" begin

		for itest in 1:50

			is   = rand(1:p.ns)
			ia   = rand(1:p.na)
			ih   = rand(1:p.nh)
			iy   = rand(1:p.ny)
			ip   = rand(1:p.np)
			iz   = rand(1:p.nz)
			itau = rand(1:p.ntau)
			ij   = rand(1:p.nJ)
			ik   = rand(1:p.nJ)
			age   = rand(1:(p.nt-1))

			tau1 = m.vh[1,ik,is,iz,iy,ip,1,m.aone,ih,ij,age]
			tau2 = m.vh[1,ik,is,iz,iy,ip,2,m.aone,ih,ij,age]

			if ij != ik
				@test tau1 > tau2
			end

		end

	end

	@testset "check conditional v is never decreasing in a" begin

		tt = mapslices(x -> diff(x),m.vh,8)
		# println(median(tt))
		# println(minimum(tt))
		# println(sum(tt.<0)/length(tt))
		@test_broken all(tt .>= 0.0)

	end

	# @testset "check conditional v is never decreasing in z" begin

	# 	tt = mapslices(x -> diff(x),m.vh,4)
	# 	@test all(tt .>= 0.0) => true

	# end

	@testset "check prob of moving" begin

		for itest in 1:10

			is   = rand(1:p.ns)
			ia   = rand(1:p.na)
			ih   = rand(1:p.nh)
			iy   = rand(1:p.ny)
			ip   = rand(1:p.np)
			iz   = rand(1:p.nz)
			itau = rand(1:p.ntau)
			ij   = rand(1:p.nJ)
			age   = rand(1:(p.nt-1))

			if any(m.v[:,is,iz,iy,ip,itau,ia,ih,ij,age][:] .> p.myNA)
				tt = m.rho[:,is,iz,iy,ip,itau,ia,ih,ij,age][:]
				
				println(sum(tt))				
				@test_broken isapprox(sum(tt) ,1.0,atol=1e-4)
				# @test minimum(tt) >= 0.0 
				# @test maximum(tt) <= 1.0 
			end


		end


	end

	@testset "check whether cons positive at feasible states" begin

		feas = m.vh .> p.myNA

		@test all(m.ch[feas] .> 0.0) 

	end

end
gc()

