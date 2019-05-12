



@testset "experiment.jl tests" begin

	@testset "test param with policy" begin

		p = Param(2)
		m = Model(p)
		@test p.policy == "NULL"
		@test p.redistribute == [0.0]
		@test p.verbose == 0
		@test p.shockReg == 0
		@test p.shockAge == 100
		@test p.shockVal == zeros(p.nt-1)

		lumpSum1 = [rand()]
		opts = Dict("policy" => "mortgageSubsidy","redistribute" => lumpSum1)

		p = mig.Param(2,opts=opts);

		@test p.policy == "mortgageSubsidy"
		@test p.redistribute == lumpSum1
		@test p.ctax == 1.0
		@test p.shockReg == 0
		@test p.shockAge == 100
		@test p.shockVal == zeros(p.nt-1)


		p0 = mig.Param(1,opts=opts);
		lumpSum2 = collect(rand(p0.nt-1))
		opts = Dict("policy" => "mortgageSubsidy","redistribute" => lumpSum2)

		p = mig.Param(1,opts=opts);

		@test p.policy == "mortgageSubsidy"
		@test p.redistribute == lumpSum2
		@test p.ctax == 1.0
		@test p.shockReg == 0
		@test p.shockAge == 100
		@test p.shockVal == zeros(p.nt-1)

	end


	@testset "checking solution under price shock" begin

		p = Param(2)
		m = Model(p)
		solve!(m,p)

		shockAge = 10

		opts = Dict("policy" => "pshock","shockYear"=>1997, "shockAge" => shockAge, "shockReg" => 6, "shockVal_p" => [0.7 for i=1:p.nt-1])

		p2 = Param(2,opts=opts)
		m2 = Model(p2)
		solve!(m2,p2)

		mig.adjustVShocks!(m2,m,p2)

		# renter's cash after shock must increase with lower price
		@test all(m2.cash[1,6,:,:,:,:,:,:,1,6,shockAge+1][:] .- m.cash[1,6,:,:,:,:,:,:,1,6,shockAge+1][:] .>= 0.0) == true

		# same for owners who sell in different region and move to 6 to rent: they're cash on hand must be higher.
		@test all(m2.cash[1,6,:,:,:,:,:,:,2,5,shockAge+1][:] .- m.cash[1,6,:,:,:,:,:,:,2,5,shockAge+1][:] .>= 0.0) == true

		# before shock or in another region, cash must be the same
		@test all(m2.cash[1,6,:,:,:,:,:,:,1,6,1][:] .== m.cash[1,6,:,:,:,:,:,:,1,6,1][:]) == true
		@test all(m2.cash[1,5,:,:,:,:,:,:,1,6,:][:] .== m.cash[1,5,:,:,:,:,:,:,1,6,:][:]) == true
		@test all(m2.cash[1,5,:,:,:,:,:,:,1,5,:][:] .== m.cash[1,5,:,:,:,:,:,:,1,5,:][:]) == true

		# values must be different in shocked region after shock hits
		@test !all(m2.vh[1,6,:,:,:,:,:,m.aone+1,1,6,shockAge+1][:] .== m.vh[1,6,:,:,:,:,:,m.aone+1,1,6,shockAge+1][:]) == true
		t1 = m2.vh[1,5,:,:,:,:,:,m.aone+1,1,6,shockAge+1][:]
		t2 = m.vh[1,5,:,:,:,:,:,m.aone+1,1,6,shockAge+1][:]
		@test !all(t1[t1 .!= p.myNA] .== t2[t2 .!= p.myNA] ) == true

		@test all(m2.vh[1,6,:,:,:,:,:,m.aone+1,1,6,shockAge-1][:] .== m.vh[1,6,:,:,:,:,:,m.aone+1,1,6,shockAge-1][:]) == true
	end
	gc()

	@testset "checking solution under income shock" begin

		p = Param(2)
		m = Model(p)
		solve!(m,p)

		shockAge = 10

		opts = Dict("policy" => "yshock","shockYear"=>1997, "shockAge" => shockAge, "shockReg" => 6, "shockVal_y" => [0.7 for i=1:p.nt-1])

		p2 = Param(2,opts=opts)
		m2 = Model(p2)
		solve!(m2,p2)

		mig.adjustVShocks!(m2,m,p2)

		# everybody's cash must decrease with lower income
		@test all(m2.cash[1,6,:,:,:,:,:,:,1,6,shockAge+1][:] .<= m.cash[1,6,:,:,:,:,:,:,1,6,shockAge+1][:]) == true
		@test all(m2.cash[1,6,:,:,:,:,:,:,2,6,shockAge+1][:] .<= m.cash[1,6,:,:,:,:,:,:,2,6,shockAge+1][:] ) == true
		@test all(m2.cash[2,6,:,:,:,:,:,:,2,6,shockAge+1][:] .<= m.cash[2,6,:,:,:,:,:,:,2,6,shockAge+1][:] ) == true
		# if you move, you earn income in origin location,i.e. 6, which is subject to shock: less cash!
		@test all(m2.cash[1,5,:,:,:,:,:,:,1,6,shockAge+1][:] .<= m.cash[1,5,:,:,:,:,:,:,1,6,shockAge+1][:]) == true

		@test all(m2.cash[2,6,:,:,:,:,:,:,1,6,shockAge-1][:] .== m.cash[2,6,:,:,:,:,:,:,1,6,shockAge-1][:]) == true
		@test all(m2.cash[1,6,:,:,:,:,:,:,2,6,shockAge-1][:] .== m.cash[1,6,:,:,:,:,:,:,2,6,shockAge-1][:] ) == true
		@test all(m2.cash[2,6,:,:,:,:,:,:,2,6,shockAge-1][:] .== m.cash[2,6,:,:,:,:,:,:,2,6,shockAge-1][:] ) == true
		@test all(m2.cash[1,5,:,:,:,:,:,:,1,5,shockAge-1][:] .== m.cash[1,5,:,:,:,:,:,:,1,5,shockAge-1][:]) == true
		@test all(m2.cash[1,5,:,:,:,:,:,:,1,6,shockAge-1][:] .== m.cash[1,5,:,:,:,:,:,:,1,6,shockAge-1][:]) == true

		# values must be different
		@test all(m2.vh[1,6,:,:,:,:,:,m.aone+1,1,6,shockAge+1][:] .!= m.vh[1,6,:,:,:,:,:,m.aone+1,1,6,shockAge+1][:]) == true
		@test all(m2.vh[1,6,:,:,:,:,:,m.aone+1,2,6,shockAge+1][:] .!= m.vh[1,6,:,:,:,:,:,m.aone+1,2,6,shockAge+1][:] ) == true
		@test all(m2.vh[2,6,:,:,:,:,:,m.aone+1,2,6,shockAge+1][:] .!= m.vh[2,6,:,:,:,:,:,m.aone+1,2,6,shockAge+1][:] ) == true
		# if you move, you earn income in origin location,i.e. 6, which is subject to shock: less cash!
		t1 = m2.vh[1,5,:,:,:,:,:,m.aone+1,1,6,shockAge+1][:]
		t2 = m.vh[1,5,:,:,:,:,:,m.aone+1,1,6,shockAge+1][:]
		@test all((t1 .!= t2)[(t1 .!= p.myNA) .& (t2 .!= p.myNA)])

		@test all(m2.vh[2,6,:,:,:,:,:,m.aone+1,1,6,shockAge-1][:] .== m.vh[2,6,:,:,:,:,:,m.aone+1,1,6,shockAge-1][:]) == true
		@test all(m2.vh[1,6,:,:,:,:,:,m.aone+1,2,6,shockAge-1][:] .== m.vh[1,6,:,:,:,:,:,m.aone+1,2,6,shockAge-1][:] ) == true
		@test all(m2.vh[2,6,:,:,:,:,:,m.aone+1,2,6,shockAge-1][:] .== m.vh[2,6,:,:,:,:,:,m.aone+1,2,6,shockAge-1][:] ) == true
		@test all(m2.vh[1,5,:,:,:,:,:,m.aone+1,1,5,shockAge-1][:] .== m.vh[1,5,:,:,:,:,:,m.aone+1,1,5,shockAge-1][:]) == true
		@test all(m2.vh[1,5,:,:,:,:,:,m.aone+1,1,6,shockAge-1][:] .== m.vh[1,5,:,:,:,:,:,m.aone+1,1,6,shockAge-1][:]) == true
	end
	gc()


	@testset "z shock sequences are identical in baseline and shocked versions" begin
		
		if !(get(ENV,"TRAVIS",false))
			p = Param(2)
			m = Model(p)
			solve!(m,p)
			sim0 = simulate(m,p)
			sim0 = sim0[.!mig.ismissing.(sim0[:cohort]),:]


			s10 = mig.computeShockAge(m,Dict("policy"=>"pshock","shockReg"=>5,"shockYear"=>2007),10);
			coh = unique(s10[:cohort])

			@test coh[1] == p.nt - 10 + 2007 - 1997

			@test all(sim0[sim0[:cohort].==coh,:z] .== s10[:z]) == true

			s10 = 0
			sim0 = 0
			gc()
		else
			@test_skip "z shock sequences"
		end

	end

end

