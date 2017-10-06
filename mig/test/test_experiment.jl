



@testset "experiment.jl tests" begin

	@testset "testing selectPolicy" begin

		p = Param(2)
		m = Model(p)
		
		j = 8
		yr = 2005 
		po = mig.selectPolicy("pshock",j,yr,0,p,m)
		@test po["policy"] == "pshock"
		@test po["shockRegion"] == j
		@test po["shockYear"] == yr
		@test po["shockAge"] == 1

		po = mig.selectPolicy("pshock",j,yr,3,p,m)
		@test po["policy"] == "pshock3"
		@test po["shockRegion"] == j
		@test po["shockYear"] == yr
		@test po["shockAge"] == 1
		@test po["shockVal_p"] == [collect(linspace(0.9,1,3))[1:3];repeat([1.0],inner=[1],outer=[p.nt-3])]

		po = mig.selectPolicy("yshock",j,yr,0,p,m)
		@test po["policy"] == "yshock"
		@test po["shockRegion"] == j
		@test po["shockYear"] == yr
		@test po["shockAge"] == 1

		po = mig.selectPolicy("yshock",j,yr,3,p,m)
		@test po["policy"] == "yshock3"
		@test po["shockRegion"] == j
		@test po["shockYear"] == yr
		@test po["shockAge"] == 1
		@test po["shockVal_y"] == [collect(linspace(0.9,1,3))[1:3];repeat([1.0],inner=[1],outer=[p.nt-3])]


		po = mig.selectPolicy("ypshock",j,yr,0,p,m)
		@test po["policy"] == "ypshock"
		@test po["shockRegion"] == j
		@test po["shockYear"] == yr
		@test po["shockAge"] == 1
		@test po["shockVal_y"] != ([1.0 for i=1:p.nt-1])
		@test po["shockVal_p"] == (1- (1- po["shockVal_y"])*m.sigma_reg["WNC"][1,2])

		po = mig.selectPolicy("ypshock",j,yr,5,p,m)
		@test po["policy"] == "ypshock5"
		@test po["shockRegion"] == j
		@test po["shockYear"] == yr
		@test po["shockAge"] == 1
		@test po["shockVal_p"] == (1- (1- po["shockVal_y"])*m.sigma_reg["WNC"][1,2])

	end


	@testset "test param with policy" begin

		p = Param(2)
		m = Model(p)
		@test p.policy == "NULL"
		@test p.redistribute == [0.0]
		@test p.verbose == 0
		@test p.shockReg == 0
		@test p.shockAge == 100
		@test p.shockVal == ones(p.nt-1)

		lumpSum1 = [rand()]
		opts = Dict("policy" => "mortgageSubsidy","redistribute" => lumpSum1)

		p = mig.Param(2,opts);

		@test p.policy == "mortgageSubsidy"
		@test p.redistribute == lumpSum1
		@test p.ctax == 1.0
		@test p.shockReg == 0
		@test p.shockAge == 100
		@test p.shockVal == ones(p.nt-1)


		p0 = mig.Param(1,opts);
		lumpSum2 = collect(rand(p0.nt-1))
		opts = Dict("policy" => "mortgageSubsidy","redistribute" => lumpSum2)

		p = mig.Param(1,opts);

		@test p.policy == "mortgageSubsidy"
		@test p.redistribute == lumpSum2
		@test p.ctax == 1.0
		@test p.shockReg == 0
		@test p.shockAge == 100
		@test p.shockVal == ones(p.nt-1)


		p0 = mig.Param(2,opts);
		opts = mig.selectPolicy("yshock",5,1997,3,p0,m)
		p = mig.Param(2,opts);

		@test p.policy == "yshock3"
		@test p.ctax == 1.0
		@test p.redistribute == [0.0]
		@test p.shockAge == 1
		@test p.shockVal_y == [collect(linspace(0.9,1,3))[1:3];repeat([1.0],inner=[1],outer=[p.nt-3])]
		@test p.shockReg == 5


		p0 = mig.Param(2);
		opts = mig.selectPolicy("yshock",5,1997,0,p0,m)
		p = mig.Param(2,opts);

		@test p.policy == "yshock"
		@test p.ctax == 1.0
		@test p.redistribute == [0.0]
		@test p.shockAge == 1
		@test p.shockVal_y == ones(p.nt-1) * opts["shockVal_y"][1]
		@test p.shockVal == ones(p.nt-1) 
		@test p.shockReg == 5

		m0 = Model(p0)
		m = Model(p)

		@test all(m.gridsXD["z"][:,1,1,2,5][:] .- m0.gridsXD["z"][:,1,1,2,5][:] .< 0.0) == true



		opts = Dict("policy" => "pshock","shockYear"=>1997, "shockAge" => 10, "shockRegion" => 6, "shockVal" => [3.3 for i=1:p.nt-1])

		p = mig.Param(2,opts);

		@test p.policy == "pshock"
		@test p.ctax == 1.0
		@test p.redistribute == [0.0]
		@test p.shockAge == 10
		@test p.shockVal == ones(p.nt-1) * 3.3
		@test p.shockReg == 6
		@test all(m.gridsXD["z"][:,1,1,21,6][:] .- m0.gridsXD["z"][:,1,1,21,6][:] .== 0.0) == true
	end


	@testset "checking solution under price shock" begin

		p = Param(2)
		m = Model(p)
		solve!(m,p)

		shockAge = 10

		opts = Dict("policy" => "pshock","shockYear"=>1997, "shockAge" => shockAge, "shockRegion" => 6, "shockVal_p" => [0.7 for i=1:p.nt-1])

		p2 = Param(2,opts)
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

	@testset "checking solution under income shock" begin

		p = Param(2)
		m = Model(p)
		solve!(m,p)

		shockAge = 10

		opts = Dict("policy" => "yshock","shockYear"=>1997, "shockAge" => shockAge, "shockRegion" => 6, "shockVal_y" => [0.7 for i=1:p.nt-1])

		p2 = Param(2,opts)
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


	@testset "z shock sequences are identical in baseline and shocked versions" begin
		
		p = Param(2)
		m = Model(p)
		solve!(m,p)
		sim0 = simulate(m,p)
		sim0 = sim0[.!mig.isna.(sim0[:cohort]),:]

		opts = mig.selectPolicy("pshock",5,2007,0,p,m)

		s10 = mig.computeShockAge(m,opts,10);
		coh = unique(s10[:cohort])

		@test coh[1] == p.nt - 10 + 2007 - 1997

		@test all(sim0[sim0[:cohort].==coh,:z] .== s10[:z]) == true

	end

end


# @testset"welfare function behaves correctly at identical policy" begin

# 	opts = ["verbose" => 0]
# 	p0 = Param(2,opts)
# 	m0 = Model(p0)
# 	solve!(p0,m0)
# 	v0 = m0.vh[1,1,1,3,2,2,2,m0.aone,1,1,1]

# 	@test mig.welfare(1.1,v0,opts) > 0.0 == true
# 	@test mig.welfare(0.9,v0,opts) < 0.0 == true

# end