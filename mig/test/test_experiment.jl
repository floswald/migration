


module test_experiment



using mig, FactCheck


facts("experiment.jl tests") do

	context("testing selectPolicy") do

		p = Param(2)
		m = Model(p)
		
		j = 8
		yr = 2005 
		po = mig.selectPolicy("pshock",j,yr,p)
		@fact po["policy"] --> "pshock"
		@fact po["shockRegion"] --> j
		@fact po["shockYear"] --> yr
		@fact po["shockAge"] --> 1
		@fact po["shockVal"] --> [po["shockVal"] for i=1:p.nt-1]

		po = mig.selectPolicy("pshock3",j,yr,p)
		@fact po["policy"] --> "pshock"
		@fact po["shockRegion"] --> j
		@fact po["shockYear"] --> yr
		@fact po["shockAge"] --> 1
		@fact po["shockVal"] --> [0.7; 0.8; 0.9; [1.0 for i=1:p.nt-3]]

		po = mig.selectPolicy("yshock",j,yr,p)
		@fact po["policy"] --> "yshock"
		@fact po["shockRegion"] --> j
		@fact po["shockYear"] --> yr
		@fact po["shockAge"] --> 1
		@fact po["shockVal"] --> [po["shockVal"] for i=1:p.nt-1]

		po = mig.selectPolicy("yshock3",j,yr,p)
		@fact po["policy"] --> "yshock"
		@fact po["shockRegion"] --> j
		@fact po["shockYear"] --> yr
		@fact po["shockAge"] --> 1
		@fact po["shockVal"] --> [0.7; 0.8; 0.9; [1.0 for i=1:p.nt-3]]

		po = mig.selectPolicy("ypshock",j,yr,p)
		@fact po["policy"] --> "ypshock"
		@fact po["shockRegion"] --> j
		@fact po["shockYear"] --> yr
		@fact po["shockAge"] --> 1
		@fact po["shockVal_y"] --> [po["shockVal"] for i=1:p.nt-1]
		@fact po["shockVal_p"] --> po["shockVal_y"]*m.sigma_reg[j][1,2]]

	end


	context("test param with policy") do

		p = Param(2)
		@fact p.policy --> "NULL"
		@fact p.redistribute --> [0.0]
		@fact p.verbose --> 0
		@fact p.shockReg --> 0
		@fact p.shockAge --> 100
		@fact p.shockVal --> ones(p.nt-1)

		lumpSum1 = [rand()]
		opts = Dict("policy" => "mortgageSubsidy","redistribute" => lumpSum1)

		p = mig.Param(2,opts);

		@fact p.policy --> "mortgageSubsidy"
		@fact p.redistribute --> lumpSum1
		@fact p.ctax --> 1.0
		@fact p.shockReg --> 0
		@fact p.shockAge --> 100
		@fact p.shockVal --> ones(p.nt-1)


		p0 = mig.Param(1,opts);
		lumpSum2 = collect(rand(p0.nt-1))
		opts = Dict("policy" => "mortgageSubsidy","redistribute" => lumpSum2)

		p = mig.Param(1,opts);

		@fact p.policy --> "mortgageSubsidy"
		@fact p.redistribute --> lumpSum2
		@fact p.ctax --> 1.0
		@fact p.shockReg --> 0
		@fact p.shockAge --> 100
		@fact p.shockVal --> ones(p.nt-1)


		p0 = mig.Param(2,opts);
		opts = mig.selectPolicy("yshock3",5,1997,p0)
		p = mig.Param(2,opts);

		@fact p.policy --> "yshock"
		@fact p.ctax --> 1.0
		@fact p.redistribute --> [0.0]
		@fact p.shockAge --> 1
		@fact p.shockVal --> [0.7; 0.8; 0.9; ones(p.nt-3)]
		@fact p.shockReg --> 5


		p0 = mig.Param(2);
		opts = mig.selectPolicy("yshock",5,1997,p0)
		p = mig.Param(2,opts);

		@fact p.policy --> "yshock"
		@fact p.ctax --> 1.0
		@fact p.redistribute --> [0.0]
		@fact p.shockAge --> 1
		@fact p.shockVal --> ones(p.nt-1) * opts["shockVal"][1]
		@fact p.shockReg --> 5

		m0 = Model(p0)
		m = Model(p)

		@fact all(m.gridsXD["z"][:,1,1,2,5][:] .- m0.gridsXD["z"][:,1,1,2,5][:] .< 0.0) --> true



		opts = Dict("policy" => "pshock","shockYear"=>1997, "shockAge" => 10, "shockRegion" => 6, "shockVal" => [3.3 for i=1:p.nt-1])

		p = mig.Param(2,opts);

		@fact p.policy --> "pshock"
		@fact p.ctax --> 1.0
		@fact p.redistribute --> [0.0]
		@fact p.shockAge --> 10
		@fact p.shockVal --> ones(p.nt-1) * 3.3
		@fact p.shockReg --> 6
		@fact all(m.gridsXD["z"][:,1,1,21,6][:] .- m0.gridsXD["z"][:,1,1,21,6][:] .== 0.0) --> true
	end


	context("checking solution under price shock") do

		p = Param(2)
		m = Model(p)
		solve!(m,p)

		shockAge = 10

		opts = Dict("policy" => "pshock","shockYear"=>1997, "shockAge" => shockAge, "shockRegion" => 6, "shockVal" => [0.7 for i=1:p.nt-1])

		p2 = Param(2,opts)
		m2 = Model(p2)
		solve!(m2,p2)

		mig.adjustVShocks!(m2,m,p2)

		# renter's cash after shock must increase with lower price
		@fact all(m2.cash[1,6,:,:,:,:,:,:,1,6,shockAge+1][:] .- m.cash[1,6,:,:,:,:,:,:,1,6,shockAge+1][:] .>= 0.0) --> true

		# same for owners who sell in different region and move to 6 to rent: they're cash on hand must be higher.
		@fact all(m2.cash[1,6,:,:,:,:,:,:,2,5,shockAge+1][:] .- m.cash[1,6,:,:,:,:,:,:,2,5,shockAge+1][:] .>= 0.0) --> true

		# before shock or in another region, cash must be the same
		@fact all(m2.cash[1,6,:,:,:,:,:,:,1,6,1][:] .== m.cash[1,6,:,:,:,:,:,:,1,6,1][:]) --> true
		@fact all(m2.cash[1,5,:,:,:,:,:,:,1,6,:][:] .== m.cash[1,5,:,:,:,:,:,:,1,6,:][:]) --> true
		@fact all(m2.cash[1,5,:,:,:,:,:,:,1,5,:][:] .== m.cash[1,5,:,:,:,:,:,:,1,5,:][:]) --> true

		# values must be different in shocked region after shock hits
		@fact !all(m2.vh[1,6,:,:,:,:,:,m.aone+1,1,6,shockAge+1][:] .== m.vh[1,6,:,:,:,:,:,m.aone+1,1,6,shockAge+1][:]) --> true
		t1 = m2.vh[1,5,:,:,:,:,:,m.aone+1,1,6,shockAge+1][:]
		t2 = m.vh[1,5,:,:,:,:,:,m.aone+1,1,6,shockAge+1][:]
		@fact !all(t1[t1 .!= -99.0] .== t2[t2 .!= -99.0] ) --> true

		@fact all(m2.vh[1,6,:,:,:,:,:,m.aone+1,1,6,shockAge-1][:] .== m.vh[1,6,:,:,:,:,:,m.aone+1,1,6,shockAge-1][:]) --> true
	end

	context("checking solution under income shock") do

		p = Param(2)
		m = Model(p)
		solve!(m,p)

		shockAge = 10

		opts = Dict("policy" => "yshock","shockYear"=>1997, "shockAge" => shockAge, "shockRegion" => 6, "shockVal" => [0.7 for i=1:p.nt-1])

		p2 = Param(2,opts)
		m2 = Model(p2)
		solve!(m2,p2)

		mig.adjustVShocks!(m2,m,p2)

		# everybody's cash must decrease with lower income
		@fact all(m2.cash[1,6,:,:,:,:,:,:,1,6,shockAge+1][:] .<= m.cash[1,6,:,:,:,:,:,:,1,6,shockAge+1][:]) --> true
		@fact all(m2.cash[1,6,:,:,:,:,:,:,2,6,shockAge+1][:] .<= m.cash[1,6,:,:,:,:,:,:,2,6,shockAge+1][:] ) --> true
		@fact all(m2.cash[2,6,:,:,:,:,:,:,2,6,shockAge+1][:] .<= m.cash[2,6,:,:,:,:,:,:,2,6,shockAge+1][:] ) --> true
		# if you move, you earn income in origin location,i.e. 6, which is subject to shock: less cash!
		@fact all(m2.cash[1,5,:,:,:,:,:,:,1,6,shockAge+1][:] .<= m.cash[1,5,:,:,:,:,:,:,1,6,shockAge+1][:]) --> true

		@fact all(m2.cash[2,6,:,:,:,:,:,:,1,6,shockAge-1][:] .== m.cash[2,6,:,:,:,:,:,:,1,6,shockAge-1][:]) --> true
		@fact all(m2.cash[1,6,:,:,:,:,:,:,2,6,shockAge-1][:] .== m.cash[1,6,:,:,:,:,:,:,2,6,shockAge-1][:] ) --> true
		@fact all(m2.cash[2,6,:,:,:,:,:,:,2,6,shockAge-1][:] .== m.cash[2,6,:,:,:,:,:,:,2,6,shockAge-1][:] ) --> true
		@fact all(m2.cash[1,5,:,:,:,:,:,:,1,5,shockAge-1][:] .== m.cash[1,5,:,:,:,:,:,:,1,5,shockAge-1][:]) --> true
		@fact all(m2.cash[1,5,:,:,:,:,:,:,1,6,shockAge-1][:] .== m.cash[1,5,:,:,:,:,:,:,1,6,shockAge-1][:]) --> true

		# values must be different
		@fact all(m2.vh[1,6,:,:,:,:,:,m.aone+1,1,6,shockAge+1][:] .!= m.vh[1,6,:,:,:,:,:,m.aone+1,1,6,shockAge+1][:]) --> true
		@fact all(m2.vh[1,6,:,:,:,:,:,m.aone+1,2,6,shockAge+1][:] .!= m.vh[1,6,:,:,:,:,:,m.aone+1,2,6,shockAge+1][:] ) --> true
		@fact all(m2.vh[2,6,:,:,:,:,:,m.aone+1,2,6,shockAge+1][:] .!= m.vh[2,6,:,:,:,:,:,m.aone+1,2,6,shockAge+1][:] ) --> true
		# if you move, you earn income in origin location,i.e. 6, which is subject to shock: less cash!
		t1 = m2.vh[1,5,:,:,:,:,:,m.aone+1,1,6,shockAge+1][:]
		t2 = m.vh[1,5,:,:,:,:,:,m.aone+1,1,6,shockAge+1][:]
		@fact all(t1[t1 .!= -99.0] .!= t2[t2 .!= -99.0] ) --> true

		@fact all(m2.vh[2,6,:,:,:,:,:,m.aone+1,1,6,shockAge-1][:] .== m.vh[2,6,:,:,:,:,:,m.aone+1,1,6,shockAge-1][:]) --> true
		@fact all(m2.vh[1,6,:,:,:,:,:,m.aone+1,2,6,shockAge-1][:] .== m.vh[1,6,:,:,:,:,:,m.aone+1,2,6,shockAge-1][:] ) --> true
		@fact all(m2.vh[2,6,:,:,:,:,:,m.aone+1,2,6,shockAge-1][:] .== m.vh[2,6,:,:,:,:,:,m.aone+1,2,6,shockAge-1][:] ) --> true
		@fact all(m2.vh[1,5,:,:,:,:,:,m.aone+1,1,5,shockAge-1][:] .== m.vh[1,5,:,:,:,:,:,m.aone+1,1,5,shockAge-1][:]) --> true
		@fact all(m2.vh[1,5,:,:,:,:,:,m.aone+1,1,6,shockAge-1][:] .== m.vh[1,5,:,:,:,:,:,m.aone+1,1,6,shockAge-1][:]) --> true
	end


	context("z shock sequences are identical in baseline and shocked versions") do
		
		p = Param(2)
		m = Model(p)
		solve!(m,p)
		sim0 = simulate(m,p)
		sim0 = sim0[!mig.isna(sim0[:cohort]),:]

		opts = mig.selectPolicy("pshock",5,2007,p)

		s10 = mig.computeShockAge(m,opts,10);
		coh = unique(s10[:cohort])

		@fact coh[1] --> p.nt - 10 + 2007 - 1997

		@fact all(sim0[sim0[:cohort].==coh,:z] .== s10[:z]) --> true

	end

end

end

# facts("welfare function behaves correctly at identical policy") do

# 	opts = ["verbose" => 0]
# 	p0 = Param(2,opts)
# 	m0 = Model(p0)
# 	solve!(p0,m0)
# 	v0 = m0.vh[1,1,1,3,2,2,2,m0.aone,1,1,1]

# 	@fact mig.welfare(1.1,v0,opts) > 0.0 --> true
# 	@fact mig.welfare(0.9,v0,opts) < 0.0 --> true

# end