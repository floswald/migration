


module test_experiment



using mig, FactCheck


facts("testing selectPolicy") do

	p = Param(2)
	
	j = 8
	yr = 1999
	po = mig.selectPolicy("p",j,yr,p)
	@fact po["policy"] => "shockp"
	@fact po["shockRegion"] => j
	@fact po["shockYear"] => yr
	@fact po["shockAge"] => 1
	@fact po["shockVal"] => [0.7 for i=1:p.nt-1]

	po = mig.selectPolicy("p3",j,yr,p)
	@fact po["policy"] => "shockp"
	@fact po["shockRegion"] => j
	@fact po["shockYear"] => yr
	@fact po["shockAge"] => 1
	@fact po["shockVal"] => [0.7, 0.8, 0.9, [1.0 for i=1:p.nt-3]]

	po = mig.selectPolicy("y",j,yr,p)
	@fact po["policy"] => "shocky"
	@fact po["shockRegion"] => j
	@fact po["shockYear"] => yr
	@fact po["shockAge"] => 1
	@fact po["shockVal"] => [0.7 for i=1:p.nt-1]

	po = mig.selectPolicy("y3",j,yr,p)
	@fact po["policy"] => "shocky"
	@fact po["shockRegion"] => j
	@fact po["shockYear"] => yr
	@fact po["shockAge"] => 1
	@fact po["shockVal"] => [0.7, 0.8, 0.9, [1.0 for i=1:p.nt-3]]

end



facts("test param with policy") do

	p = Param(2)
	@fact p.policy => "NULL"
	@fact p.mort_LumpSum => [0.0]
	@fact p.verbose => 0
	@fact p.shockReg => 0
	@fact p.shockAge => 100
	@fact p.shockVal => ones(p.nt-1)



	lumpSum1 = [rand()]
	opts = ["policy" => "mortgageSubsidy_oldyoung","lumpsum" => lumpSum1]

	p = mig.Param(2,opts);

	@fact p.policy => "mortgageSubsidy_oldyoung"
	@fact p.mort_LumpSum => lumpSum1
	@fact p.ctax => 1.0
	@fact p.shockReg => 0
	@fact p.shockAge => 100
	@fact p.shockVal => ones(p.nt-1)


	p0 = mig.Param(1,opts);
	lumpSum2 = [rand(p0.nt-1)]
	opts = ["policy" => "mortgageSubsidy_in_age","lumpsum" => lumpSum2]

	p = mig.Param(1,opts);

	@fact p.policy => "mortgageSubsidy_in_age"
	@fact p.mort_LumpSum => lumpSum2
	@fact p.ctax => 1.0
	@fact p.shockReg => 0
	@fact p.shockAge => 100
	@fact p.shockVal => ones(p.nt-1)


	p0 = mig.Param(2,opts);
	opts = mig.selectPolicy("y3",5,1997,p0)
	p = mig.Param(2,opts);

	@fact p.policy => "shocky"
	@fact p.ctax => 1.0
	@fact p.mort_LumpSum => [0.0]
	@fact p.shockAge => 1
	@fact p.shockVal => [0.7, 0.8, 0.9, ones(p.nt-3)]
	@fact p.shockReg => 5


	p0 = mig.Param(2);
	opts = mig.selectPolicy("y",5,1997,p0)
	p = mig.Param(2,opts);

	@fact p.policy => "shocky"
	@fact p.ctax => 1.0
	@fact p.mort_LumpSum => [0.0]
	@fact p.shockAge => 1
	@fact p.shockVal => ones(p.nt-1) * 0.7
	@fact p.shockReg => 5

	m0 = Model(p0)
	m = Model(p)

	@fact all(m.gridsXD["z"][:,1,1,2,5][:] .- m0.gridsXD["z"][:,1,1,2,5][:] .< 0.0) => true



	opts = ["policy" => "shockp","shockYear"=>1997, "shockAge" => 10, "shockRegion" => 6, "shockVal" => [3.3 for i=1:p.nt-1]]

	p = mig.Param(2,opts);

	@fact p.policy => "shockp"
	@fact p.ctax => 1.0
	@fact p.mort_LumpSum => [0.0]
	@fact p.shockAge => 10
	@fact p.shockVal => ones(p.nt-1) * 3.3
	@fact p.shockReg => 6
	@fact all(m.gridsXD["z"][:,1,1,21,6][:] .- m0.gridsXD["z"][:,1,1,21,6][:] .== 0.0) => true
end

end

# facts("welfare function behaves correctly at identical policy") do

# 	opts = ["verbose" => 0]
# 	p0 = Param(2,opts)
# 	m0 = Model(p0)
# 	solve!(p0,m0)
# 	v0 = m0.vh[1,1,1,3,2,2,2,m0.aone,1,1,1]

# 	@fact mig.welfare(1.1,v0,opts) > 0.0 => true
# 	@fact mig.welfare(0.9,v0,opts) < 0.0 => true

# end