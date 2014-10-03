


module test_experiment



using mig, FactCheck



facts("test param with policy") do

	p = Param(2)
	@fact p.policy => "NULL"
	@fact p.mort_LumpSum => [0.0]
	@fact p.verbose => 0
	@fact p.shockReg => 0
	@fact p.shockAge => 100
	@fact p.shockVal => 1.0



	lumpSum1 = [rand()]
	opts = ["policy" => "mortgageSubsidy_oldyoung","lumpsum" => lumpSum1]

	p = mig.Param(1,opts);

	@fact p.policy => "mortgageSubsidy_oldyoung"
	@fact p.mort_LumpSum => lumpSum1
	@fact p.ctax => 1.0
	@fact p.shockReg => 0
	@fact p.shockAge => 100
	@fact p.shockVal => 1.0


	lumpSum2 = [rand(p.nt-1)]
	opts = ["policy" => "mortgageSubsidy_in_age","lumpsum" => lumpSum2]

	p = mig.Param(1,opts);

	@fact p.policy => "mortgageSubsidy_in_age"
	@fact p.mort_LumpSum => lumpSum2
	@fact p.ctax => 1.0
	@fact p.shockReg => 0
	@fact p.shockAge => 100
	@fact p.shockVal => 1.0


	opts = ["policy" => "shocky","shockYear"=>1997, "shockAge" => 1, "shockRegion" => 5, "shockVal" => 0.3]

	p0 = mig.Param(2);
	p = mig.Param(2,opts);

	@fact p.policy => "shocky"
	@fact p.ctax => 1.0
	@fact p.mort_LumpSum => [0.0]
	@fact p.shockAge => 1
	@fact p.shockVal => 0.3
	@fact p.shockReg => 5

	m0 = Model(p0)
	m = Model(p)

	@fact all(m.gridsXD["z"][:,1,1,2,5][:] .- m0.gridsXD["z"][:,1,1,2,5][:] .< 0.0) => true



	opts = ["policy" => "shockp","shockYear"=>1997, "shockAge" => 10, "shockRegion" => 6, "shockVal" => 3.3]

	p = mig.Param(2,opts);

	@fact p.policy => "shockp"
	@fact p.ctax => 1.0
	@fact p.mort_LumpSum => [0.0]
	@fact p.shockAge => 10
	@fact p.shockVal => 3.3
	@fact p.shockReg => 6
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