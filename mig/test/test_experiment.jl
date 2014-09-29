


module test_experiment



using mig, FactCheck



facts("test param with policy") do

	lumpSum1 = [rand()]
	opts = ["policy" => "mortgageSubsidy_oldyoung","lumpsum" => lumpSum1]

	p = mig.Param(1,opts);

	@fact p.policy => "mortgageSubsidy_oldyoung"
	@fact p.mort_LumpSum => lumpSum1
	@fact p.ctax => 1.0


	lumpSum2 = [rand(p.nt-1)]
	opts = ["policy" => "mortgageSubsidy_in_age","lumpsum" => lumpSum2]

	p = mig.Param(1,opts);

	@fact p.policy => "mortgageSubsidy_in_age"
	@fact p.mort_LumpSum => lumpSum2
	@fact p.ctax => 1.0
end

facts("welfare function behaves correctly at identical policy") do

	opts = ["verbose" => 0]
	p0 = Param(2,opts)
	m0 = Model(p0)
	solve!(p0,m0)
	v0 = m0.vh[1,1,1,3,2,2,2,m0.aone,1,1,1]

	@fact mig.welfare(1.1,v0,opts) > 0.0 => true
	@fact mig.welfare(0.9,v0,opts) < 0.0 => true

end