

module test_param

using FactCheck, mig



facts("test updating of param vector") do

	p = mig.Param(1);

	di = ["beta" => 1.4, "omega2" => 34.2]
	mig.update!(p,di)

	@fact p.beta => di["beta"]
	@fact p.omega2 => di["omega2"]


end




end