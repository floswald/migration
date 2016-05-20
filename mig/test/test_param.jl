

module test_param

using FactCheck, mig



facts("Param type.") do

	context("test updating") do
		p = mig.Param(1);

		di = Dict("beta" => 1.4, "omega2" => 34.2)
		mig.update!(p,di)

		@fact p.beta => di["beta"]
		@fact p.omega2 => di["omega2"]
	end

	context("size 2") do
		p = mig.Param(2);
		@fact p.omega2 --> not(0)

	end

	context("housing states and sizes") do

		p = mig.Param(2)
		@fact p.nh --> not(p.nhh)
		@fact p.nhh --> p.nh + 1
	end


end


end