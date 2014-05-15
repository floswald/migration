

module test_param

using FactCheck
using mig

facts("testing bounds on grids") do

	p = mig.Param(1);

	# check we have all bounds in bounds
	b1 = ["asset_rent","asset_own","P","tau","Y"]
	@fact length(setdiff(b1, collect(keys(p.bounds)))) => 0
	for k in keys(p.bounds)
		@fact typeof(p.bounds[k]) == (Float64,Float64) => true
		@fact p.bounds[k][2] - p.bounds[k][1] > 0 => true
	end


	# check pbounds: bounds on prices
	# check they are all ascendign numbers
	for k in keys(p.pbounds)
		for j in 1:p.nJ
			@fact typeof(p.pbounds[k][j]) == Array{Float64,1} => true
			@fact length(p.pbounds[k][j]) == 2 => true
			@fact diff(p.pbounds[k][j])[] > 0.0 => true
		end
	end


end


facts("length of dimension vectors") do

	p = mig.Param(1);

	@fact length(p.dimvec)  == 11 => true
	@fact length(p.dimvec2) == 10 => true
	@fact length(p.dimvec3) == 9 => true

end



end