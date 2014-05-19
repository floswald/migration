

module test_param

using FactCheck
using mig

facts("testing bounds on grids") do

	p = mig.Param(1);

	# check we have all bounds in bounds
	b1 = ["asset_rent","asset_own","P","tau"]
	@fact length(setdiff(b1, collect(keys(p.bounds)))) => 0
	for k in keys(p.bounds)
		@fact typeof(p.bounds[k]) == (Float64,Float64) => true
		@fact p.bounds[k][2] - p.bounds[k][1] > 0 => true
	end


	# check pbounds: bounds on prices
	# check they are all ascendign numbers
	# get bounds data from R
	xy = readcsv("/Users/florianoswald/Dropbox/mobility/output/model/R2julia/divincome.csv")
	xy = xy[2:end,2:end]
	xp = readcsv("/Users/florianoswald/Dropbox/mobility/output/model/R2julia/divprice.csv")
	xp = xp[2:end,2:end]

	for j in 1:p.nJ
		@fact p.pbounds["p"][j,1] == xp[j,3] => true
		@fact p.pbounds["p"][j,2] == xp[j,4] => true
		@fact p.pbounds["y"][j,1] == xy[j,3] => true
		@fact p.pbounds["y"][j,2] == xy[j,4] => true
	end


end


facts("length of dimension vectors") do

	p = mig.Param(1);

	@fact length(p.dimvec)  == 10 => true
	@fact length(p.dimvec2) == 9 => true

end


facts("check that all parameter arrays have regions in same order") do

	p = mig.Param(1)

	@fact p.regnames == p.Ageprof[:,1] => true
	@fact p.regnames == p.IncomeParams[2:end,1] => true
	@fact p.regnames == p.Ageprof[:,1] => true
	@fact p.regnames == p.distance[2:end,1] => true
	@fact p.regnames[:] == p.distance[1,2:end][:] => true

end


end