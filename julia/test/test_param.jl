

module test_param

using FactCheck
using mig

facts("testing parameter setup") do

	p = mig.Param();

	@fact p.beta => 0.95


end

end