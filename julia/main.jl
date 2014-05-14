

# main programme
cd("/Users/florianoswald/git/migration/julia")
include("mods/mig.jl")
	
# testing
include("test/test_param.jl")
include("test/test_model.jl")
include("test/test_solver.jl")
include("test/test_Tensors.jl")
	

# running
p = mig.Param()
m = mig.Model(p)
mig.solve!(m,p)
# mig.solvePeriod!(1,m,p);

cp = mig.Copmod.Copula(2,0.8)


