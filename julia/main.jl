

# main programme
cd("/Users/florianoswald/git/migration/julia")
include("mods/mig.jl")
	
# testing
include("test/test_param.jl")
include("test/test_model.jl")
include("test/test_solver.jl")
include("test/test_Tensors.jl")
	

# running
p = mig.Param(1)
m = mig.Model(p)
@time mig.solve!(m,p)
# mig.solvePeriod!(1,m,p);



# without linear index: 96 secs
# with linear index: 74 secs
# speedup: 29%

# with discretized savings solution: 74 secs
# without any savings solution: 73 secs

