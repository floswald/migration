

module migtests


using FactCheck

include("test_accelerator.jl")
include("test_experiment.jl")
include("test_migincl.jl")
include("test_model.jl")
include("test_param.jl")
include("test_sim.jl")
include("test_solution.jl")
include("test_solver.jl")

FactCheck.exitstatus()

end