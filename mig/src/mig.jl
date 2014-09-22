module mig

using GLM, MOpt.transpose, PDMats, Distributions, DataFrames, DataFramesMeta, ApproXD, Lazy
import Base.show, Base.convert

export Param, Model, runObj, runSim, simulate, solve!




include("migsrc/accelerator.jl")
include("migsrc/param.jl")
include("migsrc/model.jl")
include("migsrc/mig-incl.jl")
include("migsrc/E_tensors.jl")
include("migsrc/solver.jl")
include("migsrc/simulator.jl")
include("migsrc/experiments.jl")
include("migsrc/reporting.jl")

if Sys.OS_NAME == :Darwin
	include("migsrc/plotting.jl")
end

end

