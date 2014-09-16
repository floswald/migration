module mig

using GLM, MOpt.transpose, PDMats, Distributions, DataFrames, DataFramesMeta, ApproXD
import Base.show

export Param, Model, runObj, runSim




include("migsrc/accelerator.jl")
include("migsrc/param.jl")
include("migsrc/model.jl")
include("migsrc/mig-incl.jl")
include("migsrc/E_tensors.jl")
include("migsrc/solver.jl")
include("migsrc/simulator.jl")
include("migsrc/experiments.jl")

if Sys.OS_NAME == :Darwin
	include("migsrc/plotting.jl")
end

end

