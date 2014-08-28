module mig

using GLM, MOpt.transpose, PDMats, Distributions # BSplines #, Debug , BSplines
using Reexport
@reexport using DataFrames
import Base.show

export Param, Model, runObj, runSim




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

