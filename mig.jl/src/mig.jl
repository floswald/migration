module mig

using GLM, MOpt.transpose, PDMats, Distributions # BSplines #, Debug , BSplines
using Reexport
@reexport using DataFrames
import Base.show

export Param, Model




include("migsrc/param.jl")
include("migsrc/model.jl")
include("migsrc/model2.jl")
include("migsrc/mig-incl.jl")
include("migsrc/E_tensors.jl")
include("migsrc/solver.jl")
include("migsrc/solver2.jl")
include("migsrc/simulator.jl")
include("migsrc/simulator2.jl")
include("migsrc/experiments.jl")

if Sys.OS_NAME == :Darwin
	include("migsrc/plotting.jl")
end

end

