module mig

using Copulas, GLM, MOpt.transpose, PyPlot
using Reexport
@reexport using DataFrames
import Distributions: Categorical, rand
import Base.show

export Param, Model, simplot




include("migsrc/param.jl")
include("migsrc/model.jl")
include("migsrc/mig-incl.jl")
include("migsrc/E_tensors.jl")
include("migsrc/solver.jl")
include("migsrc/plotting.jl")
include("migsrc/simulator.jl")
include("migsrc/experiments.jl")

end

