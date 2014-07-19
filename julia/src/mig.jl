module mig

using Copulas, GLM, MOpt.transpose, PyPlot, Debug, BSplines
using Reexport
@reexport using DataFrames
import Distributions: Categorical, rand
import Base.show

export Param, Model, simplot




include("migsrc/param.jl")
include("migsrc/model.jl")
include("migsrc/model2.jl")
include("migsrc/mig-incl.jl")
include("migsrc/E_tensors.jl")
include("migsrc/solver.jl")
include("migsrc/solver2.jl")
include("migsrc/plotting.jl")
include("migsrc/simulator.jl")
include("migsrc/simulator2.jl")
include("migsrc/experiments.jl")
include("migsrc/approx.jl")

end

