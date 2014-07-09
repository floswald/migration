module mig

using Debug, Copulas, GLM, MOpt.transpose
using Reexport
@reexport using DataFrames
import Distributions: Categorical, rand
import Base.show

export Param, Model




include("migsrc/param.jl")
include("migsrc/model.jl")
include("migsrc/mig-incl.jl")
include("migsrc/E_tensors.jl")
include("migsrc/solver.jl")
include("migsrc/plotting.jl")
include("migsrc/simulator.jl")
include("migsrc/experiments.jl")

end

