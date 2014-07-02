module mig

using Debug, Copulas
using Reexport
@reexport using DataFrames
import Distributions: Categorical, rand
import Base.show

export Param, Model




include("param.jl")
include("model.jl")
include("mig-incl.jl")
include("E_tensors.jl")
include("solver.jl")
include("plotting.jl")
include("simulator.jl")
include("experiments.jl")

end

