module mig

using JSON, Debug, PyPlot, DataFrames
import Distributions: Categorical, rand

export Param, Model

include("/Users/florianoswald/git/copula.jl/mods/copula.jl")
include("param.jl")
include("model.jl")
include("E_tensors.jl")
include("solver.jl")
include("plotting.jl")
include("simulator.jl")
include("experiments.jl")

end

