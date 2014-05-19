module mig

import Base.LinAlg: copytri! 
using JSON, Debug, PyPlot

export Param, Model

include("/Users/florianoswald/git/copula.jl/mods/copula.jl")
include("param.jl")
include("model.jl")
include("E_tensors.jl")
include("solver.jl")
include("plotting.jl")

end

