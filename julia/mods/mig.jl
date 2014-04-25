module mig

using Base.LinAlg, Distributions

export Param, Model, Copula, dnormCopula


include("/Users/florianoswald/git/copula.jl/mods/copula.jl")
include("param.jl")
include("model.jl")
include("E_tensors.jl")
include("solver.jl")

end

