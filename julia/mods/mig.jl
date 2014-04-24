module mig

using Base.LinAlg, Distributions

export Param, Model, Copula, dnormCopula

include("param.jl")
include("model.jl")
include("solver.jl")
include("copula.jl")

end

