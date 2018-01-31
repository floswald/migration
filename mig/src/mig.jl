module mig

using GLM, MomentOpt, PDMats, Distributions, DataFrames, DataFramesMeta, ApproXD, Optim, JLD
using CategoricalArrays: CategoricalArray
using Missings
using NLsolve
using JSON , Copulas, FileIO, RData # FixedSizeArrays #, Plots, StatPlots
# using QuantileRegression: qreg
using DataArrays
import Base.show, Base.convert, Base.print, Base.get
import Base: .+, .-, .*, ./
import Base: +, -, *, /, abs

export Param, Model, runObj, runSim, simulate, solve!, runExperiment

const NOMOVE_PEN = 10000.0


include("migsrc/param.jl")
include("migsrc/model.jl")
include("migsrc/accelerator.jl")
include("migsrc/mig-incl.jl")
# include("migsrc/E_tensors.jl")
include("migsrc/solver.jl")
include("migsrc/simulator.jl")
include("migsrc/experiments.jl")
include("migsrc/reporting.jl")

if is_apple()
	include("migsrc/plotting.jl")
end

end

