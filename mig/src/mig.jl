module mig

using GLM, MomentOpt, PDMats, Distributions, DataFrames, DataFramesMeta, ApproXD, Optim, JLD2
using CategoricalArrays: CategoricalArray
using Missings
using LaTeXStrings
using StatsBase
using JSON , Copulas, FileIO, RData 
using Plots, StatPlots
import Base.show, Base.convert, Base.print, Base.get
import Base: .+, .-, .*, ./
import Base: +, -, *, /, abs

export Param, Model, runObj, runSim, simulate, solve!, runExperiment

const NOMOVE_PEN = 10000.0


include("migsrc/param.jl")
include("migsrc/model.jl")
include("migsrc/accelerator.jl")
include("migsrc/mig-incl.jl")
include("migsrc/solver.jl")
include("migsrc/simulator.jl")
include("migsrc/experiments.jl")
include("migsrc/noMove.jl")
include("migsrc/reporting.jl")
include("migsrc/plotting.jl")

end
