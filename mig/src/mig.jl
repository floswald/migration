module mig

using GLM, MOpt, PDMats, Distributions, DataFrames, DataFramesMeta, ApproXD, Optim, HDF5, PyPlot
using JSON , Copulas, Debug
import Base.show, Base.convert, Base.print, Base.get

export Param, Model, runObj, runSim, simulate, solve!, runExperiment




include("migsrc/param.jl")
include("migsrc/model.jl")
include("migsrc/accelerator.jl")
include("migsrc/mig-incl.jl")
# include("migsrc/E_tensors.jl")
include("migsrc/solver.jl")
include("migsrc/simulator.jl")
# include("migsrc/experiments.jl")
include("migsrc/reporting.jl")

include("migsrc/plotting.jl")

end

