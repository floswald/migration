module mig

using GLM, PDMats, Distributions, DataFrames, DataFramesMeta, Optim, JLD2
using CategoricalArrays: CategoricalArray
using DataStructures: OrderedDict
using Missings
using LaTeXStrings
using StatsBase
using JSON , FileIO#, RData 
using Plots, StatPlots
import Base.show, Base.convert, Base.print, Base.get
import Base: .+, .-, .*, ./
import Base: +, -, *, /, abs
using ProgressMeter
using ClusterManagers
using Interpolations
using Roots
# using RCall  # dont use this as impossible to install on cluster.


# to install my non-registered packages
function install_floswald(name::String,tag = "v0.6")
	if !isdir(Pkg.dir(name))
		pd = pwd()
		url = "https://github.com/floswald/$name.jl"
		info("Installing non-registered $url")
		Pkg.clone(url)
		info("Checking out correct tag")
		cd(Pkg.dir(name))
		run(`git fetch`)
		run(`git fetch --tags`)
		run(`git checkout $tag`)
		cd(pd)
	end
end

install_floswald("ApproXD")
install_floswald("MomentOpt")
install_floswald("ParallelTest")

using ApproXD, MomentOpt

export Param, Model, runObj, runSim, simulate, solve!, runExperiment

const NOMOVE_PEN = 100_000_000.0


include("migsrc/param.jl")
include("migsrc/model.jl")
include("migsrc/accelerator.jl")
include("migsrc/mig-incl.jl")
include("migsrc/solver.jl")
include("migsrc/simulator.jl")
include("migsrc/experiments.jl")
include("migsrc/estimation.jl")
include("migsrc/noMove.jl")
include("migsrc/reporting.jl")
include("migsrc/plotting.jl")

end
