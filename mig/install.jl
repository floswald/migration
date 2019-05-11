Pkg.add.(["GLM","PDMats","Distributions","DataFrames","CategoricalArrays","DataFramesMeta","Optim","JLD2","GR","PGFPlots","JSON","ClusterManagers","FileIO","RData","DocOpt","Plots","StatPlots","ProgressMeter","Interpolations","Roots"])
Pkg.clone("https://github.com/floswald/ApproXD.jl")
Pkg.clone("https://github.com/floswald/MomentOpt.jl")
loc = dirname(@__FILE__)
jv6 = ENV["JULIA_PKGDIR"]
run(`ln -s $loc $(joinpath(jv6,"v0.6"))`)  # symlink this into julia package dir
println("will now precompile the package. hold tight.")
using mig