Pkg.add.(["GLM","PDMats","Distributions","DataFrames","CategoricalArrays","DataFramesMeta","Optim","JLD2","GR","PGFPlots","JSON","ClusterManagers","FileIO","RData","DocOpt","Plots","StatPlots","ProgressMeter","Interpolations","Roots"])
loc = dirname(@__FILE__)
if haskey(ENV,"JULIA_PKGDIR")
	jv6 = ENV["JULIA_PKGDIR"]
	run(`ln -s $loc $(joinpath(jv6,"v0.6"))`)  # symlink this into julia package dir
else
	run(`ln -s $loc $(Pkg.dir())`)  # symlink this into julia package dir
end

println("will now precompile the package. hold tight.")
using mig