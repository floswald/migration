

# start cluster
using ClusterManagers
println("starting cluster now")
ClusterManagers.addprocs_sge(24,qsub_env="-l ppn=12")

# here a function that runs your estimation:
include("slices.jl")

println("done. quitting cluster.")

	quit()