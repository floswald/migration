

# start cluster
using ClusterManagers
println("starting cluster now")
ClusterManagers.addprocs_sge(30)

# here a function that runs your estimation:
include("slices.jl")

println("done. quitting cluster.")

quit()