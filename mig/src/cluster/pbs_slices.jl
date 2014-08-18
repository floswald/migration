

println("Started julia")

include("iridis_launcher.jl")

bind_iridis_procs()

require("nodes.jl")

mig_slice = slices(mprob,72)
writetable(joinpath(outdir,"migslice1.csv"),mig_slice[1])
writetable(joinpath(outdir,"migslice2.csv"),mig_slice[2])

println("done. quitting cluster.")

quit()