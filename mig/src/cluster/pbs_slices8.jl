

println("Started julia")

include("iridis_launcher.jl")

bind_iridis_procs(8)

require("nodes.jl")

sl = MOpt.slices(mprob,30);

println(sl)

MOpt.write(sl,"slices.h5")

quit()