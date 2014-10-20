



# julia script to drive computation of shockp_highMC value finder


println("Started julia")

t0 = time()

include("iridis_launcher.jl")

bind_iridis_procs(7)

require("loadmig.jl")

e = runExperiment("pshock_noSaving",6,2007)


println("done after $(round((time()-t0)/60)) minutes")
println("done. quitting cluster.")

quit()

