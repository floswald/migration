



# julia script to drive computation of shockp_highMC value finder


println("Started julia")

t0 = time()

include("iridis_launcher.jl")

bind_iridis_procs(7)

require("loadmig.jl")

indir, outdir = mig.setPaths()

e = mig.exp_shockRegion_vdiff("pshock","pshock_highMC")
save(joinpath(outdir,"shockReg","exp_region6_shockp_highMC.JLD"),e)


println("done after $(round((time()-t0)/60)) minutes")
println("done. quitting cluster.")

quit()

