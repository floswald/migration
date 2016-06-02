


# make slices

t0 = time()

@everywhere include("../nodes.jl")

sl = MOpt.slices(mprob,30);
MOpt.write(sl,"slices.h5")


println("done after $(round((time()-t0)/60)) minutes")