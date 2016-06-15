


# make slices

t0 = time()

@everywhere include("nodes.jl")

sl = MOpt.slices(mprob,length(workers()));
MOpt.write(sl,joinpath(io["outdir"],"slices.h5"))


println("done after $(round((time()-t0)/60)) minutes")