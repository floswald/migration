


# make slices

t0 = time()

require("../nodes.jl")

sl = MOpt.slices(mprob,length(workers()));
MOpt.write(sl,"slices.h5")


println("done after $(round((time()-t0)/60)) minutes")
# savePlots(tdir,"png")

# save on dropbox
# run(`dropbox_uploader upload $(tdir) mobility/output/model/data_repo/out_graphs_jl/`)



