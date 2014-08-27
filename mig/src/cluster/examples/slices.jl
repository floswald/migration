


# make slices

t0 = time()

require("nodes.jl")


mig_slice = slices(mprob,32)
writetable(joinpath(outdir,"migslice1.csv"),mig_slice[1])
writetable(joinpath(outdir,"migslice2.csv"),mig_slice[2])

println("done after $(round((time()-t0)/60)) minutes")
# savePlots(tdir,"png")

# save on dropbox
# run(`dropbox_uploader upload $(tdir) mobility/output/model/data_repo/out_graphs_jl/`)



