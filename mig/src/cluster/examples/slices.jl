


# make slices

require("nodes.jl")


mig_slice = slices(mprob,32)
writetable(joinpath(outdir,"migslice1.csv"),mig_slice[1])
writetable(joinpath(outdir,"migslice2.csv"),mig_slice[2])
# savePlots(tdir,"png")

# save on dropbox
# run(`dropbox_uploader upload $(tdir) mobility/output/model/data_repo/out_graphs_jl/`)



