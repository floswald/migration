


# make slices

require("src/nodes.jl")

mig_slice = slices(mprob,length(workers()))
plotSlices(mprob,mig_slices[1],mig_slices[2])

tdir = joinpath(ENV["HOME"],"JL_tempdir")
cd(tdir)
run(`rm -rf *`)
savePlots(tdir,"png")

# save on dropbox
run(`dropbox_uploader upload $(tdir) mobility/output/model/data_repo/out_graphs_jl/`)



