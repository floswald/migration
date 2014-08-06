


# make slices

require("nodes.jl")


# if on my machine test with n points
# if length(workers())==1
# 	mig_slice = slices(mprob,2)
# else
# 	mig_slice = slices(mprob,length(workers()))
# end

# plotSlices(mprob,mig_slice[1],mig_slice[2],facet="moments")

mig_slice = slices(mprob,64)
writetable(joinpath(outdir,"migslice1.csv"),mig_slice[1])
writetable(joinpath(outdir,"migslice2.csv"),mig_slice[2])
# savePlots(tdir,"png")

# save on dropbox
# run(`dropbox_uploader upload $(tdir) mobility/output/model/data_repo/out_graphs_jl/`)



