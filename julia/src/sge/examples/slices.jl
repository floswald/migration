


# make slices

require("nodes.jl")


# if on my machine test with n points
if length(workers())==1
	mig_slice = MOpt.slices(mprob,2)
else
	mig_slice = MOpt.slices(mprob,length(workers()))
end

MOpt.plotSlices(mprob,mig_slice[1],mig_slice[2],facet="moments")

tdir = joinpath(ENV["HOME"],"JL_tempdir")
cd(tdir)
run(`rm -rf *`)
savePlots(tdir,"png")

# save on dropbox
run(`dropbox_uploader upload $(tdir) mobility/output/model/data_repo/out_graphs_jl/`)



