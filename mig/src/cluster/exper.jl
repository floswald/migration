


if ENV["USER"] == "florian_o"
	push!(DL_LOAD_PATH, "/home/florian_o/local/lib")
elseif ENV["USER"] == "eisuc151"
	push!(DL_LOAD_PATH, "/home/eisuc151/local/lib")
end


using mig

indir, outdir = mig.setPaths()

e = mig.exp_Mortgage()

writetable(joinpath(outdir,"experiment_plot.csv"),e["plotting"])
using HDF5, JLD
save(joinpath(outdir,"experiment_data.JLD"),e)

