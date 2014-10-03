

function runExperiment(which)

	if ENV["USER"] == "florian_o"
		push!(DL_LOAD_PATH, "/home/florian_o/local/lib")
	elseif ENV["USER"] == "eisuc151"
		push!(DL_LOAD_PATH, "/home/eisuc151/local/lib")
	end
	
	home = ENV["HOME"]
	require(joinpath(home,"git/migration/mig/src/cluster/loadmig.jl"))

	indir, outdir = mig.setPaths()

	if which=="mortgage_deduct"
		e = mig.exp_Mortgage(true)
	elseif which=="shockp"
		e = mig.exp_shockRegion(5,"p")
	elseif which=="shocky"
		e = mig.exp_shockRegion(5,"y")
	end

	using HDF5, JLD
	save(joinpath(outdir,"exp_$which.JLD"),e)
	println("done.")
	return e

end

