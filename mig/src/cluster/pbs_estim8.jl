println("Started julia")


include("iridis_launcher.jl")

bind_iridis_procs(8)

require("nodes.jl")

opts =[
	"N"=>nprocs(),
	"printlevel"=> 3,
	"filename" => joinpath(ENV["HOME"],"git/migration/mig/src/cluster","MA.h5"),	
	"save_frequency"=> 2,
	"user"=> ENV["USER"],
	"date"=> readall(`date`),
	"maxiter"=> 5,
	"maxtemp"=>10,
	"min_shock_sd"=>0.1,
	"max_shock_sd"=>2,
	"past_iterations"=>30,
	"min_disttol"=>0.05,	# if other chain is within 10% of my value, i consider jumping
	"max_disttol"=>0.1,
	"min_jump_prob"=>0.1,
	"max_jump_prob"=>0.1] 

MA = MAlgoBGP(mprob,opts)
runMOpt!(MA)

# save results
MOpt.save(MA,MA["filename"])

println("done. quitting cluster.")

quit()