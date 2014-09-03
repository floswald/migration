


# run estimation
t0 = time()

require("nodes.jl")

opts =[
	"N"=>nprocs()-1,
	"printlevel"=> 3,
	"filename" => joinpath(ENV["HOME"],"git/migration/mig/src/cluster","MA.h5"),	
	"save_frequency"=> 10,
	"user"=> ENV["USER"],
	"date"=> readall(`date`),
	"maxiter"=> 50,
	"maxtemp"=>100,
	"min_shock_sd"=>0.1,
	"max_shock_sd"=>2,
	"past_iterations"=>30,
	"min_accept_tol"=>1000,
	"max_accept_tol"=>1000,
	"min_disttol"=>0.05,	# if other chain is within 10% of my value, i consider jumping
	"max_disttol"=>0.05,
	"min_jump_prob"=>0.1,
	"max_jump_prob"=>0.1] 

MA = MAlgoBGP(mprob,opts)
runMOpt!(MA)

# save results
MOpt.save(MA,MA["filename"])

println("done after $(round((time()-t0)/60)) minutes")

println("quitting cluster")
quit()


