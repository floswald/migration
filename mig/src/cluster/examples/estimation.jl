


# run estimation

require("nodes.jl")

opts =[
	"N"=>nprocs()-1,
	"printlevel"=> 3,
	"filename" => joinpath(pwd(),"MA.h5"),	
	"save_frequency"=> 5,
	"maxiter"=> 20,
	"maxtemp"=>100,
	"min_shock_sd"=>0.1,
	"max_shock_sd"=>2,
	"past_iterations"=>30,
	"min_accept_tol"=>1000,
	"max_accept_tol"=>1000,
	"min_disttol"=>0.1,	# if other chain is within 10% of my value, i consider jumping
	"max_disttol"=>0.1,
	"min_jump_prob"=>0.5,
	"max_jump_prob"=>0.5] 

MA = MAlgoBGP(mprob,opts)
runMOpt!(MA)

# save results
MOpt.save(MA,MA["filename"])

println("quitting cluster")
quit()
