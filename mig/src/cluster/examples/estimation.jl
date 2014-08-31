


# run estimation

require("nodes.jl")

opts =[
	"N"=>nprocs(),
	"print_level"=> 3,
	"filename" => joinpath(pwd(),"MA.h5"),	
	"save_frequency"=> 10,
	"maxiter"=> 30,
	"maxtemp"=>100,
	"min_shock_sd"=>0.1,
	"max_shock_sd"=>1,
	"past_iterations"=>30,
	"min_accept_tol"=>1000,
	"max_accept_tol"=>1000,
	"min_disttol"=>10.0,
	"max_disttol"=>10.0,
	"min_jump_prob"=>0.5,
	"max_jump_prob"=>0.5] 

MA = MAlgoBGP(mprob,opts)
runMOpt!(MA)

# save results
MOpt.save(MA,MA["filename"])

println("quitting cluster")
quit()
