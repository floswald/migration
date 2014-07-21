


# run estimation

require("nodes.jl")

opts =[
	"N"=>2,
	"print_level"=> 3,
	"savefile" => joinpath(pwd(),"..","workdir","MA.h5"),	
	"source_on_nodes" => joinpath("src","nodes.jl"),	
	"mode"=>"mpi",
	"maxiter"=> 3,
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
MOpt.save(MA,MA["savefile"])

# make a timestamp
x=replace(readchomp(`date`)," ","")[1:end-3]
x=replace(x,":","") * ".h5"

# save on dropbox
run(`dropbox_uploader upload $(MA["savefile"]) mobility/output/model/data_repo/out_data_jl/$x`)



