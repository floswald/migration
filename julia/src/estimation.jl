


# run estimation

using MOpt, mig

# load moments from local disk:

# indir  = joinpath(ENV["HOME"],"Dropbox/mobility/output/model/R2Julia")
# moms = DataFrame(read_rda(joinpath(indir,"moments.rda"))["m"])

# get moments from dropbox:
tmpfile = tempname()
run(`dropbox_uploader download mobility/output/model/data_repo/in_data_jl/moments.rda $tmpfile`)
moms = DataFrame(read_rda(tmpfile)["m"])

# setup parameters to estimate:

p2 = ["gamma"=>1.1,"MC1"=> 0.03]
# setup params to estimate
pb = ["gamma" => [1.01,5.0]]

mprob = MProb(p2,pb,mig.objfunc,moms,moments_subset=["moved0"])
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



