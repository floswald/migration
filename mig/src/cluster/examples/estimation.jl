

if is_apple()
	maxiter=3
	nworkers=1
	addprocs(nworkers)
else 
	maxiter = parse(Int,ARGS[1])
	nworkers = parse(Int,ARGS[2])
	using ClusterManagers
	addprocs(SGEManager(nworkers,""),qsub_env="",res_list="h_vmem=5.5G,tmem=5.5G",exeflags="--depwarn=no")
	# addprocs_sge(nworkers,res_list="h_vmem=5.5G,tmem=5.5G")
end

# setup cluster

# load compute code on all nodes with `using`
include("../nodes.jl")

path = dirname(@__FILE__)
# println(path)

# MOpt options
opts =Dict(
	"N"=>length(workers()),
	"printlevel"=> 3,
	"filename" => joinpath(path,string("estim_",Dates.today(),".h5")),	
	"save_frequency"=> maxiter < 10 ? 2 : 20,
	"print_level"=> 2,
	"user"=> ENV["USER"],
	"maxiter"=> maxiter,
	"maxtemp"=>10,
	"min_shock_sd"=>0.1,
	"max_shock_sd"=>2,
	"past_iterations"=>30,
	"min_disttol"=>0.05,	# if other chain is within 10% of my value, i consider jumping
	"max_disttol"=>0.1,
	"min_jump_prob"=>0.1,
	"max_jump_prob"=>0.1)


# logdir = isdir(joinpath(path,"logs")) ? joinpath(path,"logs") : mkdir(joinpath(path,"logs"))
logfile = string(splitext(basename(opts["filename"]))[1],".log")
# mig.add_truck(mig.LumberjackTruck(logfile, "info"), "info-logger")
io = open(logfile,"w")
redirect_stdout(io)

MA = MAlgoBGP(mprob,opts)
runMOpt!(MA)
println("quitting cluster")

close(io)


# compute point estimates and SD on coldest chain
# p = MOpt.parameters_ID(MA.MChains,MA.MChains[1].i)
# means = colwise(mean,mig.@select(mig.@where(p, :id .==1 ), MA.params2s_nms))
# sds = colwise(sd,@select(@where(p, :id .==1 ), MA.params2s_nms))

# out = DataFrame(estimate=means,sd=sds)
# println(out)

quit()


