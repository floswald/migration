


t0 = time()
# run serial estimation
# called from ../
include("../nodes.jl")

opts =Dict(
	"N"=>nprocs(),
	"printlevel"=> 3,
	"filename" => joinpath(ENV["HOME"],"git/migration/mig/src/cluster",string("estim_",Dates.today(),".h5"),	
	"save_frequency"=> 2,
	"print_level"=> 2,
	"user"=> ENV["USER"],
	"maxiter"=> 5,
	"maxtemp"=>10,
	"min_shock_sd"=>0.1,
	"max_shock_sd"=>2,
	"past_iterations"=>30,
	"min_disttol"=>0.05,	# if other chain is within 10% of my value, i consider jumping
	"max_disttol"=>0.1,
	"min_jump_prob"=>0.1,
	"max_jump_prob"=>0.1)

MA = MAlgoBGP(mprob,opts)
runMOpt!(MA)

# compute point estimates and SD on coldest chain
p = parameters(MA.MChains,1:MA.MChains[1].i)
means = colwise(mean,@select(@where(p, :id .==1 ), MA.params2s_nms)
sds = colwise(sd,@select(@where(p, :id .==1 ), MA.params2s_nms)

out = DataFrame(estimate=means,sd=sds)
println(out)

println("done after $(round((time()-t0)/60)) minutes")

println("quitting cluster")
quit()


