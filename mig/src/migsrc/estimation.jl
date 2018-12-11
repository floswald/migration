

perc_bound(x,p) = [x;x*(1-p);x*(1+p)]

function setup_mprob()
	# this is loaded only on the master
	io = mig.setPaths()
	moms = mig.DataFrame(mig.FileIO.load(joinpath(io["indir"],"moments.rda"))["m"])
	mig.names!(moms,[:name,:value,:weight])
	# subsetting moments
	dont_use= ["lm_w_intercept","move_neg_equity"]
	for iw in moms[:name]
		if contains(iw,"wealth") 
			push!(dont_use,iw)
		end
	end
	use_names = setdiff(moms[:name],dont_use)
	moms_use = moms[findin(moms[:name],use_names) ,:]

	# initial value
	p0 = mig.Param(2)
	setfield!(p0,:MC0, 3.19)
	setfield!(p0,:xi1, 0.008)
	setfield!(p0,:xi2, 0.049)
	pb = Dict{String,Array{Float64}}()
	pb["xi1"]         = [p0.xi1, 0.0,0.02]
	pb["xi2"]         = [p0.xi2, 0.0,0.1]
	pb["omega2"]      = perc_bound(p0.omega2,0.2)
	pb["MC0"]         = perc_bound(p0.MC0, 0.2)
	pb["MC1"]         = [p0.MC1, 0.0,0.04]
	pb["MC2"]         = [p0.MC2, 0.0,0.01]
	pb["MC3"]         = [p0.MC3, 0.0,1]
	pb["MC4"]         = [p0.MC4, 0.01,0.9]
	pb["taudist"]     = [p0.taudist, 0.0,1]
	pb["amenity_ENC"] = [p0.amenity_ENC, 0.01,0.99]
	pb["amenity_ESC"] = [p0.amenity_ESC, 0.01,0.99]
	pb["amenity_MdA"] = [p0.amenity_MdA, 0.01,0.99]
	pb["amenity_Mnt"] = [p0.amenity_Mnt, 0.01,0.99]
	pb["amenity_NwE"] = [p0.amenity_NwE, 0.01,0.99]
	pb["amenity_Pcf"] = [p0.amenity_Pcf, 0.01,0.99]
	pb["amenity_StA"] = [p0.amenity_StA, 0.01,0.99]
	pb["amenity_WNC"] = [p0.amenity_WNC, 0.01,0.99]
	pb["amenity_WSC"] = [p0.amenity_WSC, 0.01,0.99]

	mprob = MomentOpt.MProb() 
	MomentOpt.addSampledParam!(mprob,pb) 
	MomentOpt.addMoment!(mprob,moms_use) 
	MomentOpt.addEvalFunc!(mprob,mig.objfunc)
	return mprob
end

"""
	Set up cluster and estimate model
"""
function estimate(maxiter::Int,nworkers::Int)

	tic()

	# check whether we can post to slack
	post_slack()

	if is_apple()
		if isinteractive()
			# override maxiter
			maxiter=20
		end
	else 
		if length(workers()) == 1
			if isinteractive()
			# override maxiter
				maxiter = 4
				nworkers = 3
			end
			if gethostname() == "hpc-a"
				addprocs_sge(nworkers)
			else
				addprocs(SGEManager(nworkers,""),qsub_env="",res_list="h_vmem=6G,tmem=6G")
			end
		else
			# else we started a cluster with --machinefile
			# cp("zeppos.txt","/share/apps/econ/acapp/floswald/zeppos.txt",remove_destination=true)
		end
	end

	dir = dirname(@__FILE__)	# src/migsrc

	# load moments and get initial parameter value in an mprob object
	mprob = setup_mprob()
	# gradient descent
	# if length(workers()) > 1
	# 	s = MomentOpt.optSlices(mprob,length(workers()),parallel=true,tol=0.01,filename=joinpath(dir,"trace_$(Date(now())).jld2"))
	# else
	# 	s = MomentOpt.optSlices(mprob,10,parallel=true,tol=0.01,filename=joinpath(dir,"trace_$(Date(now())).jld2"))
	# end

	nchains = length(workers())

	# MOpt options
	opts = Dict("N"=>nchains,
        "maxiter"=>maxiter,
        "maxtemp"=> 2,
		"user"=> ENV["USER"],
		"save_frequency"=> maxiter < 10 ? 2 : 5,
		"filename" => joinpath(dir,string("estim_",Dates.today(),".h5")),	
        "smpl_iters"=>1000,
        "parallel"=>true,
        "maxdists"=>[0.05 for i in 1:nchains],
        "acc_tuners"=>[1.0 for i in 1:nchains],
        "animate"=>false)

	MA = MAlgoBGP(mprob,opts)
	runMOpt!(MA)

	took = round(toq() / 3600.0,2)  # hours

	# send message to slack channel
	txt = "[mig] Estimation finished with $maxiter iterations after $took hours on $(gethostname())"
	post_slack(txt)

	# compute point estimates and SD on coldest chain
	out = Dict(:estimates => MomentOpt.median(MA.chains[1]),
			   :CI => MomentOpt.CI(MA.chains[1]))
	println("estimates:")
	print(json(out,4))
    io = mig.setPaths()
    f = open(joinpath(io["out"],"estimates.json"),"w")
    JSON.print(f,out)
    close(f)

	println("quitting cluster")
	quit()

end

function slices(nworkers::Int)

	tic()
	# check whether we can post to slack
	post_slack()

	if is_apple()
		if isinteractive()
			# override maxiter
			maxiter=20
		end
	else 
		if length(workers()) == 1
			if isinteractive()
			# override maxiter
				maxiter = 4
				nworkers = 3
			end
			if gethostname() == "hpc-a"
				addprocs_sge(nworkers)
			else
				addprocs(SGEManager(nworkers,""),qsub_env="",res_list="h_vmem=6G,tmem=6G")
			end
		else
			# else we started a cluster with --machinefile
			# cp("zeppos.txt","/share/apps/econ/acapp/floswald/zeppos.txt",remove_destination=true)
		end
	end

	dir = dirname(@__FILE__)	# src/migsrc

	logdir = isdir(joinpath(dir,"../cluster/logs/")) ? joinpath(dir,"../cluster/logs/") : mkdir(joinpath(dir,"../cluster/logs/"))

	# load moments and get initial parameter value in an mprob object
	mprob = setup_mprob()

	# MOpt options
	opts =Dict(
		"N"=>length(workers()),
		"printlevel"=> 3,
		"filename" => joinpath(logdir,string("estim_",Dates.today(),".h5")),
		"save_frequency"=> maxiter < 10 ? 2 : 5,
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


	logfile = string(splitext(basename(opts["filename"]))[1],".log")
	if isfile(logfile)
		rm(logfile)
	end

	if !isinteractive()
		io = open(logfile,"w")
		redirect_stdout(io)
	end
	sl = MOpt.slices(mprob,length(workers()));
	MOpt.write(sl,joinpath(io["outdir"],"slices.h5"))

	close(io)
	
	took = toq() / 3600.0  # hours

	# send message to slack channel
	txt = "mig Estimation finished with $maxiter iterations after $took hours."
	post_slack(txt)

	# compute point estimates and SD on coldest chain
	# p = MOpt.parameters_ID(MA.MChains,MA.MChains[1].i)
	# means = colwise(mean,mig.@select(mig.@where(p, :id .==1 ), MA.params2s_nms))
	# sds = colwise(sd,@select(@where(p, :id .==1 ), MA.params2s_nms))

	# out = DataFrame(estimate=means,sd=sds)
	# println(out)

	println("quitting cluster")
	quit()

end