

"""
	Set up cluster and estimate model
"""
function estimate(maxiter::Int,nworkers::Int)

	tic()

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
			using ClusterManagers
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

	# load compute code on all nodes with `using`
	include(joinpath(dir,"../cluster/nodes.jl"))

	# MOpt options
	opts =Dict(
		"N"=>length(workers()),
		"printlevel"=> 3,
		"filename" => joinpath(path,string("estim_",Dates.today(),".h5")),	
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


	logdir = isdir(joinpath(dir,"../cluster/logs/") ? joinpath(dir,"../cluster/logs/") : mkdir(joinpath(dir,"../cluster/logs/"))
	logfile = string(splitext(basename(opts["filename"]))[1],".log")
	if isfile(logfile)
		rm(logfile)
	end

	if !isinteractive()
		io = open(logfile,"w")
		redirect_stdout(w)
	end

	MA = MAlgoBGP(mprob,opts)
	runMOpt!(MA)
	close(io)

	took = toq() / 3600.0  # hours

	# send message to slack channel
	txt = "mig Estimation finished with $maxiter iterations after $took hours."
	TKN = "https://hooks.slack.com/services/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"
	run(`curl -X POST --data-urlencode 'payload={"text": $txt}' $TKN`) 

	# compute point estimates and SD on coldest chain
	# p = MOpt.parameters_ID(MA.MChains,MA.MChains[1].i)
	# means = colwise(mean,mig.@select(mig.@where(p, :id .==1 ), MA.params2s_nms))
	# sds = colwise(sd,@select(@where(p, :id .==1 ), MA.params2s_nms))

	# out = DataFrame(estimate=means,sd=sds)
	# println(out)

	println("quitting cluster")
	quit()

end

function slices(nworkers::Int)

	tic()

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
			using ClusterManagers
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

	logdir = isdir(joinpath(dir,"../cluster/logs/") ? joinpath(dir,"../cluster/logs/") : mkdir(joinpath(dir,"../cluster/logs/"))

	# load compute code on all nodes with `using`
	include(joinpath(dir,"../cluster/nodes.jl"))

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
		redirect_stdout(w)
	end
	sl = MOpt.slices(mprob,length(workers()));
	MOpt.write(sl,joinpath(io["outdir"],"slices.h5"))

	close(io)
	
	took = toq() / 3600.0  # hours

	# send message to slack channel
	txt = "mig slices finished after $took hours."
	TKN = "https://hooks.slack.com/services/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"
	run(`curl -X POST --data-urlencode 'payload={"text": $txt}' $TKN`) 

	# compute point estimates and SD on coldest chain
	# p = MOpt.parameters_ID(MA.MChains,MA.MChains[1].i)
	# means = colwise(mean,mig.@select(mig.@where(p, :id .==1 ), MA.params2s_nms))
	# sds = colwise(sd,@select(@where(p, :id .==1 ), MA.params2s_nms))

	# out = DataFrame(estimate=means,sd=sds)
	# println(out)

	println("quitting cluster")
	quit()

end