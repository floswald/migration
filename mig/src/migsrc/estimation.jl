

perc_bound(x,p) = [x;x-p*abs(x);x+p*abs(x)]

function setup_mprob(;keep=[])
	# this is loaded only on the master
	io = mig.setPaths()
	moms = mig.DataFrame(mig.FileIO.load(joinpath(io["indir"],"moments.rda"))["m"])
	mig.names!(moms,[:name,:value,:weight])
	# subsetting moments
	dont_use= ["lm_w_intercept","move_neg_equity","q25_move_distance","q50_move_distance","q75_move_distance","moved1","moved2plus"]
	for iw in moms[:name]
		if contains(iw,"wealth") 
			push!(dont_use,iw)
		end
	end
	use_names = setdiff(moms[:name],dont_use)
	moms_use = moms[findin(moms[:name],use_names) ,:]

	# manually set weights on each moment 
	moms_use[:weight] = 1.0
	weights = Dict(:10 => [:cov_move_h,
						   :mean_move,
						   :mean_move_ownFALSE,
						   :mean_move_ownTRUE,
						   :lm_h_age2],
		           :1.5 => [:flow_move_to_ENC,
		                  :flow_move_to_ESC,
		                  :flow_move_to_MdA,
		                  :flow_move_to_Mnt,
		                  :flow_move_to_NwE,
		                  :flow_move_to_Pcf,
		                  :flow_move_to_StA,
		                  :flow_move_to_WNC,
		                  :flow_move_to_WSC],
		           :2 => [:lm_mv_intercept,
                          :cov_own_kids],
                   :1.4 => :moved0)
	for (k,v) in weights
		moms_use[findin(moms_use[:name],String.(v)),:weight] = 1.0 ./ k 
	end



	# initial value
	p0 = mig.Param(2)
	# setfield!(p0,:MC0, 3.19)
	# setfield!(p0,:xi1, 0.008)
	# setfield!(p0,:xi2, 0.049)
	pb = Dict{String,Array{Float64}}()
	pb["xi1"]         = perc_bound(p0.xi1,0.2)
	pb["xi2"]         = [p0.xi2, 0.0,0.01]
	pb["eta"]         = [p0.eta, 0.0,2.0]
	pb["omega2"]      = perc_bound(p0.omega2,0.2)
	pb["MC0"]         = perc_bound(p0.MC0, 0.2)
	pb["MC1"]         = [p0.MC1, 0.0,0.04]
	pb["MC2"]         = [p0.MC2, 0.0,0.01]
	pb["MC3"]         = [p0.MC3, 0.0,0.4]
	pb["MC4"]         = [p0.MC4, 0.01,0.9]
	pb["taudist"]     = [p0.taudist, 0.0,1]
	pb["amenity_ENC"] = perc_bound(p0.amenity_ENC,0.2)
	pb["amenity_ESC"] = perc_bound(p0.amenity_ESC,0.2)
	pb["amenity_MdA"] = perc_bound(p0.amenity_MdA,0.2)
	pb["amenity_Mnt"] = perc_bound(p0.amenity_Mnt,0.2)
	pb["amenity_NwE"] = perc_bound(p0.amenity_NwE,0.2)
	pb["amenity_Pcf"] = perc_bound(p0.amenity_Pcf,0.2)
	pb["amenity_StA"] = perc_bound(p0.amenity_StA,0.2)
	pb["amenity_WNC"] = perc_bound(p0.amenity_WNC,0.2)
	pb["amenity_WSC"] = perc_bound(p0.amenity_WSC,0.2)

	if length(keep) > 0
		pb0 = similar(pb)
		for i in keep
			pb0[i] = pb[i]
		end
		pb = pb0
	end

	mprob = MomentOpt.MProb() 
	MomentOpt.addSampledParam!(mprob,pb) 
	MomentOpt.addMoment!(mprob,moms_use) 
	MomentOpt.addEvalFunc!(mprob,mig.objfunc)
	return mprob
end

"""
	Set up cluster and estimate model
"""
function estimate(maxit::Int;npoints=nothing,method=:BGP,keep=[])

	tic()

	# check whether we can post to slack
	post_slack()

	dir = dirname(@__FILE__)	# src/migsrc

	# load moments and get initial parameter value in an mprob object
	mprob = setup_mprob(keep=keep)
	# gradient descent
	if method==:grad
		if length(workers()) > 1
			# s = MomentOpt.optSlices(mprob,npoints,parallel=true,tol=0.01,filename=joinpath(dir,"grad_$(Dates.today()).jld2"),update=0.4)   # updates param ranges
			s = MomentOpt.optSlices(mprob,npoints,parallel=true,tol=0.01,filename=joinpath(dir,"grad_$(Dates.today()).jld2"))   # does not update param ranges
		else
			s = MomentOpt.optSlices(mprob,npoints,parallel=false,tol=0.01,filename=joinpath(dir,"grad_$(Dates.today()).jld2"))
		end
		iters = maximum(s[:history][:iter])
		took = round(toq() / 3600.0,2)  # hours

		# send message to slack channel
		txt = "[mig] grad Estimation finished after $iters iterations after $took hours on $(gethostname())"
		post_slack(txt)
	elseif method==:BGP

		nchains = length(workers())

		# MOpt options
		opts = Dict("N"=>nchains,
	        "maxiter"=>maxit,
	        "maxtemp"=> 2,
			"user"=> ENV["USER"],
			"save_frequency"=> maxit < 10 ? 2 : 100,
			"filename" => joinpath(dir,string("estim_",Dates.today(),".jld2")),	
	        "smpl_iters"=>1000,
	        "parallel"=>true,
	        "sigma" => 0.1,
	        "sigma_update_steps"=>maxit+1,  # never adjust variances
	        "maxdists"=>[0.05 for i in 1:nchains],
	        "acc_tuners"=>[2.0 for i in 1:nchains],
	        "animate"=>false)

		MA = MAlgoBGP(mprob,opts)
		runMOpt!(MA)

		took = round(toq() / 3600.0,2)  # hours

		# send message to slack channel
		txt = "[mig] Estimation finished with $maxit iterations after $took hours on $(gethostname())"
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

	else
		warn("only :BGP and :grad implemented")
	end

	println("quitting cluster")
	quit()

end


"""
	get standard errors from a set of estimates
"""
function stdErrors()
	tic()
	dir = dirname(@__FILE__)	# src/migsrc
	outd = joinpath(dir,"..","..","out")
	f = joinpath(outd,"current_estim.jld2")
	post_slack()
	x = MomentOpt.load(f)
	p = x["dout"][:best][:p]
	m = setup_mprob()
	s = MomentOpt.get_stdErrors(m,p,reps=350)

	d = Dict()
	for (k,v) in p 
		d[k] = Dict(:estimate=>v, :se => s[k])
	end
	open(joinpath(outd,"estimates.json"),"w") do fi 
		JSON.print(fi,d)
	end
	took = round(toq() / 3600.0,2)  # hours
	txt = "[mig] standard errors finished after $took hours on $(gethostname())"
	post_slack(txt)

end

"""
	Set up cluster run slices
"""
function slices(npoints::Int,nworkers::Int;keep=[])

	tic()

	# check whether we can post to slack
	post_slack()

	dir = dirname(@__FILE__)	# src/migsrc

	# load moments and get initial parameter value in an mprob object
	mprob = setup_mprob(keep=keep)

	sl = MomentOpt.doSlices(mprob,npoints,nworkers>1);
    io = mig.setPaths()
	MomentOpt.save(sl,joinpath(io["outdir"],"slices.jld2"))

	took = round(toq() / 3600.0,2)  # hours

	# send message to slack channel
	txt = "[mig] Slices finished after $took hours on $(gethostname())"

	# println("quitting cluster")
	# post_slack(txt)
	# quit()
	return sl

end
