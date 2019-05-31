

perc_bound(x,p) = [x;x-p*abs(x);x+p*abs(x)]


"""
	setup_mprob(;keep=[],gamma=false)

* `keep` is a vector of parameter names to keep from all names 
* `gamma=false` says not to include fixed params `gamma` in the set of `sampled_param` by default. 

Sets up an `MomentOpt.MProb` objects for the SMM problem in this model.
It specifies an objecitve function, which moments to use and which parameters to sample.
"""
function setup_mprob(;keep=[],gamma=false)
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
	p0 = mig.Param(2)  # by default `starval=false` initializes at current best guess.
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

	if gamma
		pb["gamma"] = perc_bound(p0.gamma,0.1)
		pb["beta"]  = perc_bound(p0.beta,0.1)
		pb["rho"]   = perc_bound(p0.rho,0.1)
		pb["sigma"] = perc_bound(p0.sigma,0.1)
		pb["phi"]   = perc_bound(p0.phi,0.1)
		pb["chi"]   = perc_bound(p0.chi,0.1)
		pb["R"]     = perc_bound(p0.R,0.1)
		pb["Rm"]    = perc_bound(p0.Rm,0.1)
	end

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
	setup_mprob_ATE()

Sets up an `MomentOpt.MProb` object to obtain the gradient of the ATE statistic
wrt to both theta and gamma (i.e. both sampled and fixed parameters). To this end
we specify the function `exp_Nomove` as the objective function here. We don't supply any moments to match.
"""
function setup_mprob_ATE()

	# initial value
	p0 = mig.Param(2)  # by default `starval=false` initializes at current best guess.
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

	pb["gamma"] = perc_bound(p0.gamma,0.1)
	pb["beta"]  = perc_bound(p0.beta,0.1)
	pb["rho"]   = perc_bound(p0.rho,0.1)
	pb["sigma"] = perc_bound(p0.sigma,0.1)
	pb["phi"]   = perc_bound(p0.phi,0.1)
	pb["chi"]   = perc_bound(p0.chi,0.1)
	pb["R"]     = perc_bound(p0.R,0.1)
	pb["Rm"]    = perc_bound(p0.Rm,0.1)

	mprob = MomentOpt.MProb() 
	MomentOpt.addSampledParam!(mprob,pb) 
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
get Gradient of moment function at theta and gamma.
This produces objects G and D required by Thomas.
"""
function gradMoments()
	tic()
	dir = dirname(@__FILE__)	# src/migsrc
	outd = joinpath(dir,"..","..","out")
	f = joinpath(outd,"current_estim.jld2")
	post_slack()
	x = MomentOpt.load(f)
	thetas = x["dout"][:best][:p]

	gammas = OrderedDict(
				  zip(gamma(), 
		              map(x -> getfield(Param(2),x), gamma() )
		          ) 
			 )

	p = merge(thetas,gammas)

	m = setup_mprob(gamma=true)
	s = MomentOpt.FD_gradient(m,p)  # a (K+L, M) matrix, K = length(thetas), L = length(gammas), M = length(moments)


	open(joinpath(outd,"thomas_W.txt"),"w") do fi 
		writedlm(fi,Diagonal([v[:weight] for (k,v) in m.moments]))
	end


	open(joinpath(outd,"thomas_G.txt"),"w") do fi 
		writedlm(fi,s[1:length(thetas), : ]')
	end

	open(joinpath(outd,"thomas_D.txt"),"w") do fi 
		writedlm(fi,s[(length(thetas)+1) : end, : ]')
	end

	took = round(toq() / 3600.0,2)  # hours
	txt = "[mig] G and D export finished after $took hours on $(gethostname())"
	post_slack(txt)

end


"""
get Gradient of noMove ATE statistic wrt theta and gamma.
This produces objects A and B required by Thomas.
"""
function gradNoMoveATE()
	tic()
	dir = dirname(@__FILE__)	# src/migsrc
	outd = joinpath(dir,"..","..","out")
	f = joinpath(outd,"current_estim.jld2")
	post_slack()
	x = MomentOpt.load(f)
	thetas = x["dout"][:best][:p]

	gammas = OrderedDict(
				  zip(gamma(), 
		              map(x -> getfield(Param(2),x), gamma() )
		          ) 
			 )

	p = merge(thetas,gammas)

	m = setup_mprob_ATE()
	s = try
			gradNoMoveATE_impl(m,p)  # a (K+L, M) matrix, K = length(thetas), L = length(gammas), M = length(moments)
		catch err
			txt = "[mig] error: $err"
			post_slack(txt)
			throw(err)
		end

	open(joinpath(outd,"thomas_B.txt"),"w") do fi 
		writedlm(fi,s[1:length(thetas), : ]')
	end

	open(joinpath(outd,"thomas_A.txt"),"w") do fi 
		writedlm(fi,s[(length(thetas)+1) : end, : ]')
	end

	took = round(toq() / 3600.0,2)  # hours
	txt = "[mig] A and B export finished after $took hours on $(gethostname())"
	post_slack(txt)

end

function gradNoMoveATE_impl(m::MProb,p::Union{Dict,OrderedDict};step_perc=0.01)

	# get g(p)
	x = exp_Nomove(save = false,do_ctax = true)
	gp = x.data[:ctax][1][:data][:ate]   # scalar. index [1] is for aggregate

	D = zeros(length(p))

	# optimal step size depends on range of param bounds
	rs = MomentOpt.range_length(m)

	# compute each partial derivative
	rows = map( [(k,v) for (k,v) in p ] ) do ip 
		k = ip[1]
		v = ip[2]
		h = rs[k] * step_perc
		pp = deepcopy(p)
		pp[k] = v + h 
		info("running exp_Nomove but changing $k from $v to $(pp[k]) by step $h")
		xx = exp_Nomove(p0 = pp, save = false, do_ctax = true)
		Dict(:p => k, :smm => (xx.data[:ctax][1][:data][:ate] - gp) / h)
	end
	d = Dict()
	for e in rows
       d[e[:p]] = e[:smm]
    end
	row = 0
	for (k,v) in d
		row += 1
		D[row] = v
	end
	return D
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
