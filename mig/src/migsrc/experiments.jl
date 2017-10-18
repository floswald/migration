


# Decompose Moving Cost of Owners
# ===============================

function decompose_MC_owners()

	o = runObj()
	s0 = o.simMoments

	# no owner MC
    # alpha_3 = 0
    p1 = Dict(:MC3 => 0.0)
	o1 = runObj(p1)
	s1 = o1.simMoments

	# no owner MgC and no transaction cost :
    # alpha_3 = 0, phi = 0
    p1[:phi] = 0.0
	o2 = runObj(p1)
	s2 = o2.simMoments

	# no transaction cost: phi = 0
    p1 = Dict(:phi => 0.0)
	o3 = runObj(p1)
	s3 = o3.simMoments

    pfun(x,y) = 100 * (x-y) / y

    d = Dict()
    d["own"] = Dict("base" => s0[:mean_own], "alpha" => s1[:mean_own], "phi" => s3[:mean_own], "alpha_phi" => s2[:mean_own])

    d["move"] = Dict("base" => pfun(s0[:mean_move],s0[:mean_move]), "alpha" => pfun(s1[:mean_move],s0[:mean_move]), "phi" => pfun(s3[:mean_move],s0[:mean_move]), "alpha_phi" => pfun(s2[:mean_move],s0[:mean_move]))

    d["move_rent"] = Dict("base" => pfun(s0[:mean_move_ownFALSE],s0[:mean_move_ownFALSE]), "alpha" => pfun(s1[:mean_move_ownFALSE],s0[:mean_move_ownFALSE]), "phi" => pfun(s3[:mean_move_ownFALSE],s0[:mean_move_ownFALSE]), "alpha_phi" => pfun(s2[:mean_move_ownFALSE],s0[:mean_move_ownFALSE]))
   
    d["move_own"] = Dict("base" => pfun(s0[:mean_move_ownTRUE],s0[:mean_move_ownTRUE]), "alpha" => pfun(s1[:mean_move_ownTRUE],s0[:mean_move_ownTRUE]), "phi" => pfun(s3[:mean_move_ownTRUE],s0[:mean_move_ownTRUE]), "alpha_phi" => pfun(s2[:mean_move_ownTRUE],s0[:mean_move_ownTRUE]))

    io = mig.setPaths()
    f = open(joinpath(io["outdir"],"decompose_MC_owners.json"),"w")
    JSON.print(f,d)
    close(f)

	return(d)
end


# VALUE OF MIGRATION
# ==================

# function sim_expost_value(m::Model,p::Param,j::Int,base_move::Bool)
# 	cutyr = 1997 - 1
# 	solve!(m,p)
# 	base = simulate(m,p);
# 	base = base[!isna(base[:cohort]),:];
# 	if base_move
# 		mv_id = @select(@where(base,(:year.>cutyr)&(:move)&(:j.==j)),id=unique(:id))
# 		base = base[findin(base[:id],mv_id[:id]),:]
# 		# do NOT condition on their tenure in j only, but entire lifecycle
# 		w0   = @linq base|>
# 			   @where((:j.==j)&(:year.>cutyr)) |>
# 			   @select(v = mean(:maxv),u=mean(:utility))
# 		return (w0,mv_id)
# 	else
# 		w0   = @linq base|>
# 			   @where((:j.==j)&(:year.>cutyr)) |>
# 			   @select(v = mean(:maxv),u=mean(:utility))
# 		return (w0,DataFrame())
#     end
# end
function sim_expost_value(m::Model,p::Param,j::Int,mv_id::Vector{Int})
	cutyr = 1997 - 1
	solve!(m,p)
	base = simulate(m,p);
	base = base[!isna(base[:cohort]),:];
	if length(mv_id)>0
		base = base[findin(base[:id],mv_id),:]
		# do NOT condition on their tenure in j only, but entire lifecycle
		w0   = @linq base|>
			   @where((:j.==j)&(:year.>cutyr)) |>
			   @select(v = mean(:maxv),u=mean(:utility))
	else
		w0   = @linq base|>
			   @where((:j.==j)&(:year.>cutyr)) |>
			   @select(v = mean(:maxv),u=mean(:utility))
    end
    return w0
end

# find ctax of baseline vs highMC
function find_ctax_value_mig_base(j::Int,mv_id::Vector{Int})

	# baseline model
	p = Param(2)
	m = Model(p)

	w0 = sim_expost_value(m,p,j,mv_id)

	ctax = optimize((x)->vdiff_value_mig_base(x,w0[:v][1],j,mv_id),0.5,1.5,show_trace=true,method=Brent(),abs_tol=1e-10)

end



function vdiff_value_mig_base(ctax::Float64,w0::Float64,j::Int,mv_id::Vector{Int})

	info("current ctax = $ctax")

	# model where moving is shut down in region j
	opts = Dict("policy" => "highMC", "shockRegion" => j)
	p2 = Param(2,opts)
	setfield!(p2,:ctax,ctax)
	m2 = Model(p2)

	w1 = sim_expost_value(m2,p2,j,mv_id)

	(w1[:v][1] - w0)^2
end

function exp_highMC(;pshock=1.0,yshock=1.0)

	# look at results after full cohorts available
	cutyr = 1997 - 1

	# baseline model

	p = Param(2)

	m = Model(p)
	solve!(m,p)
	# dimvec2 = (ns, ny, np, nz, ntau,  na, nh, nJ, nt-1 )
	EV0 = mean(m.EV[1,2,2,2,1,m.aone,1,:,2])
	basel = simulate(m,p);
	basel = basel[.!isna.(basel[:cohort]),:];

	if (pshock == 1) & (yshock == 1)
		info("noMove in ALL regions")
		opts = Dict("policy" => "noMove")
	elseif (yshock == 1)
		info("noMove in ALL regions with pshock=$(pshock)")
		opts = Dict("policy" => "noMove", "shockVal_p" => [pshock for i in 1:p.nt-1])

	else
		info("noMove in ALL regions with pshock=$(pshock),yshock=$(yshock)")
		opts = Dict("policy" => "noMove", "shockVal_p" => [pshock for i in 1:p.nt-1]"shockVal_y" => [pshock for i in 1:p.nt-1])

	end
	p2 = Param(2,opts=opts)
	m2 = Model(p2)
	solve!(m2,p2)
	EV1 = mean(m.EV[1,2,2,2,1,m.aone,1,:,2])

	pol = simulate(m2,p2);
	pol = pol[.!isna.(pol[:cohort]),:];

	return Dict(:base=>basel,:pol=>pol,:EV0=>EV0,:EV1=>EV1,:perc=>100.0*(EV1.-EV0)./abs(EV0))

end


"""
	exp_value_mig_base(;ctax::Bool=false,save::Bool=false)

compares baseline with noMove scenario. returns differences in utility and other oucomes if moving is shut down everywhere.

## keywords

* `do_ctax`: compute compensating consumption tax
* `save`: save results to disk
* `ys`: multiplicative *yshock* to be applied to the counterfactual
* `ps`: multiplicative *pshock* to be applied to the counterfactual
"""
function exp_value_mig_base(;do_ctax::Bool=false,save::Bool=false,ys::Float64=1.0,ps::Float64=1.0)

	bp = exp_highMC(yshock=ys,pshock=ps)
	base = bp[:base]
	pol = bp[:pol]
	p = Param(2)

	# look at results after full cohorts available
	cutyr = 1997 - 1
	
	ate_0 = @linq base |>
		    @where((:year.>cutyr)) |>
			@select(v=mean(:maxv),u=mean(:utility[isfinite(:utility)]),inc = mean(:income),a=mean(:a),h=mean(:h),w=mean(:wealth),y=mean(:y),p=mean(:p))
	ate_1 = @linq pol |>
		    @where((:year.>cutyr)) |>
			@select(v=mean(:maxv),u=mean(:utility[isfinite(:utility)]),inc = mean(:income),a=mean(:a),h=mean(:h),w=mean(:wealth),y=mean(:y),p=mean(:p))
	ate = copy(ate_1 .- ate_0 )
	ate_perc = convert(Dict,100.0 * (ate ./ abs(ate_0)))

	age_ate_0 = @linq base |>
		    @where((:year.>cutyr)) |>
			@by(:realage,v=mean(:maxv),u=mean(:utility[isfinite(:utility)]),inc = mean(:income),a=mean(:a),h=mean(:h),w=mean(:wealth),y=mean(:y),p=mean(:p))
	age_ate_1 = @linq pol |>
		    @where((:year.>cutyr)) |>
			@by(:realage,v=mean(:maxv),u=mean(:utility[isfinite(:utility)]),inc = mean(:income),a=mean(:a),h=mean(:h),w=mean(:wealth),y=mean(:y),p=mean(:p))
	own30_0 = @linq base |>
	    @where((:year.>cutyr)) |>
		@by(:own_30,v=mean(:maxv),
					 u=mean(:utility[isfinite(:utility)]),
					 inc = mean(:income),
					 a=mean(:a),
					 h=mean(:h),
					 w=mean(:wealth),
					 y=mean(:y),
					 p=mean(:p))
	own30_1 = @linq pol |>
	    @where((:year.>cutyr)) |>
		@by(:own_30,v=mean(:maxv),
					 u=mean(:utility[isfinite(:utility)]),
					 inc = mean(:income),
					 a=mean(:a),
					 h=mean(:h),
					 w=mean(:wealth),
					 y=mean(:y),
					 p=mean(:p))
	year_0 = @linq base |>
	    @where((:year.>cutyr)) |>
		@by(:year,v=mean(:maxv),
					 u=mean(:utility[isfinite(:utility)]),
					 inc = mean(:income),
					 a=mean(:a),
					 h=mean(:h),
					 w=mean(:wealth),
					 y=mean(:y),
					 p=mean(:p))
	year_1 = @linq pol |>
	    @where((:year.>cutyr)) |>
		@by(:year,v=mean(:maxv),
					 u=mean(:utility[isfinite(:utility)]),
					 inc = mean(:income),
					 a=mean(:a),
					 h=mean(:h),
					 w=mean(:wealth),
					 y=mean(:y),
					 p=mean(:p))
	loc_0 = @linq base |>
		    @where((:year.>cutyr)) |>
			@by(:j,v=mean(:maxv),
						 u=mean(:utility[isfinite(:utility)]),
						 inc = mean(:income),
						 a=mean(:a),
						 h=mean(:h),
						 w=mean(:wealth),
						 y=mean(:y),
						 p=mean(:p))
	loc_1 = @linq pol |>
		    @where((:year.>cutyr)) |>
			@by(:j,v=mean(:maxv),
						 u=mean(:utility[isfinite(:utility)]),
						 inc = mean(:income),
						 a=mean(:a),
						 h=mean(:h),
						 w=mean(:wealth),
						 y=mean(:y),
						 p=mean(:p))

	age_ate_perc = convert(Dict,100.0 * (age_ate_1 .- age_ate_0)./abs(age_ate_0))
	age_ate_perc = pdiff(convert(Dict,age_ate_1,:realage),convert(Dict,age_ate_0,:realage))
	loc_perc     = pdiff(convert(Dict,loc_1,:j),convert(Dict,loc_0,:j))
	own30_perc   = pdiff(convert(Dict,own30_1,:own_30),convert(Dict,own30_0,:own_30))
	year_perc    = pdiff(convert(Dict,year_1,:year),convert(Dict,year_0,:year))


	# compare the ones who did move with their virtual counterparts
	# =============================================================


	# number of moves 
	
	mv_count = @linq base |>
	        @where((:tau.==1)) |>
	        @by(:id, n_moves = sum(:move), n_moveto = sum(:moveto.!=:j))

	never_id = @linq mv_count |>
	        @where(:n_moves.==0 ) |>
	        @select(id=unique(:id))
	once_id = @linq mv_count |>
	        @where(:n_moves.>0 ) |>
	        @select(id=unique(:id))

	# people are mover type and stay till end of life. stayers.
	stay_id = base[findin(base[:id],never_id[:id]),:]

	# people who were born in j and move away
	away_id = once_id

	young_id = @select(@where(base,(:year.>cutyr).&(:age.<p.nt/2)),id=unique(:id))
	old_id = @select(@where(base,(:year.>cutyr).&(:age.>=p.nt/2)),id=unique(:id))
	mv_id_owners = @select(@where(base,(:year.>cutyr).&(:move).&(:own)),id=unique(:id))
	mv_id_renters= @select(@where(base,(:year.>cutyr).&(:move).&(!(:own))),id=unique(:id))
	# these people are "treated"

	# get a dict with percentage changes for movers, movers|rent and movers|own
	atts = Dict()
	for (k,v) in zip(("att","atn","att_young","att_old"),(away_id,stay_id,young_id,old_id))
		# subsetting
		bmv = 	base[findin(base[:id],v[:id]),:]
		bmv2 = @where(bmv,:year.>cutyr)
		pmv = pol[findin(pol[:id],v[:id]),:]
		pmv2 = @where(pmv,:year.>cutyr)
		# computing effects
		att_0 = @select(bmv2,v=mean(:maxv),u=mean(:utility),inc = mean(:income),a=mean(:a),h=mean(:h),w=mean(:wealth),y=mean(:y),p=mean(:p))
		att_1 = @select(pmv2,v=mean(:maxv),u=mean(:utility),inc = mean(:income),a=mean(:a),h=mean(:h),w=mean(:wealth),y=mean(:y),p=mean(:p))
		att = att_1 .- att_0 
		atts[k] = convert(Dict,100.0 * (att ./ abs(att_0)))
	end

	# preparing io.
	io = mig.setPaths()

	ystr=""
	pstr=""
	if ys > 1
		ystr = "hi"
	elseif ys<1
		ystr = "lo"
	end
	if ps > 1
		pstr = "hi"
	elseif ps<1
		pstr = "lo"
	end
	ostr = string("noMove_ys",ystr,"_ps",pstr,".json")
	path = joinpath(io["outdir"],ostr)
	f = open(joinpath(io["outdir"],ostr),"a+")
	js = 0:p.nJ
	ctax = Dict()
	for ij in js
		if do_ctax 
			Base.info("doing ctax for region $ij")
			ctax[ij] = Dict()
			if ij==0
				# don't subset to any region: compute aggregate impact
				# all
				x=mig.ctaxxer("noMove",:realage,t->t.==t)
				ctax[ij][:ate] = Optim.minimizer(x)
				# young
				x=mig.ctaxxer("noMove",:realage,t->t.<31)
				ctax[ij][:young]=Optim.minimizer(x)
				# young
				x=mig.ctaxxer("noMove",:realage,t->t.>30)
				ctax[ij][:old]=Optim.minimizer(x)
				# for those who did not own a house when age == 30
				x=mig.ctaxxer("noMove",:rent_30,t->t)
				ctax[ij][:rent_30]=Optim.minimizer(x)
				# for those who did own a house when age == 30
				x=mig.ctaxxer("noMove",:own_30,t->t)
				ctax[ij][:own_30]=Optim.minimizer(x)
				gc()
			else
				# do subset to region j
				# all
				x=mig.ctaxxer("noMove",:j,t->t.==ij)
				ctax[ij][:ate] = Optim.minimizer(x)
				# young
				x=mig.ctaxxer("noMove",:realage,t->t.<31,:j,t->t.==ij)
				ctax[ij][:young]=Optim.minimizer(x)
				# young
				x=mig.ctaxxer("noMove",:realage,t->t.>30,:j,t->t.==ij)
				ctax[ij][:old]=Optim.minimizer(x)
				# for those who did not own a house when age == 30
				x=mig.ctaxxer("noMove",:rent_30,t->t,:j,t->t.==ij)
				ctax[ij][:rent_30]=Optim.minimizer(x)
				# for those who did own a house when age == 30
				x=mig.ctaxxer("noMove",:own_30,t->t,:j,t->t.==ij)
				ctax[ij][:own_30]=Optim.minimizer(x)
				gc()
			end
		else
			# read from file
			ctax = JSON.parse(f)
		end
		close(f)
	end

	# merge all ATE/ATT perc dicts
	ate_att = Dict()
	for (k,v) in ate_perc
		ate_att[k] = Dict(:ate=>v,:att=>atts["att"][k],:atn=>atts["atn"][k],:att_young=>atts["att_young"][k],:att_old=>atts["att_old"][k])
	end
	# add constaxes
	ate_att[:ctax] = ctax


	# output
	# ======

	d = Dict(
		:EV_perc => bp[:perc][1],
		:ate => convert(Dict,ate),
		:age_ate_perc => age_ate_perc,
		:loc_perc => loc_perc,
		:own30_perc => own30_perc,
		:year_perc => year_perc,
		:ate_perc => ate_perc,
		:att_perc => atts["att"],
		:ate_att => ate_att)

	rm(joinpath(io["outdir"],ostr),force=true)
	if save
		f = open(joinpath(io["outdir"],ostr),"w")
		JSON.print(f,d)
		close(f)
	end
	return d
end


# simulation with subsetting to a certain group
# mig.ctaxxer("noMove",:y,t->t.==t,by=:j) subsets nothing in addition to year=>1996 but 
# this cannot work
# function ctaxxer(pol::String,var::Symbol,sel_func;kw...)
# 	s = runSim()
# 	if length(kw) > 0
# 		# could add
# 		# if any([x[1]==:by for x in kw])
# 		val = @linq s |>
# 			@where((:year.>1996) .& sel_func(_I_(var))) |>
# 			@by(kw[1][1],v=mean(:maxv),u=mean(:utility))
# 		v0 = val[:v]

# 		function ftau!(ctau::Float64,fvec,v0::Vector{Float64},pol::String)
# 			si = runSim(opt=Dict(:policy=>pol,:ctax=>ctau))
# 			val = @linq s |>
# 				@where((:year.>1996) .& sel_func(_I_(var))) |>
# 				@by(kw[1][1],v=mean(:maxv),u=mean(:utility))
# 			v1 = val[:v]
# 			fvec[:] = (v0 .- v1).^2
# 		end
# 		# ctax = optimize((x) -> ftau(x,v0,pol,at_idx,sol),0.5,2.0, show_trace=true,iterations=10)
# 		ctax = NLsolve.nlsolve((x,xvec)->ftau!(x,xvec,v0.data,pol),1.0)
# 		return ctax

# 	else
# 		val = @linq s |>
# 			@where((:year.>1996) .& sel_func(_I_(var))) |>
# 			@select(v=mean(:maxv),u=mean(:utility))
# 		v0 = val[:v][1]

# 		function ftau(ctau::Float64,v0::Float64,pol::String)
# 			si = runSim(opt=Dict(:policy=>pol,:ctax=>ctau))
# 			val = @linq si |>
# 				@where((:year.>1996) .& sel_func(_I_(var))) |>
# 				@select(v=mean(:maxv),u=mean(:utility))
# 			v1 = val[:v][1]
# 			return (v0 - v1)^2
# 		end
# 		ctax = optimize((x) -> ftau(x,v0,pol),0.5,2.0, show_trace=true,iterations=10)
# 		return ctax
# 	end
# end

# simulation with subsetting to a certain group
# mig.ctaxxer("noMove",:y,t->t.==t) subsets nothing in addition to year=>1996 and finds ctax
# mig.ctaxxer("noMove",:j,t->t.==4) subsets in addition to year=>1996 that :j==4
# mig.ctaxxer("noMove",:own_30,t->t) subsets in addition to year=>1996 that :own_30 is true
function ctaxxer(pol::String,var::Symbol,sel_func)
	info("finding consumtion tax for $pol policy. subsetting $var")
	s = runSim()
	val = @linq s |>
		@where((:year.>1996) .& sel_func(_I_(var))) |>
		@select(v=mean(:maxv),u=mean(:utility))
	v0 = val[:v][1]

	function ftau(ctau::Float64,v0::Float64,pol::String)
		si = runSim(opt=Dict(:policy=>pol,:ctax=>ctau))
		val = @linq si |>
			@where((:year.>1996) .& sel_func(_I_(var))) |>
			@select(v=mean(:maxv),u=mean(:utility))
		v1 = val[:v][1]
		return (v0 - v1)^2
	end
	ctax = optimize((x) -> ftau(x,v0,pol),0.5,2.0, show_trace=true,iterations=12)
	return ctax
end
function ctaxxer(pol::String,var1::Symbol,sel_func1,var2::Symbol,sel_func2)
	info("finding consumtion tax for $pol policy. subsetting $var1 and $var2")
	s = runSim()
	val = @linq s |>
		@where((:year.>1996) .& sel_func1(_I_(var1)) .& sel_func2(_I_(var2))) |>
		@select(v=mean(:maxv),u=mean(:utility))
	v0 = val[:v][1]

	function ftau(ctau::Float64,v0::Float64,pol::String)
		si = runSim(opt=Dict(:policy=>pol,:ctax=>ctau))
		val = @linq si |>
			@where((:year.>1996) .& sel_func1(_I_(var1)) .& sel_func2(_I_(var2))) |>
			@select(v=mean(:maxv),u=mean(:utility))
		v1 = val[:v][1]
		return (v0 - v1)^2
	end
	ctax = optimize((x) -> ftau(x,v0,pol),0.5,2.0, show_trace=true,iterations=12)
	return ctax
end

# y = DataFrame(year=repeat(1995:2000,inner=[2],outer=[1]),j = repeat(1:2,inner=[1],outer=[6]),v = rand(12))

# f = function(y,var::Symbol,sel_f)
#    @where(y,sel_f(_I_(var)))
# end
# g = t -> (t .== 2)
# f(y,:j,g)


# f = function(y,var::Symbol,sel_f,var2::Symbol,sel_f2)
#        @where(y,sel_f(_I_(var)) & sel_f2(_I_(var2)))
#        end
# f(y,:j,g,:year,g2)

# 



# """
# 	ctaxxer(pol::String;sol=true,ia=11,is=2,iz=2,iy=2,ip=1,ih=1,itau=1,ij=7,it=2,ik=7)

# computes the implied consumption tax for a given policy from model solution. This is a number τ by which optimal consumption is changed at each state. That is, if `v0` is the value of the baseline scenario, and v1 that of `pol`, then τ is such that `v1(c(τ)) = v0`.

# ### Keyword args

# * `sol`: true if measure value at a certain state of the DP solution. false if measured from average v of simulation
# * `ix`: states where to measure the value function.
# """
# function ctaxxer(pol::String;sol=true,ia=11,is=2,iz=2,iy=2,ip=1,ih=1,itau=1,ij=7,it=2,ik=7)
# 	at_idx = (ik,is,iz,iy,ip,itau,ia,ih,ij,it)
# 	# get baseline value 
# 	if sol
# 		val = runSol()
# 		v0 = val.v[at_idx...]
# 	else
# 		# simulate
# 		s = runSim()
# 		val = @linq s |>
# 			@where(:year.>1996) |>
# 			@select(v=mean(:maxv),u=mean(:utility))
# 		v0 = val[:v][1]
# 	end

# 	# dimvec  = (nJ, ns, nz, ny, np, ntau, na, nh,  nJ, nt-1 )

# 	function ftau(ctau::Float64,v0::Float64,pol::String,at::Tuple,sol::Bool)
# 		if sol
# 			so = runSol(opt=Dict(:policy=>pol,:ctax=>ctau))
# 			v1 = so.v[at...]
# 		else
# 			si = runSim(opt=Dict(:policy=>pol,:ctax=>ctau))
# 			val = @linq si |>
# 				@where(:year.>1996) |>
# 				@select(v=mean(:maxv),u=mean(:utility))
# 			v1 = val[:v][1]
# 		end
# 		return (v0 - v1)^2
# 	end
# 	ctax = optimize((x) -> ftau(x,v0,pol,at_idx,sol),0.5,2.0, show_trace=true,iterations=10)
# 	return ctax
# end


# get the consumption subsidy that makes
# you indifferent from living through the shock 
# in j with a policy applied (i.e. no moving, no saving, no)
function exp_shockRegion_vdiff(which_base::AbstractString,which_pol::AbstractString)

	# w0 = value of living in shock region from shockYear forward
	# w1 = value of living in shock region from shockYear forward UNDER POLICY

	# w0 - w1 is diff in value from policy

	# baseline: a shock to p in 2007 in region 6.
	p = Param(2)
	opts = selectPolicy(which_base,6,2007,p)

	e = exp_shockRegion(opts);
	w0 = e[1]["values"][which_base][1][1]

	e = 0
	gc()

	# the policy
	opts["policy"] = which_pol
	ctax = optimize((x)->valdiff_shockRegion(x,w0,opts),0.5,2.0,show_trace=true,method=Brent(),iterations=10)
	return ctax

end

function valdiff_shockRegion(ctax::Float64,v0::Float64,opts::Dict)

	# change value of ctax on options dict
	opts["ctax"] = ctax
		println("current ctax level = $ctax")

	# and recompute
	e = exp_shockRegion(opts);
	w = e[1]["values"][opts["policy"]][1][1]
	e = 0
	gc()

	println("baseline value is $(round(v0,2))")
	println("current value from $(opts["policy"]) is $(round(w,2))")
	println("current difference is $(w - v0)")

	return (w - v0).^2
end


# shocking a given region shockReg in a given year shockYear
# 
# this requires to compute the solution for all cohorts alive in shockYear
# this in turn requires the solution to be computed T times.
# The particular difference for each solution is that the mapping p_j = g(Y,P,j) changes
# at the date at which the cohort hit shockYear. This means that suddenly in shockYear agents realize their mapping is no longer valid and they adjust to the new reality. 
# the shock never reverts back, i.e. if the region is hit, it is depressed forever.
# the value and policy functions for t<1997 must be replaced by the ones
# from the standard solution, since otherwise agents will expect the shock
# then simulate as usual and pick up the behaviour in j around 1997
# and compare to behaviour in the non-shocked version.
function exp_shockRegion(opts::Dict)

	j         = opts["shockReg"]
	which     = opts["policy"]
	shockYear = opts["shockYear"]

	if shockYear<1998
		throw(ArgumentError("must choose years after 1997. only then full cohorts available"))
	end

	# Baseline
	# --------

	# note: we must know the baseline model in any case.
	# this is because policy functions of agents in years
	# BEFORE the shock need to be adjusted to be equal to the baseline ones.
	p = Param(2)
	m = Model(p)
	solve!(m,p)
	sim0 = simulate(m,p)
	sim0 = sim0[!isna(sim0[:cohort]),:]

	# Policy
	# ------
	
	# compute behaviour for all individuals, assuming each time the shock
	# hits at a different age. selecting the right cohort will then imply
	# that the shock hits you in a given year.
	ss = pmap(x -> computeShockAge(m,opts,x),1:p.nt-1)		

	# remove worker processes now.
	if length(workers()) > 1
		rmprocs(workers())
	end

	# stack dataframes
	# 
	df1 = ss[1]
	for i in 2:length(ss)
		df1 = vcat(df1,ss[i])
		ss[i] = 0
		gc()
	end
	df1 =  df1[!isna(df1[:cohort]),:]
	maxc = maximum(df1[:cohort])
	minc = minimum(df1[:cohort])

	if minc > 1
		# add all cohorts that were not simulated in computeShockAge
		df1 = vcat(df1,@where(sim0,:cohort.<minc))
	end


	# compute behaviour of all born into post shock world
	if get(opts,"verbose",0) > 0
		println("computing behaviour for post shock cohorts")
	end

	if which=="p3" || which == "y3"|| which == "ypshock3"
		# assume shock goes away immediately and all behave as in baseline
		sim2 = @where(sim0,:cohort.>maxc)
	else
		# assume shock stays forever
		opts["shockAge"] = 1
		p1 = Param(2,opts)
		setfield!(p1,:ctax,get(opts,"ctax",1.0))	# set the consumption tax, if there is one in opts
		mm = Model(p1)
		solve!(mm,p1)
		sim2 = simulate(mm,p1)
		sim2 = sim2[!isna(sim2[:cohort]),:]
		mm = 0
		gc()
		# keep only guys born after shockYear
		sim2 = @where(sim2,:cohort.>maxc)
	end

	# stack
	sim1 = vcat(df1,sim2)
	df1 = 0
	sim2 = 0
	gc()

	# compute summaries
	# =================

	# get discounted lifetime utility of people in j from shockYear forward
	# ----------------------------------------------
	w0   = getDiscountedValue(@where(sim0,(:j.==j)&(:year.>=shockYear)),p,m,true)
	mms0 = computeMoments(sim0,p)	

	w1 = getDiscountedValue(@where(sim1,(:j.==j)&(:year.>=shockYear)),p,m,true)
	mms1 = computeMoments(sim1,p)	


	# get flows for each region
	d = Dict{AbstractString,DataFrame}()
	d["base"] = sim0
	d[which] = sim1
	flows = getFlowStats(d,false,"$(which)_$j")

	# calculate an elasticity of out and inflows
	# -------------------------------------------

	ela = join(flows["base"][j][[:All,:Owners,:Renters,:Net,:Total_in_all,:Total_out_all,:Net_own,:Own_in_all,:Own_out_all,:Net_rent,:Rent_in_all,:Rent_out_all,:out_rent,:out_buy,:in_rent,:in_buy,:year]],flows[which][j][[:All,:Owners,:Renters,:Net,:Total_in_all,:Total_out_all,:Net_own,:Own_in_all,:Own_out_all,:Net_rent,:Rent_in_all,:Rent_out_all,:out_rent,:out_buy,:in_rent,:in_buy,:year]],on=:year)

	# for kennan figure 1
	ela1 = @linq ela |>
			@transform(d_all = (:All_1 - :All) ./ :All, d_own = (:Owners_1 - :Owners)./:Owners, d_rent = (:Renters_1 - :Renters)./:Renters,d_net_own=(:Net_own_1 - :Net_own)./ :Net_own,d_net_rent=(:Net_rent_1 - :Net_rent)./ :Net_rent,d_in_rent = (:in_rent_1 - :in_rent)./:in_rent,d_in_buy = (:in_buy_1 - :in_buy)./:in_buy,d_out_rent = (:out_rent_1 - :out_rent)./:out_rent,d_out_buy = (:out_buy_1 - :out_buy)./:out_buy,year=:year) 
			
	ela1[:pshock] = 0.0
	ela1[:yshock] = 0.0

	shockyrs = sum(ela1[:year] .>= opts["shockYear"])

	# println("shockVal_y = $(opts["shockVal_y"])")
	# println("shockVal_p = $(opts["shockVal_p"])")

	ela1[ela1[:year] .>= opts["shockYear"], :yshock] = (1-opts["shockVal_y"][1:shockyrs])
	ela1[ela1[:year] .>= opts["shockYear"], :pshock] = (1-opts["shockVal_p"][1:shockyrs])

	# println(ela1[[:year,:yshock,:pshock]])
	ela1[:d_all_p] = 0.0
	ela1[:d_own_p] = 0.0
	ela1[:d_net_own_p] = 0.0
	ela1[:d_rent_p] = 0.0
	ela1[:d_net_rent_p] = 0.0
	ela1[:d_all_y] = 0.0
	ela1[:d_own_y] = 0.0
	ela1[:d_net_own_y] = 0.0
	ela1[:d_rent_y] = 0.0
	ela1[:d_net_rent_y] = 0.0
	ela1[:d_out_buy_y] = 0.0
	ela1[:d_out_buy_p] = 0.0
	ela1[:d_in_buy_y] = 0.0
	ela1[:d_in_buy_p] = 0.0
	ela1[:d_out_rent_y] = 0.0
	ela1[:d_out_rent_p] = 0.0
	ela1[:d_in_rent_y] = 0.0
	ela1[:d_in_rent_p] = 0.0

	ela1[ela1[:pshock].!= 0.0, :d_all_p] = ela1[ela1[:pshock].!= 0.0, :d_all] ./ ela1[ela1[:pshock].!= 0.0, :pshock]
	ela1[ela1[:pshock].!= 0.0, :d_own_p] = ela1[ela1[:pshock].!= 0.0, :d_own] ./ ela1[ela1[:pshock].!= 0.0, :pshock]
	ela1[ela1[:pshock].!= 0.0, :d_rent_p] = ela1[ela1[:pshock].!= 0.0, :d_rent] ./ ela1[ela1[:pshock].!= 0.0, :pshock]
	ela1[ela1[:pshock].!= 0.0, :d_net_own_p] = ela1[ela1[:pshock].!= 0.0, :d_net_own] ./ ela1[ela1[:pshock].!= 0.0, :pshock]
	ela1[ela1[:pshock].!= 0.0, :d_net_rent_p] = ela1[ela1[:pshock].!= 0.0, :d_net_rent] ./ ela1[ela1[:pshock].!= 0.0, :pshock]
	ela1[ela1[:pshock].!= 0.0, :d_out_buy_p] = ela1[ela1[:pshock].!= 0.0, :d_out_buy] ./ ela1[ela1[:pshock].!= 0.0, :pshock]
	ela1[ela1[:pshock].!= 0.0, :d_in_buy_p] = ela1[ela1[:pshock].!= 0.0, :d_in_buy] ./ ela1[ela1[:pshock].!= 0.0, :pshock]
	ela1[ela1[:pshock].!= 0.0, :d_out_rent_p] = ela1[ela1[:pshock].!= 0.0, :d_out_rent] ./ ela1[ela1[:pshock].!= 0.0, :pshock]
	ela1[ela1[:pshock].!= 0.0, :d_in_rent_p] = ela1[ela1[:pshock].!= 0.0, :d_in_rent] ./ ela1[ela1[:pshock].!= 0.0, :pshock]

	ela1[ela1[:yshock].!= 0.0, :d_all_y] = ela1[ela1[:yshock].!= 0.0, :d_all] ./ ela1[ela1[:yshock].!= 0.0, :yshock]
	ela1[ela1[:yshock].!= 0.0, :d_own_y] = ela1[ela1[:yshock].!= 0.0, :d_own] ./ ela1[ela1[:yshock].!= 0.0, :yshock]
	ela1[ela1[:yshock].!= 0.0, :d_rent_y] = ela1[ela1[:yshock].!= 0.0, :d_rent] ./ ela1[ela1[:yshock].!= 0.0, :yshock]
	ela1[ela1[:yshock].!= 0.0, :d_net_own_y] = ela1[ela1[:yshock].!= 0.0, :d_net_own] ./ ela1[ela1[:yshock].!= 0.0, :yshock]
	ela1[ela1[:yshock].!= 0.0, :d_net_rent_y] = ela1[ela1[:yshock].!= 0.0, :d_net_rent] ./ ela1[ela1[:yshock].!= 0.0, :yshock]
	ela1[ela1[:yshock].!= 0.0, :d_out_buy_y] = ela1[ela1[:yshock].!= 0.0, :d_out_buy] ./ ela1[ela1[:yshock].!= 0.0, :yshock]
	ela1[ela1[:yshock].!= 0.0, :d_in_buy_y] = ela1[ela1[:yshock].!= 0.0, :d_in_buy] ./ ela1[ela1[:yshock].!= 0.0, :yshock]
	ela1[ela1[:yshock].!= 0.0, :d_out_rent_y] = ela1[ela1[:yshock].!= 0.0, :d_out_rent] ./ ela1[ela1[:yshock].!= 0.0, :yshock]
	ela1[ela1[:yshock].!= 0.0, :d_in_rent_y] = ela1[ela1[:yshock].!= 0.0, :d_in_rent] ./ ela1[ela1[:yshock].!= 0.0, :yshock]

	out = Dict("which" => which,
		   "j" => j, 
	       "shockYear" => shockYear, 
	       # "dfs" => dfs,
	       "flows" => flows,
	       "opts" => opts,
	       # "elasticity_net" => elas,
	       "elasticity" => ela1,
	       "values" => Dict("base" => w0, which => w1),
	       "moments" => Dict("base" => mms0, which => mms1))

	return (out,sim0,sim1)
end

function read_exp_shockRegion(f::AbstractString)
	d = JSON.parsefile(f)
	# out and in
	for (k,v) in d
		if isa(v,Dict)
		println(collect(keys(v)))
			if all(["colindex","columns"] .== collect(keys(v)))
				# this is a dataframe
				d[k] = DataFrame(v["columns"],Symbol[symbol(v["colindex"]["names"][i]) for i in 1:length(v["colindex"]["names"])])
			end
		end
	end
	return d
end


"""
	shockRegions_scenarios(save::Bool=false)

Run shockRegion experiment for each region and for different price scenarios
"""
function shockRegions_scenarios(;save::Bool=false,yrange=0.05,prange=0.05)

	p = Param(2)
	d = Dict()
	for j in 1:p.nJ
		d[j] = Dict()
		for ps in [1.0-prange, 1.0, 1.0+prange]
			for ys in [1.0-yrange, 1.0, 1.0+yrange]

				d[Symbol("ps_$ps_ys_$ys")] = exp_shockRegion_impact(Dict("shockReg"=>j,"policy"=>"ypshock","shockYear"=>2000,"shockVal_p"=>fill(ps,p.nt-1),"shockVal_y"=>fill(ys,p.nt-1)))[1]
			end
		end
	end
	io = setPaths()
	f = open(joinpath(io["outdir"],"shockRegions_scenarios.json"),"w")
	JSON.print(f,d)
	close(f)
	return d
end


"""
	exp_shockRegion_impact(opts::Dict)

Applies price/income shock to a certain region in certain year and returns measures of differences wrt the baseline of that region. **Important**: Differences are measured only in the period in which the shock actually occurs, so that GE adjustments of wages/prices play a smaller role.
"""
function exp_shockRegion_impact(opts::Dict)

	j         = opts["shockReg"]
	which     = opts["policy"]
	shockYear = opts["shockYear"]

	if shockYear<1997
		throw(ArgumentError("must choose years after 1997. only then full cohorts available"))
	end

	# Baseline
	# --------

	# note: we must know the baseline model in any case.
	# this is because policy functions of agents in years
	# BEFORE the shock need to be adjusted to be equal to the baseline ones.
	p = Param(2)
	m = Model(p)
	solve!(m,p)
	sim0 = simulate(m,p)
	sim0 = sim0[!isna(sim0[:cohort]),:]
	mv_ids = @select(@where(sim0,(:year.>1996).&(:move)),id=unique(:id))

	# Policy
	# ------
	
	# compute behaviour for all individuals, assuming each time the shock
	# hits at a different age. selecting the right cohort will then imply
	# that the shock hits you in a given year.
	ss = pmap(x -> computeShockAge(m,opts,x),1:p.nt-1)		
	info("pmap done")

	# remove worker processes now.
	if length(workers()) > 1
		rmprocs(workers())
	end

	# stack dataframes
	# 
	df1 = ss[1]
	for i in 2:length(ss)
		df1 = vcat(df1,ss[i])
		gc()
	end
	ss = 0
	gc()
	info("stack done")
	df1 =  df1[!isna(df1[:cohort]),:]
	maxc = maximum(df1[:cohort])
	minc = minimum(df1[:cohort])

	if minc > 1
		# add all cohorts that were not simulated in computeShockAge
		df1 = vcat(df1,@where(sim0,:cohort.<minc))
	end


	# stack
	sim1 = df1
	df1 = 0
	gc()

	# compute summaries
	# =================

	# get averge lifetime of all and movers in shockYear
	# ----------------------------------------------
	w0 = @linq sim0 |>
		 @where((:j.==j)&(:year.>=shockYear)) |>
		 @select(v = mean(:maxv),u = mean(:utility))
	mms0 = computeMoments(sim0,p)	

	# dataset of baseline movers and their counterparts under the shock
	b_movers = sim0[findin(sim0[:id],mv_ids)];
	movers = join(b_movers,sim1[findin(sim1[:id],mv_ids)][[:cons,:save,:move,:own,:hh,:utility,:maxv]])

	w1 = @linq sim1 |>
		 @where((:j.==j)&(:year.>=shockYear)) |>
		 @select(v = mean(:maxv),u = mean(:utility))
	mms1 = computeMoments(sim1,p)	


	# get flows for each region
	d = Dict{AbstractString,DataFrame}()
	d["base"] = sim0
	d[which] = sim1
	flows = getFlowStats(d,false,"$(which)_$j")

	# calculate an elasticity of out and inflows
	# -------------------------------------------

	ela = join(flows["base"][j][[:All,:Owners,:Renters,:Net,:Total_in_all,:Total_out_all,:Net_own,:Own_in_all,:Own_out_all,:Net_rent,:Rent_in_all,:Rent_out_all,:out_rent,:out_buy,:in_rent,:in_buy,:year]],flows[which][j][[:All,:Owners,:Renters,:Net,:Total_in_all,:Total_out_all,:Net_own,:Own_in_all,:Own_out_all,:Net_rent,:Rent_in_all,:Rent_out_all,:out_rent,:out_buy,:in_rent,:in_buy,:year]],on=:year)

	# for kennan figure 1
	ela1 = @linq ela |>
			@transform(d_all = (:All_1 - :All) ./ :All, d_own = (:Owners_1 - :Owners)./:Owners, d_rent = (:Renters_1 - :Renters)./:Renters,d_net_own=(:Net_own_1 - :Net_own)./ :Net_own,d_net_rent=(:Net_rent_1 - :Net_rent)./ :Net_rent,d_in_rent = (:in_rent_1 - :in_rent)./:in_rent,d_in_buy = (:in_buy_1 - :in_buy)./:in_buy,d_out_rent = (:out_rent_1 - :out_rent)./:out_rent,d_out_buy = (:out_buy_1 - :out_buy)./:out_buy,year=:year) 
			
	ela1[:pshock] = 0.0
	ela1[:yshock] = 0.0

	shockyrs = sum(ela1[:year] .>= opts["shockYear"])

	# println("shockVal_y = $(opts["shockVal_y"])")
	# println("shockVal_p = $(opts["shockVal_p"])")

	ela1[ela1[:year] .>= opts["shockYear"], :yshock] = (1-opts["shockVal_y"][1:shockyrs])
	ela1[ela1[:year] .>= opts["shockYear"], :pshock] = (1-opts["shockVal_p"][1:shockyrs])

	# println(ela1[[:year,:yshock,:pshock]])
	ela1[:d_all_p] = 0.0
	ela1[:d_own_p] = 0.0
	ela1[:d_net_own_p] = 0.0
	ela1[:d_rent_p] = 0.0
	ela1[:d_net_rent_p] = 0.0
	ela1[:d_all_y] = 0.0
	ela1[:d_own_y] = 0.0
	ela1[:d_net_own_y] = 0.0
	ela1[:d_rent_y] = 0.0
	ela1[:d_net_rent_y] = 0.0
	ela1[:d_out_buy_y] = 0.0
	ela1[:d_out_buy_p] = 0.0
	ela1[:d_in_buy_y] = 0.0
	ela1[:d_in_buy_p] = 0.0
	ela1[:d_out_rent_y] = 0.0
	ela1[:d_out_rent_p] = 0.0
	ela1[:d_in_rent_y] = 0.0
	ela1[:d_in_rent_p] = 0.0

	ela1[ela1[:pshock].!= 0.0, :d_all_p] = ela1[ela1[:pshock].!= 0.0, :d_all] ./ ela1[ela1[:pshock].!= 0.0, :pshock]
	ela1[ela1[:pshock].!= 0.0, :d_own_p] = ela1[ela1[:pshock].!= 0.0, :d_own] ./ ela1[ela1[:pshock].!= 0.0, :pshock]
	ela1[ela1[:pshock].!= 0.0, :d_rent_p] = ela1[ela1[:pshock].!= 0.0, :d_rent] ./ ela1[ela1[:pshock].!= 0.0, :pshock]
	ela1[ela1[:pshock].!= 0.0, :d_net_own_p] = ela1[ela1[:pshock].!= 0.0, :d_net_own] ./ ela1[ela1[:pshock].!= 0.0, :pshock]
	ela1[ela1[:pshock].!= 0.0, :d_net_rent_p] = ela1[ela1[:pshock].!= 0.0, :d_net_rent] ./ ela1[ela1[:pshock].!= 0.0, :pshock]
	ela1[ela1[:pshock].!= 0.0, :d_out_buy_p] = ela1[ela1[:pshock].!= 0.0, :d_out_buy] ./ ela1[ela1[:pshock].!= 0.0, :pshock]
	ela1[ela1[:pshock].!= 0.0, :d_in_buy_p] = ela1[ela1[:pshock].!= 0.0, :d_in_buy] ./ ela1[ela1[:pshock].!= 0.0, :pshock]
	ela1[ela1[:pshock].!= 0.0, :d_out_rent_p] = ela1[ela1[:pshock].!= 0.0, :d_out_rent] ./ ela1[ela1[:pshock].!= 0.0, :pshock]
	ela1[ela1[:pshock].!= 0.0, :d_in_rent_p] = ela1[ela1[:pshock].!= 0.0, :d_in_rent] ./ ela1[ela1[:pshock].!= 0.0, :pshock]

	ela1[ela1[:yshock].!= 0.0, :d_all_y] = ela1[ela1[:yshock].!= 0.0, :d_all] ./ ela1[ela1[:yshock].!= 0.0, :yshock]
	ela1[ela1[:yshock].!= 0.0, :d_own_y] = ela1[ela1[:yshock].!= 0.0, :d_own] ./ ela1[ela1[:yshock].!= 0.0, :yshock]
	ela1[ela1[:yshock].!= 0.0, :d_rent_y] = ela1[ela1[:yshock].!= 0.0, :d_rent] ./ ela1[ela1[:yshock].!= 0.0, :yshock]
	ela1[ela1[:yshock].!= 0.0, :d_net_own_y] = ela1[ela1[:yshock].!= 0.0, :d_net_own] ./ ela1[ela1[:yshock].!= 0.0, :yshock]
	ela1[ela1[:yshock].!= 0.0, :d_net_rent_y] = ela1[ela1[:yshock].!= 0.0, :d_net_rent] ./ ela1[ela1[:yshock].!= 0.0, :yshock]
	ela1[ela1[:yshock].!= 0.0, :d_out_buy_y] = ela1[ela1[:yshock].!= 0.0, :d_out_buy] ./ ela1[ela1[:yshock].!= 0.0, :yshock]
	ela1[ela1[:yshock].!= 0.0, :d_in_buy_y] = ela1[ela1[:yshock].!= 0.0, :d_in_buy] ./ ela1[ela1[:yshock].!= 0.0, :yshock]
	ela1[ela1[:yshock].!= 0.0, :d_out_rent_y] = ela1[ela1[:yshock].!= 0.0, :d_out_rent] ./ ela1[ela1[:yshock].!= 0.0, :yshock]
	ela1[ela1[:yshock].!= 0.0, :d_in_rent_y] = ela1[ela1[:yshock].!= 0.0, :d_in_rent] ./ ela1[ela1[:yshock].!= 0.0, :yshock]

	out = Dict("which" => which,
		   "j" => j, 
	       "shockYear" => shockYear, 
	       "movers" => movers, 
	       # "dfs" => dfs,
	       "flows" => flows,
	       "opts" => opts,
	       # "elasticity_net" => elas,
	       "elasticity" => ela1,
	       "values" => Dict("base" => w0[:maxv][1], which => w1[:maxv][1]),
	       "moments" => Dict("base" => mms0, which => mms1))

	return (out,sim0,sim1)
end



function adjustVShocks!(mm::Model,m::Model,p::Param)

	if p.shockAge > 1
		for rt in 1:p.shockAge-1
		for ij=1:p.nJ			
		for ih=1:p.nh
		for ia=1:p.na
		for itau=1:p.ntau			
		for iz=1:p.nz				
		for ip=1:p.np 				
		for iy=1:p.ny 				
		for is=1:p.ns 				
		for ik=1:p.nJ			
			mm.rho[idx10(ik,is,iz,iy,ip,itau,ia,ih,ij,rt,p)] = m.rho[idx10(ik,is,iz,iy,ip,itau,ia,ih,ij,rt,p)]
			for ihh in 1:p.nh
				mm.vh[idx11(ihh,ik,is,iz,iy,ip,itau,ia,ih,ij,rt,p)] = m.vh[idx11(ihh,ik,is,iz,iy,ip,itau,ia,ih,ij,rt,p)]
				mm.ch[idx11(ihh,ik,is,iz,iy,ip,itau,ia,ih,ij,rt,p)] = m.ch[idx11(ihh,ik,is,iz,iy,ip,itau,ia,ih,ij,rt,p)]
				mm.sh[idx11(ihh,ik,is,iz,iy,ip,itau,ia,ih,ij,rt,p)] = m.sh[idx11(ihh,ik,is,iz,iy,ip,itau,ia,ih,ij,rt,p)]
			end
		end
		end
		end
		end
		end
		end
		end
		end
		end
		end
	end
	return nothing

end


# apply a shock at a certain age.
function computeShockAge(m::Model,opts::Dict,shockAge::Int)

	# if shockAge==0
	# 	opts["shockAge"] = shockAge + 1
	# 	p = Param(2,opts)
	# 	keep = (p.nt) - shockAge + opts["shockYear"] - 1998 # relative to 1998, first year with all ages present
	# 	@assert p.shockAge == shockAge + 1
	# else
		opts["shockAge"] = shockAge
		p = Param(2,opts=opts)
		setfield!(p,:ctax,get(opts,"ctax",1.0))	# set the consumption tax, if there is one in opts
		@assert p.shockAge == shockAge
		keep = (p.nt) - shockAge + opts["shockYear"] - 1997 # relative to 1997, first year with all ages present
	# end

	if get(opts,"verbose",0) > 0
		println("applying $(opts["policy"]) at age $(p.shockAge) with shockVals=$(p.shockVal[1:4]), keeping cohort $keep")
	end
	mm = Model(p)
	solve!(mm,p)

	# replace vh,rho,ch and sh before shockAge with values in baseline model m
	adjustVShocks!(mm,m,p)

	# simulate all individuals
	# but keep only the cohort that is age = shockAge in shockYear
	ss = simulate(mm,p)
	mm = 0
	gc()
	# throw away NA cohorts
	ss = ss[.!isna.(ss[:cohort]),:]
	# keep only cohort that gets the shock at age shockAge in shockYear.
	ss = @where(ss,:cohort .== keep)
	return ss
end


# Monetize the moving cost
# ========================

# what's the dollar value of the moving cost at different points
# in the state space?

# in particular: 
# how does it vary across the asset grid and age by own/rent?

# answer:
# compute the factor xtra_ass which equalizes the baseline value (no MC) to the one with MC but where you multiply assets with xtra_ass at a certain age (only at that age, not all ages!)

# adds xtra_ass dollars to each asset grid point at age t
function valueDiff(xtra_ass::Float64,v0::Float64,opts::Dict)
	p = Param(2,opts)
	println("extra assets=$xtra_ass")
	setfield!(p,:shockVal,[xtra_ass])
	setfield!(p,:shockAge,opts["it"])
	m = Model(p)
	solve!(m,p)
	w = m.v[1,1,opts["iz"],2,2,opts["itau"],opts["asset"],opts["ih"],2,opts["it"]]   # comparing values of moving from 2 to 1 in age 1
	if w == p.myNA
		return NaN 
	else
		(w - v0)^2
	end
end



# find consumption scale ctax such that
# two policies yield identical period 1 value
function find_xtra_ass(v0::Float64,opts::Dict)
	ctax = optimize((x)->valueDiff(x,v0,opts),0.0,100000.0,show_trace=true,method=Brent(),iterations=40,abs_tol=1e-6)
	return ctax
end

function moneyMC()

	# compute a baseline without MC
	p = Param(2)
	MC = Array(Any,p.nh,p.ntau)
	setfield!(p,:noMC,true)
	m = Model(p)
	solve!(m,p)
	println("baseline without MC done.")

	whichasset = m.aone

	# at P=Y=2, price in region 2 is 163K.
	# compare a zero asset renter to an owner with 0 net wealth.
	# 0 net wealth means that assets are -163K.

	opts = Dict()
	opts["policy"] = "moneyMC"
	for ih in 0:1
		if ih==0
			opts["asset"] = whichasset
		else
			opts["asset"] = whichasset-1 
		end
		opts["ih"] = ih+1
		for itau in 1:p.ntau
			opts["itau"] = itau
			opts["iz"] = 1  	# lowest income state
				opts["it"] = 1 	# age 1
				v0 = m.v[1,1,opts["iz"],2,2,opts["itau"],opts["asset"],opts["ih"],2,opts["it"]]	# comparing values of moving from 2 to 1
				MC[ih+1,itau] = find_xtra_ass(v0,opts)
				println("done with MC ih=$ih, itau=$itau")
				println("moving cost: $(Optim.minimizer(MC[ih+1,itau]))")

		end
	end

	zs = m.gridsXD["zsupp"][:,1]
	# make an out dict
	d =Dict( "low_type" => Dict( "rent" => Optim.minimizer(MC[1,1]), "own" => Optim.minimizer(MC[2,1]), "high_type" => Dict( "rent" => Optim.minimizer(MC[1,2]), "own" => Optim.minimizer(MC[2,2]))) )

	io = mig.setPaths()
	f = open(joinpath(io["outdir"],"moneyMC2.json"),"w")
	JSON.print(f,d)
	close(f)


	return (d,MC)
end

