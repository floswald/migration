


# Decompose Moving Cost of Owners
# ===============================

function decompose_MC_owners(nosave::Bool=true)

	ps = Dict(:alpha => Dict(:MC3=>0.0),
		      :phi => Dict(:phi => 0.0),
		      :alpha_phi => Dict(:MC3=>0.0,:phi => 0.0))

	d = Dict(:own=>Dict(),
		     :move=> Dict(),
		     :move_own => Dict())

	tau = Dict(:own=>Dict(),
		     :move=> Dict(),
		     :move_own => Dict())

	# get baseline 
	info("Baseline")
	bs = runSim()
	p = Param(2)
	m = computeMoments(bs,p)
	m_b = mydf2dict(m["moments"])[1]
	t_b = m["tau"]

    pfun(x,y) = 100 * (x-y) / y

	@showprogress 1 "Simulating ..." for (k,v) in ps
		p = Param(2,opts=v)
		s = runSim(opt=v)
		m = computeMoments(s,p)
		mo = mydf2dict(m["moments"])[1]
		ta = m["tau"]	# Dict
		d[:own][k]        = pfun(mo[:mean_own],m_b[:mean_own])
		d[:move][k]       = pfun(mo[:mean_move],m_b[:mean_move])
		d[:move_own][k]   = pfun(mo[:mean_move_ownTRUE],m_b[:mean_move_ownTRUE])
		tau[:own][k]      = pfun(ta[:mean_own_1],t_b[:mean_own_1])  		# condition on tau=1 i.e. mover types
		tau[:move][k]     = pfun(ta[:mean_move_1],t_b[:mean_move_1])
		tau[:move_own][k] = pfun(ta[:mean_move_ownTRUE_1],t_b[:mean_move_ownTRUE_1])
	end

	di = Dict(:agg=>d,:tau=>tau)

	if !nosave
	    io = mig.setPaths()
	    f = open(joinpath(io["outdir"],"decompose_MC_owners.json"),"w")
	    JSON.print(f,di)
	    close(f)
		ficmd = `dbxcli put $(joinpath(io["outdir"],"decompose_MC_owners.json")) research/mobility/output/model/data_repo/outbox/decompose_MC_owners.json`
		out,proc = open(ficmd)
	end

	post_slack("[MIG] decompose_MC_owners done.")
	return(di)
end


# VALUE OF MIGRATION
# ==================

# function sim_expost_value(m::Model,p::Param,j::Int,base_move::Bool)
# 	cutyr = 1997 - 1
# 	solve!(m,p)
# 	base = simulate(m,p);
# 	base = base[!ismissing(base[:cohort]),:];
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
	base = base[!ismissing.(base[:cohort]),:];
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
# mig.ctaxxer(Dict(:policy=>"noMove",:ctax=>1.0),:y,t->t.==t) subsets nothing in addition to year=>1996 and finds ctax
# mig.ctaxxer(Dict(:policy=>"noMove",:ctax=>1.0),:j,t->t.==4) subsets in addition to year=>1996 that :j==4
# mig.ctaxxer(Dict(:policy=>"noMove",:ctax=>1.0),:own_30,t->t) subsets in addition to year=>1996 that :own_30 is true

# function wctaxxer(opt::Dict,var::Symbol,sel_func)
# 	println("finding consumption tax for $(opt[:policy]) policy. subsetting $var")
# 	info("pshock = $(opt[:shockVal_p][1]), yshock = $(opt[:shockVal_y][1]),weight=pop_wgt")
# 	s = runSim() # baseline
# 	val = @linq s |>
# 		@where((:year.>1996) .& sel_func(_I_(var))) |>
# 		@select(v=mean(:maxv,:pop_wgt),u=mean(:utility,:pop_wgt))
# 	v0 = val[:v][1]

# 	function ftau(ctau::Float64,v0::Float64,opt::Dict)
# 		opt[:ctax] = ctau
# 		si = runSim(opt=opt)
# 		val = @linq s |>
# 			@where((:year.>1996) .& sel_func(_I_(var))) |>
# 			@select(v=mean(:maxv,:pop_wgt),u=mean(:utility,:pop_wgt))
# 		v1 = val[:v][1]
# 		return (v0 - v1)^2
# 	end
# 	ctax = optimize((x) -> ftau(x,v0,opt),0.5,2.0, show_trace=false,iterations=12)
# 	return ctax
# end

function ctaxxer(opt::Dict,var::Symbol,sel_func)
	println("finding consumption tax for $(opt[:policy]) policy. subsetting $var")
	info("pshock = $(opt[:shockVal_p][1]), yshock = $(opt[:shockVal_y][1])")
	s = runSim() # baseline
	val = @linq s |>
		@where((:year.>1996) .& sel_func(_I_(var))) |>
		@select(v=mean(:maxv),u=mean(:utility))
	v0 = val[:v][1]

	function ftau(ctau::Float64,v0::Float64,opt::Dict)
		opt[:ctax] = ctau
		si = runSim(opt=opt)
		val = @linq si |>
			@where((:year.>1996) .& sel_func(_I_(var))) |>
			@select(v=mean(:maxv),u=mean(:utility))
		v1 = val[:v][1]
		return (v0 - v1)^2
	end
	ctax = optimize((x) -> ftau(x,v0,opt),0.5,2.0, show_trace=false,iterations=12)
	return ctax
end
function ctaxxer(opt::Dict,var1::Symbol,sel_func1,var2::Symbol,sel_func2)
	println("finding consumption tax for $(opt[:policy]) policy. subsetting $var1 and $var2")
	info("pshock = $(opt[:shockVal_p][1]), yshock = $(opt[:shockVal_y][1])")
	s = runSim()  # baseline
	val = @linq s |>
		@where((:year.>1996) .& sel_func1(_I_(var1)) .& sel_func2(_I_(var2))) |>
		@select(v=mean(:maxv),u=mean(:utility))
	v0 = val[:v][1]

	function ftau(ctau::Float64,v0::Float64,opts::Dict)
		opts[:ctax] = ctau
		si = runSim(opt=opts)
		val = @linq si |>
			@where((:year.>1996) .& sel_func1(_I_(var1)) .& sel_func2(_I_(var2))) |>
			@select(v=mean(:maxv),u=mean(:utility))
		v1 = val[:v][1]
		return (v0 - v1)^2
	end
	ctax = optimize((x) -> ftau(x,v0,opt),0.5,2.0, show_trace=false,iterations=12)
	return ctax
end

function ctaxxer(opt::Dict,var1::Symbol,sel_func1,var2::Symbol,sel_func2,var3::Symbol,sel_func3)
	println("finding consumption tax for $(opt[:policy]) policy. subsetting $var1 and $var2 and $var3")
	info("pshock = $(opt[:shockVal_p][1]), yshock = $(opt[:shockVal_y][1])")
	s = runSim()  # baseline
	val = @linq s |>
		@where((:year.>1996) .& sel_func1(_I_(var1)) .& sel_func2(_I_(var2)) .& sel_func3(_I_(var3))) |>
		@select(v=mean(:maxv),u=mean(:utility))
	v0 = val[:v][1]

	function ftau(ctau::Float64,v0::Float64,opts::Dict)
		opts[:ctax] = ctau
		si = runSim(opt=opts)
		val = @linq si |>
			@where((:year.>1996) .& sel_func1(_I_(var1)) .& sel_func2(_I_(var2)) .& sel_func3(_I_(var3))) |>
			@select(v=mean(:maxv),u=mean(:utility))
		v1 = val[:v][1]
		return (v0 - v1)^2
	end
	ctax = optimize((x) -> ftau(x,v0,opt),0.5,2.0, show_trace=false,iterations=12)
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


"""
	exp_shockRegion(opts::Dict)

Applies price/income shock to a certain region in certain year and returns measures of differences wrt the baseline of that region. Price scenarios can be given as members of `opts`. This experiment is complicated by the fact that I use a cohort setup: Only certain cohorts are alive in certain years. In order to simulate a shock in calendar year t, one needs two parts: first, a version of the model where the shock happens (say, different price kicking in at age j). Now, to be a real shock, this must come unexpected. In general, there will be a reaction to the coming shock at age j-1, etc. So, one needs to reset the model solution in years 1...j-1 to the baseline solution first.
"""
function exp_shockRegion(opts::Dict;same_ids=false)

	j         = opts["shockReg"]
	which     = get(opts,"policy","NULL")
	shockYear = opts["shockYear"]
	info("Applying shock to region $j")

	# is this a reverting shock?
	# reverting = any(opts["shockVal_p"].== 1.0) || any(opts["shockVal_y"].== 1.0)

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
	sim0 = sim0[.!ismissing.(sim0[:cohort]),:]
	sim0 = sim0[sim0[:year].>1996,:]
	mv_ids = @select(@where(sim0,(:year.>1996).&(:move)),id=unique(:id))
	
	mv_count = @linq sim0|>
	        @where((:year.>1996) .& (:tau.==1)) |>
	        @by(:id, n_moves = sum(:move), n_moveto = sum(:moveto.!=:j))

	stay_ids = @linq mv_count |>
	        @where((:n_moves.==0) ) |>
	        @select(id=unique(:id))

	# Policy
	# ------
	
	# compute behaviour for all individuals, assuming each time the shock
	# hits at a different age. selecting the right cohort will then imply
	# that the shock hits you in a given year.
	ss = pmap(x -> computeShockAge(m,opts,x),1:p.nt-1)		
	# debugging:
	# ss = DataFrame[sim0[rand(1:nrow(sim0),1000),:],sim0[rand(1:nrow(sim0),1000),:]]


	# stack dataframes
	# 
	df1 = ss[1]
	for i in 2:length(ss)
		df1 = vcat(df1,ss[i])
	end
	ss = 0
	gc()
	df1 =  df1[.!ismissing.(df1[:cohort]),:]
	df1 = df1[df1[:year].>1996,:]
	maxc = maximum(df1[:cohort])
	minc = minimum(df1[:cohort])

	if minc > 1
		# add all cohorts that were not simulated in computeShockAge
		df1 = vcat(df1,@where(sim0,:cohort.<minc))
	end

	# if reverting
	# 	# assume shock goes away immediately and all behave as in baseline
	# 	sim2 = @where(sim0,:cohort.>maxc)
	# else
		# assume shock stays forever
		opts["shockAge"] = 1
		p1 = Param(2,opts=opts)
		setfield!(p1,:ctax,get(opts,"ctax",1.0))	# set the consumption tax, if there is one in opts
		mm = Model(p1)
		solve!(mm,p1)
		sim2 = simulate(mm,p1)
		sim2 = sim2[.!ismissing.(sim2[:cohort]),:]
		mm = 0
		gc()
		# keep only guys born after shockYear
		sim2 = @where(sim2,(:cohort.>maxc) .& (:year .> 1996))
	# end

	# stack up results
	sim1 = vcat(df1,sim2)
	df1 = 0
	sim2 = 0
	gc()

	if same_ids
		# keep only people in baseline which also show up in the shocked regime
		# not all do!
		ids = @select(sim1,id=unique(:id))
		sim0 = sim0[findin(sim0[:id],ids[:id]),:]
	end


	# compute summaries
	# =================


	# dataset of baseline movers and their counterparts under the shock
	# ----------------------------------------------
	b_movers = sim0[findin(sim0[:id],mv_ids[:id]),:];
	p_movers = sim1[findin(sim1[:id],mv_ids[:id]),:];
	att_0 = @select(b_movers,v=mean(:maxv),u=mean(:utility),y = mean(:income),cons=mean(:cons),a=mean(:a),h=mean(:h),w=mean(:wealth),q=mean(:y),p=mean(:p))
	att_1 = @select(p_movers,v=mean(:maxv),u=mean(:utility),y = mean(:income),cons=mean(:cons),a=mean(:a),h=mean(:h),w=mean(:wealth),q=mean(:y),p=mean(:p))
	att = att_1 .- att_0 
	atts = convert(Dict,100.0 * (att ./ abs(att_0)))

	# dataset of baseline stayer and their counterparts under the shock
	# ----------------------------------------------
	b_stayers = sim0[findin(sim0[:id],stay_ids[:id]),:];
	p_stayers = sim1[findin(sim1[:id],stay_ids[:id]),:];
	atn_0 = @select(b_stayers,v=mean(:maxv),u=mean(:utility),y = mean(:income),cons=mean(:cons),a=mean(:a),h=mean(:h),w=mean(:wealth),q=mean(:y),p=mean(:p))
	atn_1 = @select(p_stayers,v=mean(:maxv),u=mean(:utility),y = mean(:income),cons=mean(:cons),a=mean(:a),h=mean(:h),w=mean(:wealth),q=mean(:y),p=mean(:p))
	atn = atn_1 .- atn_0 
	atns = convert(Dict,100.0 * (atn ./ abs(atn_0)))

	# get averge lifetime of all and movers in shockYear
	# ----------------------------------------------
	w0 = @linq sim0 |>
		 @where((:j.==j).&(:year.>=shockYear)) |>
		 @select(v = mean(:maxv),u = mean(:utility),cons=mean(:cons))
	mms0 = computeMoments(sim0,p)	


	w1 = @linq sim1 |>
		 @where((:j.==j).&(:year.>=shockYear)) |>
		 @select(v = mean(:maxv),u = mean(:utility),cons=mean(:cons))
	mms1 = computeMoments(sim1,p)	


	# get flows for each region
	d = Dict{AbstractString,DataFrame}()
	d["base"] = sim0
	d[which] = sim1
	flows = getFlowStats(d)



	out = Dict("which" => which,
		   "j" => j, 
	       "shockYear" => shockYear, 
	       "flows" => flows,
	       "opts" => opts,
	       "movers_effects" => atts,
	       "stayer_effects" => atns,
	       "d_values" => 100*(w1[:v][1] - w0[:v][1]) /  abs(w0[:v][1]),
	       "d_cons" => 100*(w1[:cons][1] - w0[:cons][1]) /  abs(w0[:cons][1]),
	       "moments" => Dict("base" => mms0, which => mms1))

	# io = setPaths()
	# f = open(joinpath(io["outdir"],"shockRegions_scenarios.json"),"w")
	# JSON.print(f,d)
	# close(f)

	return (out,sim0,sim1)
end

function get_elas(df1::Dict,df2::Dict,opts::Dict,j::Int)

	ela = join(df1[j][[:All,:Owners,:Renters,:Net,:Total_in,:Total_out_all,:Net_own,:Own_in_all,:Own_out_all,:Net_rent,:Rent_in_all,:Rent_out_all,:out_rent,:out_buy,:in_rent,:in_buy,:year]],df2[j][[:All,:Owners,:Renters,:Net,:Total_in,:Total_out_all,:Net_own,:Own_in_all,:Own_out_all,:Net_rent,:Rent_in_all,:Rent_out_all,:out_rent,:out_buy,:in_rent,:in_buy,:year]],on=:year,makeunique=true)


	ela1 = @linq ela |>
			@transform(d_all = (:All_1 - :All) ./ :All, 
				d_own = (:Owners_1 - :Owners)./:Owners, 
				d_rent = (:Renters_1 - :Renters)./:Renters,
				d_net_own=(:Net_own_1 - :Net_own)./ :Net_own,
				d_net_rent=(:Net_rent_1 - :Net_rent)./ :Net_rent,
				d_total_in = (:Total_in_1 - :Total_in)./:in_rent,
				d_in_rent = (:in_rent_1 - :in_rent)./:in_rent,
				d_in_buy = (:in_buy_1 - :in_buy)./:in_buy,
				d_out_rent = (:out_rent_1 - :out_rent)./:out_rent,
				d_out_buy = (:out_buy_1 - :out_buy)./:out_buy,
				year=:year, pshock = 1.0, yshock = 0.0) 

	

	shockyrs = sum(ela1[:year] .>= opts["shockYear"])


	ela1[ela1[:year] .>= opts["shockYear"], :yshock] = abs.(opts["shockVal_y"][1:shockyrs] - 1)
	# ela1[ela1[:year] .>= opts["shockYear"], :pshock] = (1-opts["shockVal_p"][1:shockyrs])

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
	ela1[:d_total_in_y] = 0.0
	ela1[:d_in_buy_y] = 0.0
	ela1[:d_in_buy_p] = 0.0
	ela1[:d_out_rent_y] = 0.0
	ela1[:d_out_rent_p] = 0.0
	ela1[:d_in_rent_y] = 0.0
	ela1[:d_in_rent_p] = 0.0

	# ela1[ela1[:pshock].!= 0.0, :d_all_p] = ela1[ela1[:pshock].!= 0.0, :d_all] ./ ela1[ela1[:pshock].!= 0.0, :pshock]
	# ela1[ela1[:pshock].!= 0.0, :d_own_p] = ela1[ela1[:pshock].!= 0.0, :d_own] ./ ela1[ela1[:pshock].!= 0.0, :pshock]
	# ela1[ela1[:pshock].!= 0.0, :d_rent_p] = ela1[ela1[:pshock].!= 0.0, :d_rent] ./ ela1[ela1[:pshock].!= 0.0, :pshock]
	# ela1[ela1[:pshock].!= 0.0, :d_net_own_p] = ela1[ela1[:pshock].!= 0.0, :d_net_own] ./ ela1[ela1[:pshock].!= 0.0, :pshock]
	# ela1[ela1[:pshock].!= 0.0, :d_net_rent_p] = ela1[ela1[:pshock].!= 0.0, :d_net_rent] ./ ela1[ela1[:pshock].!= 0.0, :pshock]
	# ela1[ela1[:pshock].!= 0.0, :d_out_buy_p] = ela1[ela1[:pshock].!= 0.0, :d_out_buy] ./ ela1[ela1[:pshock].!= 0.0, :pshock]
	# ela1[ela1[:pshock].!= 0.0, :d_in_buy_p] = ela1[ela1[:pshock].!= 0.0, :d_in_buy] ./ ela1[ela1[:pshock].!= 0.0, :pshock]
	# ela1[ela1[:pshock].!= 0.0, :d_out_rent_p] = ela1[ela1[:pshock].!= 0.0, :d_out_rent] ./ ela1[ela1[:pshock].!= 0.0, :pshock]
	# ela1[ela1[:pshock].!= 0.0, :d_in_rent_p] = ela1[ela1[:pshock].!= 0.0, :d_in_rent] ./ ela1[ela1[:pshock].!= 0.0, :pshock]

	ela1[ela1[:yshock].!= 0.0, :d_all_y] = ela1[ela1[:yshock].!= 0.0, :d_all] ./ ela1[ela1[:yshock].!= 0.0, :yshock]
	ela1[ela1[:yshock].!= 0.0, :d_total_in_y] = ela1[ela1[:yshock].!= 0.0, :d_total_in] ./ ela1[ela1[:yshock].!= 0.0, :yshock]
	ela1[ela1[:yshock].!= 0.0, :d_own_y] = ela1[ela1[:yshock].!= 0.0, :d_own] ./ ela1[ela1[:yshock].!= 0.0, :yshock]
	ela1[ela1[:yshock].!= 0.0, :d_rent_y] = ela1[ela1[:yshock].!= 0.0, :d_rent] ./ ela1[ela1[:yshock].!= 0.0, :yshock]
	ela1[ela1[:yshock].!= 0.0, :d_net_own_y] = ela1[ela1[:yshock].!= 0.0, :d_net_own] ./ ela1[ela1[:yshock].!= 0.0, :yshock]
	ela1[ela1[:yshock].!= 0.0, :d_net_rent_y] = ela1[ela1[:yshock].!= 0.0, :d_net_rent] ./ ela1[ela1[:yshock].!= 0.0, :yshock]
	ela1[ela1[:yshock].!= 0.0, :d_out_buy_y] = ela1[ela1[:yshock].!= 0.0, :d_out_buy] ./ ela1[ela1[:yshock].!= 0.0, :yshock]
	ela1[ela1[:yshock].!= 0.0, :d_in_buy_y] = ela1[ela1[:yshock].!= 0.0, :d_in_buy] ./ ela1[ela1[:yshock].!= 0.0, :yshock]
	ela1[ela1[:yshock].!= 0.0, :d_out_rent_y] = ela1[ela1[:yshock].!= 0.0, :d_out_rent] ./ ela1[ela1[:yshock].!= 0.0, :yshock]
	ela1[ela1[:yshock].!= 0.0, :d_in_rent_y] = ela1[ela1[:yshock].!= 0.0, :d_in_rent] ./ ela1[ela1[:yshock].!= 0.0, :yshock]

	return ela1			
end

function v_ownersWTP(x::Float64,v0::Float64,m0::Model,o::Dict)

	oo = deepcopy(o)
	oo["shockVal"] = [x]
	s = mig.exp_shockRegion(oo)[3]
	# mean of value after shockage => v1
	vd = @linq s|>
		 @where((:j.==o["shockReg"]).&(:year.==o["shockYear"]).&(:own).&(isfinite.(:maxv))) |>
		 @select(v = mean(:maxv),u = mean(:utility),cons=mean(:cons))
	println(vd)
	# compute mean of V after suitable subsetting. => v0 
	v1 = vd[:v][1]
	println("v0 = $v0")
	println("v1 = $v1")
	(v1 - v0)^2
end
function v_ownersWTP_m(x::Float64,v0::Float64,m0::Model,o::Dict)

	oo = deepcopy(o)
	oo["shockVal"] = [x]
	s = mig.exp_shockRegion(oo)[3]
	# mean of value after shockage => v1
	vd = @linq s|>
		 @where((:j.==o["shockReg"]).&(:year.==o["shockYear"]).&(:own).&(:move).&(isfinite.(:maxv))) |>
		 @select(v = mean(:maxv),u = mean(:utility),cons=mean(:cons))
	println(vd)
	# compute mean of V after suitable subsetting. => v0 
	v1 = vd[:v][1]
	println("v0 = $v0")
	println("v1 = $v1")
	(v1 - v0)^2
end


"""
	ownersWTP(nosave::Bool=false)

What is the willingness to pay of an owner to become a renter after their region is hit by a negative income or price shock? This focuses on owners at a certain age only. 
> what an owner in a down region would pay to be a renter again
This is complicated because two things happen at the same time: price shock in region j, and asset compensation to owners in region j
"""
function ownersWTP(nosave::Bool=false)

	info("runing ownersWTP computation")
	post_slack()
	tic()

	# "becoem a renter" means dd=Dict(:MC3=>0.0,:phi => 0.0)
	# v0 = shocked economy measured at v, m.aone and dd
	# v1 = shocked economy measured at v, m.aone, no dd, but + assets

	# get baseline model decision rules
	m,p = solve()

	# function wtp_impl(m::Model,p::Param,j::Int)

	# 	println("working on region $j")

	dout = Dict()

	@showprogress "Computing..." for j in 1:p.nJ
		shocks = Dict(:y=>[0.9;1.0], :p => [1.0;0.9])
		dout[j] = Dict()
		for sh in (:y,:p)
			o = Dict("shockReg" => j,
					 "shockYear" => 2000,
					 "shockVal_y" => shocks[sh][1] .* ones(32),  
					 "shockVal_p" => shocks[sh][2] .* ones(32))

			# baseline value: a renter's world
			# compare simulated mean V from `if only i were a renter now` world...
			oNomc = deepcopy(o)
			oNomc["MC3"] = 0.0
			oNomc["phi"] = 0.0
			sNomc = mig.exp_shockRegion(o)[3]
			vd = @linq sNomc |>
				 @where((:j.==o["shockReg"]).&(:year.==o["shockYear"]).&(:own).&(isfinite.(:maxv))) |>
				 @select(v = mean(:maxv),u = mean(:utility),cons=mean(:cons))
			println(vd)
			# compute mean of V after suitable subsetting. => v0 
			v0 = vd[:v]
			vd = @linq sNomc |>
				 @where((:j.==o["shockReg"]).&(:year.==o["shockYear"]).&(:own).&(:move).&(isfinite.(:maxv))) |>
				 @select(v = mean(:maxv),u = mean(:utility),cons=mean(:cons))
			println(vd)
			# compute mean of V after suitable subsetting. => v0 
			v0_move = vd[:v]

			# now turn on the policy to compensate owners 
			o["policy"] = "ownersWTP"

			res = optimize( x-> v_ownersWTP(x,v0[1],m,o), 0.0, 50.0, show_trace=length(workers())==1,method=Brent(),abs_tol=1e-2)
			res_m = optimize( x-> v_ownersWTP_m(x,v0_move[1],m,o), 0.0, 200.0, show_trace=length(workers())==1,method=Brent(),abs_tol=1e-2)
			dout[j][sh] = Dict(:own => res.minimizer, :own_move => res_m.minimizer)

		end
	# 	return dout
	end
	# y = pmap(x->wtp_impl(x),1:p.nJ)
	# y = pmap(x->wtp_impl(m,p,x),4:4)
# 	y = pmap(x->wtp_impl(v,p,x),1:1)
# 	# reorder
	# d = Dict()
	# for j in 1:p.nJ
	# 	println(map(x->get(x,:region,0)==j,y))
	# 	d[j] = y[map(x->get(x,:region,0)==j,y)]
	# end
	if !nosave
		io = setPaths()
		ostr = "ownersWTP.json" 
		f = open(joinpath(io["out"],ostr),"w")
		JSON.print(f,d)
		close(f)
	end
	info("done.")

	took = round(toc() / 3600.0,2)  # hours
	post_slack("[MIG] ownersWTP $took hours")
	return d

end


# end

"""
	elasticity(nosave::Bool=false)

Compute elasticity of income shock on migration choices: how many percent do inflows to `j` increase if income there increases by 1%?
"""
function elasticity(nosave::Bool=false)

	info("runing elasticity computation")
	post_slack()
	tic()

	p = Param(2)
	m = Model(p)
	solve!(m,p)
	sim0 = simulate(m,p)
	sim0 = sim0[.!ismissing.(sim0[:cohort]),:]
	sim0 = sim0[sim0[:year].>1996,:]

	dout = Dict()

	# opts dict 
	o = Dict("shockReg" => 1,
			 "policy" => "ypshock",
			 "shockYear" => 2000,
			 "shockVal_y" => 1.1 .* ones(32),  
			 "shockVal_p" => ones(32),  
			 "shockAge" => 0   # dummy arg
			 )

	regs = m.regnames[:Division]
	@showprogress "Computing..." for j in 1:p.nJ
		o["shockReg"] = j
		x = exp_shockRegion(o)[1]
		y = get_elas(x["flows"]["base"],x["flows"][o["policy"]],o,j)
		dout[Symbol(regs[j])] = Dict(:all => mean(y[:d_all_y]),
			:d_total_in_y =>mean(y[:d_total_in_y]),
			:d_in_rent_y =>mean(y[:d_in_rent_y]),
			:d_in_buy_y =>mean(y[:d_in_buy_y]))
	end

	if !nosave
		io = setPaths()
		ostr = "elasticity.json" 
		f = open(joinpath(io["out"],ostr),"w")
		JSON.print(f,dout)
		close(f)
	end
	info("done.")

	took = round(toc() / 3600.0,2)  # hours
	post_slack("[MIG] elasticity $took hours")
	return dout
end


function exp_shockRegion_ranges(prange,qrange,nt,j)
	d = Dict()
	d[:region] = j
	d[:data] = Dict()
	for ps in [1.0-prange, 1.0, 1.0+prange]
		for qs in [1.0-qrange, 1.0, 1.0+qrange]
			info("doing exp_shockRegion with ps=$ps, qs=$qs")
			dd = Dict("shockReg"=>j,"policy"=>"ypshock","shockYear"=>2000,"shockVal_p"=>fill(ps,nt-1),"shockVal_y"=>fill(qs,nt-1))
			d[:data][Symbol("ps_$ps"*"_qs_$qs")] = exp_shockRegion(dd)[1]
			# d[:data][Symbol("ps_$ps"*"_ys_$ys")] = Dict(:a=>1,:b=> rand())
		end
	end
	return d
end


"""
	shockRegions_scenarios(save::Bool=false,qrange=0.05,prange=0.05)

Run shockRegion experiment for each region and for different price scenarios
"""
function shockRegions_scenarios(save::Bool=false,qrange=0.05,prange=0.05)
	tic()
	p = Param(2)
	d = Dict()
	# if on_impact
	# 	y = pmap(x->exp_shockRegion(Dict("shockReg"=>x,"policy"=>"ypshock","shockYear"=>2000,"shockVal_p"=>fill(0.94,p.nt-1),"shockVal_y"=>fill(0.9,p.nt-1)))[1],1:p.nJ)
	# 	# reorder
	# 	for j in 1:p.nJ
	# 		d[j] = y[map(x->x["j"]==j,y)]
	# 	end
	# 	# d[j] = exp_shockRegion(Dict("shockReg"=>j,"policy"=>"ypshock","shockYear"=>2000,"shockVal_p"=>fill(ps,p.nt-1),"shockVal_y"=>fill(ys,p.nt-1)	))[1]

	# else
		y = pmap(x->exp_shockRegion_ranges(prange,qrange,p.nt,x),1:p.nJ)
		# reorder
		for j in 1:p.nJ
			println(map(x->get(x,:region,0)==j,y))
			d[j] = y[map(x->get(x,:region,0)==j,y)]
		end
	# end
	io = setPaths()
	# ostr = on_impact ? "shockRegions_onimpact.json" : "shockRegions_scenarios.json"
	ostr = "shockRegions_scenarios.json"
	f = open(joinpath(io["outdir"],ostr),"w")
	JSON.print(f,d)
	close(f)
	info("done.")

	took = round(toc() / 3600.0,2)  # hours
	post_slack("[MIG] shockRegions_scenarios",took,"hours")
	return (d,y)
end

"""
	adjustVShocks!(mm::Model,m::Model,p::Param)

Resets decision rules to pre-shock environment found in model `m`. Pre-shock decision rules in shocked model `mm` are contaminated by altered future values in `mm` after the shock.
"""
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
			mm.v[idx10(ik,is,iz,iy,ip,itau,ia,ih,ij,rt,p)] = m.v[idx10(ik,is,iz,iy,ip,itau,ia,ih,ij,rt,p)]
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

	# important!
	oo = deepcopy(opts)

	# if shockAge==0
	# 	opts["shockAge"] = shockAge + 1
	# 	p = Param(2,opts)
	# 	keep = (p.nt) - shockAge + opts["shockYear"] - 1998 # relative to 1998, first year with all ages present
	# 	@assert p.shockAge == shockAge + 1
	# else
		oo["shockAge"] = shockAge
		p = Param(2,opts=oo)
		setfield!(p,:ctax,get(oo,"ctax",1.0))	# set the consumption tax, if there is one in opts
		@assert p.shockAge == shockAge
		keep = (p.nt) - shockAge + oo["shockYear"] - 1997 # relative to 1997, first year with all ages present
	# end

	println("applying $(p.policy) in $(p.shockReg) at age $(p.shockAge), keeping cohort $keep")
	# println("mc = $(p.MC3)")
	# println("phi = $(p.phi)")
	# println("shockVal = $(p.shockVal)")
	# println("shockVal_y = $(p.shockVal_y)")
	# println("shockVal_p = $(p.shockVal_p)")
	mm = Model(p)
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
	ss = ss[.!ismissing.(ss[:cohort]),:]
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
function valueDiff(xtra_ass::Float64,v0::Float64,opt::Dict)
	p = Param(2,opts=opt["p"])
	println("extra assets=$xtra_ass")
	setfield!(p,:shockVal,[xtra_ass])
	setfield!(p,:shockAge,opt["it"])
	m = Model(p)
	solve!(m,p)
	w = m.v[1,1,opt["iz"],2,2,opt["itau"],opt["asset"],opt["ih"],2,opt["it"]]   # comparing values of moving from 2 to 1 in age 1
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

function moneyMC(nosave::Bool=false)

	post_slack()
	# compute a baseline without MC
	p = Param(2)
	MC = Array{Any}(p.nh,p.ntau)
	setfield!(p,:noMC,true)
	m = Model(p)
	solve!(m,p)
	println("baseline without MC done.")

	whichasset = m.aone

	# at P=Y=2, price in region 2 is 163K.
	# compare a zero asset renter to an owner with 0 net wealth.
	# 0 net wealth means that assets are -163K.

	opts = Dict()
	opts["p"] = Dict()
	opts["p"]["policy"] = "moneyMC"
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
			info("done with MC ih=$ih, itau=$itau")
			info("moving cost: $(Optim.minimizer(MC[ih+1,itau]))")

		end
	end

	zs = m.gridsXD["zsupp"][:,1]
	# make an out dict
	d =Dict( "low_type" => Dict( "rent" => Optim.minimizer(MC[1,1]), "own" => Optim.minimizer(MC[2,1]), "high_type" => Dict( "rent" => Optim.minimizer(MC[1,2]), "own" => Optim.minimizer(MC[2,2]))) )

	io = mig.setPaths()

	if !nosave
	    io = mig.setPaths()
	    fi = joinpath(io["outdir"],"moneyMC2.json")
		f = open(fi,"w")
		JSON.print(f,d)
		close(f)
		# ficmd = `dbxcli put $fi research/mobility/output/model/data_repo/outbox/$fi`
		# out,proc = open(ficmd)
	end

	post_slack("[MIG] MoneyMC done.")

	return (d,MC)
end

function shockRegion_json(;f::String="$(ENV["HOME"])/git/migration/mig/out/shockRegions_scenarios.json")

	di = Dict()
	open(f) do fi
		d = JSON.parse(fi)
		J = collect(keys(d))   # all regions
		scs = collect(keys(d["1"][1]["data"]) )  # all scenarios saved
		for s in scs
			di[s] = Dict()
			di[s][:d_value] = Dict(Symbol("reg_$j") => d[j][1]["data"][s]["d_values"] for j in J )
			di[s][:d_cons]  = Dict(Symbol("reg_$j") => d[j][1]["data"][s]["d_cons"] for j in J)
			di[s][:m_a]     = Dict(Symbol("reg_$j") => d[j][1]["data"][s]["movers_effects"]["a"] for j in J)
			di[s][:m_w]     = Dict(Symbol("reg_$j") => d[j][1]["data"][s]["movers_effects"]["w"] for j in J)
			di[s][:m_p]     = Dict(Symbol("reg_$j") => d[j][1]["data"][s]["movers_effects"]["p"] for j in J)
			di[s][:m_y]     = Dict(Symbol("reg_$j") => d[j][1]["data"][s]["movers_effects"]["y"] for j in J)
			di[s][:m_v]     = Dict(Symbol("reg_$j") => d[j][1]["data"][s]["movers_effects"]["v"] for j in J)
			di[s][:m_h]     = Dict(Symbol("reg_$j") => d[j][1]["data"][s]["movers_effects"]["h"] for j in J)
			di[s][:m_u]     = Dict(Symbol("reg_$j") => d[j][1]["data"][s]["movers_effects"]["u"] for j in J)
			di[s][:m_q]     = Dict(Symbol("reg_$j") => d[j][1]["data"][s]["stayer_effects"]["q"] for j in J)
			di[s][:s_a]     = Dict(Symbol("reg_$j") => d[j][1]["data"][s]["stayer_effects"]["a"] for j in J)
			di[s][:s_w]     = Dict(Symbol("reg_$j") => d[j][1]["data"][s]["stayer_effects"]["w"] for j in J)
			di[s][:s_p]     = Dict(Symbol("reg_$j") => d[j][1]["data"][s]["stayer_effects"]["p"] for j in J)
			di[s][:s_y]     = Dict(Symbol("reg_$j") => d[j][1]["data"][s]["stayer_effects"]["y"] for j in J)
			di[s][:s_v]     = Dict(Symbol("reg_$j") => d[j][1]["data"][s]["stayer_effects"]["v"] for j in J)
			di[s][:s_h]     = Dict(Symbol("reg_$j") => d[j][1]["data"][s]["stayer_effects"]["h"] for j in J)
			di[s][:s_u]     = Dict(Symbol("reg_$j") => d[j][1]["data"][s]["stayer_effects"]["u"] for j in J)
			di[s][:s_q]     = Dict(Symbol("reg_$j") => d[j][1]["data"][s]["stayer_effects"]["q"] for j in J)
			sc = split(s,"_")
			di[s][:scenario] = string(sc[1],"=",sc[2],", ",sc[3],"=",sc[4])
			open(joinpath("$(ENV["HOME"])/git/migration","mig/out/shockRegions_$s.json"),"w") do f
		        JSON.print(f,di[s])
	        end
		end
	end
	# open(joinpath("$(ENV["HOME"])/git/migration","mig/out/shockRegions_print.json"),"w") do f
 #       JSON.print(f,di)
 #       end
	return di

end

