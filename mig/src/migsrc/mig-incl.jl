


function post_slack(job)
	txt = "payload={'text': '$job'}"
	# println(txt)
	# println(ENV["MIG_SLACK"])
	if haskey(ENV,"SLACK_HOOK")
		run(`curl -X POST --data-urlencode $txt $(ENV["SLACK_HOOK"])`) 
		return nothing
	else
		error("you need a webhook into slack as environment variable SLACK_HOOK to post a message")
	end
end
function post_slack()
	haskey(ENV,"SLACK_HOOK") || error("you need a webhook into slack as environment variable SLACK_HOOK to post a message")
end


    # with open('slack-hook.txt','r') as myf:
    #     TKN = myf.read()
    # conn = connect()


function mydf2dict(df::DataFrame)
	if names(df) != [:moment,:model_value]
		throw(ArgumentError("need columns :moment,:model_value"))
	end
	d = Dict()
	status = 0
	for e in eachrow(df)
		# d[Symbol(e[:moment])] = [e[:model_value],e[:model_sd]]
		# does not make sense to return model_sd!
		if (ismissing(e[:model_value]) | !isfinite(e[:model_value]))
			d[Symbol(e[:moment])] = NaN
			status = -1
		else
			d[Symbol(e[:moment])] = e[:model_value]
		end
	end
	return (d,status)
end

"""
	convert(df::DataFrame,::Dict,id::Symbol)

Convert a dataframe to a dict with `id` as `keys`, and all other columns as a new dict for each key in `id`.

# Examples
```julia
julia> df = DataFrame(a=collect(1:5),c = rand(5), d=collect(linspace(5,1,5)))
julia> convert(df,Dict,:a)
```

"""
function convert(::Type{Dict},df::DataFrame,id::Symbol)
	d = Dict{AbstractString,Any}()
	cnames = names(df)[names(df) .!= id]
	for e in eachrow(df)
		d[string(e[id])] = Dict(k => e[k] for k in cnames)
	end
	return d
end

# miscellaneous includes

function cov2corr(x::Matrix)
	if size(x) != (2,2)
		error("x must be a 2 by 2 matrix")
	end
	y = similar(x)
	f = prod(sqrt.(diag(x)))
	y[1,1] = 1.0
	y[2,2] = 1.0
	y[1,2] = x[1,2] / f
	y[2,1] = y[1,2]
	return y
end

function objfunc_test(ev::Eval)

	start(ev)
	p = Param(2)	# create a default param type
	MomentOpt.fill(p,ev)      # fill p with current values on eval object
	mm = similar(MomentOpt.dataMomentd(ev))
	for (k,v) in MomentOpt.dataMomentd(ev) 
		mm[k] = rand()
	end
	setMoments!(ev,Dict(mm))
	v = Dict{Symbol,Float64}()
	for (k,mom) in MomentOpt.dataMomentd(ev)
		# if haskey(dataMomentWd(ev),k)
		# 	v[k] = ((simMoments[k] .- mom) ./ dataMomentW(ev,k)) .^2
		# else
		# 	v[k] = ((simMoments[k] .- mom) ) .^2
		# end
		v[k] = rand()
		# v[k] = v[k] / 1000
	end
	vv = mean(collect(values(v)))
	setValue!(ev, (ismissing(vv) | !isfinite(vv)) ? NaN : vv )
	finish(ev)

	return ev
end

function FD_gradient_g(p::OrderedDict)
	# for length(p) params, you need length(p) + 1 evaluations

	# get g(p)
	x = runObj_test(p)
	gp = collect(values(x.simMoments))
	D = zeros(length(p),length(gp))

	# optimal step size
	h = sqrt(eps())
	h = 

	# compute each partial derivative
	row = 0
	@showprogress "Computing..." for (k,v) in p
		row += 1
		pp = deepcopy(p)
		pp[k] = v + h 
		println("changing $v to $(pp[k])")
		xx = runObj_test(pp)
		D[row,:] = (collect(values(xx.simMoments)) .- gp) / h
	end

	return D
end

"""
	objfunc_wrapper(x::Vector{Float64})

Wrap objfunc such that suitable for use with Finite Difference function
"""
function objfunc_wrapper(x::Vector{Float64})

	p = OrderedDict(
		:xi1         => x[1],
        :xi2         => x[2],
        :eta         => x[3],
        :omega2      => x[4],
        :MC0        =>  x[5],
        :MC1        =>  x[6],
        :MC2        =>  x[7],
        :MC3        =>  x[8],
        :MC4        =>  x[9],
        :taudist    =>  x[10],
        :amenity_ENC => x[11],
        :amenity_ESC => x[12],
        :amenity_MdA => x[13],
        :amenity_Mnt => x[14],
        :amenity_NwE => x[15],
        :amenity_Pcf => x[16],
        :amenity_StA => x[17],
        :amenity_WNC => x[18],
        :amenity_WSC => x[19]
		)
	e = runObj(p)
	return e.value
end


# objective function to work with MomentOpt
function objfunc(ev::Eval)

	start(ev)

	p = Param(2)	# create a default param type
	if get(ev.options,:noseed,false)
		p.seed = false
	else
		p.seed = true
	end
	MomentOpt.fill(p,ev)      # fill p with current values on eval object

	m = Model(p)
	mig.solve!(m,p)
	gc()
	s   = simulate(m,p)
	m = 0
	gc()
	smm = computeMoments(s,p)	
	gc()
	# mms   = simulate_parts(m,p,5)	# simulate and compute moments in 5 pars
	simMoments,status = mydf2dict(smm["moments"])
	setMoments!(ev,simMoments)

	mm = MomentOpt.check_moments(ev)
	nm = filter(x->!in(x,[:moment,:data,:data_sd,:simulation,:distance]),names(mm))
	v = Dict()
	for k in nm
		v[k] = mean(mm[k])
	end
	value = v[:abs_percent_SD_weighted] 

	# v = Dict{Symbol,Float64}()
	# for (k,mom) in MomentOpt.dataMomentd(ev)
	# 	# if haskey(dataMomentWd(ev),k)
	# 	# 	v[k] = ((simMoments[k] .- mom) ./ dataMomentW(ev,k)) .^2
	# 	# else
	# 	# 	v[k] = ((simMoments[k] .- mom) ) .^2
	# 	# end
	# 	v[k] = abs(100.0* ((simMoments[k] .- mom) ./ mom) )
	# 	# println("perc dev of moment $k is $(v[k])")
	# 	# v[k] = v[k] / 1000
	# end
	# vv = mean(collect(values(v)))
	setValue!(ev, (ismissing(value) | !isfinite(value)) ? NaN : value )

	status = (ismissing(value) | !isfinite(value)) ? -1 : status

    if get(ev.options,"printm",false) 
    	d = Dict()
    	for e in eachrow(mm)
       		d[e[:moment]] = Dict("data"=>e[:data],"model"=>e[:simulation])
       	end
    	# change age brackets
    	inp,outp = setPaths()

    	f = open(joinpath(ENV["HOME"],"Dropbox/research/mobility/output/model/fit/moms.json"),"w")
    	JSON.print(f,d)
    	close(f)
		# writetable("/Users/florianoswald/Dropbox/mobility/output/model/fit/moms.csv",mom2)
	end

	ev.status = status
	# println("value = $value")

	finish(ev)

	return ev
end




function mywrap()
	p = mig.Param(2)
	m = mig.Model(p)
    mig.solve!(m,p)
end

function runSol(;opt=Dict())
	if length(opt)>0
		p = Param(2,opts=opt)	# 
	else
		p = Param(2)	# create a default param type
	end
	m = Model(p)
	mig.solve!(m,p)
	return m
end

function runSim(;opt=Dict())
	if length(opt)>0
		p = Param(2,opts=opt)	# 
	else
		p = Param(2)	# create a default param type
	end
	m = Model(p)
	mig.solve!(m,p)
	s   = simulate(m,p)
	s = s[.!ismissing.(s[:cohort]),:]
	# s2 = @where(s,:year.>1996)
	# showall(@by(s2,[:own,:realage],m=mean(:move)))
	# showall(@by(s2,[:realage],own=mean(:own),buy=mean((:h.==0).*(:hh.==1)),sell=mean((:h.==1).*(:hh.==0))))
	# own=@by(s2,[:realage],m=mean(:own))
	# # mig.plot(own[:realage],own[:m])
	# # figure()
	# simplot(s,5)
	# x=computeMoments(s,p)
	# MomentOpt.check_moments()
	# showall(x["moments"])
	# showall(x["yearly"])
	return s
end


# single test run of objective
function runObj(printm::Bool=false,subset=true)
	# create MProb

	io = mig.setPaths()
	moms = mig.DataFrame(mig.FileIO.load(joinpath(io["indir"],"moments.rda"))["m"])
	mig.names!(moms,[:name,:value,:weight])
	# subsetting moments
	dont_use = ""
	if subset
		dont_use= ["lm_w_intercept","move_neg_equity"]
		# dont_use= ["lm_w_intercept","move_neg_equity","q25_move_distance","q50_move_distance","q75_move_distance"]
		for iw in moms[:name]
			if contains(iw,"wealth") 
				push!(dont_use,iw)
			end
		end
	end
	use_names = setdiff(moms[:name],dont_use)
	moms_use = moms[findin(moms[:name],use_names) ,:]

	# mprob = MomentOpt.MProb() 
	# MomentOpt.addMoment!(mprob,moms_use) 
	# MomentOpt.addEvalFunc!(mprob,mig.objfunc)

	# create Eval
	ev = MomentOpt.Eval(Dict(),moms_use)
	if printm
		ev.options["printm"] = printm
	end

	ev = objfunc(ev)
	MomentOpt.check_moments(ev)
	return ev
end
function runObj(p::Union{Dict,OrderedDict})
	# create MProb

	# io = mig.setPaths()
	# moms = mig.DataFrame(mig.FileIO.load(joinpath(io["indir"],"moments.rda"))["m"])
	# mig.names!(moms,[:name,:value,:weight])
	# # subsetting moments
	# # dont_use= ["lm_w_intercept","move_neg_equity"]
	# dont_use= ["lm_w_intercept","move_neg_equity","q25_move_distance","q50_move_distance","q75_move_distance","lm_h_age2"]
	# for iw in moms[:name]
	# 	if contains(iw,"wealth") 
	# 		push!(dont_use,iw)
	# 	end
	# end
	# use_names = setdiff(moms[:name],dont_use)
	# moms_use = moms[findin(moms[:name],use_names) ,:]

	m = setup_mprob()

	# create Eval
	ev = MomentOpt.Eval(m,p)
	ev = objfunc(ev)
	MomentOpt.check_moments(ev)
	return ev
end

function runObj_test(p::Union{Dict,OrderedDict})
	# create MProb

	io = mig.setPaths()
	moms = mig.DataFrame(mig.FileIO.load(joinpath(io["indir"],"moments.rda"))["m"])
	mig.names!(moms,[:name,:value,:weight])
	# subsetting moments
	# dont_use= ["lm_w_intercept","move_neg_equity"]
	dont_use= ["lm_w_intercept","move_neg_equity","q25_move_distance","q50_move_distance","q75_move_distance","lm_h_age2"]
	for iw in moms[:name]
		if contains(iw,"wealth") 
			push!(dont_use,iw)
		end
	end
	use_names = setdiff(moms[:name],dont_use)
	moms_use = moms[findin(moms[:name],use_names) ,:]

	# create Eval
	ev = MomentOpt.Eval(p,moms_use)
	ev = objfunc_test(ev)
	return ev
end

		
# asset grid scaling
function scaleGrid(lb::Float64,ub::Float64,n::Int,order::Int,cutoff::Float64,partition=0.5) 
	out = zeros(n)
	if order==1
		off = 1
		if lb<0 
			off = 1 - lb #  adjust in case of neg bound
		end
		out[1] = log(lb + off) 
		out[n] = log(ub + off) 
		out    = linspace(out[1],out[n],n)
		out    = exp(out) - off  
	elseif order==2
		off = 1
		if lb<0 
			off = 1 - lb #  adjust in case of neg bound
		end
		out[1] = log( log(lb + off) + off )
		out[n] = log( log(ub + off) + off )
		out    = linspace(out[1],out[n],n)
		out    = exp( exp(out) - off ) - off
	elseif order == 3
		npos = int(ceil(n*partition))
		nneg = n-npos
		if nneg < 1
			error("need at least one point in neg space")
		end
		nneg += 1
		# positive: log scale
		pos = exp( linspace(log(cutoff),log( ub + 1) ,npos) ) -1 
		# negative: linear scale
		neg = linspace(lb,cutoff,nneg)
		return [neg[1:(nneg-1)],pos]
	else
		error("supports only double log grid")
	end
end


# converts
# function convert(::Type{DataFrame},cc::CoefTable)
# 	DataFrame(Variable=cc.rownms,Estimate=cc.mat[:,1],StdError=cc.mat[:,2],tval=cc.mat[:,3],pval=cc.mat[:,4])
# end

# convert(::Type{Array{Int64,1}}, PooledDataArray{Int64,Uint32,1})
mylog(x::Float64) = ccall((:log, "libm"), Float64, (Float64,), x)
myexp(x::Float64) = ccall((:exp, "libm"), Float64, (Float64,), x)
mylog2(x::Float64) = ccall(:log, Cdouble, (Cdouble,), x)
myexp2(x::Float64) = ccall(:exp, Cdouble, (Cdouble,), x)


function setPaths()
# get moments from dropbox:
	d = dirname(@__FILE__)
	ind = out = ""
	if is_apple()
		indir = joinpath(ENV["HOME"],"Dropbox/research/mobility/output/model/data_repo/in_data_jl")
		outdir = joinpath(ENV["HOME"],"Dropbox/research/mobility/output/model/data_repo/out_data_jl")
		outbox = joinpath(ENV["HOME"],"Dropbox/research/mobility/output/model/data_repo/outbox")
		out    = joinpath(d,"..","..","out")
		ind    = joinpath(d,"..","..","in")
		outg   = joinpath(ENV["HOME"],"Dropbox/research/mobility/output/model/data_repo/out_graphs_jl")
	elseif is_windows()
		indir = "C:\\Users\\florian_o\\Dropbox\\mobility\\output\\model\\data_repo\\in_data_jl"
		outdir = "C:\\Users\\florian_o\\Dropbox\\mobility\\output\\model\\data_repo\\out_data_jl"
		outg   = "C:\\Users\\florian_o\\Dropbox\\mobility\\output\\model\\data_repo\\out_graphs_jl"
	elseif is_linux()
		indir  = joinpath(joinpath(dirname(@__FILE__),"..","..","in"))
		ind    = joinpath(d,"..","..","in")
		outdir = joinpath(joinpath(dirname(@__FILE__),"..","..","out"))
		out    = joinpath(joinpath(dirname(@__FILE__),"..","..","out"))
		outg   = outdir
		outbox = "null"
	end
	rem_in = "~/data_repo/mig/in_data_jl"
	rem_out = "~/data_repo/mig/out_data_jl"
	return Dict("indir"=>indir, "outdir" => outdir, "out_graphs"=>outg, "remote_in" => rem_in, "remote_out"=> rem_out,"in"=>ind,"out"=>out,"outbox" => outbox)
end

# set outpath rel to dropbox/mobility/output/model
function setPaths(p::String)
	if is_apple()
		indir = joinpath(ENV["HOME"],"Dropbox/research/mobility/output/model/data_repo/in_data_jl")
		outdir = joinpath(ENV["HOME"],"Dropbox/research/mobility/output/model",p)
	else
		warn("no dropbox on this system")
	end
	return (indir,outdir)
end

function setupMC(autoload::Bool)
	indir, outdir = setPaths()

	if autoload
		# load model-generated data
		moms = readtable(joinpath(indir,"MCtrue.csv"))
	else
		# make model-generated data
		p = Param(2)
		m = Model(p)
	    solve!(m,p)
		s   = simulate(m,p)
		x=computeMoments(s,p,m)

		mom = DataFrame(read_rda(joinpath(indir,"moments.rda"))["m"])
		moms = join(mom,x,on=:moment)
		delete!(moms,[:data_value,:data_sd])
		names!(moms,[:moment,:data_value,:data_sd])

		writetable(joinpath(indir,"MCtrue.csv"),moms)
	end
	return moms
end

# """
#     DataFrame arithmetic.
# """
for op = map(x->Symbol(:.,x),(:+,:-,:*,:/))
    @eval begin
        function ($op)(d1::DataFrame,d2::DataFrame)
            if !(nrow(d1)==nrow(d2))
                error("need same num of rows")
            end
            if !(all(names(d1).==names(d2)))
                error("need same colnames")
            end
            df = deepcopy(DataFrame(d1))
            for n in names(d1)
                df[n] = $op(d1[n],d2[n])
            end
            return df
        end
    end
end
function abs(d1::DataFrame)
    df = deepcopy(DataFrame(d1))
    for n in names(d1)
        df[n] = abs.(d1[n])
    end
    return df
end
for op = (:+,:-,:*,:/)
    @eval begin
        function ($op)(x::Number,d1::DataFrame)
            df = deepcopy(DataFrame(d1))
            for n in names(d1)
                df[n] = $op(x,d1[n])
            end
            return df
        end
    end
end
function convert(::Type{Dict},x::DataFrame)
	if nrow(x) > 1
		Dict(k=>x[k] for k in names(x))
	else
		Dict(k=>x[k][1] for k in names(x))
	end
end
# same but with column pivot as the first key, then nested values under that
function convert(::Type{Dict},x::DataFrame,pivot::Symbol)
	vnames = setdiff(names(x),[pivot])
	r = Dict()
	for ro in eachrow(x)
		r[ro[pivot]] = Dict(k=>ro[k] for k in vnames)
	end
	return r
end
pdiff(x,y) = 100*(x.-y) ./ abs.(y)
function pdiff(x::Dict,y::Dict)
	r = Dict()
	for k in keys(y)
		r[k] = Dict(kk => pdiff(x[k][kk],y[k][kk]) for kk in keys(x[k]))
	end
	return r
end
function pdiff(x::DataFrame,y::DataFrame,p::Symbol)
	# check same colnames
	@assert names(x)==names(y)

	pd = copy(x)
	for ic in filter(x->x!=p,names(y))
		pd[ic] = pdiff(x[ic],y[ic])
	end
	return pd
end

#' computes flow statistics of 
#' baseline model. 
#' 1. whats population growth by year in each region
#' 2. what are the in and outflows relative to different populations.
function getFlowStats(dfs::Dict)

	# s is a simulation output
	d = Dict()

	for (k,v) in dfs
		v = v[.!(ismissing.(v[:cohort])),:]
		d[k] = Dict()

		for j in unique(v[:j]) 

			# population of j over time
			a = @linq v |>
				@where((:year.>1997) .& (:j.==j)) |>
				@by(:year, Owners=sum(:own),Renters=sum(.!:own),All=length(:own)) |>
				@transform(popgrowth = [diff(:All);0.0]./:All)

			# movers to j over time
			m_in = @linq v |>
				@where((:year.>1997) .& (:j.!=j)) |>
				@by(:year, Total_in=sum(:moveto.==j), Owners_in=sum((:moveto.==j).*(:h.==1)), Renters_in=sum((:moveto.==j).*(:h.==0)), in_buy =sum((:moveto.==j).*(:hh.==1)), in_rent =sum((:moveto.==j).*(:hh.==0)))

			# movers from j over time
			m_out = @linq v |>
				@where((:year.>1997) .& (:j.==j)) |>
				@by(:year, Total_out=sum(:move), Owners_out=sum((:move).*(:h.==1)), Renters_out=sum((:move).*(:h.==0)), out_buy =sum((:move).*(:hh.==1)), out_rent =sum((:move).*(:hh.==0)))

			# merge
			ma = join(a,m_in,on=:year)
			ma = join(ma,m_out,on=:year)
			ma = @transform(ma,Total_in_all=:Total_in./:All,Total_out_all=:Total_out./:All,Rent_in_all=:Renters_in./:All,Rent_in_rent=:Renters_in./:Renters,Own_in_all=:Owners_in./:All,Own_in_own=:Owners_in./:Owners,Rent_out_all=:Renters_out./:All,Rent_out_rent=:Renters_out./:Renters,Own_out_all=:Owners_out./:All,Own_out_own=:Owners_out./:Owners,Net = (:Total_in - :Total_out),Net_own = (:Owners_in - :Owners_out),Net_rent = (:Renters_in - :Renters_out))

			d[k][j] = ma
		end
	end

	return d
end


# get flows plot
function FlowsPlot(s::DataFrame,m::Model)

       flows = map(x-> proportionmap(@where(s,(:year.==x)&(:j.!=:moveto))[:moveto]),1997:2012)
       nms = m.regnames[:Division]

       fmat = zeros(9,length(flows))
       for i in 1:length(flows)
       for (k,v) in flows[i]
       fmat[k,i] = v
       end
       end
       df=names!(convert(DataFrame,fmat'),map(Symbol,convert(Array,m.regnames[:Division])))
       df[:year] = collect(1997:2012)
       mdf = melt(df,:year)
       names!(mdf,[:Destination,:v,:year])
       # Plots.gadfly()
       # pl = Plots.plot(mdf,:year,:v,group=:Destination,linewidth=2,ylabel="proportion of moves",title="Destination of Moves")
       pl = Gadfly.plot(mdf,x="year",y="v",color="Destination",ylabel="proportion of moves",title="Destination of Moves",Geom.line)
       inp, outp = setPaths("data_repo/out_graphs_jl")
       Gadfly.draw(Gadfly.PDF(joinpath(outp,"flows.pdf"),10cm,8cm),pl)
       return df
   end

	
function growthExample(pcf::Float64, wnc::Float64)

	P1 = 100.0
	P2 = 90.0

	chi = 0.8
	d = Dict()
	d["pcf"] = Dict("p1" => P1*pcf, "m1" => P1*pcf*chi, "eq1" => P1*pcf*(1-chi), "p2" => P2*pcf, "m2" => P1*pcf*chi, "eq2" => P2*pcf-P1*pcf*chi)
	d["wnc"] = Dict("p1" => P1*wnc, "m1" => P1*wnc*chi, "eq1" => P1*wnc*(1-chi), "p2" => P2*wnc, "m2" => P1*wnc*chi, "eq2" => P2*wnc-P1*wnc*chi)
	d["pcf"]["deq"] = (d["pcf"]["eq2"] - d["pcf"]["eq1"]) / d["pcf"]["eq1"]
	d["wnc"]["deq"] = (d["wnc"]["eq2"] - d["wnc"]["eq1"]) / d["wnc"]["eq1"]

	(i,o) = setPaths("properties")
	f = open(joinpath(o,"growthEx.json"),"w")
	JSON.print(f,d)
	close(f)

	return d
end
