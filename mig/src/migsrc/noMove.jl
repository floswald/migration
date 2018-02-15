# type to store results from noMove experiment
struct noMoveRes
	d::Dict
	scenario::Symbol
end

"""
	exp_Nomove(;ctax::Bool=false,save::Bool=false,ys::Float64=1.0,ps::Float64=1.0)

compares baseline with noMove scenario: shut down moving in all regions. returns differences in utility and other oucomes if moving is shut down everywhere. Results are by region.

## keywords

* `do_ctax`: compute compensating consumption tax
* `save`: save results to disk
* `ys`: multiplicative *yshock* to be applied to the counterfactual
* `ps`: multiplicative *pshock* to be applied to the counterfactual
"""
function exp_Nomove(;do_ctax::Bool=false,save::Bool=false,ys::Float64=1.0,ps::Float64=1.0)

	bp = shutdownMoving(yshock=ys,pshock=ps)
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

	function ctaxxers(ij::Int)
		println("doing ctax for region $ij")
		ctax = Dict()
		ctax[:region] = ij
		ctax[:data] = Dict()
		if ij==0
			# don't subset to any region: compute aggregate impact
			# all
			x=mig.ctaxxer("noMove",:realage,t->t.==t)
			ctax[:data][:ate] = Optim.minimizer(x)
			# young
			x=mig.ctaxxer("noMove",:realage,t->t.<31)
			ctax[:data][:young]=Optim.minimizer(x)
			# young
			x=mig.ctaxxer("noMove",:realage,t->t.>30)
			ctax[:data][:old]=Optim.minimizer(x)
			# for those who did not own a house when age == 30
			x=mig.ctaxxer("noMove",:rent_30,t->t)
			ctax[:data][:rent_30]=Optim.minimizer(x)
			# for those who did own a house when age == 30
			x=mig.ctaxxer("noMove",:own_30,t->t)
			ctax[:data][:own_30]=Optim.minimizer(x)
			gc()
		else
			# do subset to region j
			# all
			x=mig.ctaxxer("noMove",:j,t->t.==ij)
			ctax[:data][:ate] = Optim.minimizer(x)
			# young
			x=mig.ctaxxer("noMove",:realage,t->t.<31,:j,t->t.==ij)
			ctax[:data][:young]=Optim.minimizer(x)
			# young
			x=mig.ctaxxer("noMove",:realage,t->t.>30,:j,t->t.==ij)
			ctax[:data][:old]=Optim.minimizer(x)
			# for those who did not own a house when age == 30
			x=mig.ctaxxer("noMove",:rent_30,t->t,:j,t->t.==ij)
			ctax[:data][:rent_30]=Optim.minimizer(x)
			# for those who did own a house when age == 30
			x=mig.ctaxxer("noMove",:own_30,t->t,:j,t->t.==ij)
			ctax[:data][:own_30]=Optim.minimizer(x)
			gc()
		end
		return ctax
	end

	# preparing io.
	io = mig.setPaths()

	scenario = string("ps_",ps,"_ys_",ys)
	ostr = string("noMove_",scenario,".jld2")
	path = joinpath(io["outdir"],ostr)
	f = open(path,"a+")
	js = 0:p.nJ
	if do_ctax 
		ctax = pmap(x->ctaxxers(x),js)
		
	else
		# read from file
		ctax = f.d[:ctax]
	end
	close(f)

	# merge all ATE/ATT perc dicts
	ate_att = Dict()
	for (k,v) in ate_perc
		ate_att[k] = Dict(:ate=>v,:att=>atts["att"][k],:atn=>atts["atn"][k],:att_young=>atts["att_young"][k],:att_old=>atts["att_old"][k])
	end


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
		:ate_att => ate_att,
		:ctax => ctax,
		:scenario => scenario)

	n = noMoveRes(d,Symbol(scenario))
	@save path n

	# if save
	# 	rm(joinpath(io["outdir"],ostr),force=true)
	# 	open(joinpath(io["outdir"],ostr),"w") do f
	# 		JSON.print(f,d)
	# 	end
	# end
	return n
end


# helper function for exp_Nomove
function shutdownMoving(;pshock=1.0,yshock=1.0)

	# look at results after full cohorts available
	cutyr = 1997 - 1

	# baseline model

	p = Param(2)

	m = Model(p)
	solve!(m,p)
	# dimvec2 = (ns, ny, np, nz, ntau,  na, nh, nJ, nt-1 )
	EV0 = mean(m.EV[1,2,2,2,1,m.aone,1,:,2])
	basel = simulate(m,p);
	basel = basel[.!ismissing.(basel[:cohort]),:];

	if (pshock == 1) & (yshock == 1)
		info("noMove in ALL regions")
		opts = Dict("policy" => "noMove")
	elseif (yshock == 1)
		info("noMove in ALL regions with pshock=$(pshock)")
		opts = Dict("policy" => "noMove", "shockVal_p" => [pshock for i in 1:p.nt-1])

	else
		info("noMove in ALL regions with pshock=$(pshock),yshock=$(yshock)")
		opts = Dict("policy" => "noMove", "shockVal_p" => [pshock for i in 1:p.nt-1]"shockVal_y" => [yshock for i in 1:p.nt-1])

	end
	p2 = Param(2,opts=opts)
	m2 = Model(p2)
	solve!(m2,p2)
	EV1 = mean(m2.EV[1,2,2,2,1,m.aone,1,:,2])

	pol = simulate(m2,p2);
	pol = pol[.!ismissing.(pol[:cohort]),:];

	return Dict(:base=>basel,:pol=>pol,:EV0=>EV0,:EV1=>EV1,:perc=>100.0*(EV1.-EV0)./abs(EV0))

end


# Reporting Results

function noMove_plot(jkey::String,plotkey::Symbol,di::Dict)
	s = di[jkey]
	d = DataFrame()
	# d[plotkey] = parse.(Int,collect(keys(s)))   # breaks for non integer x axis
	d[plotkey] = collect(keys(s))   # breaks for non integer x axis
	for vi in keys(s[collect(keys(s))[1]])
        # d[Symbol(vi)] = missings(Float64,nrow(d))
        d[Symbol(vi)] = [v[vi] for (k,v) in s]
        if any(d[Symbol(vi)].==nothing)
        	d[Symbol(vi)][d[Symbol(vi)].==nothing] = NaN
        end
    end
    sort!(d,cols=plotkey)
    p = Any[]
    for ic in filter(x->x!=plotkey,names(d))
    	xi = @df d plot(cols(plotkey),cols(ic),label=ic)
    	push!(p,xi)
    end
    return plot(p...)
end



# function read_noMove(;f::String="$(ENV["HOME"])/git/migration/mig/out/noMove_ys_ps.json")


function plot_noMove(ps,ys)

	scenario = string("ps_",ps,"_ys_",ys)
	ostr = string("noMove_",scenario,".jld2")
	path = joinpath(io["outdir"],ostr)
	if !isfile(path)
		warn("This experiment does not exit yet. run with exp_noMove()")
		return 0
	else
		@load path

	end
end

function plot_noMove(;f::String="$(ENV["HOME"])/git/migration/mig/out/noMove_ys_ps.json")
	di = open(f) do fi
		JSON.parse(fi)
	end

	# percentage changes by age
	p_age = noMove_plot("age_ate_perc",:age,di)
	p_year = noMove_plot("year_perc",:year,di)
	p_loc = noMove_plot("loc_perc",:loc,di)
	p_own30 = noMove_plot("own30_perc",:own_30,di)
	# s = di["age_ate_perc"]
	# d = DataFrame()
	# d[:age] = parse.(Int,collect(keys(s)))
	# for vi in keys(s["20"])
 #       d[Symbol(vi)] = [v[vi] for (k,v) in s]
 #    end
 #    sort!(d,cols=:age)
 #    p_age = Any[]
 #    for ic in filter(x->x!=:age,names(d))
 #    	xi = @df d plot(:age,cols(ic))
 #    	push!(p_age,xi)
 #    end
 #    return plot(p...,layout=(3,3))

 	return (p_age,p_year,p_loc,p_own30)

end
