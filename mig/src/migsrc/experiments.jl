	
# list of experiments:
# 1) changing the mortgage interest rate deduction
# 2) fannie mae and freddie max guarantee: mortgage interest rate is too low.
# 3) imperfect rental markets: rent is too high
# 4) moving voucher: moving is too costly


function runExperiment(which::String,region::Int,year::Int)

	# unneccesary?
	# home = ENV["HOME"]
	# require(joinpath(home,"git/
	indir, outdir = mig.setPaths()

	p = Param(2)

	if which=="mortgage_deduct"
		e = mig.exp_Mortgage(true)
	elseif which=="pshock_highMC"
		e = mig.exp_shockRegion_vdiff("pshock","pshock_highMC")
		save(joinpath(outdir,"shockReg","exp_region$(region)_$which.JLD"),e)
	elseif which=="pshock_noBuying"
		e = mig.exp_shockRegion_vdiff("pshock","pshock_noBuying")
		save(joinpath(outdir,"shockReg","exp_region$(region)_$which.JLD"),e)
	elseif which=="pshock_noSaving"
		e = mig.exp_shockRegion_vdiff("pshock","pshock_noSaving")
		save(joinpath(outdir,"shockReg","exp_region$(region)_$which.JLD"),e)
	elseif which=="halfMC"
		e = mig.exp_changeMC("halfMC")
	elseif which=="doubleMC"
		e = mig.exp_changeMC("doubleMC")
	elseif which=="moneyMC"
		e = mig.exp_changeMC("doubleMC")
	elseif which=="noShocks"
		e = mig.noShocks()
	elseif which=="smallShocks"
		e = mig.smallShocks()

	elseif in(which,["pshock","pshock3","yshock","yshock3","noBuying","highMC","noSaving"])
		opts = selectPolicy(which,region,year,p)
		e = mig.exp_shockRegion(opts)
		f = open(joinpath(outdir,"shockReg","exp_region$(region)_$(which)_flows.json"),"w")
		JSON.print(f,e[1]["flows"])
		close(f)
	else
		throw(ArgumentError("no valid experiment chosen"))
	end

	println("done.")
	return e

end

function plotShockRegions2(printp=false)

	# download experiments
	# run(`scp -r sherlock:~/data_repo/mig/out_data_jl/shockReg   ~/git/migration/data/`)

	# load all experiments
	pth = "/Users/florianoswald/git/migration/data/shockReg/"
	opth = "/Users/florianoswald/Dropbox/mobility/output/model/experiments/exp_yp"
	fi = readdir(pth)

	# get only csv's for now
	fi = fi[map(x->contains(x,"csv"),fi)]

	out  = Dict()
	away  = Dict()
	toj  = Dict()

	# plot pshock
	# ===========

	both = readtable(joinpath(pth,"exp_region6_pshock_both.csv"))
	both_toj = readtable(joinpath(pth,"exp_region6_pshock_both_toj.csv"))

	exp_typ = "permanent"
	exp_var = "p"
	j = 6
	
	p_away = Dict()
	p_toj  = Dict()
	# moving away from j
	p_away["move"] = plot(both,x="year",y="move",color="regime",Geom.line(),Theme(line_width=0.07cm),Guide.title("People leaving region $j, $exp_typ shock to $exp_var in 2007"))
	p_away["move_own"] = plot(both,x="year",y="move_own",color="regime",Geom.line(),Theme(line_width=0.07cm),Guide.title("Owners leaving region $j, $exp_typ shock to $exp_var in 2007"))
	p_away["move_rent"] = plot(both,x="year",y="move_rent",color="regime",Geom.line(),Theme(line_width=0.07cm),Guide.title("Renters leaving region $j, $exp_typ shock to $exp_var in 2007"))

	# moving to j
	p_toj["move_own"] = plot(both_toj,x="year",y="move_own",color="regime",Geom.line(),Theme(line_width=0.07cm),Guide.title("Owners moving to region $j, $exp_typ shock to $exp_var in 2007"))

	p_toj["move_rent"] = plot(both_toj,x="year",y="move_rent",color="regime",Geom.line(),Theme(line_width=0.07cm),Guide.title("Renters moving to region $j, $exp_typ shock to $exp_var in 2007"))

	away["pshock"] = p_away
	toj["pshock"] = p_toj

	# compute average increase in migration rates
	# -------------------------------------------

	pp = @select(both,:year,:move,:move_own,:move_rent,:regime)
	pp = hcat(@select(@where(pp,:regime.=="baseline"),:year,base_move=:move,base_move_own=:move_own,base_move_rent=:move_rent),@select(@where(pp,:regime.=="pshock"),shock_move=:move,shock_move_own=:move_own,shock_move_rent=:move_rent))

	pp = @transform(pp,d_move = :shock_move .- :base_move)
	pp = @transform(pp,d_move_own =  :shock_move_own  .- :base_move_own  )
	pp = @transform(pp,d_move_rent = :shock_move_rent .- :base_move_rent )
	pp = @transform(pp,p_move = (:shock_move .- :base_move) ./:base_move)
	pp = @transform(pp,p_move_own =  (:shock_move_own  .- :base_move_own ) ./:base_move_own  )
	pp = @transform(pp,p_move_rent = (:shock_move_rent .- :base_move_rent) ./:base_move_rent )

	pp_sum = @select(@where(pp,:year.>2006),d_move=mean(:d_move),d_move_own=mean(:d_move_own),d_move_rent=mean(:d_move_rent),p_move=mean(:p_move),p_move_own=mean(:p_move_own),p_move_rent=mean(:p_move_rent))

	pp_sum =  [ i => pp_sum[i][1] for i in names(pp_sum)]

	f = open(joinpath(opth,string(j,"pshock.json")),"w")
	JSON.print(f,pp_sum)
	close(f)





	# plot yshock
	# ===========

	both = readtable(joinpath(pth,"exp_region6_yshock_both.csv"))
	both_toj = readtable(joinpath(pth,"exp_region6_yshock_both_toj.csv"))

	exp_typ = "permanent"
	exp_var = "y"
	j = 6
	
	p_away = Dict()
	p_toj  = Dict()
	# moving away from j
	p_away["move"] = plot(both,x="year",y="move",color="regime",Geom.line(),Theme(line_width=0.07cm),Guide.title("People leaving region $j, $exp_typ shock to $exp_var in 2007"))
	p_away["move_own"] = plot(both,x="year",y="move_own",color="regime",Geom.line(),Theme(line_width=0.07cm),Guide.title("Owners leaving region $j, $exp_typ shock to $exp_var in 2007"))
	p_away["move_rent"] = plot(both,x="year",y="move_rent",color="regime",Geom.line(),Theme(line_width=0.07cm),Guide.title("Renters leaving region $j, $exp_typ shock to $exp_var in 2007"))

	# moving to j
	p_toj["move_own"] = plot(both_toj,x="year",y="move_own",color="regime",Geom.line(),Theme(line_width=0.07cm),Guide.title("Owners moving to region $j, $exp_typ shock to $exp_var in 2007"))

	p_toj["move_rent"] = plot(both_toj,x="year",y="move_rent",color="regime",Geom.line(),Theme(line_width=0.07cm),Guide.title("Renters moving to region $j, $exp_typ shock to $exp_var in 2007"))

	away["yshock"] = p_away
	toj["yshock"] = p_toj


	# compute average increase in migration rates
	# -------------------------------------------

	yy = @select(both,:year,:move,:move_own,:move_rent,:regime)
	yy = hcat(@select(@where(yy,:regime.=="baseline"),:year,base_move=:move,base_move_own=:move_own,base_move_rent=:move_rent),@select(@where(yy,:regime.=="yshock"),shock_move=:move,shock_move_own=:move_own,shock_move_rent=:move_rent))

	yy = @transform(yy,d_move = :shock_move .- :base_move)
	yy = @transform(yy,d_move_own =  :shock_move_own  .- :base_move_own  )
	yy = @transform(yy,d_move_rent = :shock_move_rent .- :base_move_rent )
	yy = @transform(yy,p_move = (:shock_move .- :base_move) ./:base_move)
	yy = @transform(yy,p_move_own =  (:shock_move_own  .- :base_move_own ) ./:base_move_own  )
	yy = @transform(yy,p_move_rent = (:shock_move_rent .- :base_move_rent) ./:base_move_rent )

	yy_sum = @select(@where(yy,:year.>2006),d_move=mean(:d_move),d_move_own=mean(:d_move_own),d_move_rent=mean(:d_move_rent),p_move=mean(:p_move),p_move_own=mean(:p_move_own),p_move_rent=mean(:p_move_rent))

	yy_sum =  [ i => yy_sum[i][1] for i in names(yy_sum)]
	f = open(joinpath(opth,"yshock.json"),"w")
	JSON.print(f,yy_sum)
	close(f)

	if printp

		# print price
		draw(PDF(joinpath(opth,"away_pshock_move.pdf"),6inch,5inch),away["pshock"]["move"])
		draw(PDF(joinpath(opth,"away_pshock_own_move.pdf"),6inch,5inch),away["pshock"]["move_own"])
		draw(PDF(joinpath(opth,"away_pshock_rent_move.pdf"),6inch,5inch),away["pshock"]["move_rent"])

		draw(PDF(joinpath(opth,"toj_pshock_own_move.pdf"),6inch,5inch),toj["pshock"]["move_own"])
		draw(PDF(joinpath(opth,"toj_pshock_rent_move.pdf"),6inch,5inch),toj["pshock"]["move_rent"])
		
		# print income
		draw(PDF(joinpath(opth,"away_yshock_move.pdf"),6inch,5inch),away["yshock"]["move"])
		draw(PDF(joinpath(opth,"away_yshock_own_move.pdf"),6inch,5inch),away["yshock"]["move_own"])
		draw(PDF(joinpath(opth,"away_yshock_rent_move.pdf"),6inch,5inch),away["yshock"]["move_rent"])

		draw(PDF(joinpath(opth,"toj_yshock_own_move.pdf"),6inch,5inch),toj["yshock"]["move_own"])
		draw(PDF(joinpath(opth,"toj_yshock_rent_move.pdf"),6inch,5inch),toj["yshock"]["move_rent"])

	end


	return out

end

function plotShockRegions(print=false)

	# download experiments
	# run(`scp -r sherlock:~/data_repo/mig/out_data_jl/shockReg   ~/git/migration/data/`)

	# load all experiments
	pth = "/Users/florianoswald/git/migration/data/shockReg/"
	opth = "/Users/florianoswald/Dropbox/mobility/output/model/experiments/exp_yp"
	fi = readdir(pth)

	out = Dict()

	for i in fi
		x = load(joinpath(pth,i))
		df = x["dfs"]
		# writetable(joinpath(pth,"toj_own_buy_$(x["which"])_reg$(x["j"])_year$(x["shockYear"]).csv"),df["toj_own_buy"])
		# writetable(joinpath(pth,"fromj_$(x["which"])_reg$(x["j"])_year$(x["shockYear"]).csv"),df["fromj"])
		# writetable(joinpath(pth,"fromj_rent_$(x["which"])_reg$(x["j"])_year$(x["shockYear"]).csv"),df["fromj_rent"])
		# writetable(joinpath(pth,"fromj_own_$(x["which"])_reg$(x["j"])_year$(x["shockYear"]).csv"),df["fromj_own"])
		# writetable(joinpath(pth,"toj_own_rent_$(x["which"])_reg$(x["j"])_year$(x["shockYear"]).csv"),df["toj_own_rent"])
		# writetable(joinpath(pth,"toj_rent_rent_$(x["which"])_reg$(x["j"])_year$(x["shockYear"]).csv"),df["toj_rent_rent"])
		# writetable(joinpath(pth,"toj_rent_buy_$(x["which"])_reg$(x["j"])_year$(x["shockYear"]).csv"),df["toj_rent_buy"])
		# writetable(joinpath(pth,"toj_$(x["which"])_reg$(x["j"])_year$(x["shockYear"]).csv"),df["toj"])

		# make plots
		exp_typ = contains(x["which"],"3") ? "3-year" : "permanent"
		exp_var = contains(x["which"],"y") ? "y" : "p"
		dd = Dict()
		data = Dict()
		# fill in data dict with data
		for (k,v) in df
			data[k] = melt(v,:year)
		end

		# fill in dd dict with plots
		dd["fromj_own"] = plot(@where(data["fromj_own"],:year.>1996),x="year",y="value",color="variable",Geom.line(),Theme(line_width=0.07cm),Guide.title("owners leaving region $(x["j"]), $exp_typ shock to $exp_var in $(x["shockYear"])"))
		# data["fromj_rent"] = melt(df["fromj_rent"],:year)
		dd["fromj_rent"] = plot(@where(data["fromj_rent"],:year.>1996),x="year",y="value",color="variable",Geom.line(),Theme(line_width=0.07cm),Guide.title("Renters leaving region $(x["j"]), $exp_typ shock to $exp_var in $(x["shockYear"])"))
		# data["toj_own_buy"] = melt(df["toj_own_buy"],:year)
		dd["toj_own_buy"] = plot(@where(data["toj_own_buy"],:year.>1996),x="year",y="value",color="variable",Geom.line(),Theme(line_width=0.07cm),Guide.title("owners moving to region $(x["j"]), buying, $exp_typ shock to $exp_var in $(x["shockYear"])"))
		# data["toj_own_rent"] = melt(df["toj_own_rent"],:year)
		dd["toj_own_rent"] = plot(@where(data["toj_own_rent"],:year.>1996),x="year",y="value",color="variable",Geom.line(),Theme(line_width=0.07cm),Guide.title("owners moving to region $(x["j"]), renting, $exp_typ shock to $exp_var in $(x["shockYear"])"))
		# data["toj_rent_rent"] = melt(df["toj_rent_rent"],:year)
		dd["toj_rent_rent"] = plot(@where(data["toj_rent_rent"],:year.>1996),x="year",y="value",color="variable",Geom.line(),Theme(line_width=0.07cm),Guide.title("renters moving to region $(x["j"]), renting, $exp_typ shock to $exp_var in $(x["shockYear"])"))
		# data["toj_rent_buy"] = melt(df["toj_rent_buy"],:year)
		dd["toj_rent_buy"] = plot(@where(data["toj_rent_buy"],:year.>1996),x="year",y="value",color="variable",Geom.line(),Theme(line_width=0.07cm),Guide.title("renters moving to region $(x["j"]), buying, $exp_typ shock to $exp_var in $(x["shockYear"])"))

		kkey = "$(x["which"])_$(x["j"])_$(exp_typ)_$(x["shockYear"])"
		plotkey = "plots_"*kkey
		out[plotkey] = dd
		if print
			for (k,v) in dd
				draw(PDF(joinpath(opth,string(kkey,"_",k,".pdf")),6inch,5inch),v)
			end
		end
		out["data_$(x["which"])_$(x["j"])_$(exp_typ)_$(x["shockYear"])"] = data
	end
	return out

end


function exp_changeMC(which)

	println("computing baseline")

	p0   = Param(2)
	m0   = Model(p0)
	solve!(m0,p0)
	sim0 = simulate(m0,p0)
	w0 = getDiscountedValue(sim0,p0,m0,true)

	println("done.")

	# find welfare
	# measure welfare net of moving cost
	opts = ["policy" => which, "noMove" => true]

	println("finding ctax.")
	ctax = findctax(w0[1][1],opts)
	println("done.")

	# summarize data
	p1 = Param(2,opts)
	m1 = Model(p1)
	solve!(m1,p1)
	sim1 = simulate(m1,p1)

	sim0 = sim0[!isna(sim0[:cohort]),:]
	sim1 = sim1[!isna(sim1[:cohort]),:]

	agg_own = hcat(@select(sim0,own_baseline=mean(:own)),@select(sim1,own_policy=mean(:own)))
	agg_mv = hcat(@by(sim0,:own,move_baseline=mean(:move)),@by(sim1,:own,move_policy=mean(:move)))
	agg_own_age = hcat(@by(sim0,[:own,:realage],move_baseline=mean(:move)),@by(sim1,[:own,:realage],move_policy=mean(:move)))

	indir, outdir = mig.setPaths()

	# writetable(joinpath(outdir,"exp_$which","mv.csv"),agg_mv)
	# writetable(joinpath(outdir,"exp_$which","own.csv"),agg_own)
	# writetable(joinpath(outdir,"exp_$which","own_mv.csv"),agg_own_age)


	out = ["move"=>agg_mv,"move_age"=>agg_own_age,"own"=>agg_own,"ctax" => ctax]
	return out
end



function getDiscountedValue(df::DataFrame,p::Param,m::Model)

	w = @> begin
		df
		@transform(beta=p.beta .^ :age)
		@where(!isna(:cohort))
		@transform(vbeta = :v .* :beta)
		@by(:id, meanv = mean(:vbeta.data,WeightVec(:density.data)))
		@select(meanv = mean(:meanv))
	end
	return w
end

function getDiscountedValue(df::DataFrame,p::Param,m::Model,noMove::Bool)

	if noMove

		w = @> begin
			df
			@transform(beta=p.beta .^ :age)
			@where((!isna(:cohort)) & (!:move))
			@transform(vbeta = :v .* :beta)
			@by(:id, meanv = mean(:vbeta))
			@select(meanv = mean(:meanv))
		end
		return w

	else

		w = @> begin
			df
			@transform(beta=p.beta .^ :age)
			@where(!isna(:cohort))
			@transform(vbeta = :v .* :beta)
			@by(:id, meanv = mean(:vbeta))
			@select(meanv = mean(:meanv))
		end
		return w
	end
end

# age 1 utility difference between 2 policies
function welfare(ctax::Float64,v0::Float64,opts::Dict)
	p = Param(2,opts)
	setfield!(p,:ctax,ctax)
	m = Model(p)
	solve!(m,p)
	s = simulate(m,p)
	w = getDiscountedValue(s,p,m,get(opts,"noMove",false))
	(w[1][1] - v0)^2
end

# find consumption scale ctax such that
# two policies yield identical period 1 value
function findctax(v0::Float64,opts::Dict)
	ctax = optimize((x)->welfare(x,v0,opts),0.5,1.5,show_trace=true,method=:brent)
	return ctax
end

# collect some sim output from policies
function policyOutput(df::DataFrame,pol::ASCIIString)
	# subset to estimation sample: 1997 - 2012
	sim_sample = @where(df,(:year.>1997) )
	fullw = WeightVec(array(sim_sample[:density]))

	own_move = @> begin
		sim_sample
		@select(own=mean(:h.data,fullw),move=mean(:move.data,fullw),income=mean(:income.data,fullw),assets=mean(:a.data,fullw),q10=quantile(:income,0.1),q50=quantile(:income,0.5))
	end
	own_inc = @> begin
		sim_sample
		@transform(ybin = cut(:income,[0.0,20.0,40.0,60.0]))
		@by(:ybin, own = mean(:own.data,WeightVec(:density.data)), value=mean(:v.data,WeightVec(:density.data)))
	end

	own_move_age = @> begin
		sim_sample
		@by(:realage,own=mean(:h),move=mean(:move),income=mean(:income),assets=mean(:a),policy=pol)
	end
	move_own_age = @by(sim_sample,[:own,:realage],move=mean(:move),income=mean(:income),assets=mean(:a),policy=pol)

	out = ["own_move" => own_move, "own_inc" => own_inc, "own_move_age" => own_move_age, "move_own_age"=> move_own_age]
	return out
end

# How much subsidy does an owner get over their lifetime when they buy
# at age t? assume they never sell
function npv(x::DataArray,r,from::Int)
	y = 0.0
	@assert(from <= length(x))
	for i in from:length(x)
		y += x[i]/((1+r)^i) 
	end
	return y
end

function npv(x::DataArray,r)
	n = length(x)
	y = zeros(n)
	for from=1:n
		for i in from:n
			y[from] += x[i]/((1+r)^(i-from+1)) 
		end
	end
	return y
end




# computes: 
# * baseline model (with mortgage subsidy)
# * various policies that differ in the redistribution applied
# * for each policy the compensation consumption tax/subsidy that would make individuals indifferent between both schemes
function exp_Mortgage(ctax=false)

	# pol_type = 1: redestribute tax receipts as a lump sum to 
	# every age 20 individual. redestributes from old and rich to poor and young
	# pol_type = 2: redistribute only within age bin, i.e. 20 year old owners give
	# money to 20 year old renters.


	# baseline model:
	# ===============

	println("computing baseline")

	p   = Param(2)
	m   = Model(p)
	solve!(m,p)
	sim = simulate(m,p)

	println("done.")
	# get baseline expected lifetime utility at age 1
	w0 = getDiscountedValue(sim,p,m,false)

	# collect some baseline output
	# ----------------------------

	# throw away incomplete cohorts
	sim = @where(sim,!isna(:cohort));

	base_out = policyOutput(sim,"baseline")

	# get some tax receipts data
	# --------------------------
	# look at one complete cohort:
	# 1982 cohort
	# their behaviour is pretty uniform, so doesn't matter
	sim_T = @where(sim, :cohort.==16)
	N_T   = length(unique(sim_T[:id]))  # number of people in that cohort

	# keep in mind that in this model rich people = owners.

	# redistribution 0
	# ----------------
	Redist0 = zeros(p.nt-1)    # burn the money. (take it to iraq.)

	# redistribution 1: give all the money to 20 year olds entering the model in an equally split amount 
	# ----------------
	Tot_tax = sum(array(sim_T[:subsidy]))	
	Redist1 = [Tot_tax / N_T, zeros(p.nt-2)]

	# redistribution 2: give all the tax generated by x year old owners to all x year olds
	# ----------------

	# get per capita tax expenditure by age
	Tot_tax_age = @by(sim_T,:realage,receipts=sum(:subsidy),N_own = sum(:own),N=length(:subsidy))
	Tot_tax_age = @transform(Tot_tax_age,per_owner_subsidy = :receipts ./ :N_own,redist1 = Redist1,redist2 = :receipts ./ :N,redist3 = repeat([Tot_tax / (N_T * (p.nt-1))],inner=[p.nt-1],outer=[1]),own_rate = :N_own ./ :N)

	# check
	@assert all(abs(array(@select(Tot_tax_age,s1 = sum(N_T.*:redist1),s2=sum(N_T.*:redist2),s3 = sum(N_T.*:redist3))) .- Tot_tax) .< 1e-8)

	# get expected net present value of subsidy conditional on age.
	x = @> begin
		sim_T
		@by(:id,npv_at_age = npv(:subsidy,p.R-1),realage=:realage)
		@by(:realage, npv_at_age = mean(:npv_at_age))
	end
	Tot_tax_age = join(Tot_tax_age,x,on=:realage)

	npv_age_income = @> begin
		sim_T
		@transform(ybin = cut(:income,round(quantile(:income,[1 : (5- 1)] / 5))))
		@by(:id,npv_at_age = npv(:subsidy,p.R-1),realage=:realage,ybin=:ybin)
		@by([:realage,:ybin], npv_at_age = mean(:npv_at_age))
	end

	Redist2 = array(Tot_tax_age[:redist2])

	# redistribution 3: in each period give back Tot_tax / nsim*T
	# ----------------
	Redist3 = repeat([Tot_tax / (N_T * (p.nt-1))],inner=[p.nt-1],outer=[1])

	# free some memory
	m0 = 0
	sim = 0
	gc()

	println("starting experiments")

	# model under no redistribution at all: burning money
	# ---------------------------------------------------

	opts = ["policy" => "mortgageSubsidy","redistribute" => Redist0 ,"verbose"=>0]

	# utility equalizing consumption scaling:
	if ctax
		ctax0 = findctax(w0[1][1],opts)
	else
		ctax0 = 0
	end

	# run simulation
	p0   = Param(2,opts)
	m0   = Model(p0)
	solve!(m0,p0)
	sim0 = simulate(m0,p0)

	# some output form this policy:
	# throw away incomplete cohorts
	sim0 = @where(sim0,!isna(:cohort));
	pol0_out = policyOutput(sim0,"burning_money")

	m0 = 0
	sim0 = 0
	gc()
	println("experiment with all money to 20yr old")

	# model under redistribution policy 1: lump sum to all 20 year olds
	# -----------------------------------------------------------------

	opts = ["policy" => "mortgageSubsidy","redistribute" => Redist1 ,"verbose"=>0]

	# utility equalizing consumption scaling:
	if ctax
		ctax1 = findctax(w0[1][1],opts)
	else
		ctax1 = 0
	end

	# run simulation
	p1   = Param(2,opts)
	m1   = Model(p1)
	solve!(m1,p1)
	sim1 = simulate(m1,p1)

	# some output form this policy:
	# throw away incomplete cohorts
	sim1 = @where(sim1,!isna(:cohort))
	pol1_out = policyOutput(sim1,"lumpsum_age20")

	m1 = 0
	sim1 = 0
	gc()


	println("experiment with within age redistribution")
	# model under redistribution policy 2: lump sum from owners of age group
	# -----------------------------------------------------------------

	opts = ["policy" => "mortgageSubsidy","redistribute" => Redist2 ,"verbose"=>0]

	# utility equalizing consumption scaling:
	if ctax
		ctax2 = findctax(w0[1][1],opts)
	else
		ctax2 = 0
	end

	# some output form this policy:
	p2 = Param(2,opts)
	m2 = Model(p2)	# 1.5 secs
	solve!(m2,p2)
	sim2 = simulate(m2,p2)
	# throw away incomplete cohorts
	sim2 = @where(sim2,(!isna(:cohort)) )
	pol2_out = policyOutput(sim2,"lumpsum_by_age")


	println("experiment with per capita redistribution")
	# model under redistribution policy 3: per capita redistribution
	# -----------------------------------------------------------------

	opts = ["policy" => "mortgageSubsidy","redistribute" => Redist3 ,"verbose"=>0]

	# utility equalizing consumption scaling:
	if ctax
		ctax3 = findctax(w0[1][1],opts)
	else
		ctax3 = 0
	end

	# some output form this policy:
	p3 = Param(2,opts)
	m3 = Model(p3)	# 1.5 secs
	solve!(m3,p3)
	sim3 = simulate(m3,p3)
	# throw away incomplete cohorts
	sim3 = @where(sim3,(!isna(:cohort)) )
	pol3_out = policyOutput(sim3,"lumpsum_per_capita")


	println("with price adjustment")
	# model under redistribution policy 4: per capita redistribution and house prices downward adjust by 5%
	# 5% comes from http://kamilasommer.net/Taxes.pdf
	# -----------------------------------------------------------------

	opts = ["policy" => "mortgageSubsidy_padjust","redistribute" => Redist3 ,"verbose"=>0,"shockVal"=>[0.95]]

	# utility equalizing consumption scaling:
	if ctax
		ctax4 = findctax(w0[1][1],opts)
	else
		ctax4 = 0
	end

	# some output form this policy:
	p4 = Param(2,opts)
	m4 = Model(p4)	# 1.5 secs
	solve!(m4,p4)
	sim4 = simulate(m4,p4)
	# throw away incomplete cohorts
	sim4 = @where(sim4,(!isna(:cohort)) )
	pol4_out = policyOutput(sim4,"price_adjust")

	# create a dataframe with all pol results by age stacked
	own_move = vcat(base_out["own_move"],pol0_out["own_move"],pol1_out["own_move"],pol2_out["own_move"],pol3_out["own_move"],pol4_out["own_move"])
	# create a dataframe with all pol results by age stacked
	move_own_age = vcat(base_out["move_own_age"],pol0_out["move_own_age"],pol1_out["move_own_age"],pol2_out["move_own_age"],pol3_out["move_own_age"],pol4_out["move_own_age"])


	# out dict
	d = Dict()
	d["own"] = ["base" => base_out["own_move"][:own][1], "burn" => pol0_out["own_move"][:own][1], "redist1" => pol1_out["own_move"][:own][1], "redist2" => pol2_out["own_move"][:own][1],"redist3" => pol3_out["own_move"][:own][1],"redist4" => pol4_out["own_move"][:own][1]]
	d["move"] = ["base" => base_out["own_move"][:move][1], "burn" => pol0_out["own_move"][:move][1], "redist1" => pol1_out["own_move"][:move][1], "redist2" => pol2_out["own_move"][:move][1],"redist3" => pol3_out["own_move"][:move][1],"redist4" => pol4_out["own_move"][:move][1]]
	d["income"] = ["base" => base_out["own_move"][:income][1], "burn" => pol0_out["own_move"][:income][1], "redist1" => pol1_out["own_move"][:income][1], "redist2" => pol2_out["own_move"][:income][1],"redist3" => pol3_out["own_move"][:income][1],"redist4" => pol4_out["own_move"][:income][1]]
	d["q10"] = ["base" => base_out["own_move"][:q10][1], "burn" => pol0_out["own_move"][:q10][1], "redist1" => pol1_out["own_move"][:q10][1], "redist2" => pol2_out["own_move"][:q10][1],"redist3" => pol3_out["own_move"][:q10][1],"redist4" => pol4_out["own_move"][:q10][1]]
	d["q50"] = ["base" => base_out["own_move"][:q50][1], "burn" => pol0_out["own_move"][:q50][1], "redist1" => pol1_out["own_move"][:q50][1], "redist2" => pol2_out["own_move"][:q50][1],"redist3" => pol3_out["own_move"][:q50][1],"redist4" => pol4_out["own_move"][:q50][1]]
	d["own_inc1"] = ["base" => base_out["own_inc"][1,:own], "burn" => pol0_out["own_inc"][1,:own], "redist1" => pol1_out["own_inc"][1,:own], "redist2" => pol2_out["own_inc"][1,:own],"redist3" => pol3_out["own_inc"][1,:own],"redist4" => pol4_out["own_inc"][1,:own]]
	d["val_inc1"] = ["base" => base_out["own_inc"][1,:value], "burn" => pol0_out["own_inc"][1,:value], "redist1" => pol1_out["own_inc"][1,:value], "redist2" => pol2_out["own_inc"][1,:value],"redist3" => pol3_out["own_inc"][1,:value],"redist4" => pol4_out["own_inc"][1,:value]]
	d["own_inc2"] = ["base" => base_out["own_inc"][2,:own], "burn" => pol0_out["own_inc"][2,:own], "redist1" => pol1_out["own_inc"][2,:own], "redist2" => pol2_out["own_inc"][2,:own],"redist3" => pol3_out["own_inc"][2,:own],"redist4" => pol4_out["own_inc"][2,:own]]
	d["val_inc2"] = ["base" => base_out["own_inc"][2,:value], "burn" => pol0_out["own_inc"][2,:value], "redist1" => pol1_out["own_inc"][1,:value], "redist2" => pol2_out["own_inc"][1,:value],"redist3" => pol3_out["own_inc"][2,:value],"redist4" => pol4_out["own_inc"][2,:value]]
	d["own_inc3"] = ["base" => base_out["own_inc"][3,:own], "burn" => pol0_out["own_inc"][3,:own], "redist1" => pol1_out["own_inc"][3,:own], "redist2" => pol2_out["own_inc"][3,:own],"redist3" => pol3_out["own_inc"][3,:own],"redist4" => pol4_out["own_inc"][3,:own]]
	d["val_inc3"] = ["base" => base_out["own_inc"][3,:value], "burn" => pol0_out["own_inc"][3,:value], "redist1" => pol1_out["own_inc"][1,:value], "redist2" => pol2_out["own_inc"][1,:value],"redist3" => pol3_out["own_inc"][3,:value],"redist4" => pol4_out["own_inc"][3,:value]]
	d["assets"] = ["base" => base_out["own_move"][:assets][1], "burn" => pol0_out["own_move"][:assets][1], "redist1" => pol1_out["own_move"][:assets][1], "redist2" => pol2_out["own_move"][:assets][1],"redist3" => pol3_out["own_move"][:assets][1],"redist4" => pol4_out["own_move"][:assets][1]]
	d["p_own"] = Dict()
	d["p_move"] = Dict()
	for (k,v) in d["own"] 
		d["p_own"][k] = 100 * (v - d["own"]["base"]) / d["own"]["base"]
	end
	for (k,v) in d["move"] 
		d["p_move"][k] = 100 * (v - d["move"]["base"]) / d["move"]["base"]
	end
	d["delta"] = ["base" => 0.0, "burn" => ctax0.minimum, "redist1" => ctax1.minimum, "redist2" => ctax2.minimum,"redist3" => ctax3.minimum,"redist4" => ctax4.minimum]

	indir, outdir = mig.setPaths()
	f = open(joinpath(outdir,"exp_Mortgage","morgage.json"),"w")
	JSON.print(f,d)
	close(f)


	# return
	out = ["Receipts" => Tot_tax, "base_out" => base_out, "pol0_out" => pol0_out, "Redistributions" => ["R0" => Redist0,"R1" => Redist1,"R2" => Redist2,"R3" => Redist3], "Receipts_age"=>Tot_tax_age, "npv_age_income"=>npv_age_income,"ctax0" => ctax0, "ctax1" => ctax1, "pol1_out" =>pol1_out, "ctax2" => ctax2,  "ctax3" => ctax3, "ctax4" => ctax4,"pol2_out"=> pol2_out, "pol3_out"=> pol3_out, "pol4_out"=> pol4_out, "move_own" => own_move, "move_own_age" => move_own_age, "summary" => d]

	return out

end



function selectPolicy(which::ASCIIString,j::Int,shockYear::Int,p::Param)

	# shocks p at shockAge for ever after
	if which=="pshock"
		opts = ["policy" => which,"shockRegion" => j,"shockYear"=>shockYear,"shockAge"=>1, "shockVal"=> repeat([0.7],inner=[1],outer=[p.nt-1])]
	# shocks p at shockAge for the next 3 periods reverting back to trend afterwards
	elseif which=="pshock3"
		opts = ["policy" => "pshock","shockRegion" => j,"shockYear"=>shockYear,"shockAge"=>1, "shockVal"=> [0.7,0.8,0.9,repeat([1.0],inner=[1],outer=[p.nt-3])]]
	elseif which=="yshock3"
		opts = ["policy" => "yshock","shockRegion" => j,"shockYear"=>shockYear,"shockAge"=>1, "shockVal"=> [0.7,0.8,0.9,repeat([1.0],inner=[1],outer=[p.nt-3])]]
	elseif which=="yshock"
		opts = ["policy" => "yshock","shockRegion" => j,"shockYear"=>shockYear,"shockAge"=>1, "shockVal"=> repeat([0.9],inner=[1],outer=[p.nt-1])]

	elseif which=="highMC"
		opts = ["policy" => which,"shockRegion" => j,"shockYear"=>shockYear,"shockAge"=>1, "shockVal"=> ones(p.nt-1)]
	elseif which=="pshock_highMC"
		opts = ["policy" => which,"shockRegion" => j,"shockYear"=>shockYear,"shockAge"=>1, "shockVal"=> repeat([0.7],inner=[1],outer=[p.nt-1])]
	elseif which=="yshock_highMC"
		opts = ["policy" => which,"shockRegion" => j,"shockYear"=>shockYear,"shockAge"=>1, "shockVal"=> repeat([0.9],inner=[1],outer=[p.nt-1])]
	elseif which=="pshock_noBuying"
		opts = ["policy" => which,"shockRegion" => j,"shockYear"=>shockYear,"shockAge"=>1, "shockVal"=> repeat([0.7],inner=[1],outer=[p.nt-1])]
	elseif which=="pshock_noSaving"
		opts = ["policy" => which,"shockRegion" => j,"shockYear"=>shockYear,"shockAge"=>1, "shockVal"=> repeat([0.7],inner=[1],outer=[p.nt-1])]
	else 
		throw(ArgumentError("invalid policy $which selected"))
	end
	return opts

end

function valdiff_pshock_highMC(ctax::Float64,v0::Float64,opts::Dict,pmv_id::DataFrame)

	# change value of ctax on options dict
	opts["ctax"] = ctax
		println("current ctax level = $ctax")

	# and recompute
	p0 = exp_shockRegion(opts);
	p = p0[3];
	p0 = 0
	gc()
	pmv = p[findin(p[:id],pmv_id[:id]),:]

	pmv2 = @select(@where(pmv,(:year.>2006)&(!:move)),v=mean(:v),a=mean(:a.data,WeightVec(:density.data)),w=mean(:wealth.data,WeightVec(:density.data)),cons=mean(:cons.data,WeightVec(:density.data)),h=mean(:h.data,WeightVec(:density.data)))

	# baseline value
	v1 = pmv2[:v][1]

	println("baseline value is $(round(v0,2))")
	println("current value from $(opts["policy"]) is $(round(v1,2))")
	println("current difference is $(v1 - v0)")

	return (v1 - v0).^2
end

function pshock_highMC_cdiff()

	par = Param(2)
	opts = selectPolicy("pshock",6,2007,par)
	p0 = exp_shockRegion(opts);

	# returns a triple. third element is a data.frame with policy results.
	p = p0[3];
	p0 = 0
	gc()
	# people who moved away from j=6 after shock hits in 2007
	pmv_id = @select(@where(p,(:year.>2006)&(:move)&(:j.==6)),id=unique(:id))
	pmv = p[findin(p[:id],pmv_id[:id]),:]

	pmv2 = @select(@where(pmv,(:year.>2006)&(!:move)),v=mean(:v),a=mean(:a.data,WeightVec(:density.data)),w=mean(:wealth.data,WeightVec(:density.data)),cons=mean(:cons.data,WeightVec(:density.data)),h=mean(:h.data,WeightVec(:density.data)))

	# baseline value
	v0 = pmv2[:v][1]

	opts["policy"] = "pshock_highMC"

	ctax = optimize((x)->valdiff_pshock_highMC(x,v0,opts,pmv_id),1.0,2.0,show_trace=true,method=:brent,iterations=10)
	return ctax

end

# VALUE OF MIGRATION
# ==================

# find ctax of baseline vs highMC
function find_ctax_value_mig_base(j::Int)

	cutyr = 1997 - 1

	# baseline model
	p = Param(2)
	m = Model(p)
	solve!(m,p)
	base = simulate(m,p);
	base = base[!isna(base[:cohort]),:];
	w0   = getDiscountedValue(@where(base,(:j.==j)&(:year.>cutyr)),p,m)

	ctax = optimize((x)->vdiff_value_mig_base(x,w0[1,1],j),0.5,1.5,show_trace=true,method=:brent)

end

function vdiff_value_mig_base(ctax::Float64,w0::Float64,j::Int)

	# look at results after full cohorts available
	cutyr = 1997 - 1

	println("current ctax = $ctax")

	# model where moving is shut down in region j
	opts = ["policy" => "highMC", "shockRegion" => j]
	p2 = Param(2,opts)
	setfield!(p2,:ctax,ctax)
	m2 = Model(p2)
	solve!(m2,p2)
	pol = simulate(m2,p2);
	pol = pol[!isna(pol[:cohort]),:];
	w1   = getDiscountedValue(@where(pol,(:j.==j)&(:year.>cutyr)),p2,m2)
	(w1[1,1] - w0)^2
end




# compares baesline with highMC
# differences in utility if moving in region j
# is shut down.
function exp_value_mig_base(j::Int)

	# look at results after full cohorts available
	cutyr = 1997 - 1

	# baseline model
	p = Param(2)
	m = Model(p)
	solve!(m,p)
	base = simulate(m,p);
	base = base[!isna(base[:cohort]),:];

	regname = m.regnames[j,:Division]

	w0   = getDiscountedValue(@where(base,(:j.==j)&(:year.>cutyr)),p,m,true)

	# model where moving is shut down in region j
	opts = ["policy" => "highMC", "shockRegion" => j]
	p2 = Param(2,opts)
	m2 = Model(p2)
	solve!(m2,p2)

	pol = simulate(m2,p2);
	pol = pol[!isna(pol[:cohort]),:];

	w1   = getDiscountedValue(@where(pol,(:j.==j)&(:year.>cutyr)),p,m,true)

	# get values by age and for different regions
	v0   = @> begin
		base
		@where((:j.!=j)&(:year.>cutyr)&(!:move))
		@by(:realage,v=mean(:v))
		@transform(region="outside of $regname",regime="baseline")
	end
	v1   = @> begin
		pol
		@where((:j.!=j)&(:year.>cutyr)&(!:move))
		@by(:realage,v=mean(:v))
		@transform(region="outside of $regname",regime="noMove")
	end
	v01 = vcat(v0,v1)
	v0j   = @> begin
		base
		@where((:j.==j)&(:year.>cutyr)&(!:move))
		@by(:realage,v=mean(:v))
		@transform(region="inside of $regname",regime="baseline")
	end
	v1j   = @> begin
		pol
		@where((:j.==j)&(:year.>cutyr)&(!:move))
		@by(:realage,v=mean(:v))
		@transform(region="inside of $regname",regime="noMove")
	end
	v01j = vcat(v0,v1,v0j,v1j)






	# compare the ones who did move with their virtual counterparts
	# =============================================================

	# people who moved away from j in the baseline
	mv_id = @select(@where(base,(:year.>cutyr)&(:move)&(:j.==j)),id=unique(:id))
	# own_mv_id = @select(@where(base,(:year.>cutyr)&(:move)&(:j.==j)&(:h.==1)),id=unique(:id))
	# rent_mv_id = @select(@where(base,(:year.>cutyr)&(:move)&(:j.==j)&(:h.==0)),id=unique(:id))
	
	# own_bmv = base[findin(base[:id],own_mv_id[:id]),:]
	# rent_bmv = base[findin(base[:id],rent_mv_id[:id]),:]
	# own_bmv2 = @where(own_bmv,:year.>cutyr)
	# own_bmv2 = @transform(own_bmv2,row_idx=1:size(own_bmv2,1))
	# rent_bmv2 = @where(rent_bmv,:year.>cutyr)
	# rent_bmv2 = @transform(rent_bmv2,row_idx=1:size(rent_bmv2,1))

	# own_pmv = pol[findin(pol[:id],own_mv_id[:id]),:];
	# own_pmv = @transform(own_pmv,buy = (:h.==0)&(:hh.==1))
	# own_pmv2 = @where(own_pmv,:year.>cutyr)
	# own_pmv2 = @transform(own_pmv2,row_idx=1:size(own_pmv2,1))

	# rent_pmv = pol[findin(pol[:id],rent_mv_id[:id]),:];
	# rent_pmv = @transform(rent_pmv,buy = (:h.==0)&(:hh.==1))
	# rent_pmv2 = @where(rent_pmv,:year.>cutyr)
	# rent_pmv2 = @transform(rent_pmv2,row_idx=1:size(rent_pmv2,1))

	bmv = base[findin(base[:id],mv_id[:id]),:]
	bmv = @transform(bmv,buy = (:h.==0)&(:hh.==1))

	bmv2 = @where(bmv,:year.>cutyr)
	bmv2 = @transform(bmv2,row_idx=1:size(bmv2,1))

	# find those guys in the policy environment
	pmv = pol[findin(pol[:id],mv_id[:id]),:];
	pmv = @transform(pmv,buy = (:h.==0)&(:hh.==1))
	pmv2 = @where(pmv,:year.>cutyr)
	pmv2 = @transform(pmv2,row_idx=1:size(pmv2,1))



	# out string
	ostr = string("noMove",j,"mig_value_baseline.json")

	# this ctax gives equal values for
	# pmv4[:value] and hmv4[:value] below
	# opts["ctax"] = 1.13

	# what's the first observationin j=6?
	# drop all periods before first in cutyr
	drops = Int[]
	for i in unique(pmv2[:id])
		goon = true
		ti = minimum(@where(pmv2,:id.==i)[:age])
		maxage = maximum(@where(pmv2,:id.==i)[:age])
		while goon
			if pmv2[(pmv2[:id].==i)&(pmv2[:age].==ti),:j][1] != j
				push!(drops,pmv2[(pmv2[:id].==i)&(pmv2[:age].==ti),:row_idx][1])
				if ti == maxage
					goon=false
				else
					ti+=1
				end
			else
				goon = false
			end
		end
	end

	pmv3 = pmv2[setdiff(1:size(pmv2,1),drops),:]
	bmv3 = bmv2[setdiff(1:size(bmv2,1),drops),:]

	# want to separate people by ownership status in period of moving


	bmv4 = @select(@where(bmv3,(:year.>cutyr)&(!:move)),v=mean(:v),a=mean(:a.data,WeightVec(:density.data)),inc=mean(:income.data,WeightVec(:density.data)),w=mean(:wealth.data,WeightVec(:density.data)),cons=mean(:cons.data,WeightVec(:density.data)),p=mean(:p.data,WeightVec(:density.data)),y=mean(:y.data,WeightVec(:density.data)),h=mean(:h.data,WeightVec(:density.data)),buy=mean(:buy.data,WeightVec(:density.data)))
	# bmv4 = @by(@where(bmv3,(:year.>cutyr)&(!:move)),:own,v=mean(:v),a=mean(:a.data,WeightVec(:density.data)),inc=mean(:income.data,WeightVec(:density.data)),w=mean(:wealth.data,WeightVec(:density.data)),cons=mean(:cons.data,WeightVec(:density.data)),p=mean(:p.data,WeightVec(:density.data)),y=mean(:y.data,WeightVec(:density.data)),h=mean(:h.data,WeightVec(:density.data)),buy=mean(:buy.data,WeightVec(:density.data)))
	pmv4 = @select(@where(pmv3,(:year.>cutyr)&(!:move)),v=mean(:v),a=mean(:a.data,WeightVec(:density.data)),inc=mean(:income.data,WeightVec(:density.data)),w=mean(:wealth.data,WeightVec(:density.data)),cons=mean(:cons.data,WeightVec(:density.data)),p=mean(:p.data,WeightVec(:density.data)),y=mean(:y.data,WeightVec(:density.data)),h=mean(:h.data,WeightVec(:density.data)),buy=mean(:buy.data,WeightVec(:density.data)))

	# get counts of owners in j by year
	counts0 = @> begin
			base	
			@where((:j.==j) & (:year.>cutyr))
			@by(:year,n_all = length(:own) ,n_rent = sum(!(:own)) , n_own=sum(:own) )
		end
	counts1 = @> begin
			pol	
			@where((:j.==j) & (:year.>cutyr))
			@by(:year,n_all = length(:own) ,n_rent = sum(!(:own)) , n_own=sum(:own) )
		end


	# get immigration rates for after cutyr
	# uses data on everybody outside of j
	b_in = @> begin
			base
			@where((:year.>cutyr)&(:j.!=j))
			@transform(mig = (:moveto.==j),mig_own = (:moveto.==j).*(:own),mig_rent = (:moveto.==j).*(!:own))
		end
	b_in = join(b_in,counts0,on=:year);
	b_in = @> begin
			b_in
			@by(:year,mig = 100 .* mean(:mig), mig_own = 100 .* mean(:mig_own), mig_rent = 100 .* mean(:mig_rent), rel_mig = 100 .* sum(:mig)./:n_all[1], rel_mig_own = 100 .* sum(:mig_own)./:n_own[1], rel_mig_rent = 100 .* sum(:mig_rent)./:n_rent[1], abs_mig = sum(:mig), abs_mig_own = sum(:mig_own), abs_mig_rent =sum(:mig_rent))
		end

	p_in = @> begin
			pol
			@where((:year.>cutyr)&(:j.!=j))
			@transform(mig = (:moveto.==j),mig_own = (:moveto.==j).*(:own),mig_rent = (:moveto.==j).*(!:own))
		end
	p_in = join(p_in,counts1,on=:year);
	p_in = @> begin
			p_in
			@by(:year,mig = 100 .* mean(:mig), mig_own = 100 .* mean(:mig_own), mig_rent = 100 .* mean(:mig_rent), rel_mig = 100 .* sum(:mig)./:n_all[1], rel_mig_own = 100 .* sum(:mig_own)./:n_own[1], rel_mig_rent = 100 .* sum(:mig_rent)./:n_rent[1], abs_mig = sum(:mig), abs_mig_own = sum(:mig_own), abs_mig_rent =sum(:mig_rent))
		end

	# get outmigration rates for after cutyr
	# uses data on everybody in j
	b_out = @> begin
			base
			@where((:year.>cutyr)&(:j.==j))
			@transform(mig_own = (:move).*(:own),mig_rent = (:move).*(!:own))
			@select(mig = 100 .* mean(:move.data,WeightVec(:density.data)), mig_own = 100 .* mean(:mig_own.data,WeightVec(:density.data)),mig_rent = 100 .* mean(:mig_rent.data,WeightVec(:density.data)), rel_mig_own = 100 .* sum(:mig_own.data,WeightVec(:density.data))./ sum(:own,WeightVec(:density.data)),rel_mig_rent = 100 .* sum(:mig_rent.data,WeightVec(:density.data))./ sum(!:own,WeightVec(:density.data)))
		end

	p_out = @> begin
			pol
			@where((:year.>cutyr)&(:j.==j))
			@transform(mig_own = (:move).*(:own),mig_rent = (:move).*(!:own))
			@select(mig = 100 .* mean(:move.data,WeightVec(:density.data)), mig_own = 100 .* mean(:mig_own.data,WeightVec(:density.data)),mig_rent = 100 .* mean(:mig_rent.data,WeightVec(:density.data)), rel_mig_own = 100 .* sum(:mig_own.data,WeightVec(:density.data))./ sum(:own,WeightVec(:density.data)),rel_mig_rent = 100 .* sum(:mig_rent.data,WeightVec(:density.data))./ sum(!:own,WeightVec(:density.data)))
		end


    # output dict
    # ===========

	d=Dict()
	ss = "noMove"
	d["EV"] = Dict()
	d["EV"] = ["base" => w0[1,1], ss => w1[1,1], "pct" => 100*(w1[1,1] - w0[1,1])/w0[1,1] ]
	d["flows"] = Dict()
	d["flows"]["inmig"]       = ["base" => b_in[:mig][1],     ss =>p_in[:mig][1],       "pct" => 100*(p_in[:mig][1] - b_in[:mig][1])/b_in[:mig][1] ]
	d["flows"]["inmig_rel"]   = ["base" => b_in[:rel_mig][1],     ss =>p_in[:rel_mig][1],       "pct" => 100*(p_in[:rel_mig][1] - b_in[:rel_mig][1])/b_in[:rel_mig][1] ]
	d["flows"]["inmig_own"]   = ["base" => b_in[:mig_own][1], ss =>p_in[:mig_own][1],   "pct" => 100*(p_in[:mig_own][1] - b_in[:mig_own][1])/b_in[:mig_own][1] ]
	d["flows"]["inmig_own_rel"]   = ["base" => b_in[:rel_mig_own][1], ss =>p_in[:rel_mig_own][1],   "pct" => 100*(p_in[:rel_mig_own][1] - b_in[:rel_mig_own][1])/b_in[:rel_mig_own][1] ]
	d["flows"]["inmig_rent"]  = ["base" => b_in[:mig_rent][1], ss =>p_in[:mig_rent][1], "pct" => 100*(p_in[:mig_rent][1] - b_in[:mig_rent][1])/b_in[:mig_rent][1] ]
	d["flows"]["inmig_rent_rel"]   = ["base" => b_in[:rel_mig_rent][1], ss =>p_in[:rel_mig_rent][1],   "pct" => 100*(p_in[:rel_mig_rent][1] - b_in[:rel_mig_rent][1])/b_in[:rel_mig_rent][1] ]
	d["flows"]["outmig"]      = ["base" => b_out[:mig][1],     ss =>p_out[:mig][1]      ,"pct" => 100*(p_out[:mig][1] - b_out[:mig][1])/b_out[:mig][1] ]
	d["flows"]["outmig_own"]  = ["base" => b_out[:mig_own][1], ss =>p_out[:mig_own][1]  ,"pct" => 100*(p_out[:mig_own][1] - b_out[:mig_own][1])/b_out[:mig_own][1] ]
	d["flows"]["outmig_own_rel"]   = ["base" => b_out[:rel_mig_own][1], ss =>p_out[:rel_mig_own][1],   "pct" => 100*(p_out[:rel_mig_own][1] - b_out[:rel_mig_own][1])/b_out[:rel_mig_own][1] ]
	d["flows"]["outmig_rent"] = ["base" => b_out[:mig_rent][1], ss =>p_out[:mig_rent][1],"pct" => 100*(p_out[:mig_rent][1] - b_out[:mig_rent][1])/b_out[:mig_rent][1] ]
	d["flows"]["outmig_rent_rel"]   = ["base" => b_out[:rel_mig_rent][1], ss =>p_out[:rel_mig_rent][1],   "pct" => 100*(p_out[:rel_mig_rent][1] - b_out[:rel_mig_rent][1])/b_out[:rel_mig_rent][1] ]

	d["movers"] = Dict()
	d["movers"]["v"] = ["base" => bmv4[:v][1],    ss => pmv4[:v][1], "pct" => 100*(pmv4[:v][1] - bmv4[:v][1])/bmv4[:v][1] ]
	d["movers"]["a"] = ["base" => bmv4[:a][1],    ss => pmv4[:a][1], "pct" => 100*(pmv4[:a][1] - bmv4[:a][1])/bmv4[:a][1] ]
	d["movers"]["inc"] = ["base" => bmv4[:inc][1],    ss => pmv4[:inc][1], "pct" => 100*(pmv4[:inc][1] - bmv4[:inc][1])/bmv4[:inc][1] ]
	d["movers"]["y"] = ["base" => bmv4[:y][1],    ss => pmv4[:y][1],  "pct" => 100*(pmv4[:y][1] - bmv4[:y][1])/bmv4[:y][1] ]
	d["movers"]["p"] = ["base" => bmv4[:p][1],    ss => pmv4[:p][1],  "pct" => 100*(pmv4[:p][1] - bmv4[:p][1])/bmv4[:p][1] ]
	d["movers"]["inc"] = ["base" => bmv4[:inc][1],    ss => pmv4[:inc][1], "pct" => 100*(pmv4[:inc][1] - bmv4[:inc][1])/bmv4[:inc][1] ]
	d["movers"]["w"] = ["base" => bmv4[:w][1],    ss => pmv4[:w][1],  "pct" => 100*(pmv4[:w][1] - bmv4[:w][1])/bmv4[:w][1] ]
	d["movers"]["c"] = ["base" => bmv4[:cons][1], ss => pmv4[:cons][1], "pct" => 100*(pmv4[:cons][1] - bmv4[:cons][1])/bmv4[:cons][1] ]
	d["movers"]["h"] = ["base" => bmv4[:h][1],    ss => pmv4[:h][1],  "pct" => 100*(pmv4[:h][1] - bmv4[:h][1])/bmv4[:h][1] ]


	indir, outdir = mig.setPaths()
	ostr = string("noMove",j,"mig_value_baseline.json")
	f = open(joinpath(outdir,ostr),"w")
	JSON.print(f,d)
	close(f)

	# save csvs
	fi = readdir(outdir)
	if !in(string("noMove",j),fi)
		mkpath(string(joinpath(outdir,string("noMove",j))))
	end
	writetable(joinpath(outdir,string("noMove",j),"values.csv"),v01j)

	return d

end





# compares pshock with pshcock_highMC
# ss = {"pshock","yshock"}
function exp_value_mig(ss::ASCIIString,j::Int,yr::Int)

	par = Param(2)
	opts = selectPolicy(ss,j,yr,par)
	pr = exp_shockRegion(opts);
	base = pr[2];
	p = pr[3];

	# release memory
	pr = 0
	gc()

	if ss=="pshock"
		opts = selectPolicy("pshock_highMC",j,yr,par)
	else 
		opts = selectPolicy("yshock_highMC",j,yr,par)
	end
	ostr = string(ss,j,"mig_value.json")

	# this ctax gives equal values for
	# pmv4[:value] and hmv4[:value] below
	# opts["ctax"] = 1.13

	h = exp_shockRegion(opts);
	h = h[3];

	cutyr = yr - 1

	# people who moved away from j after shock hits in yr 
	# this includes people who move even in the baseline!

	pmv_id = @select(@where(p,(:year.>cutyr)&(:move)&(:j.==j)),id=unique(:id))
	pmv = p[findin(p[:id],pmv_id[:id]),:]
	bmv = base[findin(base[:id],pmv_id[:id]),:]
	pmv = @transform(pmv,buy = (:h.==0)&(:hh.==1))
	bmv = @transform(bmv,buy = (:h.==0)&(:hh.==1))

	pmv3 = @where(pmv,:year .> cutyr)
	pmv3 = @transform(pmv3,row_idx=1:size(pmv3,1))

	bmv3 = @where(bmv,:year .> cutyr)
	bmv3 = @transform(bmv3,row_idx=1:size(bmv3,1))

	hmv = h[findin(	h[:id],pmv_id[:id]),:]
	hmv = @transform(hmv,buy = (:h.==0)&(:hh.==1))
	hmv3 = @where(hmv,:year .> cutyr)
	hmv3 = @transform(hmv3,row_idx=1:size(hmv3,1))
	# what's the first observationin j=6?
	# drop all periods before first in cutyr
	drops = Int[]
	for i in unique(pmv3[:id])
		goon = true
		ti = minimum(@where(pmv3,:id.==i)[:age])
		maxage = maximum(@where(pmv3,:id.==i)[:age])
		while goon
			if pmv3[(pmv3[:id].==i)&(pmv3[:age].==ti),:j][1] != j
				push!(drops,pmv3[(pmv3[:id].==i)&(pmv3[:age].==ti),:row_idx][1])
				if ti == maxage
					goon=false
				else
					ti+=1
				end
			else
				goon = false
			end
		end
	end

	pmv3 = pmv3[setdiff(1:size(pmv3,1),drops),:]
	bmv3 = bmv3[setdiff(1:size(bmv3,1),drops),:]
	hmv3 = hmv3[setdiff(1:size(hmv3,1),drops),:]

	bmv4 = @select(@where(bmv3,(:year.>cutyr)&(!:move)),v=mean(:v),a=mean(:a.data,WeightVec(:density.data)),w=mean(:wealth.data,WeightVec(:density.data)),cons=mean(:cons.data,WeightVec(:density.data)),h=mean(:h.data,WeightVec(:density.data)),buy=mean(:buy))
	pmv4 = @select(@where(pmv3,(:year.>cutyr)&(!:move)),v=mean(:v),a=mean(:a.data,WeightVec(:density.data)),w=mean(:wealth.data,WeightVec(:density.data)),cons=mean(:cons.data,WeightVec(:density.data)),h=mean(:h.data,WeightVec(:density.data)),buy=mean(:buy))
	hmv4 = @select(@where(hmv3,(:year.>cutyr)&(!:move)),v=mean(:v),a=mean(:a.data,WeightVec(:density.data)),w=mean(:wealth.data,WeightVec(:density.data)),cons=mean(:cons.data,WeightVec(:density.data)),h=mean(:h.data,WeightVec(:density.data)),buy=mean(:buy))

	# get immigration rates for after cutyr
	# uses data on everybody outside of j
	b_in = @> begin
			base
			@where((:year.>cutyr)&(:j.!=j))
			@transform(mig = (:moveto.==j),mig_own = (:moveto.==j).*(:own),mig_rent = (:moveto.==j).*(!:own))
			@select(mig = 100 .* mean(:mig.data,WeightVec(:density.data)), mig_own = 100 .* mean(:mig_own.data,WeightVec(:density.data)), mig_rent = 100 .* mean(:mig_rent.data,WeightVec(:density.data)))
		end

	p_in = @> begin
			p
			@where((:year.>cutyr)&(:j.!=j))
			@transform(mig = (:moveto.==j),mig_own = (:moveto.==j).*(:own),mig_rent = (:moveto.==j).*(!:own))
			@select(mig = 100 .* mean(:mig.data,WeightVec(:density.data)), mig_own = 100 .* mean(:mig_own.data,WeightVec(:density.data)), mig_rent = 100 .* mean(:mig_rent.data,WeightVec(:density.data)))
		end

	h_in = @> begin
			h
			@where((:year.>cutyr)&(:j.!=j))
			@transform(mig = (:moveto.==j),mig_own = (:moveto.==j).*(:own),mig_rent = (:moveto.==j).*(!:own))
			@select(mig = 100 .* mean(:mig.data,WeightVec(:density.data)), mig_own = 100 .* mean(:mig_own.data,WeightVec(:density.data)), mig_rent = 100 .* mean(:mig_rent.data,WeightVec(:density.data)))
		end

	# get outmigration rates for after cutyr
	# uses data on those who would have migrated after pshock
	b_out = @> begin
			base
			@where((:year.>cutyr)&(:j.==j))
			@transform(mig_own = (:move).*(:own),mig_rent = (:move).*(!:own))
			@select(mig = 100 .* mean(:move.data,WeightVec(:density.data)), mig_own = 100 .* mean(:mig_own.data,WeightVec(:density.data)), mig_rent = 100 .* mean(:mig_rent.data,WeightVec(:density.data)))
		end

	p_out = @> begin
			p
			@where((:year.>cutyr)&(:j.==j))
			@transform(mig_own = (:move).*(:own),mig_rent = (:move).*(!:own))
			@select(mig = 100 .* mean(:move.data,WeightVec(:density.data)), mig_own = 100 .* mean(:mig_own.data,WeightVec(:density.data)), mig_rent = 100 .* mean(:mig_rent.data,WeightVec(:density.data)))
		end

	h_out = @> begin
			h
			@where((:year.>cutyr)&(:j.==j))
			@transform(mig_own = (:move).*(:own),mig_rent = (:move).*(!:own))
			@select(mig = 100 .* mean(:move.data,WeightVec(:density.data)), mig_own = 100 .* mean(:mig_own.data,WeightVec(:density.data)), mig_rent = 100 .* mean(:mig_rent.data,WeightVec(:density.data)))
		end

	d=Dict()
	d["inmig"]  = ["base" => b_in[:mig][1], ss =>p_in[:mig][1], "nomove"=> h_in[:mig][1], "pct" => 100*(p_in[:mig][1] - h_in[:mig][1])/h_in[:mig][1] ]
	d["inmig_own"]   = ["base" => b_in[:mig_own][1], ss =>p_in[:mig_own][1], "nomove"=> h_in[:mig_own][1], "pct" => 100*(p_in[:mig_own][1] - h_in[:mig_own][1])/h_in[:mig_own][1] ]
	d["inmig_rent"]  = ["base" => b_in[:mig_rent][1], ss =>p_in[:mig_rent][1], "nomove"=> h_in[:mig_rent][1], "pct" => 100*(p_in[:mig_rent][1] - h_in[:mig_rent][1])/h_in[:mig_rent][1] ]
	d["outmig"] = ["base" => b_out[:mig][1], ss =>p_out[:mig][1], "nomove"=> h_out[:mig][1]]
	d["outmig_own"] = ["base" => b_out[:mig_own][1], ss =>p_out[:mig_own][1], "nomove"=> h_out[:mig_own][1]]
	d["outmig_rent"] = ["base" => b_out[:mig_rent][1], ss =>p_out[:mig_rent][1], "nomove"=> h_out[:mig_rent][1] ]

	d["v"] = ["base" => bmv4[:v][1],    ss => pmv4[:v][1],    "nomove"=>hmv4[:v][1], "pct" => 100*(pmv4[:v][1] - hmv4[:v][1])/hmv4[:v][1] ]
	d["a"] = ["base" => bmv4[:a][1],    ss => pmv4[:a][1],    "nomove"=>hmv4[:a][1], "pct" => 100*(pmv4[:a][1] - hmv4[:a][1])/hmv4[:a][1] ]
	d["w"] = ["base" => bmv4[:w][1],    ss => pmv4[:w][1],    "nomove"=>hmv4[:w][1], "pct" => 100*(pmv4[:w][1] - hmv4[:w][1])/hmv4[:w][1] ]
	d["c"] = ["base" => bmv4[:cons][1], ss => pmv4[:cons][1], "nomove"=>hmv4[:cons][1], "pct" => 100*(pmv4[:cons][1] - hmv4[:cons][1])/hmv4[:cons][1] ]
	d["h"] = ["base" => bmv4[:h][1],    ss => pmv4[:h][1],    "nomove"=>hmv4[:h][1], "pct" => 100*(pmv4[:h][1] - hmv4[:h][1])/hmv4[:h][1] ]


	indir, outdir = mig.setPaths()
	f = open(joinpath(outdir,ostr),"w")
	JSON.print(f,d)
	close(f)

	d = ["p"=>pmv3,"h"=>hmv3,"summary"=>d]
	return d

	# compare
end


# get the consumption subsidy that makes
# you indifferent from living through the shock 
# in j with a policy applied (i.e. no moving, no saving, no)
function exp_shockRegion_vdiff(which_base::ASCIIString,which_pol::ASCIIString)

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
	ctax = optimize((x)->valdiff_shockRegion(x,w0,opts),0.5,2.0,show_trace=true,method=:brent,iterations=10)
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

	j         = opts["shockRegion"]
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

	if which=="p3" || which == "y3"
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
	mms0 = computeMoments(sim0,p,m)	

	w1 = getDiscountedValue(@where(sim1,(:j.==j)&(:year.>=shockYear)),p,m,true)
	mms1 = computeMoments(sim1,p,m)	


	# get flows for each region
	d = Dict{ASCIIString,DataFrame}()
	d["base"] = sim0
	d[which] = sim1
	flows = getFlowStats(d,string(which,j))


	# migration outflows
	# ==================

	

	# b_out = @> begin
	# 	sim0	
	# 	@where((:j.==j) & (:year.>1997)) 
	# 	@transform(mig_own = (:move).*(:own),mig_rent = (:move).*(!:own))
	# 	@by(:year,mig = 100 .* mean(:move.data,WeightVec(:density.data)), mig_own = 100 .* mean(:mig_own.data,WeightVec(:density.data)),mig_rent = 100 .* mean(:mig_rent.data,WeightVec(:density.data)), rel_mig_own = 100 .* sum(:mig_own.data,WeightVec(:density.data))./ sum(:own,WeightVec(:density.data)),rel_mig_rent = 100 .* sum(:mig_rent.data,WeightVec(:density.data))./ sum(!:own,WeightVec(:density.data)))

	# 	end

	# p_out = @> begin
	# 		sim1
	# 		@where((:year.>1997)&(:j.==j))
	# 		@transform(mig_own = (:move).*(:own),mig_rent = (:move).*(!:own))
	# 		@by(:year,mig = 100 .* mean(:move.data,WeightVec(:density.data)), mig_own = 100 .* mean(:mig_own.data,WeightVec(:density.data)),mig_rent = 100 .* mean(:mig_rent.data,WeightVec(:density.data)), rel_mig_own = 100 .* sum(:mig_own.data,WeightVec(:density.data))./ sum(:own,WeightVec(:density.data)),rel_mig_rent = 100 .* sum(:mig_rent.data,WeightVec(:density.data))./ sum(!:own,WeightVec(:density.data)))
	# 	end

	# # migration inflows
	# # =================

	# # how many renters/owners do you have each year in j?
	# counts0 = @> begin
	# 		sim0
	# 		@where((:j.==j) & (:year.>1997))
	# 		@by(:year,n_all = length(:own) ,n_rent = sum(!(:own)) , n_own=sum(:own) )
	# 	end
	# counts1 = @> begin
	# 		sim1
	# 		@where((:j.==j) & (:year.>1997))
	# 		@by(:year,n_all = length(:own) ,n_rent = sum(!(:own)) , n_own=sum(:own) )
	# 	end

	# b_in = @> begin
	# 		sim0	
	# 		@where((:year.>1997)&(:j.!=j))
	# 		@transform(mig = (:moveto.==j),mig_own = (:moveto.==j).*(:own),mig_rent = (:moveto.==j).*(!:own))
	# 	end
	# b_in = join(b_in,counts0,on=:year);
	# b_in = @> begin
	# 		b_in
	# 		@by(:year,mig = 100 .* mean(:mig), mig_own = 100 .* mean(:mig_own), mig_rent = 100 .* mean(:mig_rent), rel_mig = 100 .* sum(:mig)./:n_all[1], rel_mig_own = 100 .* sum(:mig_own)./:n_own[1], rel_mig_rent = 100 .* sum(:mig_rent)./:n_rent[1], abs_mig = sum(:mig), abs_mig_own = sum(:mig_own), abs_mig_rent =sum(:mig_rent))
	# 	end

	# p_in = @> begin
	# 		sim1
	# 		@where((:year.>1997)&(:j.!=j))
	# 		@transform(mig = (:moveto.==j),mig_own = (:moveto.==j).*(:own),mig_rent = (:moveto.==j).*(!:own))
	# 	end
	# p_in = join(p_in,counts1,on=:year);
	# p_in = @> begin
	# 		p_in
	# 		@by(:year,mig = 100 .* mean(:mig), mig_own = 100 .* mean(:mig_own), mig_rent = 100 .* mean(:mig_rent), rel_mig = 100 .* sum(:mig)./:n_all[1], rel_mig_own = 100 .* sum(:mig_own)./:n_own[1], rel_mig_rent = 100 .* sum(:mig_rent)./:n_rent[1], abs_mig = sum(:mig), abs_mig_own = sum(:mig_own), abs_mig_rent =sum(:mig_rent))
	# 	end

	out = ["which" => which,
		   "j" => j, 
	       "shockYear" => shockYear, 
	       # "dfs" => dfs,
	       "flows" => flows,
	       "values" => ["base" => w0, which => w1],
	       "moments" => ["base" => mms0, which => mms1]]

	return (out,sim0,sim1)
end

function read_exp_shockRegion(f::ASCIIString)
	d = JSON.parsefile(f)

	pth = replace(f,".json","")
	di = Dict()
	# out and in
	for (k,v) in d
		for (kk,vv) in v
			str = string(pth,k,kk,".csv")
			df = DataFrame(vv["columns"])
			names!(df,Symbol[symbol(vv["colindex"]["names"][i]) for i in 1:length(vv["columns"])])
			writetable(str,df)
			di[string(k,kk)] = df
		end
	end
	return di
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
		p = Param(2,opts)
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
	ss = ss[!isna(ss[:cohort]),:]
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
	setfield!(p,:shockVal,[xtra_ass])
	setfield!(p,:shockAge,opts["it"])
	m = Model(p)
	solve!(m,p)
	w = m.v[1,1,opts["iz"],2,2,1,m.aone,opts["ih"],2,opts["it"]]   # comparing values of moving from 2 to 1 in age 1
	if w == p.myNA
		return NaN 
	else
		(w - v0)^2
	end
end



# find consumption scale ctax such that
# two policies yield identical period 1 value
function find_xtra_ass(v0::Float64,opts::Dict)
	ctax = optimize((x)->valueDiff(x,v0,opts),0.0,10000.0,show_trace=true,method=:brent,iterations=40,abs_tol=0.1)
	return ctax
end

function moneyMC()

	# compute a baseline without MC
	p = Param(2)
	MC = Array(Any,2,p.nz,2)
	setfield!(p,:noMC,true)
	m = Model(p)
	solve!(m,p)

	df = DataFrame(a = 0.0,v0 = 0.0,v1 = 0.0,Type="",h=0,z=0,it=0)

	ages = (1,30)

	opts = Dict()
	opts["policy"] = "moneyMC"
	for ih in 0:1
		opts["ih"] = ih+1
		for iz in 1:p.nz
			opts["iz"] = iz
			for it in 1:2
				opts["it"] = ages[it]
				v0 = m.v[1,1,opts["iz"],2,2,1,m.aone,opts["ih"],2,opts["it"]]	# comparing values of moving from 2 to 1
				MC[ih+1,iz,it] = find_xtra_ass(v0,opts)
				println("done with MC for iz=$iz, ih=$ih, it=$it")
				println("moving cost: $(MC[ih+1,iz,it].minimum)")

				p1 = Param(2,opts)
				setfield!(p1,:shockAge,opts["it"])
				# plug in money 
				setfield!(p1,:shockVal,[MC[ih+1,iz,it].minimum])
				m1 = Model(p1)
				solve!(m1,p1)
				df = vcat(df,DataFrame(a = m.grids["assets"],v0 = m.v[1,1,opts["iz"],2,2,1,:,opts["ih"],2,opts["it"]][:],v1 = m1.v[1,1,opts["iz"],2,2,1,:,opts["ih"],2,opts["it"]][:],h=opts["ih"],z=opts["iz"],it=opts["it"]))
				println("done with recomputing model for iz=$iz, ih=$ih, it=$it")
			end
		end
	end

	zs = m.gridsXD["zsupp"][:,1]
	# make an out dict
	d =[  "z$i" => [ "age_$ti" => ["z" => zs[i], "rent" => MC[1,i,ti].minimum, "own" => MC[2,i,ti].minimum] for ti in 1:2] for i in 1:p.nz] 

	indir, outdir = mig.setPaths()
	f = open(joinpath(outdir,"moneyMC.json"),"w")
	JSON.print(f,d)
	close(f)


	return (df,MC)
end



# run Model without aggregate shocks
function noShocks()

	p = Param(2)
	m = Model(p)
	solve!(m,p)
	s = simulate(m,p)
	s = @where(s,!isna(:cohort))

	opts=Dict()
	opts["policy"] = "noShocks"

	p1 = Param(2,opts)
	m1 = Model(p1)
	solve!(m1,p1)
	s1 = simulate(m1,p1)
	s1 = @where(s1,!isna(:cohort))

	println("baseline")
	println(mean(s[:move]))
	println("noShocks")
	println(mean(s1[:move]))

	return (s,s1)

end


# run Model with only small deviations from aggregate
function smallShocks()

	p = Param(2)
	m = Model(p)
	solve!(m,p)
	s = simulate(m,p)
	s = @where(s,(!isna(:cohort) & (:year.>1996)))
	s = @transform(s,movetoReg = ^(m.regnames[:Division])[:moveto])

	opts=Dict()
	opts["policy"] = "smallShocks"

	p1 = Param(2,opts)
	m1 = Model(p1)
	solve!(m1,p1)
	s1 = simulate(m1,p1)
	s1 = @where(s1,(!isna(:cohort) & (:year.>1996)))
	s1 = @transform(s1,movetoReg = ^(m.regnames[:Division])[:moveto])

	# make several out dicts
	mv_rent  = proportionmap(@where(s,(:move.==true)&(:h.==0))[:movetoReg])
	mv_own   = proportionmap(@where(s,(:move.==true)&(:h.==1))[:movetoReg])
	mv_rent_small = proportionmap(@where(s1,(:move.==true)&(:h.==0))[:movetoReg])
	mv_own_small = proportionmap(@where(s1,(:move.==true)&(:h.==1))[:movetoReg])
	y = @by(s,:Division,y=mean(:y),p2y = mean(:p2y))
	y_s = @by(s1,:Division,p2y=mean(:p2y))

	# get percent difference in moveto distribution
	out = Dict()
	out["moveto"] = [  r => [ "own" => 100*(mv_own_small[r]-mv_own[r])/mv_own[r], "rent" => 100*(mv_rent_small[r]-mv_rent[r])/mv_rent[r] , "y" => @where(y,:Division.== r)[:y][1], "p2y" => @where(y,:Division.== r)[:p2y][1],"p2y_small" => @where(y_s,:Division.== r)[:p2y][1]] for r in keys(mv_own) ] 

	# out["moveto"] = [ "own" =>  [ r => (mv_own_small[r]-mv_own[r])/mv_own[r] for r in keys(mv_own) ], "rent" =>  [ r => (mv_rent_small[r]-mv_rent[r])/mv_rent[r] for r in keys(mv_rent) ] ]

	# summaries
	summa = Dict()

	summa["move_by_own"] = ["own" => ["baseline" => @with(@where(s,:h.==1),mean(:move)), "smallShocks" => @with(@where(s1,:h.==1),mean(:move))], "rent" => ["baseline" => @with(@where(s,:h.==0),mean(:move)), "smallShocks" => @with(@where(s1,:h.==0),mean(:move))] ]
	summa["move"] = ["baseline" => @with(s,mean(:move)), "smallShocks" => @with(s1,mean(:move))] 
	summa["own"] = ["baseline" => @with(s,mean(:own)), "smallShocks" => @with(s1,mean(:own))] 

	out["summary"] = summa

	f = open("/Users/florianoswald/Dropbox/mobility/output/model/data_repo/out_data_jl/smallShocks.json","w")
	JSON.print(f,out)
	close(f)

	return (s,s1,out)

end




# run model without the ability to save!
function exp_noSavings()

	p = Param(2)
	m = Model(p)
	solve!(m,p)
	s = simulate(m,p)
	w0 = getDiscountedValue(s,p,m,false)
	mms = computeMoments(s,p,m)	

	opts=Dict()
	opts["policy"] = "noSaving"

	p1 = Param(2,opts)
	m1 = Model(p1)
	solve!(m1,p1)
	s1 = simulate(m1,p1)
	w1 = getDiscountedValue(s1,p1,m1,false)
	mms1 = computeMoments(s1,p1,m1)	

	d = ["moms" => ["base" => mms, "noSave" => mms1], "vals" => ["base" => w0, "noSave" => w1]]

	return d
end


