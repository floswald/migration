	
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
	opts = selectPolicy(which,region,year,p)

	if which=="mortgage_deduct"
		e = mig.exp_Mortgage(true)
	elseif which=="p"
		e = mig.exp_shockRegion(region,"p",year)
		save(joinpath(outdir,"shockReg","exp_region$(region)_$which.JLD"),e[1])
	elseif which=="p3"
		e = mig.exp_shockRegion(region,"p3",year)
		save(joinpath(outdir,"shockReg","exp_region$(region)_$which.JLD"),e[1])
	elseif which=="shockp_highMC"
		e = mig.exp_shockRegion(opts)
		save(joinpath(outdir,"shockReg","exp_region$(region)_$which.JLD"),e[1])
	elseif which=="shockp_noBuying"
		e = mig.exp_shockRegion(region,which,year)
		save(joinpath(outdir,"shockReg","exp_region$(region)_$which.JLD"),e[1])
	elseif which=="shockp_noSaving"
		e = mig.exp_shockRegion(region,which,year)
		save(joinpath(outdir,"shockReg","exp_region$(region)_$which.JLD"),e[1])
	elseif which=="y"
		e = mig.exp_shockRegion(region,"y",year)
		save(joinpath(outdir,"shockReg","exp_region$(region)_$which.JLD"),e[1])
	elseif which=="y3"
		e = mig.exp_shockRegion(region,"y3",year)
		save(joinpath(outdir,"shockReg","exp_region$(region)_$which.JLD"),e[1])
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
	else
		throw(ArgumentError("no valid experiment chosen"))
	end

	println("done.")
	return e

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
		@by(:id, meanv = mean(:vbeta))
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
		@select(own=mean(convert(Array{Float64},:h),fullw),move=mean(convert(Array{Float64},:move),fullw),income=mean(convert(Array{Float64},:income),fullw),policy=pol)
	end
	own_move_age = @> begin
		sim_sample
		@by(:realage,own=mean(:h),move=mean(:move),income=mean(:income),assets=mean(:a),policy=pol)
	end
	move_own_age = @by(sim_sample,[:own,:realage],move=mean(:move),income=mean(:income),assets=mean(:a),policy=pol)

	out = ["own_move" => own_move, "own_move_age" => own_move_age, "move_own_age"=> move_own_age]
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



	# return
	out = ["Receipts" => Tot_tax, "base_out" => base_out, "pol0_out" => pol0_out, "Redistributions" => ["R0" => Redist0,"R1" => Redist1,"R2" => Redist2,"R3" => Redist3], "Receipts_age"=>Tot_tax_age, "npv_age_income"=>npv_age_income,"ctax0" => ctax0, "ctax1" => ctax1, "pol1_out" =>pol1_out, "ctax2" => ctax2,  "ctax3" => ctax3, "ctax4" => ctax4,"pol2_out"=> pol2_out, "pol3_out"=> pol3_out, "pol4_out"=> pol4_out, "move_own" => own_move, "move_own_age" => move_own_age]

	return out

end



function selectPolicy(which::ASCIIString,j::Int,shockYear::Int,p::Param)

	# shocks p at shockAge for ever after
	if which=="p"
		opts = ["policy" => "shockp","shockRegion" => j,"shockYear"=>shockYear,"shockAge"=>1, "shockVal"=> repeat([0.7],inner=[1],outer=[p.nt-1])]
	# shocks p at shockAge for the next 3 periods reverting back to trend afterwards
	elseif which=="p3"
		opts = ["policy" => "shockp","shockRegion" => j,"shockYear"=>shockYear,"shockAge"=>1, "shockVal"=> [0.7,0.8,0.9,repeat([1.0],inner=[1],outer=[p.nt-3])]]
	elseif which=="y3"
		opts = ["policy" => "shocky","shockRegion" => j,"shockYear"=>shockYear,"shockAge"=>1, "shockVal"=> [0.7,0.8,0.9,repeat([1.0],inner=[1],outer=[p.nt-3])]]
	elseif which=="y"
		opts = ["policy" => "shocky","shockRegion" => j,"shockYear"=>shockYear,"shockAge"=>1, "shockVal"=> repeat([0.7],inner=[1],outer=[p.nt-1])]

	elseif which=="shockp_highMC"
		opts = ["policy" => which,"shockRegion" => j,"shockYear"=>shockYear,"shockAge"=>1, "shockVal"=> repeat([0.7],inner=[1],outer=[p.nt-1])]
	elseif which=="shockp_noBuying"
		opts = ["policy" => which,"shockRegion" => j,"shockYear"=>shockYear,"shockAge"=>1, "shockVal"=> repeat([0.7],inner=[1],outer=[p.nt-1])]
	elseif which=="shockp_noSaving"
		opts = ["policy" => which,"shockRegion" => j,"shockYear"=>shockYear,"shockAge"=>1, "shockVal"=> repeat([0.7],inner=[1],outer=[p.nt-1])]
	end
	return opts

end


# get the consumption subsidy that makes
# you indifferent from living through the shock 
# in j with a policy applied (i.e. no moving, no saving, no)
function exp_shockRegion_vdiff(which::String)

	# w0 = value of living in shock region from shockYear forward
	# w1 = value of living in shock region from shockYear forward UNDER POLICY

	# w0 - w1 is diff in value from policy

	# baseline: a shock to p in 2007 in region 6.
	opts = ["policy" => "shockp", "shockRegion" => 6, "shockYear"=> 2007,"shockAge"=>1, "shockVal"=> repeat([0.7],inner=[1],outer=[p.nt-1])]

	e = exp_shockRegion(opts);
	w0 = e[1]["values"]["p"][1]

	# the policy: same shock as in baseline, but:
	opts["policy"] = which
	ctax = optimize((x)->valdiff_shockRegion(x,w0,opts),0.5,1.8,show_trace=true,method=:brent)
	return ctax

end

function valdiff_shockRegion(ctax::Float64,v0::Float64,opts::Dict)

	# change value of ctax on options dict
	opts["ctax"] = ctax
	println("current ctax level = $ctax")

	# and recompute
	e = exp_shockRegion(opts);
	w = e[1]["values"][opts["policy"]][1]

	return (w - v0) ^2
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

	p = Param(2)
	m = Model(p)
	solve!(m,p)
	sim0 = simulate(m,p)
	sim0 = sim0[!isna(sim0[:cohort]),:]


	# Policy
	# ------
	
	opts = selectPolicy(which,j,shockYear,p)

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
		df1 = vcat(df1,@where(sim0,:cohort.<=minc))
	end


	# compute behaviour of all born into post shock world
	println("computing behaviour for post shock cohorts")

	if which=="p3" || which == "y3"
		# assume shock goes away immediately and all behave as in baseline
		sim2 = @where(sim0,:cohort.>maxc)
	else
		# assume shock stays forever
		opts["shockAge"] = 1
		p1 = Param(2,opts)
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
	gc()

	# compute summaries
	# =================

	# get discounted lifetime utility of people in j
	# ---------------------------------------------------
	w0   = getDiscountedValue(@where(sim0,:j.==j),p,m,false)
	mms0 = computeMoments(sim0,p,m)	

	w1 = getDiscountedValue(@where(sim1,:j.==j),p,m,false)
	mms1 = computeMoments(sim1,p,m)	


	# get dataframes for plotting.

	dd1 = @by(@where(sim0,:j.==j),:year,baseline_move=mean(:move),baseline_own=mean(:own),baseline_p=mean(:p),baseline_y=mean(:y),baseline_inc=mean(:income))
	dd2 = @by(@where(sim1,:j.==j),:year,shock_move=mean(:move),shock_own=mean(:own),shock_p=mean(:p),shock_y=mean(:y),shock_inc=mean(:income))
	df_fromj = join(dd1,dd2,on=:year)

	dd1 = @by(@where(sim0,(:j.==j)&(:h.==1)),:year,baseline=mean(:move))
	dd2 = @by(@where(sim1,(:j.==j)&(:h.==1)),:year,shock=mean(:move))
	df_fromj_own  = join(dd1,dd2,on=:year)

	dd1 = @by(@where(sim0,(:j.==j)&(:h.==0)),:year,baseline=mean(:move))
	dd2 = @by(@where(sim1,(:j.==j)&(:h.==0)),:year,shock=mean(:move))
	df_fromj_rent  = join(dd1,dd2,on=:year)

	dd1 = @by(@where(sim0,:j.!=j),:year,baseline_move=mean(:moveto.==j),baseline_own=mean(:own),baseline_p=mean(:p),baseline_y=mean(:y),baseline_inc=mean(:income))
	dd2 = @by(@where(sim1,:j .!= j),:year,shock_move=mean(:moveto.==j),shock_own=mean(:own),shock_p=mean(:p),shock_y=mean(:y),shock_inc=mean(:income))
	df_toj = join(dd1,dd2,on=:year)

	# owners who move to j and rent
	dd1 = @by(@where(sim0,(:j.!=j)&(:h.==1)&(:hh.==0)),:year,baseline=mean(:moveto.==j))
	dd2 = @by(@where(sim1,(:j.!=j)&(:h.==1)&(:hh.==0)),:year,shock=mean(:moveto.==j))
	df_toj_own_rent = join(dd1,dd2,on=:year)

	# owners who move to j and buy 
	dd1 = @by(@where(sim0,(:j.!=j)&(:h.==1)&(:hh.==1)),:year,baseline=mean(:moveto.==j))
	dd2 = @by(@where(sim1,(:j.!=j)&(:h.==1)&(:hh.==1)),:year,shock=mean(:moveto.==j))
	df_toj_own_buy  = join(dd1,dd2,on=:year)

	# renters who move to j and rent
	dd1 = @by(@where(sim0,(:j.!=j)&(:h.==0)&(:hh.==0)),:year,baseline=mean(:moveto.==j))
	dd2 = @by(@where(sim1,(:j.!=j)&(:h.==0)&(:hh.==0)),:year,shock=mean(:moveto.==j))
	df_toj_rent_rent  = join(dd1,dd2,on=:year)

	# renters who move to j and buy
	dd1 = @by(@where(sim0,(:j.!=j)&(:h.==0)&(:hh.==1)),:year,baseline=mean(:moveto.==j))
	dd2 = @by(@where(sim1,(:j.!=j)&(:h.==0)&(:hh.==1)),:year,shock=mean(:moveto.==j))
	df_toj_rent_buy  = join(dd1,dd2,on=:year)

	dfs = ["fromj" => df_fromj, 
	       "fromj_own" => df_fromj_own,
	       "fromj_rent" => df_fromj_rent,
	       "toj" => df_toj, 
	       "toj_own_rent" => df_toj_own_rent,
	       "toj_own_buy" => df_toj_own_buy,
	       "toj_rent_rent" => df_toj_rent_rent,
	       "toj_rent_buy" => df_toj_rent_buy]

	indir, outdir = mig.setPaths()

	out = ["which" => which,
		   "j" => j, 
	       "shockYear" => shockYear, 
	       "dfs" => dfs,
	       "values" => ["base" => w0, which => w1],
	       "moments" => ["base" => mms0, which => mms1]]

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
		p = Param(2,opts)
		setfield!(p,:ctax,get(opts,"ctax",1.0))	# set the consumption tax, if there is one in opts
		@assert p.shockAge == shockAge
		keep = (p.nt) - shockAge + opts["shockYear"] - 1997 # relative to 1997, first year with all ages present
	# end

	println("applying $(opts["policy"]) at age $(p.shockAge) with shockVals=$(p.shockVal[1:4]), keeping cohort $keep")
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


