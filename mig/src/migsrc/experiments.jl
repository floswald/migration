



# exper1
# changing the mortgage interest rate deduction
# ----------------------------------------------


# age 1 utility difference between 2 policies
function welfare(ctax::Float64,v0::Float64,opts::Dict)
	p = Param(2,opts)
	setfield!(p,:ctax,ctax)
	m = Model(p)
	solve!(m,p)
	m.vh[1,1,1,3,2,2,2,m.aone,1,1,1] - v0
end

# find consumption scale ctax such that
# two policies yield identical period 1 value
function findctax(v0::Float64,opts::Dict)
	if get(opts,"verbose",0) > 0 
		ctax = optimize((x)->welfare(x,v0,opts),0.5,1.5,show_trace=true,method=:brent)
	else
		ctax = optimize((x)->welfare(x,v0,opts),0.5,1.5,method=:brent)
	end
	return ctax
end

# collect some sim output from policies
function policyOutput(df::DataFrame,pol::ASCIIString)
	# subset to estimation sample: 1997 - 2012
	sim_sample = @where(df,(:year.>1997) )
	fullw = WeightVec(array(sim_sample[:density]))

	own_move = @> begin
		sim_sample
		@select(own=mean(convert(Array{Float64},:h),fullw),move=mean(convert(Array{Float64},:move),fullw),income=mean(convert(Array{Float64},:income),fullw))
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

	p0   = Param(2)
	m0   = Model(p0)
	solve!(m0,p0)
	sim0 = simulate(m0,p0)

	println("done.")
	# get baseline expected lifetime utility at age 1
	# age = 20 
	# j = 1
	# h = 0
	# assets = 0
	# tau = 2
	# y,p = 2,2
	# income = z2
	# s = 1
	# k = 1
	# hh = 1
	v0 = m0.vh[1,1,1,3,2,2,2,m0.aone,1,1,1]

	# collect some baseline output
	# ----------------------------

	# throw away incomplete cohorts
	sim0 = @where(sim0,!isna(:cohort))

	base_out = policyOutput(sim0,"baseline")

	# get some tax receipts data
	# --------------------------
	# look at one complete cohort:
	# 1982 cohort
	sim0_T = @where(sim0, :cohort.==16)


	# redistribution 1
	# ----------------
	Tot_tax = sum(array(sim0_T[:subsidy]))	
	lumpSum1 = [Tot_tax / length(unique(sim0_T[:id]))]

	# redistribution 2
	# ----------------

	# get per capita tax expenditure by age
	Tot_tax_age = @by(sim0_T,:realage,receipts=sum(:subsidy),N_own = sum(:own),N=length(:subsidy))
	Tot_tax_age = @transform(Tot_tax_age,per_owner_subsidy = :receipts ./ :N_own,pc_lumpsum = :receipts ./ :N)

	Tot_tax_age = @transform(Tot_tax_age,npv_at_age = 0.0)
	for i in 1:size(Tot_tax_age,1)
		Tot_tax_age[i,:npv_at_age] = npv(Tot_tax_age[:per_owner_subsidy],p0.R-1,i)
	end
	lumpSum2 = array(Tot_tax_age[:pc_lumpsum])

	# free some memory
	m0 = 0
	sim0 = 0
	gc()

	println("starting experiments")

	# model under no redistribution at all: burning money
	# ---------------------------------------------------

	opts = ["policy" => "mortgageSubsidy_oldyoung","lumpsum" => [0.0],"verbose"=>1]

	# utility equalizing consumption scaling:
	if ctax
		ctax0 = findctax(v0,opts)
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
	sim0 = @where(sim0,!isna(:cohort))
	pol0_out = policyOutput(sim0,"burning_money")

	m0 = 0
	sim0 = 0
	gc()

	# model under redistribution policy 1: lump sum to all 20 year olds
	# -----------------------------------------------------------------

	opts = ["policy" => "mortgageSubsidy_oldyoung","lumpsum" => lumpSum1,"verbose"=>1]

	# utility equalizing consumption scaling:
	if ctax
		ctax1 = findctax(v0,opts)
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


	# model under redistribution policy 2: lump sum from owners of age group
	# -----------------------------------------------------------------

	opts = ["policy" => "mortgageSubsidy_in_age","lumpsum" => lumpSum2,"verbose"=>1]

	# utility equalizing consumption scaling:
	if ctax
		ctax2 = findctax(v0,opts)
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

	# create a dataframe with all pol results stacked
	move_own_age = vcat(base_out["move_own_age"],pol0_out["move_own_age"],pol1_out["move_own_age"],pol2_out["move_own_age"])



	# return
	out = ["Receipts" => Tot_tax, "lumpSum_20yrs" => lumpSum1, "Receipts_age"=>Tot_tax_age, "ctax0" => ctax0, "ctax1" => ctax1, "pol1_out" =>pol1_out, "ctax2" => ctax2, "pol2_out"=> pol2_out, "plotting" => move_own_age]

	return out

end

