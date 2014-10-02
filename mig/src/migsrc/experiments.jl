

# list of experiments:
# 1) changing the mortgage interest rate deduction
# 2) fannie mae and freddie max guarantee: mortgage interest rate is too low.
# 3) imperfect rental markets: rent is too high
# 4) moving voucher: moving is too costly


# age 1 utility difference between 2 policies
function welfare(ctax::Float64,v0::Float64,opts::Dict)
	p = Param(2,opts)
	setfield!(p,:ctax,ctax)
	m = Model(p)
	solve!(m,p)
	(m.vh[1,1,1,3,2,2,2,m.aone,1,1,1] - v0)^2
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
	out = ["Receipts" => Tot_tax, "base_out" => base_out, "lumpSum_20yrs" => lumpSum1, "Receipts_age"=>Tot_tax_age, "ctax0" => ctax0, "ctax1" => ctax1, "pol1_out" =>pol1_out, "ctax2" => ctax2, "pol2_out"=> pol2_out, "plotting" => move_own_age]

	return out

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
function exp_shockRegion(j::Int,which::ASCIIString,shockYear=1997)

	if shockYear<1997
		throw(ArgumentError("must choose years after 1996. only then full cohorts available"))
	end

	p = Param(2)
	m = Model(p)
	solve!(m,p)
	sim0 = simulate(m,p)

	if which=="p"
		opts = ["policy" => "shockp","shockRegion" => j,"shockYear"=>shockYear,"shockAge"=>1, "shockVal"=> 0.7]
		# shock price
		# stacking all in ss gives you a dataset
		# where everything is normal until the year shockYear, 
		# at which point a shock occurs in region shockRegion

		# you then compare that dataset to sim0, where this does not happen.

	elseif which=="y"
		opts = ["policy" => "shocky","shockRegion" => j,"shockYear"=>shockYear,"shockAge"=>1, "shockVal"=> 0.7]
		# shock income
	end

	ss = pmap(x -> computeShockAge(m,opts,x),1:p.nt-1)		

	# stack dataframes
	df1 = ss[1]
	for i in 2:length(ss)
		df1 = hcat(df1,ss[i])
		ss[i] = 0
	end

	sim0 = sim0[!isna(sim0[:cohort]),:]
	df1  = df1[!isna(df1[:cohort]),:]

	df2 = hcat(@by(@where(sim0,:j.==j),:year,normal=mean(:move)),@by(@where(df0,:j .== j),:year,policy=mean(:move)))
	println(df2)

	return (sim0,df1,df2)

end

function computeShockAge(m::Model,opts::Dict,shockAge::Int)

	println("applying $(opts["policy")) at age $shockAge")

	opts["shockAge"] = shockAge

	p = Param(2,opts)
	mm = Model(p)
	solve!(mm,p)

	# replace vh,rho,ch and sh before shockAge with values in baseline model m

	if shockAge > 1
		for rt in 1:shockAge-1
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

	# simulate all individuals
	# but keep only the cohort that is age = shockAge in shockYear
	ss = simulate(mm,p)
	keep = p.nt - shockAge + opts["shockYear"] - 1997 # relative to 1997, first year with all ages present
	# throw away NA cohorts
	ss = ss[!isna(ss[:cohort]),:]
	# keep only cohort that gets the shock at age shockAge in shockYear.
	ss = @where(ss,:cohort .== keep)
	return ss
end


