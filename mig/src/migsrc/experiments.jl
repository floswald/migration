



# exper1
# changing the mortgage interest rate deduction
# ----------------------------------------------

# this means in practice a tax saving for certain
# kinds of households. in particular high earners
# will save more income tax.
# my model is in real terms, i.e. there is no tax.
# therefore a tax saving is an increase in disposable income.

# take money away from owners
# register as tax receipts
# redistribute to everybody lump sum
# measure welfare difference

# output: mean ownership rate, mean mobility, mean mobility conditional on ownership

function exp_Mortgage()

# step 1: get savings schedule from Poterba&Sinai

	# baseline model:
	# ===============

	# get value, and potentially some sim output?
	p0   = Param(2)
	m0   = Model(p0)
	sol0 = solve!(m0,p0)
	sim0 = simulate(m0,p0)
	sim0 = @where(sim0,(:year.>1997) & (!isna(:cohort)) )
	fullw = WeightVec(array(sim0[:density]))

	# base = @select(sim0,own=mean(convert(Array{Float64},:h),fullw),move=mean(convert(Array{Float64},:move),fullw))
	base = @> begin
		sim0
		@select(own=mean(convert(Array{Float64},:h),fullw),move=mean(convert(Array{Float64},:move),fullw))
	end
	# base_age = @by(sim0,:realage,own=mean(:h),move=mean(:move),income=mean(:income))
	base_age = @> begin
		sim0
		@by(:realage,own=mean(:h),move=mean(:move),income=mean(:income))
	end

	# moving and income by ownership
	base_own = @by(sim0,[:own,:realage],move=mean(:move),income=mean(:income))

	# v0 = m.vh[1,1,1,2,2,2,1,m0.aone,1,1,1] # value of a renter with 0 assets at age 1, in region 1, not buying, no kids, 

	# model under policy
	# ===================

	m1 = Model(p0,policy="mortgageSubsidy")	# 1.5 secs
	sol1 = solve!(m1,p0)
	sim1 = simulate(m1,p0)
	sim1 = @where(sim1,(:year.>1997) & (!isna(:cohort)) )
	fullw = WeightVec(array(sim1[:density]))

	# base = @select(sim1,own=mean(convert(Array{Float64},:h),fullw),move=mean(convert(Array{Float64},:move),fullw))
	policy = @> begin
		sim1
		@select(own=mean(convert(Array{Float64},:h),fullw),move=mean(convert(Array{Float64},:move),fullw))
	end
	# base_age = @by(sim1,:realage,own=mean(:h),move=mean(:move),income=mean(:income))
	policy_age = @> begin
		sim1
		@by(:realage,own=mean(:h),move=mean(:move),income=mean(:income))
	end

	# moving and income by ownership
	policy_own = @by(sim1,[:own,:realage],move=mean(:move),income=mean(:income))

	rent=hcat( @select(@where(policy_own,:own.==false),age=:realage,policy_move=:move), @select(@where(base_own,:own.==false),base_move=:move))
	rent43 = @where(rent,:age.<43)
	mig.plot(rent43[:age],rent43[:policy_move])
	mig.plot(rent43[:age],rent43[:base_move])

	own=hcat( @select(@where(policy_own,:own.==true),age=:realage,policy_move=:move,policy_income=:income), @select(@where(base_own,:own.==true),base_move=:move,base_income=:income))
	rent43 = @where(rent,:age.<43)
	mig.plot(own[:age],own[:policy_move])
	mig.plot(own[:age],own[:base_move])

	# owners move less because they have less money. given high moving cost, loss in income means the margin of non-movers is lower now.
	lm(move ~ realage + income,sim0)



	# mv = join(baseline_j[[:j,:mobility]],noSubsidy_j[[:j,:mobility]],on=:j)
	# @transform(mv,change=100*(:mobility_1 - :mobility)./:mobility)



	# # mig.plot(baseline_age[:realage],baseline_age[:ownership])
	# # mig.plot(noSubsidy_age[:realage],noSubsidy_age[:ownership])

	# # mig.figure(2)
	# # mig.plot(baseline_age[:realage],baseline_age[:mobility])
	# # mig.plot(noSubsidy_age[:realage],noSubsidy_age[:mobility])


	# # v1 = m.vh[1,1,1,2,2,2,1,m0.aone,1,1,1] # value of a renter with 0 assets at age 1, in region 1, not buying, no kids, 
	out = ["base" => base, "base_age" => base_age, "base_own" => base_own,"policy" => policy, "policy_age" => policy_age, "policy_own" => policy_own]

	return out
end

