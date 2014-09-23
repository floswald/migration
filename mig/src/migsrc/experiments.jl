



# exper1
# changing the mortgage interest rate deduction
# ----------------------------------------------

# this means in practice a tax saving for certain
# kinds of households. in particular high earners
# will save more income tax.
# my model is in real terms, i.e. there is no tax.
# therefore a tax saving is an increase in disposable income.

# step 1: get savings schedule from Poterba&Sinai

# sinai = DataFrame(age=[34,50],inc_40K_minus=[208.0,216.0],inc_40_75K=[592.0,719.0],inc_75_125K=[1817.0,1483.0],inc_125_250K=[3603.0,3599.0])


# step 2: solve model under adjusting disposable income of owners downwards by the amount
# they are saving under the curretn regime.

function exp_Mortgage()

# step 1: get savings schedule from Poterba&Sinai

	# baseline model:
	# ===============

	# get value, and potentially some sim output?
	p0 = Param(2)
	m0 = Model(p0)
	sol0 = solve!(m0,p0)
	sim0 = simulate(m0,p0)
	baseline_y = @> begin
		sim0
		@where((:year.>1997) & (!isna(:cohort)) )
		@transform(ybin = cut(:income,6))
		@by(:ybin,ownership=mean(:h),mobility=mean(:move))
	end
	baseline_age = @> begin
		sim0
		@where((:year.>1997) & (!isna(:cohort)) )
		@by(:realage,ownership=mean(:h),mobility=mean(:move))
	end



	# v0 = m.vh[1,1,1,2,2,2,1,m0.aone,1,1,1] # value of a renter with 0 assets at age 1, in region 1, not buying, no kids, 

	# model under policy
	# ===================

	m1 = Model(p0,policy="mortgageSubsidy")	# 1.5 secs
	sol1 = solve!(m1,p0)
	sim1 = simulate(m1,p0)
	noSubsidy_y = @> begin
		sim1
		@where((:year.>1997) & (!isna(:cohort)) )
		@transform(ybin = cut(:income,6))
		@by(:ybin,ownership=mean(:h),mobility=mean(:move))
	end	
	noSubsidy_age = @> begin
		sim1
		@where((:year.>1997) & (!isna(:cohort)) )
		@by(:realage,ownership=mean(:h),mobility=mean(:move))
	end

	# mig.plot(baseline_age[:realage],baseline_age[:ownership])
	# mig.plot(noSubsidy_age[:realage],noSubsidy_age[:ownership])

	# mig.figure(2)
	# mig.plot(baseline_age[:realage],baseline_age[:mobility])
	# mig.plot(noSubsidy_age[:realage],noSubsidy_age[:mobility])


	# v1 = m.vh[1,1,1,2,2,2,1,m0.aone,1,1,1] # value of a renter with 0 assets at age 1, in region 1, not buying, no kids, 
	out = ["base_age" => baseline_age, "base_y" => baseline_y, "noSubsidy_age"=>noSubsidy_age, "noSubsidy_y"=>noSubsidy_y]

	(baseline_age, baseline_y, noSubsidy_age, noSubsidy_y)

end





# step 3: simulate and record changes to baseline
