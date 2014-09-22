

# model reporting
# ===============


# report some solution characteristics
function solReport()
	p = Param(2)
	m = Model(p)
	mig.solve!(m,p)

	# report
end

function simCsv(s::DataFrame)
	s2 = s[!mig.isna(s[:cohort]),:]
	writetable("/Users/florianoswald/Dropbox/mobility/output/model/data_repo/out_data_jl/simdata.csv")
end




# some simulation characteristics
function writeSimReport()
	s = mig.runSim()

	# save some data
	sim_year = mig.by(@where(s,:year.>1994),:year,model->DataFrame(model_mig=mean(model[:move]),model_own=mean(model[:own])))
	sim_age = mig.by(@where(s,:year.>1994),:realage,model->DataFrame(model_mig=mean(model[:move]),model_own=mean(model[:own])))
	sim_year_reg = mig.by(@where(s,:year.>1994),[:year,:Division],model->DataFrame(model_own=mean(model[:own])))
	writetable("/Users/florianoswald/Dropbox/mobility/output/model/data_repo/out_data_jl/sim_year.csv",sim_year)
	writetable("/Users/florianoswald/Dropbox/mobility/output/model/data_repo/out_data_jl/sim_year_reg.csv",sim_year_reg)

end

# illustrate implied housing tenure model:
# price to income ratio is very strong determinant in model!
function simRegs(s::DataFrame)

	s2 = s[!mig.isna(s[:cohort]),:]
	s96 = @where(s2,:year .> 1994)
	pool!(s96[:Division])

	lm1 = lm(hh ~ p2y + p2w + realage + Division,s96)
	glm1 = lm(hh ~ p2y + p2w + realage + Division,s96,Binomial(),ProbitLink())

	d = Dict()
	d["lm"] = lm1
	d["glm"] = glm1
	return d
end

function simMovers(s::DataFrame)

	s = s[!mig.isna(s[:cohort]),:]
	mvid = @> begin 
		s
		@where(:move.==true)
		@select(:id)
	end
	mvid = unique(mvid)

	# mover's data
	mv = s[findin(s[:id],mvid[:id]),:]

end











