

# model reporting
# ===============


# report some solution characteristics
function solReport()
	p = Param(2)
	m = Model(p)
	mig.solve!(m,p)

	# report
end


# some simulation characteristics
function simReport()
	s = mig.runSim()

	# save some data
	sim_year = mig.by(@where(s,:year.>1994),:year,model->DataFrame(model_mig=mean(model[:move]),model_own=mean(model[:own])))
	sim_age = mig.by(@where(s,:year.>1994),:realage,model->DataFrame(model_mig=mean(model[:move]),model_own=mean(model[:own])))
	sim_year_reg = mig.by(@where(s,:year.>1994),[:year,:Division],model->DataFrame(model_own=mean(model[:own])))
	writetable("/Users/florianoswald/Dropbox/mobility/output/model/data_repo/out_data_jl/sim_year.csv",sim_year)
	writetable("/Users/florianoswald/Dropbox/mobility/output/model/data_repo/out_data_jl/sim_year_reg.csv",sim_year_reg)

end


