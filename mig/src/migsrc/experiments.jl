

# Monte Carlo
function monteCarlo(autoload=false)

	if autoload
		# load model-generated data
		moms = readtable("Dropbox/mobility/output/model/data_repo/in_data_jl/MCtrue.csv")

	else
		# make model-generated data
		p = mig.Param(2)
		m = mig.Model(p)
	    mig.solve!(m,p)
		s   = simulate(m,p)
		x=computeMoments(s,p,m)

		indir = joinpath(ENV["HOME"],"Dropbox/mobility/output/model/data_repo/in_data_jl")
		mom = mig.DataFrame(mig.read_rda(joinpath(indir,"moments.rda"))["m"])
		moms = join(mom,x,on=:moment)
		names!(moms,[:moment,:model_value,:model_sd,:data_value,:data_sd,:sq,:perc])

		delete!(moms,[:model_value,:model_sd,:sq,:perc])

		writetable(moms,"Dropbox/mobility/output/model/data_repo/in_data_jl/MCtrue.csv")
	end

	# start values for p
	p = mig.Param(2)
	# p2["gamma"]   = 1.35
	p2["xi1"]     = 0.02
	# p2["xi2"]     = 0.07
	# p2["omega2"]  = 0.0
	# p2["MC0"]     = 0.0
	# p2["MC1"]     = 4.0
	# p2["MC3"]     = -0.2
	# p2["MC3_2"]     = 0.0
	# p2["MC4"]     = 0.0
	# p2["taudist"] = 0.25


	pb = Dict{ASCIIString,Array{Float64,1}}()
	pb["xi1"]    = [0.0,0.1]

	# options for objective function
	objfunc_opts = ["printlevel" => 1,"printmoms"=>false]


	# subsetting moments
	submom = setdiff(moms[:moment],["lm_w_intercept","move_neg_equity","q25_move_distance","q50_move_distance","q75_move_distance"])

	# setup the minimization problem
	mprob = MProb(p2,pb,mig.objfunc,moms,moments_subset=submom,objfunc_opts=objfunc_opts)
end


