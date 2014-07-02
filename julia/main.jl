

# main programme
home = ENV["HOME"]
cd("$home/git/migration/julia")
include("src/estimation.jl")
include("src/slices.jl")

include("src/mig.jl")

# session 1
p = mig.Param(2)
m = mig.Model(p)
@time mig.solve!(m,p)

# session 2
@time mig.mywrap()

function wrap2(p::mig.Param,m::mig.Model)

	# p0 = mig.Param(2)	# constructing p inside doubles time.
	# m0 = Model(p0)
	# mig.solve!(m0,p0)

	mig.solve!(m,p)

end


p0 = mig.Param(2)
function wrap3(p::mig.Param)
	# mig.update!(p,x)
	# println(p)
	wrap2(p)
end


@time s = mig.simulate(m,p);
@time mms = mig.computeMoments(s,p,m);

	




# testing
include("test/test_param.jl")
include("test/test_model.jl")
include("test/test_solver.jl")
include("test/test_Tensors.jl")
	



# run experiments
MC1 = mig.experMC(1,3);
MC2 = mig.experMC(2,3);
# MC3 = mig.experMC(3,3);

# save experiments
mig.writetable("/Users/florianoswald/Dropbox/mobility/output/model/Julia2R/MC1.csv",MC1[1])
mig.writetable("/Users/florianoswald/Dropbox/mobility/output/model/Julia2R/MC2.csv",MC2[1])
mig.writetable("/Users/florianoswald/Dropbox/mobility/output/model/Julia2R/MC3.csv",MC3[1])


# running
# @profile mig.solve!(m,p)

# plot value functions
mig.vhplot(m,p,(1,1,1,1,1,1,1,1))
mig.vplot(m,p)


# simulating


showall(by(s,:age,d -> mean(d[:income])))
	
	


# without linear index: 96 secs
# with linear index: 74 secs
# speedup: 29%

# with discretized savings solution: 74 secs
# without any savings solution: 73 secs

# with bounds checking: 74 secs
# without bounds checking: 70 secs	(put @inbounds begin ... end around entire loop)


# after rebuilding the code. got rid of several redundant loops in expectations calculation by initiation the conditional expectations calculation on state ix as soon as state ix is computed (before computed all, and then had to reloop through all again), all linear indices, : 4 secs
# after rebuilding the code: 4 secs

# that was with
	# na    = 10
	# 		nz    = 3
	# 		nh    = 2
	# 		ntau  = 2
	# 		nP    = 3
	# 		# nY    = 3
	# 		np    = 2
	# 		ny    = 3
	# 		nJ    = 9
	# 		nt    = 30


