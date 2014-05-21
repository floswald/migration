

# main programme
cd("/Users/florianoswald/git/migration/julia")
include("mods/mig.jl")
	
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
p = mig.Param(2)


m = mig.Model(p)
@time mig.solve!(m,p)
# @profile mig.solve!(m,p)

# plot value functions
mig.vhplot(m,p,(1,1,1,1,1,1,1,1))
mig.vplot(m,p)


# simulating



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


