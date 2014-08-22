

# main programme
home = ENV["HOME"]
cd("$home/git/migration/mig")


# include("src/estimation.jl")
cd("$home/git/migration/mig.jl/src/sge")
include("examples/slices.jl")

include("src/mig.jl")

	
# run simulation
@time x = mig.runSim()

p = Param(2,1)
@time m = Model(p)	# 1.5 secs
@time mig.solve!(m,p)	
mig.vhplot(m,p,(7,1,2,3,3,1,7,1))
mig.vhplot(m,p,(1,1,1,1,1,1,1,1))
mig.vhplot(m,p,(1,1,1,1,1,1,1,29))
mig.vplot(m,p)
@time s = mig.simulate(m,p);	
		

maximum(m.EV[:,:,:,:,:,:,:,:,29])
m.sh[1,1,1,1,1,1,:,1,1,1,1]
m.vh[1,1,2,3,3,1,1,:,1,1,1][:]
reshape(m.v[:,1,1,1,1,m.aone,1,1,:,1],p.nJ,p.nJ)
reshape(m.v[:,1,1,1,1,m.aone,1,1,:,1],p.nJ,p.nJ)
reshape(m.rho[:,1,1,1,1,m.aone,1,1,:,1],p.nJ,p.nJ)

mig.simplot(s,5)
	


show(mig.DataFrame(moment=["move","own"],value=[mean(s[:move]),mean(s[:h])]))
	
@time mms = mig.computeMoments(s,p,m);

# run objective
p2 = Dict{ASCIIString,Float64}()
p2["gamma"] = 3.1

x = mig.objfunc(p2,moms,array(moms[:moment]))
mprob = MOpt.MProb(p2,pb,mig.objfunc,moms,moments_subset=setdiff(moms[:moment],["moved0","moved1","moved2","move_rate","move_rate_h0","move_rate_h1","own_rate","wealth_h_0","wealth_h_1"]))


# testing
include("test/test_param.jl")
include("test/test_model.jl")
include("test/test_solver.jl")
include("test/test_solution.jl")
include("test/test_sim.jl")    # disabled
	



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





