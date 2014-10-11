

# main programme
home = ENV["HOME"]
cd("$home/git/migration/mig")


# include("src/estimation.jl")
cd("$home/git/migration/mig.jl/src/sge")
include("examples/slices.jl")

include("src/mig.jl")
x=mig.runObj(true)
					
# run simulation
# plot 5 guys
# return simulation
@time s = mig.runSim();

p = mig.Param(2)
@time m = mig.Model(p)	# 1.5 secs
@time m = mig.Model(p,policy="mortgageSubsidy")	# 1.5 secs
@time mig.solve!(m,p)	
@time s = mig.simulate(m,p);	
x=mig.computeMoments(s,p,m)
@profile s = mig.simulate(m,p);	


s2 = s[!mig.isna(s[:cohort]),:]
mig.simplot(s2[s2[:cohort].<=14,:],5)
s96 = @where(s2,:year .> 1996)

# gadfly plotting
# ---------------
oc = @by(s2,[:realage,:cohort],o=mean(:own),buy=mean((:h.==0)&(:hh.==1)),sell=mean((:h.==1)&(:hh.==0)))
oc96 = @by(s96,[:realage,:cohort],o=mean(:own),buy=mean((:h.==0)&(:hh.==1)),sell=mean((:h.==1)&(:hh.==0)))
plot(oc,x="realage",y="o",color="cohort",Geom.line)
plot(oc96,x="realage",y="o",color="cohort",Geom.line)


mig.vhplot(m,p,(7,1,2,3,3,1,7,1))
mig.vhplot(m,p,(7,1,2,3,3,1,8,1))

mig.vhplot(m,p,(1,1,1,1,1,1,1,1))
mig.vhplot(m,p,(1,1,1,3,3,1,1,1))
mig.vhplot(m,p,(1,1,4,3,3,1,1,1))
mig.vhplot(m,p,(1,1,4,3,3,1,1,30))
mig.vhplot(m,p,(1,1,2,1,1,1,1,30))

hcat(m.vh[1,1,1,4,3,3,1,:,1,1,30][:],m.ch[1,1,1,4,3,3,1,:,1,1,30][:],m.sh[1,1,1,4,3,3,1,:,1,1,30][:])

mig.vhplot(m,p,(1,1,4,3,3,1,1,30))
mig.vhplot(m,p,(1,1,4,3,3,1,1,29))
mig.vhplot(m,p,(1,1,4,3,3,1,1,28))
mig.vhplot(m,p,(1,1,4,3,3,1,1,27))

mig.vhplot(m,p,(1,1,4,3,3,1,1,28))
mig.vhplot(m,p,(1,1,4,3,3,1,1,27))
mig.vhplot(m,p,(1,1,4,3,3,1,1,26))
mig.vhplot(m,p,(1,1,4,3,3,1,1,25))
mig.vhplot(m,p,(1,1,4,3,3,1,1,24))
mig.vhplot(m,p,(1,1,4,3,3,1,1,22))

mig.vhplot(m,p,(1,1,4,3,3,2,1,1))
mig.vhplot(m,p,(1,1,4,3,3,2,1,28))
mig.vhplot(m,p,(1,1,1,1,1,1,1,28))
mig.vhplot(m,p,(4,1,4,3,1,1,4,10))
mig.vplot(m,p)


mig.simplot(s,5)
@profile s = mig.simulate(m,p);	

# mms2 = mig.simulate_parts(m,p,5)

# write sim to disk
outdir = joinpath(ENV["HOME"],"Dropbox/mobility/output/model/data_repo/out_data_jl")
mig.simexport(s,[1,7338,6960,13056,11303,7],joinpath(outdir,"simdata.csv"))


maximum(m.EV[:,:,:,:,:,:,:,:,29])
m.sh[1,1,1,1,1,1,:,1,1,1,1]
hcat(m.vh[1,1,1,1,1,1,1,:,1,1,28][:],m.ch[1,1,1,1,1,1,1,:,1,1,28][:],m.sh[1,1,1,1,1,1,1,:,1,1,28][:])
hcat(m.vh[1,1,1,1,1,1,1,:,1,1,1][:],m.ch[1,1,1,1,1,1,1,:,1,1,1][:],m.sh[1,1,1,1,1,1,1,:,1,1,1][:])
hcat(m.vh[1,1,1,1,1,1,1,:,2,1,28][:],m.ch[1,1,1,1,1,1,1,:,2,1,28][:],m.sh[1,1,1,1,1,1,1,:,2,1,28][:])

reshape(m.vh[1,:,1,1,1,1,m.aone,1,1,:,1],p.nJ,p.nJ)
reshape(m.v[:,1,1,1,1,m.aone,1,1,:,1],p.nJ,p.nJ)
reshape(m.rho[:,1,1,1,1,m.aone,1,1,:,1],p.nJ,p.nJ)


show(mig.DataFrame(moment=["move","own"],value=[mean(s[:move]),mean(s[:h])]))
	
@time mms = mig.computeMoments(s,p,m);

# run objective
p2 = Dict{ASCIIString,Float64}()
p2["gamma"] = 1.4
indir = joinpath(ENV["HOME"],"Dropbox/mobility/output/model/data_repo/in_data_jl")
moms = mig.DataFrame(mig.read_rda(joinpath(indir,"moments.rda"))["m"])

@time x = mig.objfunc(p2,moms,array(moms[:moment]))
mprob = MOpt.MProb(p2,pb	,mig.objfunc,moms,moments_subset=setdiff(moms[:moment],["moved0","moved1","moved2","move_rate","move_rate_h0","move_rate_h1","own_rate","wealth_h_0","wealth_h_1"]))


# testing
include("test/test_param.jl")
include("test/test_model.jl")
include("test/test_solver.jl")
include("test/test_solution.jl")
include("test/test_experiment.jl")
include("test/test_sim.jl")    
include("test/test_accelerator.jl")    