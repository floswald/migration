

using mig

# get moments from dropbox:
tmpfile = tempname()
run(`dropbox_uploader download mobility/output/model/data_repo/in_data_jl/moments.rda $tmpfile`)
moms = mig.DataFrame(mig.read_rda(tmpfile)["m"])

# setup parameters to estimate:

# p2 = ["gamma"=>1.1,"MC1"=> 0.03,"MC2"=> 0.02,"MC3"=> 0.01,"omega1"=>0.3, "omega2"=> 0.1

p2 = Dict{ASCIIString,Float64}()
p2["gamma"]  = 2.1
p2["lambda"] = 2.1
p2["xi1"]    = 2.1
p2["xi2"]    = 2.1
p2["omega1"] = 2.1
p2["omega2"] = 2.1
p2["MC1"]    = 0.1
p2["MC2"]    = 0.1
p2["MC3"]    = 0.1
p2["MC4"]    = 0.1

# setup params to estimate
pb = Dict{ASCIIString,Array{Float64,1}}()
pb["gamma"]  = [1.1,5]
pb["lambda"] = [0.0,10]
pb["xi1"]    = [0.0,3.0]
pb["xi2"]    = [0.0,3.0]
pb["omega1"] = [0.0,1.0]
pb["omega2"] = [0.0,1.0]
pb["MC1"]    = [0.0,0.5]
pb["MC2"]    = [0.0,0.5]
pb["MC3"]    = [0.0,0.5]
pb["MC4"]    = [0.0,0.5]

mprob = MOpt.MProb(p2,pb,mig.objfunc,moms,moments_subset=setdiff(moms[:moment],["moved0","moved1","moved2","move_rate","move_rate_h0","move_rate_h1","own_rate","wealth_h_0","wealth_h_1"]))
