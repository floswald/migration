

using mig, MOpt

# get moments from dropbox:
if Sys.OS_NAME == :Darwin
	indir = joinpath(ENV["HOME"],"Dropbox/mobility/output/model/data_repo/in_data_jl")
else
	indir = joinpath(ENV["HOME"],"data_repo/mig/in_data_jl")
end
moms = mig.DataFrame(mig.read_rda(joinpath(indir,"moments.rda"))["m"])

# setup parameters to estimate:

# p2 = ["gamma"=>1.1,"MC1"=> 0.03,"MC2"=> 0.02,"MC3"=> 0.01,"omega1"=>0.3, "omega2"=> 0.1

# want to estimate those:
plist = ["gamma","lambda","xi1","xi2","omega1","omega2","MC1","MC2","MC3","MC4","Rm"]

p2 = Dict{ASCIIString,Float64}()

# get from initial p
p0 = Param(2)
for p in plist
	p2[p] = getfield(p0,symbol(p))
end

# setup params to estimate
# define bounds
pb = Dict{ASCIIString,Array{Float64,1}}()
pb["gamma"]  = [1.1,3]
pb["lambda"] = [0.0,10]
pb["xi1"]    = [0.0,3.0]
pb["xi2"]    = [0.0,3.0]
pb["omega1"] = [0.0,1.0]
pb["omega2"] = [0.0,1.0]
pb["MC1"]    = [0.0,0.5]
pb["MC2"]    = [0.0,0.5]
pb["MC3"]    = [0.0,0.5]
pb["MC4"]    = [0.0,0.5]
pb["Rm"]    = [1.03,1.2]

mprob = MProb(p2,pb,mig.objfunc,moms,moments_subset=setdiff(moms[:moment],["moved0","moved1","moved2","move_rate","move_rate_h0","move_rate_h1","own_rate","wealth_h_0","wealth_h_1"]))
