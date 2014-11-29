

if ENV["USER"] == "florian_o"
	push!(DL_LOAD_PATH, "/home/florian_o/local/lib")
elseif ENV["USER"] == "eisuc151"
	push!(DL_LOAD_PATH, "/home/eisuc151/local/lib")
end


using mig, MOpt


indir, outdir = mig.setPaths()
moms = mig.DataFrame(mig.read_rda(joinpath(indir,"moments.rda"))["m"])

# want to estimate those:
plist = ["xi1","xi2","omega2","MC0","MC1","MC2","MC3","MC4","taudist"]

p2 = Dict{ASCIIString,Float64}()

# get starting value from initial p
p0 = mig.Param(2)
for p in plist
	p2[p] = getfield(p0,symbol(p))
end

# or assign different starting values:
# p2["gamma"]   = 2.0
# p2["xi1"]     = 1.0
# p2["xi2"]     = 1.0
# p2["omega2"]  = 0.0
# p2["MC0"]     = 0.0
# p2["MC1"]     = 4.0
# p2["MC3"]     = -0.2
# p2["MC3_2"]     = 0.0
# p2["MC4"]     = 0.0
# p2["taudist"] = 0.25


# setup params to estimate
# define bounds
pb = Dict{ASCIIString,Array{Float64,1}}()
# pb["lambda"] = [0.0,2]
pb["xi1"]    = [0.0,0.1]
pb["xi2"]    = [0.0,0.1]
# pb["omega1"] = [0.0,3]
# pb["omega1"] = [0.9,1.1]
pb["omega2"] = [4.0,6.1]
pb["MC0"]    = [1,4]
pb["MC1"]    = [0.0,0.04]
pb["MC2"]    = [0.0,0.05]
pb["MC3"]    = [0,1]
pb["MC4"]    = [0,1]
pb["taudist"]    = [0.01,0.99]

# options for objective function
objfunc_opts = ["printlevel" => 1,"printmoms"=>false]


# subsetting moments
dont_use= ["lm_w_intercept","move_neg_equity","q25_move_distance","q50_move_distance","q75_move_distance"]
for iw in moms[:moment]
	if contains(iw,"wealth") 
		push!(dont_use,iw)
	end
end

submom = setdiff(moms[:moment],dont_use)

# setup the minimization problem
mprob = MProb(p2,pb,mig.objfunc,moms,moments_subset=submom,objfunc_opts=objfunc_opts)
