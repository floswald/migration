

if ENV["USER"] == "florian_o"
	push!(DL_LOAD_PATH, "/home/florian_o/local/lib")
elseif ENV["USER"] == "eisuc151"
	push!(DL_LOAD_PATH, "/home/eisuc151/local/lib")
end


using mig, MOpt


# get moments from dropbox:
if Sys.OS_NAME == :Darwin
	indir = joinpath(ENV["HOME"],"Dropbox/mobility/output/model/data_repo/in_data_jl")
	outdir = joinpath(ENV["HOME"],"Dropbox/mobility/output/model/data_repo/out_data_jl")
elseif Sys.OS_NAME == :Windows
	indir = "C:\\Users\\florian_o\\Dropbox\\mobility\\output\\model\\data_repo\\in_data_jl"
	outdir = "C:\\Users\\florian_o\\Dropbox\\mobility\\output\\model\\data_repo\\out_data_jl"
else
	indir = joinpath(ENV["HOME"],"data_repo/mig/in_data_jl")
	outdir = joinpath(ENV["HOME"],"data_repo/mig/out_data_jl")
end
moms = mig.DataFrame(mig.read_rda(joinpath(indir,"moments.rda"))["m"])

# want to estimate those:
plist = ["gamma","xi1","xi2","omega3","MC1","MC3","MC3_2","MC4","taudist"]

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
pb["gamma"]  = [1.1,3]
# pb["lambda"] = [0.0,2]
pb["xi1"]    = [0.0,0.1]
pb["xi2"]    = [0.0,0.1]
# pb["omega1"] = [0.0,3]
pb["omega3"] = [0.0,0.1]
# pb["MC0"]    = [4,10]
pb["MC1"]    = [0.0,0.5]
# pb["MC2"]    = [0.0,0.0005]
pb["MC3"]    = [0,0.2]
pb["MC3_2"]    = [0.0,0.0003]
pb["MC4"]    = [0,1]
pb["taudist"]    = [0.01,0.99]

# options for objective function
objfunc_opts = ["printlevel" => 1,"printmoms"=>false]


# subsetting moments
submom = setdiff(moms[:moment],["lm_w_intercept","move_neg_equity","q25_move_distance","q50_move_distance","q75_move_distance"])

# setup the minimization problem
mprob = MProb(p2,pb,mig.objfunc,moms,moments_subset=submom,objfunc_opts=objfunc_opts)
