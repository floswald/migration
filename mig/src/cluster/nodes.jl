

if length(DL_LOAD_PATH==0)
	push!(DL_LOAD_PATH, "~/local/lib")
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
plist = ["gamma","xi1","xi2","omega2","MC0","MC1","MC3","MC3_2","MC4","taudist"]

p2 = Dict{ASCIIString,Float64}()

# get from initial p
p0 = mig.Param(2)
for p in plist
	p2[p] = getfield(p0,symbol(p))
end

# setup params to estimate
# define bounds
pb = Dict{ASCIIString,Array{Float64,1}}()
pb["gamma"]  = [1.1,3]
# pb["lambda"] = [0.0,2]
pb["xi1"]    = [0.0,1]
pb["xi2"]    = [0.0,1]
# pb["omega1"] = [0.0,3]
pb["omega2"] = [0.0,3]
pb["MC0"]    = [0,8]
pb["MC1"]    = [0.0,5]
# pb["MC2"]    = [0.0,0.0005]
pb["MC3"]    = [-0.25,0.25]
pb["MC3_2"]    = [-0.01,0.025]
pb["MC4"]    = [-3,3]
pb["taudist"]    = [0.01,0.99]


# subsetting moments
submom = setdiff(moms[:moment],["lm_w_intercept","move_neg_equity","q25_move_distance","q50_move_distance","q75_move_distance"])

# setup the minimization problem
mprob = MProb(p2,pb,mig.objfunc,moms,moments_subset=submom)
