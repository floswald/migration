

# Monte Carlo node file

autoload=true

if ENV["USER"] == "florian_o"
	push!(DL_LOAD_PATH, "/home/florian_o/local/lib")
elseif ENV["USER"] == "eisuc151"
	push!(DL_LOAD_PATH, "/home/eisuc151/local/lib")
end

using mig, MOpt


moms = mig.setupMC(autoload)

p2 = Dict{ASCIIString,Float64}()

# estimate plist
plist = ["xi1"]

# get starting value from initial p
p0 = mig.Param(2)
for p in plist
	p2[p] = getfield(p0,symbol(p))
end

# set off initial value
p2["xi1"]     = 0.01

pb = Dict{ASCIIString,Array{Float64,1}}()
pb["xi1"]    = [0.0,0.1]

# options for objective function
objfunc_opts = ["printlevel" => 1,"printmoms"=>false]

# subsetting moments
dont_use= ["lm_w_intercept","move_neg_equity","q25_move_distance","q50_move_distance","q75_move_distance"]
for iw in moms[:moment]
	if contains(iw,"wealth") && (!contains(iw,"own"))
		push!(dont_use,iw)
	end
end
push!(dont_use,"mean_own_ENC")
push!(dont_use,"mean_own_ESC")
push!(dont_use,"mean_own_MdA")
push!(dont_use,"mean_own_Mnt")
push!(dont_use,"mean_own_NwE")
push!(dont_use,"mean_own_Pcf")
push!(dont_use,"mean_own_StA")
push!(dont_use,"mean_own_WNC")
push!(dont_use,"mean_own_WSC")

submom = setdiff(moms[:moment],dont_use)


# setup the minimization problem
mprob = MProb(p2,pb,mig.objfunc,moms,moments_subset=submom,objfunc_opts=objfunc_opts)
end


