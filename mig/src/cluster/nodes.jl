
# sets up an MProb on each node

using mig, MOpt


io = mig.setPaths()
moms = mig.DataFrame(mig.read_rda(joinpath(io["indir"],"moments.rda"))["m"])
mig.names!(moms,[:name,:value,:weight])
# subsetting moments
dont_use= ["lm_w_intercept","move_neg_equity"]
for iw in moms[:name]
	if contains(iw,"wealth") 
		push!(dont_use,iw)
	end
end
use_names = setdiff(moms[:name],dont_use)
moms_use = moms[findin(moms[:name],use_names) ,:]

# initial value
p0 = mig.Param(2)
pb = Dict{ASCIIString,Array{Float64}}()
pb["xi1"] = [p0.xi1, 0.0,0.02]
pb["xi2"] = [p0.xi2, 0.0,0.1]
pb["omega2"] = [p0.omega2, 2.0,4.1]
pb["MC0"] = [p0.MC0, 2.0,4.0]
pb["MC1"] = [p0.MC1, 0.0,0.04]
pb["MC2"] = [p0.MC2, 0.0,0.01]
pb["MC3"] = [p0.MC3, 0.0,1]
pb["MC4"] = [p0.MC4, 0.0,1]
pb["taudist"] = [p0.taudist, 0.0,1]
pb["amenity_ENC"] = [p0.amenity_ENC, 0.0,1]
pb["amenity_ESC"] = [p0.amenity_ESC, 0.0,1]
pb["amenity_MdA"] = [p0.amenity_MdA, 0.0,1]
pb["amenity_Mnt"] = [p0.amenity_Mnt, 0.0,1]
pb["amenity_NwE"] = [p0.amenity_NwE, 0.0,1]
pb["amenity_Pcf"] = [p0.amenity_Pcf, 0.0,1]
pb["amenity_StA"] = [p0.amenity_StA, 0.0,1]
pb["amenity_WNC"] = [p0.amenity_WNC, 0.0,1]
pb["amenity_WSC"] = [p0.amenity_WSC, 0.0,1]

mprob = MOpt.MProb() 
MOpt.addSampledParam!(mprob,pb) 
MOpt.addMoment!(mprob,moms_use) 
MOpt.addEvalFunc!(mprob,mig.objfunc)

