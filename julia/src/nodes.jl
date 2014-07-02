

using MOpt, mig

# load moments from local disk:

# indir  = joinpath(ENV["HOME"],"Dropbox/mobility/output/model/R2Julia")
# moms = DataFrame(read_rda(joinpath(indir,"moments.rda"))["m"])

# get moments from dropbox:
tmpfile = tempname()
run(`dropbox_uploader download mobility/output/model/data_repo/in_data_jl/moments.rda $tmpfile`)
moms = DataFrame(read_rda(tmpfile)["m"])

# setup parameters to estimate:

p2 = ["gamma"=>1.1,"MC1"=> 0.03]
# setup params to estimate
pb = ["gamma" => [1.01,5.0]]

mprob = MProb(p2,pb,mig.objfunc,moms,moments_subset=["moved0"])
