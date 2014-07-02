

using MOpt, mig

# get moments from dropbox:
tmpfile = tempname()
run(`dropbox_uploader download mobility/output/model/data_repo/in_data_jl/moments.rda $tmpfile`)
moms = DataFrame(read_rda(tmpfile)["m"])

# setup parameters to estimate:

# p2 = ["gamma"=>1.1,"MC1"=> 0.03,"MC2"=> 0.02,"MC3"=> 0.01,"omega1"=>0.3, "omega2"=> 0.1]
p2 = ["gamma"=>1.1]
# setup params to estimate
pb = ["gamma" => [1.01,5.0]]
# pb = ["gamma" => [1.01,5.0],"MC1"=> [0.00,1.0],"MC2"=> [0.00,1.0],"MC3"=> [0.00,1.0], "Omega1"=> [0.00,1.0],"Omega2"=> [0.00,1.0]]

mprob = MProb(p2,pb,mig.objfunc,moms,moments_subset=["moved0"])
