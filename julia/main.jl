

# main programme
cd("/Users/florianoswald/git/migration/julia")
include("mods/mig.jl")
p = mig.Param()
m = mig.Model(p)
m = mig.solve(m,p)

cp = mig.Copula(2,0.8)
mig.dnormCopula([0.1,0.2],cp)
