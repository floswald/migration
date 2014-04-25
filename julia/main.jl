

# main programme
cd("/Users/florianoswald/git/migration/julia")
include("mods/mig.jl")
p = mig.Param()
m = mig.Model(p)
m = mig.solve(m,p)

cp = mig.Copmod.Copula(2,0.8)
