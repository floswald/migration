

# main programme
cd("/Users/florianoswald/git/migration/julia")

include("mods/mig.jl")


# testing
include("test/test_param.jl")
include("test/test_model.jl")



# running
p = mig.Param()
m = mig.Model(p)
m = mig.solve(m,p)

cp = mig.Copmod.Copula(2,0.8)


