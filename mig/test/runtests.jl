

using mig 
using Base.Test
using TestSetExtensions


@testset ExtendedTestSet "Running mig tests" begin

    @includetests ARGS

end
    

