


@testset "test updating of param vector" begin

	@testset "standard" begin
		p = mig.Param(1);

		di = Dict("beta" => 1.4, "omega2" => 34.2)
		mig.update!(p,di)

		@test p.beta == di["beta"]
		@test p.omega2 == di["omega2"]
	end

	@testset "size 2" begin
		p = mig.Param(2);
		@test p.omega2 != 0

	end


end
gc()




