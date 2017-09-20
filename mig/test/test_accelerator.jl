



@testset "accelerator tests" begin

	@testset "testing searchsorted1step" begin

	n = 11
	y = collect(linspace(1,11.0,n))
	x = 8.9
	i = 1
	@test mig.searchsorted1step(y,x,i,n) == 8

	n = 13
	y = collect(linspace(1,11.0,n))
	x = 0.9
	i = 4
	@test mig.searchsorted1step(y,x,i,n) == 3

	n = 15
	y = collect(linspace(1,15,n))
	x = 1.999
	i = 1
	@test mig.searchsorted1step(y,x,i,n) == 1

	x = 2.999
	@test mig.searchsorted1step(y,x,i,n) == 2
	x = 8.999
	@test mig.searchsorted1step(y,x,i,n) == 8
	end

	@testset "testing  1 step bracket search" begin

		acc = mig.Accelerator(1)
		n = 18
		x = collect(linspace(1,18,n))
		y = 2.29 .* x

		@test mig.linearapprox(x,y,0.0,n,acc) == 0.0
		@test isapprox(mig.linearapprox(x,y,100.0,n,acc), 100*2.29)

		@test mig.linearapprox(x,y,1.9,n,acc) == 2.29 * 1.9
		@test acc.i == 1

		@test mig.linearapprox(x,y,8.9,n,acc) == 2.29 * 8.9
		@test acc.i == 8

		@test mig.linearapprox(x,y,13.1,n,acc) == 2.29 * 13.1
		@test acc.i == 13

		@test isapprox(mig.linearapprox(x,y,17.1,n,acc) , 2.29 * 17.1)
		@test acc.i == 17
		@test isapprox(mig.linearapprox(x,y,18.1,n,acc) , 2.29 * 18.1)
		@test acc.i == 17
	end
end
