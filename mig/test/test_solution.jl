

module test_solution

using FactCheck, mig



facts("checking some properties of the solution") do

	p = Param(2)
	m = Model(p)
	println("solving model")
	mig.solve!(m,p)
	println("done solving model. now testing.")

	context("test the range of outputs from final value") do

		mval = p.omega1 + p.omega2 * log(maximum(m.grids["assets"]) + maximum(m.gridsXD["p"]) )
		
		@fact minimum(m.EVfinal) => p.myNA
		@fact maximum(m.EVfinal) => mval
		@fact all(m.EVfinal[1:(m.aone-1),:,:,:] .== p.myNA) => true

	end

	context("check conditional v is never decreasing in a") do

		tt = mapslices(x -> diff(x),m.vh,8)
		@fact all(tt .>= 0.0) => true

	end

	# context("check conditional v is never decreasing in z") do

	# 	tt = mapslices(x -> diff(x),m.vh,4)
	# 	@fact all(tt .>= 0.0) => true

	# end

	context("check prob of moving") do

		for itest in 1:10000

			is   = rand(1:p.ns)
			ia   = rand(1:p.na)
			ih   = rand(1:p.nh)
			iy   = rand(1:p.ny)
			ip   = rand(1:p.np)
			iz   = rand(1:p.nz)
			itau = rand(1:p.ntau)
			ij   = rand(1:p.nJ)
			age   = rand(1:(p.nt-1))

			if any(m.v[:,is,iz,iy,ip,itau,ia,ih,ij,age][:] .> p.myNA)
				tt = m.rho[:,is,iz,iy,ip,itau,ia,ih,ij,age][:]

				@fact sum(tt) => roughly(1.0,atol=1e-9)
				@fact minimum(tt) >= 0.0 => true
				@fact maximum(tt) <= 1.0 => true
			end


		end


	end

	context("check whether cons positive at feasible states") do

		feas = m.vh .> p.myNA

		@fact all(m.ch[feas] .> 0.0) => true

	end

end


end # module