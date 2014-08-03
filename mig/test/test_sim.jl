
module test_sim


using FactCheck
using mig

facts("test fx space indexers") do

	p = Param(2)

	context("getting the discrete index") do

		v   = rand(p.nh,p.nJ,p.ns,p.nh,p.ntau,p.nJ,(p.nt-1))
		rho = rand(p.nJ,p.ns,p.nh,p.ntau,p.nJ,(p.nt-1))

		for ix in 1:10

			ihh  = rand(1:p.nh)
			ik   = rand(1:p.nJ)
			is   = rand(1:p.ns)
			ih   = rand(1:p.nh)
			itau = rand(1:p.ntau)
			ij   = rand(1:p.nJ)
			it   = rand(1:(p.nt-1))

			@fact v[ihh,ik,is,ih,itau,ij,it] => v[mig.fx_idx(ihh,ik,is,ih,itau,ij,it,p)]
			@fact rho[ik,is,ih,itau,ij,it] => rho[mig.fx_idx_rho(ik,is,ih,itau,ij,it,p)]
		end
	end

	context("getting continuous dimension values") do

		m = Model(p)
		mig.setrand!(m)

		myv11 = zeros(p.na,p.np,p.ny,p.nz)
		myv10 = zeros(p.na,p.np,p.ny,p.nz)
		
		ihh  = rand(1:p.nh)
		ik   = rand(1:p.nJ)
		is   = rand(1:p.ns)
		ih   = rand(1:p.nh)
		itau = rand(1:p.ntau)
		ij   = rand(1:p.nJ)
		it   = rand(1:(p.nt-1))

		vout11 = reshape(mig.get_cont_vals(ihh,ik,is,ih,itau,ij,it,m.vh,p),size(myv11))
		vout10 = reshape(mig.get_cont_vals(ik,is,ih,itau,ij,it,m.rho,p),size(myv10))


		for iz = 1:p.nz
		for iy = 1:p.ny
		for ip = 1:p.np
		for ia = 1:p.na

			myv11[ia,ip,iy,iz] = m.vh[mig.idx11(ihh,ik,is,iy,ip,iz,ia,ih,itau,ij,it,p)]
			myv10[ia,ip,iy,iz] = m.rho[mig.idx10(ik,is,iy,ip,iz,ia,ih,itau,ij,it,p)]

			@fact myv11[ia,ip,iy,iz] => vout11[ia,ip,iy,iz]
			@fact myv10[ia,ip,iy,iz] => vout10[ia,ip,iy,iz]

		end
		end
		end
		end

	end

end

facts("testing setupFSpaceXD") do

	p = Param(2)
	m = Model(p)
	mig.setincreasing!(m)

	# test bounds on a random state

	ihh  = rand(1:p.nh)
	ik   = rand(1:p.nJ)
	is   = rand(1:p.ns)
	ih   = rand(1:p.nh)
	itau = rand(1:p.ntau)
	ij   = rand(1:p.nJ)
	it   = rand(1:(p.nt-1))

	@profile fx = mig.setupFSpaceXD(m,p)


	rhoidx = fx_idx_rho(ij,age,itau,ih,is,ik)
	@fact fx["rho"][rhoidx].basis[1].lower => m.grids["assets"][1]

end













end
















end

