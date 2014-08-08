
module test_sim


using FactCheck
using mig

facts("test fx space indexers") do

	p = Param(2)

	context("getting the discrete index") do

		v   = rand(p.nh,p.nJ,p.ns,p.nh,p.ntau,p.nJ,p.nt-1)
		rho = rand(p.nJ,p.ns,p.nh,p.ntau,p.nJ,p.nt-1)

		for ix in 1:10

			ihh  = rand(1:p.nh)
			ik   = rand(1:p.nJ)
			is   = rand(1:p.ns)
			ih   = rand(1:p.nh)
			itau = rand(1:p.ntau)
			ij   = rand(1:p.nJ)
			iage   = rand(1:p.nt-1)

			@fact v[ihh,ik,is,ih,itau,ij,iage] => v[mig.fx_idx(ihh,ik,is,ih,itau,ij,iage,p)]
			@fact rho[ik,is,ih,itau,ij,iage] => rho[mig.fx_idx_rho(ik,is,ih,itau,ij,iage,p)]
		end
	end

	context("getting continuous dimension values") do

		m = Model(p)
		mig.setrand!(m)

		myv11 = zeros(p.na,p.ny,p.np,p.nz)
		myv10 = zeros(p.na,p.ny,p.np,p.nz)
		
		# get a random discrete state
		ihh  = rand(1:p.nh)
		ik   = rand(1:p.nJ)
		is   = rand(1:p.ns)
		ih   = rand(1:p.nh)
		itau = rand(1:p.ntau)
		ij   = rand(1:p.nJ)
		iage   = rand(1:p.nt-1)

		vout11 = reshape(mig.get_cont_vals(ihh,ik,is,ih,itau,ij,iage,m.vh,p),size(myv11))
		vout10 = reshape(mig.get_cont_vals(    ik,is,ih,itau,ij,iage,m.rho,p),size(myv10))

		for ix = 1:40

			iz = rand(1:p.nz)
			ip = rand(1:p.np)
			iy = rand(1:p.ny)
			ia = rand(1:p.na)

									# dimvecH  = (nh, nJ, ns, nz, ny, np, na, nh, ntau,  nJ, nt-1 )
			myv11[ia,iy,ip,iz] = m.vh[mig.idx11(ihh,ik,is,iz,iy,ip,ia,ih,itau,ij,iage,p)]
			myv10[ia,iy,ip,iz] = m.rho[mig.idx10(ik,is,iz,iy,ip,ia,ih,itau,ij,iage,p)]

			@fact myv11[ia,iy,ip,iz] => vout11[ia,iy,ip,iz]
			@fact myv10[ia,iy,ip,iz] => vout10[ia,iy,ip,iz]

		end
		
	end

end

facts("testing setupFSpaceXD") do

	p = Param(2)
	m = Model(p)
	mig.setincreasing!(m)
	mig.setrand!(m)

	# test on a random state

	ihh  = rand(1:p.nh)
	ik   = rand(1:p.nJ)
	is   = rand(1:p.ns)
	ih   = rand(1:p.nh)
	itau = rand(1:p.ntau)
	iage   = rand(1:p.nt-1)

	t0 = time()
	fx = mig.setupFSpaceXD(m,p);
	println("setupFSpaceXDtiming: $(time()-t0)")

	# checking bounds: they vary with ij
	for jj in 1:p.nJ
		@fact fx["rho"][mig.fx_idx_rho(ik,is,ih,itau,jj,iage,p)].basis[2].lower => m.gridsXD["y"][1,jj]
		@fact fx["rho"][mig.fx_idx_rho(ik,is,ih,itau,jj,iage,p)].basis[3].lower => m.gridsXD["p"][1,jj]
		@fact fx["rho"][mig.fx_idx_rho(ik,is,ih,itau,jj,iage,p)].basis[4].lower => m.gridsXD["zsupp"][1,jj]

		@fact fx["rho"][mig.fx_idx_rho(ik,is,ih,itau,jj,iage,p)].basis[2].upper => m.gridsXD["y"][end,jj]
		@fact fx["rho"][mig.fx_idx_rho(ik,is,ih,itau,jj,iage,p)].basis[3].upper => m.gridsXD["p"][end,jj]
		@fact fx["rho"][mig.fx_idx_rho(ik,is,ih,itau,jj,iage,p)].basis[4].upper => m.gridsXD["zsupp"][end,jj]
	end


	for it in 1:10

		ij   = rand(1:p.nJ)
		ia = rand(1:p.na)
		iy = rand(1:p.ny)
		ip = rand(1:p.np)
		iz = rand(1:p.nz)

		a = m.grids["assets"][ia]
		y = m.gridsXD["y"][iy,ij]
		pr = m.gridsXD["p"][ip,ij]
		z = m.gridsXD["zsupp"][iz,ij]
	
		# get some points on rho
		rhoidx = mig.fx_idx_rho(ik,is,ih,itau,ij,iage,p)

		@fact mig.getValue([a,y,pr,z],fx["rho"][rhoidx]) => roughly(m.rho[ik,is,iz,iy,ip,ia,ih,itau,ij,iage],atol=1e-6)

		# get some on vh
		vidx = mig.fx_idx(ihh,ik,is,ih,itau,ij,iage,p)
		@fact mig.getValue([a,y,pr,z],fx["vh"][vidx]) => roughly(m.vh[ihh,ik,is,iz,iy,ip,ia,ih,itau,ij,iage],atol=1e-6)
		println("approx v = $(mig.getValue([a,y,pr,z],fx["vh"][vidx]))")
		println("true v   = $(m.vh[ihh,ik,is,iz,iy,ip,ia,ih,itau,ij,iage])")

		# get some on sh
		@fact mig.getValue([a,y,pr,z],fx["sh"][vidx]) => roughly(m.sh[ihh,ik,is,iz,iy,ip,ia,ih,itau,ij,iage],atol=1e-6)
		println("approx s = $(mig.getValue([a,y,pr,z],fx["sh"][vidx]))")
		println("true s   = $(m.sh[ihh,ik,is,iz,iy,ip,ia,ih,itau,ij,iage])")

		# get some on ch
		@fact mig.getValue([a,y,pr,z],fx["ch"][vidx]) => roughly(m.ch[ihh,ik,is,iz,iy,ip,ia,ih,itau,ij,iage],atol=1e-6)
		println("approx c = $(mig.getValue([a,y,pr,z],fx["ch"][vidx]))")
		println("true c   = $(m.ch[ihh,ik,is,iz,iy,ip,ia,ih,itau,ij,iage])")


	end




end

end	#module

