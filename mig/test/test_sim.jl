
module test_sim


using FactCheck
using mig


facts("testing fill_azmats!, fill_ktmp!, and H-choice versions") do

	p = Param(2)
	m = Model(p)
	mig.setrand!(m)

	agrid = m.grids["assets"]
	zsupps = Dict{Int,Array{Float64,1}}()
	for j in 1:p.nJ
		zsupps[j] = m.gridsXD["zsupp"][:,j]
	end
	azmat_v = zeros(p.na,p.nz)
	azmat_c = zeros(p.na,p.nz)
	azmat_s = zeros(p.na,p.nz)

	ihh  = rand(1:p.nh)
	ik   = rand(1:p.nJ)
	is   = rand(1:p.ns)
	ih   = rand(1:p.nh)
	iy   = rand(1:p.ny)
	ip   = rand(1:p.np)
	itau = rand(1:p.ntau)
	ij   = rand(1:p.nJ)
	it   = rand(1:(p.nt-1))

	context("testing fill_azmats!") do

		mig.fill_azmats!(azmat_v,azmat_c,azmat_s,ihh,ik,is,iy,ip,itau,ih,ij,it,p,m)

		for iia in 1:p.na
			for iz in 1:p.nz
				idx = mig.idx11(ihh,ik,is,iz,iy,ip,itau,iia,ih,ij,it,p)
				@fact azmat_v[iia,iz] => m.vh[idx]
				@fact azmat_c[iia,iz] => m.ch[idx]
				@fact azmat_s[iia,iz] => m.sh[idx]
			end
		end
	end

	context("testing fill_ktmp!") do


		for itest in 1:50


			is   = rand(1:p.ns)
			ih   = rand(1:p.nh)
			iy   = rand(1:p.ny)
			ip   = rand(1:p.np)
			itau = rand(1:p.ntau)
			ij   = rand(1:p.nJ)
			it   = rand(1:(p.nt-1))

			# get a random point
			a = rand() * (agrid[end] - agrid[1]) + agrid[1]
			z = rand() * (zsupps[ij][end] - zsupps[ij][1]) + zsupps[ij][1]

			ktmp = zeros(p.nJ)
			ktmp2 = zeros(p.nJ)

			for ik in 1:p.nJ

				# interpolate rho function in asset and z dim
				for iia in 1:p.na
					for iz in 1:p.nz
						azmat_v[iia + p.na*(iz-1)] = m.rho[mig.idx10(ik,is,iz,iy,ip,itau,iia,ih,ij,it,p)] 	# TODO can to faster indexing here
					end
				end
				ktmp[ik] = mig.bilinearapprox(a,z,agrid,zsupps[ij],azmat_v)
			end

			# fast
			fill!(azmat_v,0.0)

			mig.fill_ktmp!(ktmp2,azmat_v,a,z,is,iy,ip,itau,ih,ij,it,p,agrid,zsupps,m.rho)

			# check
			@fact sumabs(ktmp .- ktmp2) => roughly(0.0,atol=1e-12)

		end




	end

	context("testing fill_azmats! for ousing choice") do
		
		mig.fill_azmats_h!(azmat_v,azmat_c,ik,is,iy,ip,itau,ih,ij,it,p,m)

		for iia in 1:p.na
			for iz in 1:p.nz
				@fact azmat_v[iia + p.na*(iz-1)] => m.vh[mig.idx11(1,ik,is,iz,iy,ip,itau,iia,ih,ij,it,p)]
				@fact azmat_c[iia + p.na*(iz-1)] => m.vh[mig.idx11(2,ik,is,iz,iy,ip,itau,iia,ih,ij,it,p)]
			end
		end


	end

	context("testing fill_azmats! for 2 values") do
		
		mig.fill_azmats!(azmat_v,azmat_c,ihh,ik,is,iy,ip,itau,ih,ij,it,p,m.vh,m.ch)

		for iia in 1:p.na
			for iz in 1:p.nz
				@fact azmat_v[iia + p.na*(iz-1)] => m.vh[mig.idx11(ihh,ik,is,iz,iy,ip,itau,iia,ih,ij,it,p)]
				@fact azmat_c[iia + p.na*(iz-1)] => m.ch[mig.idx11(ihh,ik,is,iz,iy,ip,itau,iia,ih,ij,it,p)]
			end
		end


	end

end


facts("testing fill_azmats_rent") do

	p = Param(2)
	m = Model(p)
	mig.setrand!(m)

	agrid = m.grids["assets"]
	agrid_rent = m.grids["assets"][m.aone:end]
	zsupps = Dict{Int,Array{Float64,1}}()
	for j in 1:p.nJ
		zsupps[j] = m.gridsXD["zsupp"][:,j]
	end

	na_rent = length(m.grids["assets_rent"])
	aone = m.aone

	azmat_v_rent = zeros(na_rent,p.nz)
	azmat_s_rent = zeros(length(agrid_rent),p.nz)
	azmat_c_rent = zeros(length(agrid_rent),p.nz)

	for itest = 1:50

		ihh  = rand(1:p.nh)
		ik   = rand(1:p.nJ)
		is   = rand(1:p.ns)
		ih   = rand(1:p.nh)
		iy   = rand(1:p.ny)
		ip   = rand(1:p.np)
		itau = rand(1:p.ntau)
		ij   = rand(1:p.nJ)
		it   = rand(1:(p.nt-1))

		mig.fill_azmats_rent!(azmat_v_rent,azmat_c_rent,azmat_s_rent,ihh,ik,is,iy,ip,itau,ih,ij,it,p,m)

		for iia in aone:p.na
			for iz in 1:p.nz
				idx = mig.idx11(ihh,ik,is,iz,iy,ip,itau,iia,ih,ij,it,p)
				@fact azmat_v_rent[iia-aone+1 + na_rent*(iz-1)] => m.vh[idx]
				@fact azmat_c_rent[iia-aone+1 + na_rent*(iz-1)] => m.ch[idx]
				@fact azmat_s_rent[iia-aone+1 + na_rent*(iz-1)] => m.sh[idx]
			end
		end
	end

end



end	#module

