
module test_sim


using FactCheck
using mig


facts("testing get_rho_ktmp") do

	p = Param(2)
	m = Model(p)
	mig.setrand!(m)

	agrid = m.grids["assets"]
	Ygrid = m.grids["Y"]
	Pgrid = m.grids["P"]
	zsupps = Dict{Int,Array{Float64,1}}()
	for j in 1:p.nJ
		zsupps[j] = m.gridsXD["zsupp"][:,j]
	end

	gs = Vector{Float64}[]
	push!(gs,agrid)
	push!(gs,zsupps[1])
	push!(gs,Ygrid)
	push!(gs,Pgrid)
	rho_arr = Array{Float64}[]
	for i in 1:p.nJ
		push!(rho_arr,zeros(p.na,p.nz,p.ny,p.np))
	end
	l = mig.lininterp(rho_arr,gs)


	# get a point on the grid
	ia = 3
	iz = 3
	iy = 2
	ip = 3

	for i=1:5

		is = rand(1:p.ns)
		itau = rand(1:p.ntau)
		ih = rand(1:p.nh)
		ij = rand(1:p.nJ)
		it = rand(1:p.nt-1)
		azYP = [agrid[ia],zsupps[ij][iz],Ygrid[iy],Pgrid[ip]]

		mig.ApproXD.setGrid(l,2,zsupps[ij])

		evals = mig.get_rho_ktmp(l,azYP,is,itau,ih,ij,it,p,m)

		for k in 1:p.nJ
			@fact evals[k] => m.rho[mig.idx10(k,is,iz,iy,ip,itau,ia,ih,ij,it,p)]
		end
		mig.ApproXD.resetCache!(l)
	end

	# set rho to a linear combination of indices
	function rhofun(xk,xs,xz,xy,xp,xtau,xa,xh,xj,xt) 
		0.1*xk + 0.2*xs + 0.3*xz + 0.4*xy + 0.5*xp + 0.6*xtau + 0.7*xa + 0.8*xh + 0.9*xj + xt
	end

	for ik=1:p.nJ
	for is=1:p.ns
	for iz=1:p.nz
	for iy=1:p.ny
	for ip=1:p.np
	for itau=1:p.ntau
	for ia=1:p.na
	for ih=1:p.nh
	for ij=1:p.nJ
	for it=1:p.nt-1
		m.rho[mig.idx10(ik,is,iz,iy,ip,itau,ia,ih,ij,it,p)] = rhofun(ik,is,zsupps[ij][iz],Ygrid[iy],Pgrid[ip],itau,agrid[ia],ih,ij,it)
	end
	end
	end
	end
	end
	end
	end
	end
	end
	end


	# pick some random points on rho
	for i in 1:50
		is = rand(1:p.ns)
		itau = rand(1:p.ntau)
		ih = rand(1:p.nh)
		ij = rand(1:p.nJ)
		it = rand(1:p.nt-1)

		mig.ApproXD.setGrid(l,2,zsupps[ij])

		a = rand() * (agrid[end]-agrid[1]) + agrid[1]
		z = rand() * (zsupps[ij][end]-zsupps[ij][1]) + zsupps[ij][1]
		Y = rand() * (Ygrid[end]-Ygrid[1]) + Ygrid[1]
		P = rand() * (Pgrid[end]-Pgrid[1]) + Pgrid[1]
		v = mig.get_rho_ktmp(l,[a,z,Y,P],is,itau,ih,ij,it,p,m)
		for k in 1:p.nJ
			@fact v[k] - rhofun(k,is,z,Y,P,itau,a,ih,ij,it) => roughly(0.0,atol=1e-6)
		end
		mig.ApproXD.resetCache!(l)
	end
end


facts("testing get_vcs") do

	p = Param(2)
	m = Model(p)
	mig.setrand!(m)

	agrid = m.grids["assets"]
	Ygrid = m.grids["Y"]
	Pgrid = m.grids["P"]
	zsupps = Dict{Int,Array{Float64,1}}()
	for j in 1:p.nJ
		zsupps[j] = m.gridsXD["zsupp"][:,j]
	end

	gs = Vector{Float64}[]
	push!(gs,agrid)
	push!(gs,zsupps[1])
	push!(gs,Ygrid)
	push!(gs,Pgrid)
	vcs_arr = Array{Float64}[]
	push!(vcs_arr,zeros(p.na,p.nz,p.ny,p.np),zeros(p.na,p.nz,p.ny,p.np),zeros(p.na,p.nz,p.ny,p.np))
	l = mig.lininterp(vcs_arr,gs)


	# get a point on the grid
	ia = 10
	iz = 3
	iy = 2
	ip = 3

	for i=1:5

		is = rand(1:p.ns)
		itau = rand(1:p.ntau)
		ih = rand(1:p.nh)
		ihh = rand(1:p.nh)
		ij = rand(1:p.nJ)
		ik = rand(1:p.nJ)
		it = rand(1:p.nt-1)
		azYP = [agrid[ia],zsupps[ij][iz],Ygrid[iy],Pgrid[ip]]

		mig.ApproXD.setGrid(l,2,zsupps[ij])

		evals = mig.get_vcs(l,azYP,ihh,ik,is,itau,ih,ij,it,p,m)

# dimvecH  = (nh, nJ, ns, nz, ny, np, na, nh, ntau,  nJ, nt-1 )
		@fact evals[1] => m.vh[mig.idx11(ihh,ik,is,iz,iy,ip,itau,ia,ih,ij,it,p)]
		@fact evals[2] => m.ch[mig.idx11(ihh,ik,is,iz,iy,ip,itau,ia,ih,ij,it,p)]
		@fact evals[3] => m.sh[mig.idx11(ihh,ik,is,iz,iy,ip,itau,ia,ih,ij,it,p)]

		mig.ApproXD.resetCache!(l)
	end

	# set rho to a linear combination of indices
	function vfun(xhh,xk,xs,xz,xy,xp,xtau,xa,xh,xj,xt) 
		188*xhh + 100*xk + 0.2*xs + 0.3*xz + 0.4*xy + 0.5*xp + 0.6*xtau + 0.7*xa + 0.8*xh + 0.9*xj + xt
	end
	function cfun(xhh,xk,xs,xz,xy,xp,xtau,xa,xh,xj,xt) 
		0.188*xhh + 10*xk + 20*xs + 0.3*xz + 0.4*xy + 0.5*xp + 0.09*xtau + 0.7*xa + 0.8*xh + 0.9*xj + xt
	end
	function sfun(xhh,xk,xs,xz,xy,xp,xtau,xa,xh,xj,xt) 
		0.9*xhh + xk + 0.2*xs + 0.3*xz + 0.4*xy + 50*xp + 0.6*xtau + 0.7*xa + 86*xh + 0.9*xj + xt
	end

	for ik=1:p.nJ
	for is=1:p.ns
	for iz=1:p.nz
	for iy=1:p.ny
	for ip=1:p.np
	for itau=1:p.ntau
	for ia=1:p.na
	for ih=1:p.nh
	for ihh=1:p.nh
	for ij=1:p.nJ
	for it=1:p.nt-1
		m.vh[mig.idx11(ihh,ik,is,iz,iy,ip,itau,ia,ih,ij,it,p)] = vfun(ihh,ik,is,zsupps[ij][iz],Ygrid[iy],Pgrid[ip],itau,agrid[ia],ih,ij,it)
		m.ch[mig.idx11(ihh,ik,is,iz,iy,ip,itau,ia,ih,ij,it,p)] = cfun(ihh,ik,is,zsupps[ij][iz],Ygrid[iy],Pgrid[ip],itau,agrid[ia],ih,ij,it)
		m.sh[mig.idx11(ihh,ik,is,iz,iy,ip,itau,ia,ih,ij,it,p)] = sfun(ihh,ik,is,zsupps[ij][iz],Ygrid[iy],Pgrid[ip],itau,agrid[ia],ih,ij,it)
	end
	end
	end
	end
	end
	end
	end
	end
	end
	end
	end


	# pick some random points on rho
	for i in 1:50
		is = rand(1:p.ns)
		itau = rand(1:p.ntau)
		ih = rand(1:p.nh)
		ihh = rand(1:p.nh)
		ij = rand(1:p.nJ)
		ik = rand(1:p.nJ)
		it = rand(1:p.nt-1)

		mig.ApproXD.setGrid(l,2,zsupps[ij])

		a = rand() * (agrid[end]-agrid[1]) + agrid[1]
		z = rand() * (zsupps[ij][end]-zsupps[ij][1]) + zsupps[ij][1]
		Y = rand() * (Ygrid[end]-Ygrid[1]) + Ygrid[1]
		P = rand() * (Pgrid[end]-Pgrid[1]) + Pgrid[1]
		v = mig.get_vcs(l,[a,z,Y,P],ihh,ik,is,itau,ih,ij,it,p,m)
		@fact v[1] => roughly(vfun(ihh,ik,is,z,Y,P,itau,a,ih,ij,it),atol=1e-6)
		@fact v[2] => roughly(cfun(ihh,ik,is,z,Y,P,itau,a,ih,ij,it),atol=1e-6)
		@fact v[3] => roughly(sfun(ihh,ik,is,z,Y,P,itau,a,ih,ij,it),atol=1e-6)
		mig.ApproXD.resetCache!(l)
	end
end

	# context("testing fill_azmats!") do

	# 	mig.fill_azmats!(azmat_v,azmat_c,azmat_s,ihh,ik,is,iy,ip,itau,ih,ij,it,p,m)

	# 	for iia in 1:p.na
	# 		for iz in 1:p.nz
	# 			idx = mig.idx11(ihh,ik,is,iz,iy,ip,itau,iia,ih,ij,it,p)
	# 			@fact azmat_v[iia,iz] => m.vh[idx]
	# 			@fact azmat_c[iia,iz] => m.ch[idx]
	# 			@fact azmat_s[iia,iz] => m.sh[idx]
	# 		end
	# 	end
	# end

	# context("testing fill_ktmp!") do


	# 	for itest in 1:50


	# 		is   = rand(1:p.ns)
	# 		ih   = rand(1:p.nh)
	# 		iy   = rand(1:p.ny)
	# 		ip   = rand(1:p.np)
	# 		itau = rand(1:p.ntau)
	# 		ij   = rand(1:p.nJ)
	# 		it   = rand(1:(p.nt-1))

	# 		# get a random point
	# 		a = rand() * (agrid[end] - agrid[1]) + agrid[1]
	# 		z = rand() * (zsupps[ij][end] - zsupps[ij][1]) + zsupps[ij][1]
	# 		Y = rand() * (Ygrid[end] - Ygrid[1]) + Ygrid[1]
	# 		P = rand() * (Pgrid[end] - Pgrid[1]) + Pgrid[1]

	# 		ktmp = zeros(p.nJ)
	# 		ktmp2 = zeros(p.nJ)

	# 		for ik in 1:p.nJ

	# 			# interpolate rho function in asset and z dim
	# 			for iia in 1:p.na
	# 				for iP = 1:p.np
	# 					for iY = 1:p.np
	# 						for iz = 1:p.nz


	# 				for iz in 1:p.nz
	# 					azmat_v[iia + p.na*(iz-1)] = m.rho[mig.idx10(ik,is,iz,iy,ip,itau,iia,ih,ij,it,p)] 	# TODO can to faster indexing here
	# 				end
	# 			end
	# 			ktmp[ik] = mig.bilinearapprox(a,z,agrid,zsupps[ij],azmat_v)
	# 		end

	# 		# fast
	# 		fill!(azmat_v,0.0)

	# 		mig.fill_ktmp!(ktmp2,azmat_v,a,z,is,iy,ip,itau,ih,ij,it,p,agrid,zsupps,m.rho)

	# 		# check
	# 		@fact sumabs(ktmp .- ktmp2) => roughly(0.0,atol=1e-12)

	# 	end




	# end

	# context("testing fill_azmats! for ousing choice") do
		
	# 	mig.fill_azmats_h!(azmat_v,azmat_c,ik,is,iy,ip,itau,ih,ij,it,p,m)

	# 	for iia in 1:p.na
	# 		for iz in 1:p.nz
	# 			@fact azmat_v[iia + p.na*(iz-1)] => m.vh[mig.idx11(1,ik,is,iz,iy,ip,itau,iia,ih,ij,it,p)]
	# 			@fact azmat_c[iia + p.na*(iz-1)] => m.vh[mig.idx11(2,ik,is,iz,iy,ip,itau,iia,ih,ij,it,p)]
	# 		end
	# 	end


	# end

	# context("testing fill_azmats! for 2 values") do
		
	# 	mig.fill_azmats!(azmat_v,azmat_c,ihh,ik,is,iy,ip,itau,ih,ij,it,p,m.vh,m.ch)

	# 	for iia in 1:p.na
	# 		for iz in 1:p.nz
	# 			@fact azmat_v[iia + p.na*(iz-1)] => m.vh[mig.idx11(ihh,ik,is,iz,iy,ip,itau,iia,ih,ij,it,p)]
	# 			@fact azmat_c[iia + p.na*(iz-1)] => m.ch[mig.idx11(ihh,ik,is,iz,iy,ip,itau,iia,ih,ij,it,p)]
	# 		end
	# 	end


	# end



# facts("testing fill_azmats_rent") do

# 	p = Param(2)
# 	m = Model(p)
# 	mig.setrand!(m)

# 	agrid = m.grids["assets"]
# 	agrid_rent = m.grids["assets"][m.aone:end]
# 	zsupps = Dict{Int,Array{Float64,1}}()
# 	for j in 1:p.nJ
# 		zsupps[j] = m.gridsXD["zsupp"][:,j]
# 	end

# 	na_rent = length(m.grids["assets_rent"])
# 	aone = m.aone

# 	azmat_v_rent = zeros(na_rent,p.nz)
# 	azmat_s_rent = zeros(length(agrid_rent),p.nz)
# 	azmat_c_rent = zeros(length(agrid_rent),p.nz)

# 	for itest = 1:50

# 		ihh  = rand(1:p.nh)
# 		ik   = rand(1:p.nJ)
# 		is   = rand(1:p.ns)
# 		ih   = rand(1:p.nh)
# 		iy   = rand(1:p.ny)
# 		ip   = rand(1:p.np)
# 		itau = rand(1:p.ntau)
# 		ij   = rand(1:p.nJ)
# 		it   = rand(1:(p.nt-1))

# 		mig.fill_azmats_rent!(azmat_v_rent,azmat_c_rent,azmat_s_rent,ihh,ik,is,iy,ip,itau,ih,ij,it,p,m)

# 		for iia in aone:p.na
# 			for iz in 1:p.nz
# 				idx = mig.idx11(ihh,ik,is,iz,iy,ip,itau,iia,ih,ij,it,p)
# 				@fact azmat_v_rent[iia-aone+1 + na_rent*(iz-1)] => m.vh[idx]
# 				@fact azmat_c_rent[iia-aone+1 + na_rent*(iz-1)] => m.ch[idx]
# 				@fact azmat_s_rent[iia-aone+1 + na_rent*(iz-1)] => m.sh[idx]
# 			end
# 		end
# 	end

# end



end	#module

