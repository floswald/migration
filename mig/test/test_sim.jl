
@testset "simulator." begin


	@testset "testing fill_interp_arrays" begin

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
		vcs_arr = Array{Float64}[]
		for vcs in 1:3  # value, cons and savings
			for j in 1:p.nJ  # at each loc choice
				for ihh in 1:p.nh 	# at each housing choice
					push!(vcs_arr,zeros(p.na,p.nz,p.ny,p.np))
				end
			end
		end
		L = Dict{String,mig.Lininterp}()
		L["l_vcs"] = mig.Lininterp(vcs_arr,gs)
		L["l_rho"] = mig.Lininterp(rho_arr,gs)

		# random discrete state
		is = rand(1:p.ns)
		itau = rand(1:p.ntau)
		ih = rand(1:p.nh)
		ij = rand(1:p.nJ)
		it = rand(1:p.nt-1)

		# call the func
		mig.fill_interp_arrays!(L,is,ih,itau,ij,it,p,m)

		# check that arrays on the interpolator correspond to the ones in m:

		for ia in rand(1:p.na,2)
		for iz in rand(1:p.nz,2)
		for iy in rand(1:p.ny,2)
		for ip in rand(1:p.np,2)

						for ik in 1:p.nJ
							@test L["l_rho"].vals[ik][ia,iz,iy,ip] == m.v[mig.idx10(ik,is,iz,iy,ip,itau,ia,ih,ij,it,p)]

							for ihh in 1:p.nh
								l_idx  = 3*(ik-1 + p.nJ*(ihh-1))
								idx = mig.idx11(ihh,ik,is,iz,iy,ip,itau,ia,ih,ij,it,p)
								@test L["l_vcs"].vals[1+l_idx][ia,iz,iy,ip] == m.vh[idx]
								@test L["l_vcs"].vals[2+l_idx][ia,iz,iy,ip] == m.ch[idx]
								@test L["l_vcs"].vals[3+l_idx][ia,iz,iy,ip] == m.sh[idx]
							end
						end
					end
				end
			end
		end
	end
	gc()


	@testset "testing get_rho_ktmp" begin


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
		vcs_arr = Array{Float64}[]
		for vcs in 1:3  # value, cons and savings
			for j in 1:p.nJ  # at each loc choice
				for ihh in 1:p.nh 	# at each housing choice
					push!(vcs_arr,zeros(p.na,p.nz,p.ny,p.np))
				end
			end
		end
		L = Dict{String,mig.Lininterp}()
		L["l_vcs"] = mig.Lininterp(vcs_arr,gs)
		L["l_rho"] = mig.Lininterp(rho_arr,gs)

		# random discrete state
		is = rand(1:p.ns)
		itau = rand(1:p.ntau)
		ih = rand(1:p.nh)
		ij = rand(1:p.nJ)
		it = rand(1:p.nt-1)

		mig.fill_interp_arrays!(L,is,ih,itau,ij,it,p,m)

		# get_rho_ktmp should give back a J vector with probabilies

		# point on grid
		ia = 4
		iz = 3
		ip = 2
		iy = 2
		azYP = [agrid[ia],zsupps[ij][iz],Ygrid[iy],Pgrid[ip]]
		# set zgrid on interpolator
		mig.setGrid!(L["l_rho"],2,zsupps[ij])

		ktmp = mig.get_rho_ktmp(L["l_rho"],azYP,p)

		for ik in 1:p.nJ
			@test ktmp[ik] == m.v[ik,is,iz,iy,ip,itau,ia,ih,ij,it]
		end
	end
	gc()


	if !(haskey(ENV,"TRAVIS"))
		@testset "testing get_vcs" begin


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
			vcs_arr = Array{Float64}[]
			for vcs in 1:3  # value, cons and savings
				for j in 1:p.nJ  # at each loc choice
					for ihh in 1:p.nh 	# at each housing choice
						push!(vcs_arr,zeros(p.na,p.nz,p.ny,p.np))
					end
				end
			end
			L = Dict{String,mig.Lininterp}()
			L["l_vcs"] = mig.Lininterp(vcs_arr,gs)
			L["l_rho"] = mig.Lininterp(rho_arr,gs)

			# random discrete state
			is = rand(1:p.ns)
			itau = rand(1:p.ntau)
			ih = rand(1:p.nh)
			ij = rand(1:p.nJ)
			it = rand(1:p.nt-1)

			mig.fill_interp_arrays!(L,is,ih,itau,ij,it,p,m)

			# get_rho_ktmp should give back a J vector with probabilies

			# point on grid
			ia = 4
			iz = 3
			ip = 2
			iy = 2
			azYP = [agrid[ia],zsupps[ij][iz],Ygrid[iy],Pgrid[ip]]
			# set zgrid on interpolator
			mig.setGrid!(L["l_vcs"],2,zsupps[ij])

			ihh = 1
			ik = 4
			vcs = mig.get_vcs(L["l_vcs"],azYP,ihh,ik,p)

			@test vcs[1] == m.vh[ihh,ik,is,iz,iy,ip,itau,ia,ih,ij,it]
			@test vcs[2] == m.ch[ihh,ik,is,iz,iy,ip,itau,ia,ih,ij,it]
			@test vcs[3] == m.sh[ihh,ik,is,iz,iy,ip,itau,ia,ih,ij,it]

			ihh = 2
			ik = 6
			vcs = mig.get_vcs(L["l_vcs"],azYP,ihh,ik,p)

			@test vcs[1] == m.vh[ihh,ik,is,iz,iy,ip,itau,ia,ih,ij,it]
			@test vcs[2] == m.ch[ihh,ik,is,iz,iy,ip,itau,ia,ih,ij,it]
			@test vcs[3] == m.sh[ihh,ik,is,iz,iy,ip,itau,ia,ih,ij,it]

			function vfun(xhh,xk,xs,xz,xy,xp,xtau,xa,xh,xj,xt)
				188*xhh + 100*xk + 2*xs + 2.8*xz + 1.4*xy + 1.5*xp + 6*xtau + 7*xa + 80*xh + 10*xj + xt
			end
			function cfun(xhh,xk,xs,xz,xy,xp,xtau,xa,xh,xj,xt)
				8*xhh + 10*xk + 1*xs + 4.8*xz + 1.4*xy + 1.5*xp + 1*xtau + 2*xa + 8*xh + 80*xj + xt
			end
			function sfun(xhh,xk,xs,xz,xy,xp,xtau,xa,xh,xj,xt)
				18*xhh + 1*xk + 20*xs + 3.8*xz + 1*xy + 5*xp + 3*xtau + 4*xa + 0*xh + 11*xj + xt
			end

			info("filling m.vh, m.sh and m.ch with lincom. this takes a while.")
			for is=1:p.ns
			for iz=1:p.nz
			for iy=1:p.ny
			for ip=1:p.np
			for itau=1:p.ntau
			for ia=1:p.na
			for ih=1:p.nh
			for ij=1:p.nJ
			for it=1:p.nt-1
			for ik=1:p.nJ
			for ihh=1:p.nh
				idx = mig.idx11(ihh,ik,is,iz,iy,ip,itau,ia,ih,ij,it,p)
				m.vh[idx] = vfun(ihh,ik,is,zsupps[ij][iz],Ygrid[iy],Pgrid[ip],itau,agrid[ia],ih,ij,it)
				m.ch[idx] = cfun(ihh,ik,is,zsupps[ij][iz],Ygrid[iy],Pgrid[ip],itau,agrid[ia],ih,ij,it)
				m.sh[idx] = sfun(ihh,ik,is,zsupps[ij][iz],Ygrid[iy],Pgrid[ip],itau,agrid[ia],ih,ij,it)
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

			info("done")
			for itest in 1:20
				# pick some random discrete states
				is = rand(1:p.ns)
				itau = rand(1:p.ntau)
				ih = rand(1:p.nh)
				ij = rand(1:p.nJ)
				ik = rand(1:p.nJ)
				it = rand(1:p.nt-1)
				# is   = 2
				# itau = 2
				# ih   = 1
				# ij   = 3
				# ik   = 9
				# it   = 5
				# println("discrete state: is=$is,itau=$itau,ih=$ih,ij=$ij,ik=$ik,it=$it")
				mig.fill_interp_arrays!(L,is,ih,itau,ij,it,p,m)

				mig.setGrid!(L["l_vcs"],2,zsupps[ij])

				# pick random continuous states
				a = rand() * (agrid[end]-agrid[1]) + agrid[1]
				z = rand() * (zsupps[ij][end]-zsupps[ij][1]) + zsupps[ij][1]
				Y = rand() * (Ygrid[end]-Ygrid[1]) + Ygrid[1]
				P = rand() * (Pgrid[end]-Pgrid[1]) + Pgrid[1]

				# println("continuous state: a=$a,z=$z,Y=$Y,P=$P")
				v1v2 = mig.get_v1v2(L["l_vcs"],[a,z,Y,P],ik,p)
				vcs1  = mig.get_vcs(L["l_vcs"],[a,z,Y,P],1,ik,p)
				vcs2  = mig.get_vcs(L["l_vcs"],[a,z,Y,P],2,ik,p)

				# println("approx=$(v[1])") 
				# println("true=$(vfun(1,ik,is,z,Y,P,itau,a,ih,ij,it))")
				# println("approx2=$(v[2])") 
				# println("true2=$(vfun(2,ik,is,z,Y,P,itau,a,ih,ij,it))")

				@test isapprox(v1v2[1], vfun(1,ik,is,z,Y,P,itau,a,ih,ij,it) ,atol=1e-6)
				@test isapprox(v1v2[2], vfun(2,ik,is,z,Y,P,itau,a,ih,ij,it) ,atol=1e-6)
				@test isapprox(vcs1[1], vfun(1,ik,is,z,Y,P,itau,a,ih,ij,it) ,atol=1e-6)
				@test isapprox(vcs1[2], cfun(1,ik,is,z,Y,P,itau,a,ih,ij,it) ,atol=1e-6)
				@test isapprox(vcs1[3], sfun(1,ik,is,z,Y,P,itau,a,ih,ij,it) ,atol=1e-6)
				@test isapprox(vcs2[1], vfun(2,ik,is,z,Y,P,itau,a,ih,ij,it) ,atol=1e-6)
				@test isapprox(vcs2[2], cfun(2,ik,is,z,Y,P,itau,a,ih,ij,it) ,atol=1e-6)
				@test isapprox(vcs2[3], sfun(2,ik,is,z,Y,P,itau,a,ih,ij,it) ,atol=1e-6)
				mig.resetCache!(L["l_vcs"])
			end
		end
		gc()

		@testset "testing get_v1v2" begin


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
			vcs_arr = Array{Float64}[]
			for vcs in 1:3  # value, cons and savings
				for j in 1:p.nJ  # at each loc choice
					for ihh in 1:p.nh 	# at each housing choice
						push!(vcs_arr,zeros(p.na,p.nz,p.ny,p.np))
					end
				end
			end
			L = Dict{String,mig.Lininterp}()
			L["l_vcs"] = mig.Lininterp(vcs_arr,gs)
			L["l_rho"] = mig.Lininterp(rho_arr,gs)

			# random discrete state
			is = rand(1:p.ns)
			itau = rand(1:p.ntau)
			ih = rand(1:p.nh)
			ij = rand(1:p.nJ)
			it = rand(1:p.nt-1)

			mig.fill_interp_arrays!(L,is,ih,itau,ij,it,p,m)

			# point on grid
			ia = 4
			iz = 3
			ip = 2
			iy = 2
			azYP = [agrid[ia],zsupps[ij][iz],Ygrid[iy],Pgrid[ip]]
			# set zgrid on interpolator
			mig.setGrid!(L["l_vcs"],2,zsupps[ij])

			ihh = 1
			ik = 4
			v1v2 = mig.get_v1v2(L["l_vcs"],azYP,ik,p)

			@test v1v2[1] == m.vh[1,ik,is,iz,iy,ip,itau,ia,ih,ij,it]
			@test v1v2[2] == m.vh[2,ik,is,iz,iy,ip,itau,ia,ih,ij,it]
		end
		gc()

		@testset "testing get_cs" begin

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
			vcs_arr = Array{Float64}[]
			for vcs in 1:3  # value, cons and savings
				for j in 1:p.nJ  # at each loc choice
					for ihh in 1:p.nh 	# at each housing choice
						push!(vcs_arr,zeros(p.na,p.nz,p.ny,p.np))
					end
				end
			end
			L = Dict{String,mig.Lininterp}()
			L["l_vcs"] = mig.Lininterp(vcs_arr,gs)
			L["l_rho"] = mig.Lininterp(rho_arr,gs)

			# random discrete state
			is = rand(1:p.ns)
			itau = rand(1:p.ntau)
			ih = rand(1:p.nh)
			ij = rand(1:p.nJ)
			it = rand(1:p.nt-1)

			mig.fill_interp_arrays!(L,is,ih,itau,ij,it,p,m)

			# point on grid
			ia = 4
			iz = 3
			ip = 2
			iy = 2
			azYP = [agrid[ia],zsupps[ij][iz],Ygrid[iy],Pgrid[ip]]
			# set zgrid on interpolator
			mig.setGrid!(L["l_vcs"],2,zsupps[ij])

			ihh = 1
			ik = 4
			v1v2 = mig.get_cs(L["l_vcs"],azYP,ihh,ik,p)

			@test v1v2[1] == m.ch[ihh,ik,is,iz,iy,ip,itau,ia,ih,ij,it]
			@test v1v2[2] == m.sh[ihh,ik,is,iz,iy,ip,itau,ia,ih,ij,it]
		end
	else
		@testset "testing get_vcs" begin
			# runs out of memory
			println("skipping on travis")
		end

		@testset "testing get_v1v2" begin
			println("skipping on travis")
		end
	end

end
gc()



