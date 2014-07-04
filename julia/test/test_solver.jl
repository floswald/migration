


# testing solver.jl

module test_solver

using FactCheck, mig

facts("test linear index functions") do

	p = mig.Param(1)
	m = mig.Model(p)

	context("testing 11 dim index") do

		for itest in 1:50

			# choose a random state
			ik   = rand(1:p.nJ)
			is   = rand(1:p.ns)
			ia   = rand(1:p.na)
			ih   = rand(1:p.nh)
			iy   = rand(1:p.ny)
			ip   = rand(1:p.np)
			iP   = rand(1:p.nP)
			iz   = rand(1:p.nz)
			itau = rand(1:p.ntau)
			ij   = rand(1:p.nJ)
			it   = rand(1:(p.nt-1))

			@fact m.v[ik,is,iy,ip,iP,iz,ia,ih,itau,ij,it] == m.v[mig.idx11(ik,is,iy,ip,iP,iz,ia,ih,itau,ij,it,p)] => true
		end
	end

	context("testing 10 dim index") do

		for itest in 1:50

			# choose a random state
			is   = rand(1:p.ns)
			ia   = rand(1:p.na)
			ih   = rand(1:p.nh)
			iy   = rand(1:p.ny)
			ip   = rand(1:p.np)
			iP   = rand(1:p.nP)
			iz   = rand(1:p.nz)
			itau = rand(1:p.ntau)
			ij   = rand(1:p.nJ)
			it   = rand(1:(p.nt-1))

			@fact m.vbar[is,iy,ip,iP,iz,ia,ih,itau,ij,it] == m.vbar[mig.idx10(is,iy,ip,iP,iz,ia,ih,itau,ij,it,p)] => true
		end
	end

	context("testing final index function") do

		for itest in 1:50

			# choose a random state
			ik   = rand(1:p.nJ)
			ia   = rand(1:p.na)
			ih   = rand(1:p.nh)
			ip   = rand(1:p.np)
			iP   = rand(1:p.nP)

			@fact m.EVfinal[ia,ih,iP,ip,ik] == m.EVfinal[mig.idxFinal(ia,ih,iP,ip,ik,p)] => true
		end
	end

end

facts("testing utility function") do

	p = mig.Param(1)
	for iown in 0:1
		for itau in 1:p.ntau
			for is in 1:p.ns
				for idef in [true,false]
					for mc in [-10.0,0,10.0]
						x = rand()
						if is==1
							z = (1/(1-p.gamma)) * x^(1-p.gamma) + iown*p.xi1 - idef*p.lambda - mc + (itau-1) * p.tau
						else
							z = (1/(1-p.gamma)) * x^(1-p.gamma) + iown*p.xi2 - idef*p.lambda - mc + (itau-1) * p.tau
						end
						@fact z - mig.ufun(x,is,iown,itau,mc,idef,p) => roughly(0.0,atol=0.00001)
					end
				end
			end
		end
	end
end


facts("testing payments function") do

	
	p = mig.Param(1)

	context("payments when move==false") do

		move = false

		ih   = 0
		ihh  = 0
		pr   = rand()
		j    = rand(1:p.nJ)

		z = p.kappa[j] * pr
	
		@fact z - mig.pifun(ih,ihh,pr,move,j,p) => roughly(0.0,atol=0.0000001)

		ih   = 0
		ihh  = 1
		pr   = rand()
		j    = rand(1:p.nJ)

		z = pr

		@fact z - mig.pifun(ih,ihh,pr,move,j,p) => roughly(0.0,atol=0.0000001)	

		ih   = 1
		ihh  = 0
		pr   = rand()
		j    = rand(1:p.nJ)

		z = -pr*(1-p.phi-p.kappa[j])

		@fact z - mig.pifun(ih,ihh,pr,move,j,p) => roughly(0.0,atol=0.0000001)

		ih   = 1
		ihh  = 1
		pr   = rand()
		j    = rand(1:p.nJ)

		z = 0.0

		@fact z - mig.pifun(ih,ihh,pr,move,j,p) => roughly(0.0,atol=0.0000001)

	end


	context("payments when move==true") do

		move = true

		ih   = 0
		ihh  = 0
		pr   = rand()
		j    = rand(1:p.nJ)

		z = p.kappa[j] * pr

		@fact z - mig.pifun(ih,ihh,pr,move,j,p) => roughly(0.0,atol=0.0000001)

		ih   = 0
		ihh  = 1
		pr   = rand()
		j    = rand(1:p.nJ)

		@fact_throws mig.pifun(ih,ihh,pr,move,j,p)

		ih   = 1
		ihh  = 0
		pr   = rand()
		j    = rand(1:p.nJ)

		z = -pr*(1-p.phi-p.kappa[j])

		@fact z - mig.pifun(ih,ihh,pr,move,j,p) => roughly(0.0,atol=0.0000001)

		ih   = 1
		ihh  = 1
		pr   = rand()
		j    = rand(1:p.nJ)

		@fact_throws mig.pifun(ih,ihh,pr,move,j,p)

	end
end




facts("testing grossSaving function") do

	p = mig.Param(1)
	x = rand()

	@fact mig.grossSaving(x,p) => x*p.R

	x = -x
	@fact mig.grossSaving(x,p) => x*p.Rm

end


facts("testing cashFunction") do

	p = mig.Param(1)
	j = rand(1:p.nJ)

	for ik in 1:p.nJ
		for is in 1:p.ns
			for ih in 0:1
				for move in [true,false]
					if move
						ihh = 0
						a = rand()
						y = rand()
						pr = rand()*10
						z = mig.grossSaving(a,p) + y - mig.pifun(ih,ihh,pr,move,ik,p)
						if is==2
							z = z/p.sscale
						end
						@fact abs(mig.cashFunction(a,y,is,ih,ihh,pr,move,ik,p)) - abs(z) => roughly(0.0)
					else
						for ihh in 0:1
							a = rand()
							y = rand()
							pr = rand()*10
							z = mig.grossSaving(a,p) + y - mig.pifun(ih,ihh,pr,move,ik,p)
						if is==2
							z = z/p.sscale
						end
							@fact abs(mig.cashFunction(a,y,is,ih,ihh,pr,move,ik,p)) - abs(z) => roughly(0.0)
						end
					end
				end
			end
		end
	end
end



facts("testing maxvalue function") do

	p = mig.Param(1)
	m = mig.Model(p)
	s = m.grids["asset_own"]

	context("maxalue for owners non default") do

		p = mig.Param(1)
		m = mig.Model(p)

		w = zeros(p.na)
		fill!(w,p.myNA)


		for is in 1:p.ns
			for itau in 1:p.ntau
				for own in 0:1
					for def in [false,true]
						x = rand()
						mc = rand()
						EV = rand(p.na)


						for i=1:p.na
							if x-s[i] > 0
								w[i] = mig.ufun(x-s[i],is,own,itau,mc,def,p) + p.beta * EV[i]
							end
						end

						r = findmax(w)

						@fact mig.maxvalue(x,is,itau,p,s,w,own,mc,def,EV,m.aone) => r

						fill!(w,p.myNA)
						x = -10000.0
						def = false

						@fact mig.maxvalue(x,is,itau,p,s,w,own,mc,def,EV,m.aone) => (p.myNA,1)
					end
				end
			end
		end
	end

	context("maxvalue throws errors") do


		p = mig.Param(1)
		m = mig.Model(p)

		w = zeros(p.na)
		fill!(w,p.myNA)


		x = rand()
		mc = rand()
		def = false
		own = 0

		EV = rand(p.na-1)
		@fact_throws mig.maxvalue(x,1,1,p,s,w,own,mc,def,EV,m.aone) 
	end

end



facts("testing EVfunChooser") do

	p    = mig.Param(1)
	m    = mig.Model(p)

	

	context("testing EVfunChooser in period T-1") do

		# test for penultimate period
		ti = p.nt-1
		ev = zeros(p.na)

		for itest = 1:50

			fill!(ev,p.myNA)

		# choose a random state
			ik   = rand(1:p.nJ)
			ih   = rand(1:p.nh)
			is   = rand(1:p.ns)
			iy   = rand(1:p.ny)
			ip   = rand(1:p.np)
			iP   = rand(1:p.nP)
			iz   = rand(1:p.nz)
			itau = rand(1:p.ntau)
			ij   = rand(1:p.nJ)

			EV = m.EVfinal[:,ih,iP,ip,ik]

			mig.EVfunChooser!(ev,is,iz,ih,itau,iP,ip,iy,ij,ik,ti,m,p)

			@fact ev[:] .- EV[:] => roughly(zeros(p.na),atol=0.000001)

		end
	end


	context("testing EVfunChooser in period T-2") do
	# test for previous periods

		ti = p.nt-2
		
		ev = zeros(p.na)

		for itest = 1:50
			fill!(ev,p.myNA)
		# choose a random state
			ik   = rand(1:p.nJ)
			is   = rand(1:p.ns)
			ih   = rand(1:p.nh)
			iy   = rand(1:p.ny)
			ip   = rand(1:p.np)
			iP   = rand(1:p.nP)
			iz   = rand(1:p.nz)
			itau = rand(1:p.ntau)
			ij   = rand(1:p.nJ)

		# V[y,p,P,z,a,h,tau,j,age]
			EV = m.EV[is,iy,ip,iP,iz,:,ih,itau,ik,ti+1]
			EVM = m.EVMove[is,iy,ip,iP,iz,:,ih,itau,ik,ti+1]

			mig.EVfunChooser!(ev,is,iz,ih,itau,iP,ip,iy,ij,ik,ti,m,p)

			if ij==ik
				@fact ev[:] .- EV[:] => roughly(zeros(p.na),atol=0.000001)
			else
				@fact ev[:] .- EVM[:] => roughly(zeros(p.na),atol=0.000001)
			end

		end
	end

end



facts("test integration of vbar: getting EV") do

	context("test uniform weights return original array") do

		p    = mig.Param(1)
		m    = mig.Model(p)


		Gz = m.gridsXD["Gz"]
		GzM = m.gridsXD["GzM"]
		Gy = m.gridsXD["Gy"]
		Gp = m.gridsXD["Gp"]
		GP = m.grids2D["GP"]

		# 1/number of all integration states
		num = p.nz * p.ny * p.np * p.nP
		fill!(m.vbar,1/num)
		fill!(Gz,1/p.nz)
		fill!(GzM,1/p.nz)
		fill!(Gy,1/p.ny)
		fill!(Gp,1/p.np)
		fill!(GP,1/p.nP)

		for itest = 1:50

			# choose a random state
			ia   = rand(1:p.na)
			is   = rand(1:p.ns)
			ih   = rand(1:p.nh)
			iy   = rand(1:p.ny)
			ip   = rand(1:p.np)
			iP   = rand(1:p.nP)
			iz   = rand(1:p.nz)
			itau = rand(1:p.ntau)
			ij   = rand(1:p.nJ)
			it   = rand(1:(p.nt-1))

			Gs = squeeze(m.gridsXD["Gs"][:,:,it],3)

			# calling integrateVbar must return vbar.
			tmp = mig.integrateVbar(ia,is,ih,iy,ip,iP,iz,itau,ij,it,p,Gz,GzM,Gy,Gp,GP,Gs,m)

			@fact tmp[1] - m.vbar[mig.idx10(is,iy,ip,iP,iz,ia,ih,itau,ij,it,p)] => roughly(0.0,atol=0.00001)
			@fact tmp[2] - m.vbar[mig.idx10(is,iy,ip,iP,iz,ia,ih,itau,ij,it,p)] => roughly(0.0,atol=0.00001)

		end
	end

	context("test whether equal to hand integration") do

		p    = mig.Param(1)
		m    = mig.Model(p)

		myEV = zeros(m.dimvec2[1:9])
		myEVM = zeros(m.dimvec2[1:9])

		age = p.nt-2

		Gz = m.gridsXD["Gz"]
		GzM = m.gridsXD["GzM"]
		Gy = m.gridsXD["Gy"]
		Gp = m.gridsXD["Gp"]
		GP = m.grids2D["GP"]
		Gs = squeeze(m.gridsXD["Gs"][:,:,age],3)

		for ij=1:p.nJ				# current location
		for itau=1:p.ntau			# type
		for ih=1:p.nh
		for ia=1:p.na

		# start integration loop
		for iz=1:p.nz				# individual income shock
		for iP=1:p.nP 				# national price index
		for ip=1:p.np 				# regional price deviation
		for iy=1:p.ny 				# regional income deviation
		for is=1:p.ns 				# regional income deviation

		# dimvec2 = (ny, np, nP, nz, na, nh, ntau,  nJ, nt-1 )
			for iz1=1:p.nz				# individual income shock
			for iP1=1:p.nP 				# national price index
			for ip1=1:p.np 				# regional price deviation
			for iy1=1:p.ny 				# regional income deviation
			for is1=1:p.ns 				# regional income deviation

				myEV[is,iy,ip,iP,iz,ia,ih,itau,ij] += m.vbar[mig.idx10(is1,iy1,ip1,iP1,iz1,ia,ih,itau,ij,age,p)] * Gz[iz,iz1,ij] * Gp[ip,ip1,ij] * Gy[iy,iy1,ij] * GP[iP,iP1]* Gs[is,is1]
				myEVM[is,iy,ip,iP,iz,ia,ih,itau,ij] += m.vbar[mig.idx10(is1,iy1,ip1,iP1,iz1,ia,ih,itau,ij,age,p)] * GzM[iz,iz1,ij] * Gp[ip,ip1,ij] * Gy[iy,iy1,ij] * GP[iP,iP1]* Gs[is,is1]
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
		# end integration loop

		end
		end
		end
		end
		# end computation loop


		# choose a random state
		ia   = rand(1:p.na)
		ih   = rand(1:p.nh)
		itau = rand(1:p.ntau)
		ij   = rand(1:p.nJ)
		# start test loop
		for iz=1:p.nz				# individual income shock
		for ip=1:p.np 				# regional price deviation
		for iP=1:p.nP 				# national price index
		for iy=1:p.ny 				# regional income deviation
		for is=1:p.ns 				# regional income deviation

			tmp = mig.integrateVbar(ia,is,ih,iy,ip,iP,iz,itau,ij,age,p,Gz,GzM,Gy,Gp,GP,Gs,m)
			@fact myEV[is,iy,ip,iP,iz,ia,ih,itau,ij] - tmp[1] => roughly(0.0,atol=0.00001)
			@fact myEVM[is,iy,ip,iP,iz,ia,ih,itau,ij] - tmp[2] => roughly(0.0,atol=0.00001)
		end
		end
		end
		end
		end
		# end test locationp
	end
end




end # module