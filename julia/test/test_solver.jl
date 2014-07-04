


# testing solver.jl

module test_solver

using FactCheck, mig

facts("test linear index functions") do

	p = mig.Param(1)
	m = mig.Model(p)

	context("testing 10 dim index") do

		for itest in 1:50

			# choose a random state
			ik   = rand(1:p.nJ)
			ia   = rand(1:p.na)
			ih   = rand(1:p.nh)
			iy   = rand(1:p.ny)
			ip   = rand(1:p.np)
			iP   = rand(1:p.nP)
			iz   = rand(1:p.nz)
			itau = rand(1:p.ntau)
			ij   = rand(1:p.nJ)
			it   = rand(1:(p.nt-1))

			@fact m.v[ik,iy,ip,iP,iz,ia,ih,itau,ij,it] == m.v[mig.idx10(ik,iy,ip,iP,iz,ia,ih,itau,ij,it,p)] => true
		end
	end

	context("testing 9 dim index") do

		for itest in 1:50

			# choose a random state
			ia   = rand(1:p.na)
			ih   = rand(1:p.nh)
			iy   = rand(1:p.ny)
			ip   = rand(1:p.np)
			iP   = rand(1:p.nP)
			iz   = rand(1:p.nz)
			itau = rand(1:p.ntau)
			ij   = rand(1:p.nJ)
			it   = rand(1:(p.nt-1))

			@fact m.vbar[iy,ip,iP,iz,ia,ih,itau,ij,it] == m.vbar[mig.idx9(iy,ip,iP,iz,ia,ih,itau,ij,it,p)] => true
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
	x = 1.0
	own = 1
	def = true
	mc = 10.0
	z = (1/(1-p.gamma)) * x^(1-p.gamma) + own*p.xi - def*p.lambda - mc

	@fact z == mig.ufun(x,own,mc,def,p) => true

	def = false 
	mc = -10.0
	z = (1/(1-p.gamma)) * x^(1-p.gamma) + own*p.xi - def*p.lambda - mc

	@fact z == mig.ufun(x,own,mc,def,p) => true

	own = 0
	def = true
	mc = 10.0
	z = (1/(1-p.gamma)) * x^(1-p.gamma) + own*p.xi - def*p.lambda - mc

	@fact z == mig.ufun(x,own,mc,def,p) => true

	def = false 
	mc = 10.0
	z = (1/(1-p.gamma)) * x^(1-p.gamma) + own*p.xi - def*p.lambda - mc

	@fact z == mig.ufun(x,own,mc,def,p) => true

	x = rand()
	own = 1
	def = true
	mc = 0.0
	z = (1/(1-p.gamma)) * x^(1-p.gamma) + own*p.xi - def*p.lambda - mc

	@fact z - mig.ufun(x,own,mc,def,p) => roughly(0.0,atol=0.0000001)

	def = false 
	mc = 10.0
	z = (1/(1-p.gamma)) * x^(1-p.gamma) + own*p.xi - def*p.lambda - mc

	@fact z - mig.ufun(x,own,mc,def,p) => roughly(0.0,atol=0.0000001)

	own = 0
	def = true
	mc = 10.0
	z = (1/(1-p.gamma)) * x^(1-p.gamma) + own*p.xi - def*p.lambda - mc

	@fact z - mig.ufun(x,own,mc,def,p) => roughly(0.0,atol=0.0000001)

	def = false 
	mc = 10.0
	z = (1/(1-p.gamma)) * x^(1-p.gamma) + own*p.xi - def*p.lambda - mc

	@fact z - mig.ufun(x,own,mc,def,p) => roughly(0.0,atol=0.0000001)
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
	a = rand()
	y = rand()
	j = rand(1:p.nJ)


	ih = 0
	ihh = 0
	pr = rand()*10
	move = false

	z = mig.grossSaving(a,p) + y - mig.pifun(ih,ihh,pr,move,j,p)

	@fact mig.cashFunction(a,y,ih,ihh,pr,move,j,p) => roughly(z,atol=0.000001)

	move = true
	z = mig.grossSaving(a,p) + y - mig.pifun(ih,ihh,pr,move,j,p)
	@fact mig.cashFunction(a,y,ih,ihh,pr,move,j,p) => roughly(z,atol=0.000001)

	# ========================
	
	ih = 1
	ihh = 0
	pr = rand()*10
	move = false

	z = mig.grossSaving(a,p) + y - mig.pifun(ih,ihh,pr,move,j,p)

	@fact mig.cashFunction(a,y,ih,ihh,pr,move,j,p) => roughly(z,atol=0.000001)

	move = true
	z = mig.grossSaving(a,p) + y - mig.pifun(ih,ihh,pr,move,j,p)

	@fact mig.cashFunction(a,y,ih,ihh,pr,move,j,p) => roughly(z,atol=0.000001)

	ih = 0
	ihh =1
	pr = rand()*10
	move = false

	z = mig.grossSaving(a,p) + y - mig.pifun(ih,ihh,pr,move,j,p)

	@fact mig.cashFunction(a,y,ih,ihh,pr,move,j,p) => roughly(z,atol=0.000001)

	ih = 1
	ihh =1
	pr = rand()*10
	move = false

	z = mig.grossSaving(a,p) + y - mig.pifun(ih,ihh,pr,move,j,p)

	@fact mig.cashFunction(a,y,ih,ihh,pr,move,j,p) => roughly(z,atol=0.000001)

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


		x = rand()
		mc = rand()
		def = false
		EV = rand(p.na)
		own = 1


		for i=1:p.na
			if x-s[i] > 0
				w[i] = mig.ufun(x-s[i],own,mc,def,p) + p.beta * EV[i]
			end
		end

		r = findmax(w)

		@fact mig.maxvalue(x,p,s,w,own,mc,def,EV,m.aone) => r

		fill!(w,p.myNA)
		x = -10000.0
		def = false

		@fact mig.maxvalue(x,p,s,w,own,mc,def,EV,m.aone) => (p.myNA,1)
	end


	context("maxalue for owners default") do

		p = mig.Param(1)
		m = mig.Model(p)

		w = zeros(p.na)
		fill!(w,p.myNA)


		x = rand()
		mc = rand()
		def = true
		EV = rand(p.na)
		own = 1

		for i=1:p.na
			if x-s[i] > 0
				w[i] = mig.ufun(x-s[i],own,mc,def,p) + p.beta * EV[i]
			end
		end

		r = findmax(w)

		@fact mig.maxvalue(x,p,s,w,own,mc,def,EV,m.aone) => r

		fill!(w,p.myNA)
		x = -10000.0
		def = false

		@fact mig.maxvalue(x,p,s,w,own,mc,def,EV,m.aone) => (p.myNA,1)
	end


	context("maxalue for renters") do

		p = mig.Param(1)
		m = mig.Model(p)

		w = zeros(p.na)
		fill!(w,p.myNA)


		x = rand()
		mc = rand()
		def = false
		EV = rand(p.na)
		own = 0

		for i=1:p.na
			if x-s[i] > 0
				w[i] = mig.ufun(x-s[i],own,mc,def,p) + p.beta * EV[i]
			end
		end

		r = findmax(w)

		@fact mig.maxvalue(x,p,s,w,own,mc,def,EV,m.aone) => r

		fill!(w,p.myNA)
		x = -100000.0
		def = false

		@fact mig.maxvalue(x,p,s,w,own,mc,def,EV,m.aone) => (p.myNA,1)
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
		@fact_throws mig.maxvalue(x,p,s,w,own,mc,def,EV,m.aone) 
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
			iy   = rand(1:p.ny)
			ip   = rand(1:p.np)
			iP   = rand(1:p.nP)
			iz   = rand(1:p.nz)
			itau = rand(1:p.ntau)
			ij   = rand(1:p.nJ)

			EV = m.EVfinal[:,ih,iP,ip,ik]

			mig.EVfunChooser!(ev,iz,ih,itau,iP,ip,iy,ij,ik,ti,m,p)

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
			ih   = rand(1:p.nh)
			iy   = rand(1:p.ny)
			ip   = rand(1:p.np)
			iP   = rand(1:p.nP)
			iz   = rand(1:p.nz)
			itau = rand(1:p.ntau)
			ij   = rand(1:p.nJ)

		# V[y,p,P,z,a,h,tau,j,age]
			EV = m.EV[iy,ip,iP,iz,:,ih,itau,ik,ti+1]
			EVM = m.EVMove[iy,ip,iP,iz,:,ih,itau,ik,ti+1]

			mig.EVfunChooser!(ev,iz,ih,itau,iP,ip,iy,ij,ik,ti,m,p)

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
			ih   = rand(1:p.nh)
			iy   = rand(1:p.ny)
			ip   = rand(1:p.np)
			iP   = rand(1:p.nP)
			iz   = rand(1:p.nz)
			itau = rand(1:p.ntau)
			ij   = rand(1:p.nJ)
			it   = rand(1:(p.nt-1))

			# calling integrateVbar must return vbar.
			tmp = mig.integrateVbar(ia,ih,iy,ip,iP,iz,itau,ij,it,p,Gz,GzM,Gy,Gp,GP,m)

			@fact tmp[1] - m.vbar[mig.idx9(iy,ip,iP,iz,ia,ih,itau,ij,it,p)] => roughly(0.0,atol=0.00001)
			@fact tmp[2] - m.vbar[mig.idx9(iy,ip,iP,iz,ia,ih,itau,ij,it,p)] => roughly(0.0,atol=0.00001)

		end
	end

	context("test whether equal to hand integration") do

		p    = mig.Param(1)
		m    = mig.Model(p)

		myEV = zeros(m.dimvec2[1:8])
		myEVM = zeros(m.dimvec2[1:8])

		age = p.nt-2

		Gz = m.gridsXD["Gz"]
		GzM = m.gridsXD["GzM"]
		Gy = m.gridsXD["Gy"]
		Gp = m.gridsXD["Gp"]
		GP = m.grids2D["GP"]

		for ij=1:p.nJ				# current location
		for itau=1:p.ntau			# type
		for ih=1:p.nh
		for ia=1:p.na

		# start integration loop
		for iz=1:p.nz				# individual income shock
		for iP=1:p.nP 				# national price index
		for ip=1:p.np 				# regional price deviation
		for iy=1:p.ny 				# regional income deviation

		# dimvec2 = (ny, np, nP, nz, na, nh, ntau,  nJ, nt-1 )
			for iz1=1:p.nz				# individual income shock
			for iP1=1:p.nP 				# national price index
			for ip1=1:p.np 				# regional price deviation
			for iy1=1:p.ny 				# regional income deviation

				myEV[iy,ip,iP,iz,ia,ih,itau,ij] += m.vbar[mig.idx9(iy1,ip1,iP1,iz1,ia,ih,itau,ij,age,p)] * Gz[iz,iz1,ij] * Gp[ip,ip1,ij] * Gy[iy,iy1,ij] * GP[iP,iP1]
				myEVM[iy,ip,iP,iz,ia,ih,itau,ij] += m.vbar[mig.idx9(iy1,ip1,iP1,iz1,ia,ih,itau,ij,age,p)] * GzM[iz,iz1,ij] * Gp[ip,ip1,ij] * Gy[iy,iy1,ij] * GP[iP,iP1]
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

			tmp = mig.integrateVbar(ia,ih,iy,ip,iP,iz,itau,ij,age,p,Gz,GzM,Gy,Gp,GP,m)
			@fact myEV[iy,ip,iP,iz,ia,ih,itau,ij] - tmp[1] => roughly(0.0,atol=0.00001)
			@fact myEVM[iy,ip,iP,iz,ia,ih,itau,ij] - tmp[2] => roughly(0.0,atol=0.00001)
		end
		end
		end
		end
		# end test locationp
	end
end




end # module