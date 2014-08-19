


# testing solver.jl

module test_solver

using FactCheck, mig

facts("test linear index functions") do

	p = mig.Param(2)
	m = mig.Model(p)

	# set arrays to random numbers
	mig.setrand!(m)

	context("testing 11 dim index") do

		for itest in 1:50

			# choose a random state
			ihh  = rand(1:p.nh)
			ik   = rand(1:p.nJ)
			is   = rand(1:p.ns)
			ia   = rand(1:p.na)
			ih   = rand(1:p.nh)
			iy   = rand(1:p.ny)
			ip   = rand(1:p.np)
			iz   = rand(1:p.nz)
			itau = rand(1:p.ntau)
			ij   = rand(1:p.nJ)
			it   = rand(1:(p.nt-1))

			@fact m.vh[ihh,ik,is,iz,iy,ip,itau,ia,ih,ij,it] => roughly(m.vh[mig.idx11(ihh,ik,is,iz,iy,ip,itau,ia,ih,ij,it,p)],atol=1e-6)
		end
	end

	context("testing 10 dim index") do

		for itest in 1:50

			# choose a random state
			ik   = rand(1:p.nJ)
			is   = rand(1:p.ns)
			ia   = rand(1:p.na)
			ih   = rand(1:p.nh)
			iy   = rand(1:p.ny)
			ip   = rand(1:p.np)
			iz   = rand(1:p.nz)
			itau = rand(1:p.ntau)
			ij   = rand(1:p.nJ)
			it   = rand(1:(p.nt-1))

			@fact m.v[ik,is,iz,iy,ip,itau,ia,ih,ij,it] => roughly(m.v[mig.idx10(ik,is,iz,iy,ip,itau,ia,ih,ij,it,p)],atol=1e-6)
		end
	end

	context("testing 9 dim index") do

		for itest in 1:50

			# choose a random state
			is   = rand(1:p.ns)
			ia   = rand(1:p.na)
			ih   = rand(1:p.nh)
			iy   = rand(1:p.ny)
			ip   = rand(1:p.np)
			iz   = rand(1:p.nz)
			itau = rand(1:p.ntau)
			ij   = rand(1:p.nJ)
			it   = rand(1:(p.nt-1))

			@fact m.vbar[is,iz,iy,ip,itau,ia,ih,ij,it] == m.vbar[mig.idx9(is,iz,iy,ip,itau,ia,ih,ij,it,p)] => true
		end
	end

	context("testing final index function") do

		for itest in 1:50

			# choose a random state
			ik   = rand(1:p.nJ)
			ia   = rand(1:p.na)
			ih   = rand(1:p.nh)
			ip   = rand(1:p.np)

			@fact m.EVfinal[ia,ih,ip,ik] == m.EVfinal[mig.idxFinal(ia,ih,ip,ik,p)] => true
		end
	end

end

facts("testing utility function") do

	p = mig.Param(1)

	c = rand()
	ev = rand()

	u = (1/(1-p.gamma)) * c^(1-p.gamma) + p.beta * ev

	@fact u - mig.ufun(c,ev,p) => roughly(0.0,atol=1e-8)

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
						z = a + y - mig.pifun(ih,ihh,pr,move,ik,p)
						@fact abs(mig.cashFunction(a,y,ih,ihh,pr,move,ik,p)) - abs(z) => roughly(0.0)
					else
						for ihh in 0:1
							a = rand()
							y = rand()
							pr = rand()*10
							z = a + y - mig.pifun(ih,ihh,pr,move,ik,p)
							@fact abs(mig.cashFunction(a,y,ih,ihh,pr,move,ik,p)) - abs(z) => roughly(0.0)
						end
					end
				end
			end
		end
	end
end



facts("testing vsavings!()") do
	
	# assumptions
	# ===========
	p = Param(1)
	m = Model(p)

    w = zeros(p.namax)
    cons = zeros(p.namax)
    a = m.grids["assets"]

    w_t = 0.0
    cons_t = 0.0

    fac = 2.25
    EV = a.*fac  	# just take a straight line with slope fac
    cash = rand()
    mc = rand()
    lb = 0
    s = linspace(lb,cash-0.01,p.namax)
	ub = a[end]
	for is = 1:p.ns
	for ih = 0:1
	for itau = 1:p.ntau
	for def in [true, false]

		# compute vsavings!()
		mig.vsavings!(w,a,EV,s,cons,cash,ub,is,ih,itau,mc,def,p)

		# compute values at each savings grid
		consta =  ih*((is==1)*p.xi1 + (is==2)*p.xi2) - def*p.lambda - mc + (itau-1)*p.tau

		for i = 1:p.namax
			# get savings vector
			x = s[i] / p.R

			# get cons at that choice
			cc = (is==1)*(cash-x) + (is==2)*(cash-x)*p.sscale
			if cc < 0
				w_t = p.myNA
			else
				w_t = mig.ufun(cc,s[i]*fac,p)
				w_t += consta
			end
			@fact cc => cons[i]
			@fact w_t => roughly(w[i],atol=0.00001)
		end

	end
	end
	end
	end
end





facts("testing EVfunChooser") do

	p    = mig.Param(1)
	m    = mig.Model(p)
	mig.setrand!(m)

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
			iz   = rand(1:p.nz)
			itau = rand(1:p.ntau)
			ij   = rand(1:p.nJ)

			EV = m.EVfinal[:,ih,ip,ik]

			mig.EVfunChooser!(ev,is,iz,ih,itau,ip,iy,ij,ik,ti,m,p)

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
			iz   = rand(1:p.nz)
			itau = rand(1:p.ntau)
			ij   = rand(1:p.nJ)

		# V[y,p,P,z,a,h,tau,j,age]
			EV = m.EV[is,iz,iy,ip,itau,:,ih,ik,ti+1]

			mig.EVfunChooser!(ev,is,iz,ih,itau,ip,iy,ij,ik,ti,m,p)

			@fact ev[:] .- EV[:] => roughly(zeros(p.na),atol=0.000001)

		end
	end

end



facts("test integration of vbar: getting EV") do

	context("test uniform weights return original array") do

		p    = mig.Param(1)
		m    = mig.Model(p)

		mig.setrand!(m)

		Gz = m.gridsXD["Gz"]
		Gyp = m.gridsXD["Gyp"]
		Gtau = m.grids["Gtau"]

		# 1/number of all integration states
		num = p.nz * p.ny * p.np 
		fill!(m.vbar,1/num)
		fill!(Gz,1/p.nz)
		fill!(Gyp,1/(p.ny*p.ny))
		fill!(Gtau,1/p.ntau	)

		for itest = 1:50

			# choose a random state
			ia   = rand(1:p.na)
			is   = rand(1:p.ns)
			ih   = rand(1:p.nh)
			iy   = rand(1:p.ny)
			ip   = rand(1:p.np)
			iz   = rand(1:p.nz)
			itau = rand(1:p.ntau)
			ij   = rand(1:p.nJ)
			it   = rand(1:(p.nt-2))

			Gs = squeeze(m.gridsXD["Gs"][:,:,it],3)

			# calling integrateVbar must return vbar.
			mig.integrateVbar!(iz,iy,ip,is,it,Gz,Gyp,Gs,Gtau,m,p)

			@fact m.vbar[mig.idx9(is,iz,iy,ip,itau,ia,ih,ij,it,p)] - m.EV[mig.idx9(is,iz,iy,ip,itau,ia,ih,ij,it,p)] => roughly(0.0,atol=0.00001)

		end
	end

	context("test whether equal to hand integration") do

		p    = mig.Param(2)
		m    = mig.Model(p)
		mig.setrand!(m)

		myEV = zeros(m.dimvec2[1:8])

		age = p.nt-2

		Gz = m.gridsXD["Gz"]
		Gyp = m.gridsXD["Gyp"]
		Gs = squeeze(m.gridsXD["Gs"][:,:,age],3)
		Gtau = m.grids["Gtau"]

		for ij=1:p.nJ				# current location
		for ih=1:p.nh
		for ia=1:p.na
		for itau=1:p.ntau			# type

		# start integration loop
		for iz=1:p.nz				# individual income shock
		for ip=1:p.np 				# regional price deviation
		for iy=1:p.ny 				# regional income deviation
		for is=1:p.ns 				# regional income deviation

		# dimvec2 = (ny, np, nP, nz, na, nh, ntau,  nJ, nt-1 )
			for itau1=1:p.ntau			# type
			for iz1=1:p.nz				# individual income shock
			for ip1=1:p.np 				# regional price deviation
			for iy1=1:p.ny 				# regional income deviation
			for is1=1:p.ns 				# regional income deviation

				myEV[is,iz,iy,ip,itau,ia,ih,ij] += m.vbar[mig.idx9(is1,iz1,iy1,ip1,itau1,ia,ih,ij,age,p)] * Gz[iz,iz1,ij] * Gyp[iy + p.ny * (ip-1) , iy1 + p.ny * (ip1-1), ij ] * Gs[is,is1] * Gtau[itau1]
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
		# end computation loop


		# choose a random state
		ia   = rand(1:p.na)
		ih   = rand(1:p.nh)
		ij   = rand(1:p.nJ)
		# start test loop
		for itau=1:p.ntau			# type
		for iz=1:p.nz				# individual income shock
		for ip=1:p.np 				# regional price deviation
		for iy=1:p.ny 				# regional income deviation
		for is=1:p.ns 				# regional income deviation
		mig.integrateVbar!(iz,iy,ip,is,age,Gz,Gyp,Gs,Gtau,m,p)
			@fact myEV[is,iz,iy,ip,itau,ia,ih,ij] - m.EV[is,iz,iy,ip,itau,ia,ih,ij,age] => roughly(0.0,atol=0.00001)
		end
		end
		end
		end
		end
		# end test locationp
	end
end



facts("checking some properties of the solution") do

	p = Param(2)
	m = Model(p)
	mig.solve!(m,p)

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

	context("check conditional v is never decreasing in z") do

		tt = mapslices(x -> diff(x),m.vh,4)
		@fact all(tt .>= 0.0) => true

	end

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





end # module