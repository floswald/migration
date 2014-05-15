


# testing solver.jl
# test computation functions here and not
# correct array contents. thats in test_model

module test_solver

using FactCheck, mig

facts("test linear index functions") do

	p = mig.Param(1)
	m = mig.Model(p)

	context("testing 11 dim index") do

		# choose a random state
		ihh  = rand(1:p.nh)
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

		@fact m.v[ihh,ik,ia,ih,iy,ip,iP,iz,itau,ij,it] == m.v[mig.idx11(ihh,ik,ia,ih,iy,ip,iP,iz,itau,ij,it,p)] => true

		# choose another one
		ihh  = rand(1:p.nh)
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

		@fact m.v[ihh,ik,ia,ih,iy,ip,iP,iz,itau,ij,it] == m.v[mig.idx11(ihh,ik,ia,ih,iy,ip,iP,iz,itau,ij,it,p)] => true

	end


	context("testing 10 dim index") do
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

		@fact m.vh[ik,ia,ih,iy,ip,iP,iz,itau,ij,it] == m.vh[mig.idx10(ik,ia,ih,iy,ip,iP,iz,itau,ij,it,p)] => true

		# choose another one
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

		@fact m.vh[ik,ia,ih,iy,ip,iP,iz,itau,ij,it] == m.vh[mig.idx10(ik,ia,ih,iy,ip,iP,iz,itau,ij,it,p)] => true

	end

end

facts("testing utility function") do

	p = mig.Param(1)
	x = 1.0
	own = 1
	def = true
	mc = 10.0
	z = (1/(1-p.gamma)) * x^(1-p.gamma) + own*p.xi - def*p.lambda - mc

	@fact z == mig.mig.ufun(x,own,mc,def,p) => true

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

end



facts("testing maxvalue function") do

	context("maxalue for owners") do

		p = mig.Param(1)
		m = mig.Model(p)

		w = zeros(p.na)
		fill!(w,p.myNA)


		x = rand()
		mc = rand()
		def = false
		EV = rand(p.na)
		own = 1

		s = mig.agridChooser( own, m)

		for i=1:p.na
			if x-s[i] > 0
				w[i] = mig.ufun(x-s[i],own,mc,def,p) + p.beta * EV[i]
			end
		end

		r = findmax(w)

		@fact mig.maxvalue(x,p,s,w,own,mc,def,EV) => r

		fill!(w,p.myNA)
		x = -10000.0
		def = false

		@fact mig.maxvalue(x,p,s,w,own,mc,def,EV) => (p.myNA,1)
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

		s = mig.agridChooser( own, m)

		for i=1:p.na
			if x-s[i] > 0
				w[i] = mig.ufun(x-s[i],own,mc,def,p) + p.beta * EV[i]
			end
		end

		r = findmax(w)

		@fact mig.maxvalue(x,p,s,w,own,mc,def,EV) => r

		fill!(w,p.myNA)
		x = -100000.0
		def = false

		@fact mig.maxvalue(x,p,s,w,own,mc,def,EV) => (p.myNA,1)
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

		s = mig.agridChooser( own, m)
		EV = rand(p.na-1)
		@fact_throws mig.maxvalue(x,p,s,w,own,mc,def,EV) 
	end

end



facts("testing EVfunChooser") do

	p    = mig.Param(1)
	m    = mig.Model(p)

	# choose a random state
	iz   = rand(1:p.nz)
	itau = rand(1:p.ntau)
	iP   = rand(1:p.nP)
	# iY   = rand(1:p.nY)
	ip   = rand(1:p.np)
	iy   = rand(1:p.ny)
	ij   = rand(1:p.nJ)
	ihh  = 0

	# test for penultimate period
	ti = p.nt-1
	EV = m.EVfinal[:,ihh+1,iP,ip,ij]

	@fact mig.EVfunChooser(iz,ihh,itau,iP,ip,iy,ij,ti,m,p)[:] => EV[:] 

	# test for previous

	ti = p.nt-2
	EV = m.EV[:,ihh+1,iy,ip,iP,iz,itau,ij,ti+1]

	@fact mig.EVfunChooser(iz,ihh,itau,iP,ip,iy,ij,ti,m,p)[:] => EV[:] 

	# choose another one
	iz   = rand(1:p.nz)
	itau = rand(1:p.ntau)
	iP   = rand(1:p.nP)
	# iY   = rand(1:p.nY)
	ip   = rand(1:p.np)
	iy   = rand(1:p.ny)
	ij   = rand(1:p.nJ)
	ihh  = 1

	# test for penultimate period
	ti = p.nt-1
	EV = m.EVfinal[:,ihh+1,iP,ip,ij]

	@fact mig.EVfunChooser(iz,ihh,itau,iP,ip,iy,ij,ti,m,p)[:] => EV[:] 

	# test for previous

	ti = p.nt-2
	EV = m.EV[:,ihh+1,iy,ip,iP,iz,itau,ij,ti+1]

	@fact mig.EVfunChooser(iz,ihh,itau,iP,ip,iy,ij,ti,m,p)[:] => EV[:] 
end


# facts("test myFindMax: hard coded to max over dim 11") do

# 	p = mig.Param()
# 	A = rand(p.dimvec)
# 	x = mapslices(findmax,A,11)

# 	# z = mig.ismaxfun(A,11)
# 	# ArrayViews.jl does not support dims>4
# 	z = myFindMax(A)

# 	@fact x==z => true

# end

# facts("testing Housing Discrete Choice function") do

# 	p    = mig.Param()
# 	m    = mig.Model(p)
# 	age  = p.nt - 2

# 	v = mapslices(findmax,m.vh[:,:,:,:,:,:,:,:,:,age,:,:],11)
# 	vh = map(x->x[1],v)
# 	dh = map(x->x[2],v)

# 	# call function on model object
# 	mig.computeHousingDchoice!(age,m)

# 	# check results
# 	@fact vh[:] .== m.vh[:] => trues(length(vh))
# 	@fact dh[:] .== m.dh[:] => trues(length(dh))

# end










end # module