


# testing solver.jl
# test computation functions here and not
# correct array contents. thats in test_model

module test_solver

using FactCheck, mig

facts("testing utility function") do

	p = mig.Param()
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

	
	p = mig.Param()

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

	p = mig.Param()
	x = rand()

	@fact mig.grossSaving(x,p) => x*p.R

	x = -x
	@fact mig.grossSaving(x,p) => x*p.Rm

end


facts("testing cashFunction") do

	p = mig.Param()
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

		p = mig.Param()
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

		p = mig.Param()
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


		p = mig.Param()
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

	p = mig.Param()
	m = mig.Model(p)
	iz = rand(1:p.nz)
	itau = rand(1:p.ntau)
	iP = rand(1:p.nP)
	iY = rand(1:p.nP)
	ip = rand(1:p.np)
	iy = rand(1:p.ny)
	ij = rand(1:p.nJ)
	ihh = 0

	# test for penultimate period
	ti = p.nt-1
	EV = m.EVfinal[:,ihh+1,iP,ip,ij]

	@fact mig.EVfunChooser(iz,ihh,itau,iP,iY,ip,iy,ij,ti,m,p)[:] => EV[:] 

	# test for previous
end









end # module