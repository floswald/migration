


# testing solver.jl


@testset "solver: linear index functions" begin

	p = mig.Param(2)
	m = mig.Model(p)

	# set arrays to random numbers
	mig.setrand!(m)

	@testset "testing 11 dim index" begin

		for itest in 1:10

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

			@test isapprox(m.vh[ihh,ik,is,iz,iy,ip,itau,ia,ih,ij,it],m.vh[mig.idx11(ihh,ik,is,iz,iy,ip,itau,ia,ih,ij,it,p)],atol=1e-6)
		end
	end

	@testset "testing 10 dim index" begin

		for itest in 1:10

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

			@test isapprox(m.v[ik,is,iz,iy,ip,itau,ia,ih,ij,it],m.v[mig.idx10(ik,is,iz,iy,ip,itau,ia,ih,ij,it,p)],atol=1e-6)
			@test isapprox(m.rho[ik,is,iz,iy,ip,itau,ia,ih,ij,it],m.rho[mig.idx10(ik,is,iz,iy,ip,itau,ia,ih,ij,it,p)],atol=1e-6)
		end
	end

	# @testset "testing 10_a index" begin

	# 	# choose a random state
	# 	ik   = rand(1:p.nJ)
	# 	is   = rand(1:p.ns)
	# 	ih   = rand(1:p.nh)
	# 	iy   = rand(1:p.ny)
	# 	ip   = rand(1:p.np)
	# 	iz   = rand(1:p.nz)
	# 	itau = rand(1:p.ntau)
	# 	ij   = rand(1:p.nJ)
	# 	it   = rand(1:(p.nt-1))

	# 	myidx = mig.idx10_a(ik,is,iz,iy,ip,itau,ih,ij,it,p)

	# 	for ia in 1:p.na

	# 		println( mig.idx10(ik,is,iz,iy,ip,itau,ia,ih,ij,it,p) )
	# 		println(myidx[ia])
	# 		# @test mig.idx10(ik,is,iz,iy,ip,itau,ia,ih,ij,it,p) == myidx[ia]

	# 	end

	# end

	@testset "testing 9 dim index" begin

		for itest in 1:10

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

			@test m.vbar[is,iz,iy,ip,itau,ia,ih,ij,it] == m.vbar[mig.idx9(is,iz,iy,ip,itau,ia,ih,ij,it,p)]
		end
	end

	@testset "testing final index function" begin

		for itest in 1:10

			# choose a random state
			ik   = rand(1:p.nJ)
			ia   = rand(1:p.na)
			ih   = rand(1:p.nh)
			ip   = rand(1:p.np)
			iy   = rand(1:p.ny)

			@test m.EVfinal[ia,ih,ip,iy,ik] == m.EVfinal[mig.idxFinal(ia,ih,ip,iy,ik,p)]
		end
	end

	@testset "moving cost indexer" begin

		for itest in 1:10
			# choose a random state
			it   = rand(1:(p.nt-1))
			ij   = rand(1:p.nJ)
			ik   = rand(1:p.nJ)
			itau   = rand(1:p.ntau)
			ih   = rand(1:p.nh)
			is   = rand(1:p.ns)
			@test m.gridsXD["movecost"][it,ij,ik,itau,ih,is] == m.gridsXD["movecost"][mig.idxMC(it,ij,ik,itau,ih,is,p)]
		end
	end

end
gc()

@testset "Solver. other parts." begin

	@testset "testing utility function" begin

		p = mig.Param(1)

		c = rand()
		ev = rand()

		u = (p.eta /(1-p.gamma)) * c^(1-p.gamma) + p.beta * ev

		@test isapprox(u , mig.ufun(c,ev,p) ,atol=1e-8)

	end


	@testset "testing payments function" begin

		
		p = mig.Param(1)

			move = true
			ih   = 0
			ihh  = 0
			pr_j  = rand()
			pr_k  = rand()
			j    = rand(1:p.nJ)
		
			@test isapprox(-p.kappa[j] * pr_k , mig.pifun(ih,ihh,pr_j,pr_k,move,j,p),atol=0.0000001)

			move = false
			@test isapprox(-p.kappa[j] * pr_k , mig.pifun(ih,ihh,pr_j,pr_k,move,j,p),atol=0.0000001)

			# rent - buy
			move = true
			ih   = 0
			ihh  = 1
			pr_j  = rand()
			pr_k  = rand()
			j    = rand(1:p.nJ)

			@test isapprox(-pr_k , mig.pifun(ih,ihh,pr_j,pr_k,move,j,p),atol=0.0000001)	
			move = false
			@test isapprox(-pr_k , mig.pifun(ih,ihh,pr_j,pr_k,move,j,p),atol=0.0000001)	

			# own - sell
			move = true
			ih   = 1
			ihh  = 0
			pr_j  = rand()
			pr_k  = rand()
			j    = rand(1:p.nJ)

			@test isapprox((pr_j*(1-p.phi) - pr_k*p.kappa[j]) , mig.pifun(ih,ihh,pr_j,pr_k,move,j,p) ,atol=0.0000001)
			move = false
			@test isapprox(((1-p.phi) - p.kappa[j])*pr_k , mig.pifun(ih,ihh,pr_j,pr_k,move,j,p) ,atol=0.0000001)

			# own - buy
			move = true
			ihh  = 1
			@test isapprox((pr_j*(1-p.phi) - pr_k) ,mig.pifun(ih,ihh,pr_j,pr_k,move,j,p) ,atol=0.0000001)

			# own - stay
			move = false
			@test mig.pifun(ih,ihh,pr_j,pr_k,move,j,p) == 0.0

	end



	@testset "testing cashFunction" begin

		p = mig.Param(1)
		j = rand(1:p.nJ)

		pr_j = rand()*10
		for ik in 1:p.nJ
			for is in 1:p.ns
				for ih in 0:1
					for move in [true,false]
						if move
							pr_k = rand()*10
						else
							pr_k = pr_j
						end
						for ihh in 0:1
							a = rand()
							y = rand()
							z = a + y + mig.pifun(ih,ihh,pr_j,pr_k,move,ik,p)
							@test isapprox(mig.cashFunction(a,y,ih,ihh,pr_j,pr_k,move,ik,p), z )
						end
					end
				end
			end
		end
	end


	@testset "testing maxvalue" begin
		
		# assumptions
		# ===========
		p = Param(2)
		setfield!(p,:R,1.9)
		m = Model(p)

		acc = mig.Accelerator(1)

	    w = zeros(p.namax)
	    savings = zeros(p.namax)
	    cons = zeros(p.namax)
	    a = m.grids["assets"]

	    ih = 0

	    fac = 2
	    EV = a.^fac  	# just take a straight line with slope fac
	    cash = 80.0
	    mc = rand()
	    lb = 0.0

		consta =  ih*((is==1)*p.xi1 + (is==2)*p.xi2) - mc + p.amenity_ENC

	    noSaving = false
	    v1 = mig.maxvalue(cash,1,p,a,w,0,mc,EV,lb,1,acc,noSaving,consta,savings,cons)
	    println(v1)
	    @test v1[2] > 0.0 

	    noSaving = true
		acc = mig.Accelerator(1)
	    w = zeros(p.namax)
	    v2 = mig.maxvalue(cash,1,p,a,w,0,mc,EV,lb,1,acc,noSaving,consta,savings,cons)
		    println(v2)
	    @test v2[2] == 0.0 
	end


	@testset "testing vsavings!()" begin
		
		# assumptions
		# ===========
		p = Param(1)
		m = Model(p)

		acc = mig.Accelerator(1)

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
	    s = collect(linspace(lb,cash-0.01,p.namax))
		for is = 1:p.ns
		for ih = 0:1

			# compute values at each savings grid
			consta =  ih*((is==1)*p.xi1 + (is==2)*p.xi2) - mc + p.amenity_WSC

			# compute vsavings!()
			mig.vsavings!(w,a,EV,s,cons,cash,is,ih,mc,p,acc,consta)


			for i = 1:p.namax
				# get savings vector
				x = s[i] / p.R

				# get cons at that choice
				cc = (cash-x)*p.sscale[is]
				if cc < 0
					w_t = p.myNA
				else
					w_t = mig.ufun(cc,s[i]*fac,p)
					w_t += consta
				end
				@test cc == cons[i]
				@test isapprox(w_t ,w[i],atol=0.00001)
			end

		end
		end
	end

	@testset "testing vsavings!() if ctax is on" begin
		
		# assumptions
		# ===========
		p = Param(1)
		setfield!(p,:ctax,0.9)
		m = Model(p)

		acc = mig.Accelerator(1)

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
	    s = collect(linspace(lb,cash-0.01,p.namax))
		for is = 1:p.ns
		for ih = 0:1

			consta =  ih*((is==1)*p.xi1 + (is==2)*p.xi2) - mc + p.amenity_WSC

			# compute vsavings!()
			mig.vsavings!(w,a,EV,s,cons,cash,is,ih,mc,p,acc,true,consta)


			for i = 1:p.namax
				# get savings vector
				x = s[i] / p.R

				# get cons at that choice
				cc = (cash-x)*p.sscale[is]
				if cc < 0
					w_t = p.myNA
				else
					cc *= p.ctax
					w_t = mig.ufun(cc,s[i]*fac,p)
					w_t += consta
				end
				@test cc == cons[i]
				@test isapprox(w_t ,w[i],atol=0.00001)
			end

		end
		end
	end

	# @testset "testing bilinear approx" begin
		
	# 	xgrid = linspace(-1.5,3,10)
	# 	ygrid = linspace(1.5,3.8,12)

	# 	myfun(i1,i2) = ((i1-0.1)+(i2*5.1))*3.4

	# 	zmat = Float64[ myfun(i,j) for i in xgrid, j in ygrid ]

	# 	@test zmat[1,1] == mig.bilinearapprox(xgrid[1],ygrid[1],xgrid,ygrid,zmat)
	# 	@test zmat[3,6] == mig.bilinearapprox(xgrid[3],ygrid[6],xgrid,ygrid,zmat)
	# 	@test zmat[10,12] ==  mig.bilinearapprox(xgrid[10],ygrid[12],xgrid,ygrid,zmat) 

	# 	x = xgrid[5]+rand()
	# 	y = ygrid[5]+rand()*0.1
	# 	z = myfun(x,y)
	# 	@test z == roughly(mig.bilinearapprox(x,y,xgrid,ygrid,zmat),atol=1e-5)

	# 	x = xgrid[end]-0.01
	# 	y = ygrid[end]-0.02
	# 	z = myfun(x,y)
	# 	@test z == roughly(mig.bilinearapprox(x,y,xgrid,ygrid,zmat),atol=1e-5)

	# 	for i in 1:100
	# 		x = rand() * (3+1.5) - 1.5
	# 		y = rand() * (3.8-1.5) + 1.5
	# 		z = myfun(x,y)
	# 		@test z == roughly(mig.bilinearapprox(x,y,xgrid,ygrid,zmat),atol=1e-5)
	# 	end

	# 	x = xgrid[]+rand()
	# 	y = ygrid[5]+rand()*0.1
	# 	z = myfun(x,y)
	# 	@test z == roughly(mig.bilinearapprox(x,y,xgrid,ygrid,zmat),atol=1e-5)

	# 	# out of bounds
	# 	@test zmat[1,1] == mig.bilinearapprox(-2.0,0.0,xgrid,ygrid,zmat)
	# 	@test zmat[10,12] == mig.bilinearapprox(100.1,5.5,xgrid,ygrid,zmat)

	# end

	# @testset "testing bilinear approx for mulitple values" begin
		
	# 	xgrid = linspace(-1.5,3,10)
	# 	ygrid = linspace(1.5,3.8,12)

	# 	zmat = Float64[ (i-0.1)*(j+0.1)*3.4 for i in xgrid, j in ygrid ]
	# 	zmat2= Float64[ (i-0.2)*(j+0.1)*0.4 for i in xgrid, j in ygrid ]
	# 	zmat3= Float64[ (i-0.1)*(j+0.3)*8.2 for i in xgrid, j in ygrid ]

	# 	@test length(mig.bilinearapprox(xgrid[1],ygrid[1],xgrid,ygrid,zmat)) == 1
	# 	@test length(mig.bilinearapprox(xgrid[1],ygrid[1],xgrid,ygrid,zmat,zmat)) == 2
	# 	@test length(mig.bilinearapprox(xgrid[1],ygrid[1],xgrid,ygrid,zmat,zmat,zmat)) == 3

	# 	z = mig.bilinearapprox(xgrid[1],ygrid[1],xgrid,ygrid,zmat,zmat2,zmat3)

	# 	@test zmat[1,1] == z[1]
	# 	@test zmat2[1,1] == z[2]
	# 	@test zmat3[1,1] == z[3]

	# 	z = mig.bilinearapprox(xgrid[3],ygrid[6],xgrid,ygrid,zmat,zmat2,zmat3)

	# 	@test zmat[3,6] == z[1]
	# 	@test zmat2[3,6] == z[2]
	# 	@test zmat3[3,6] == z[3]

	# 	x = xgrid[5]+rand()
	# 	y = ygrid[5]+rand()*0.1
	# 	z = (x-0.1)*(y+0.1)*3.4
	# 	z2= (x-0.2)*(y+0.1)*0.4
	# 	z3= (x-0.1)*(y+0.3)*8.2

	# 	zout = mig.bilinearapprox(x,y,xgrid,ygrid,zmat,zmat2,zmat3)

	# 	@test z == roughly(zout[1],atol=1e-5)
	# 	@test z2== roughly(zout[2],atol=1e-5)
	# 	@test z3== s(zout[3],atol=1e-5)

	# end


	p    = mig.Param(1)
	m    = mig.Model(p)
	mig.setrand!(m)

	@testset "testing EVfunChooser in period T-1" begin

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

			EV = m.EVfinal[:,ih,ip,iy,ik]

			mig.EVfunChooser!(ev,is,iz,ih,itau,ip,iy,ij,ik,ti,m,p)

			@test isapprox(ev[:] , EV[:] ,atol=0.000001)

		end
	end


	@testset "testing EVfunChooser in period T-2" begin
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

			@test isapprox(ev[:] , EV[:] ,atol=0.000001)

		end
	end

	@testset "test integration: uniform weights return original array" begin

		p    = mig.Param(2)
		m    = mig.Model(p)

		mig.setrand!(m)

		Gz = m.gridsXD["Gz"]
		Gyp = m.gridsXD["Gyp"]

		# 1/number of all integration states
		num = p.nz * p.ny * p.np 
		fill!(m.vbar,1/num)
		fill!(Gz,1/p.nz)
		fill!(Gyp,1/(p.ny*p.np))

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

			Gs = m.gridsXD["Gs"][:,:,it]

			# calling integrateVbar must return vbar.
			mig.integrateVbar!(iz,iy,ip,is,it,Gz,Gyp,Gs,m,p)

			@test isapprox(m.vbar[mig.idx9(is,iz,iy,ip,itau,ia,ih,ij,it,p)] , m.EV[mig.idx9(is,iz,iy,ip,itau,ia,ih,ij,it,p)] ,atol=0.00001)

		end
	end

	@testset "test integration: constant array returns original array" begin

		p    = mig.Param(2)
		m    = mig.Model(p)

		mig.setones!(m)

		Gz = m.gridsXD["Gz"]
		Gyp = m.gridsXD["Gyp"]

		# # 1/number of all integration states
		# num = p.nz * p.ny * p.np 
		# fill!(m.vbar,1/num)
		# fill!(Gz,1/p.nz)
		# fill!(Gyp,1/(p.ny*p.np))

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

			Gs = m.gridsXD["Gs"][:,:,it]

			# calling integrateVbar must return vbar.
			mig.integrateVbar!(iz,iy,ip,is,it,Gz,Gyp,Gs,m,p)

			@test isapprox(m.EV[mig.idx9(is,iz,iy,ip,itau,ia,ih,ij,it,p)] ,1.0,atol=0.00001)

		end
	end

	@testset "test integration: equal to hand integration?" begin

		p    = mig.Param(1)
		m    = mig.Model(p)
		mig.setrand!(m)

		myEV = zeros(m.dimvec2[1:8])

		age = p.nt-2

		Gz = m.gridsXD["Gz"]
		Gyp = m.gridsXD["Gyp"]
		Gs = m.gridsXD["Gs"][:,:,age]

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
			for iz1=1:p.nz				# individual income shock
			for ip1=1:p.np 				# regional price deviation
			for iy1=1:p.ny 				# regional income deviation
			for is1=1:p.ns 				# regional income deviation

				myEV[is,iz,iy,ip,itau,ia,ih,ij] += m.vbar[mig.idx9(is1,iz1,iy1,ip1,itau,ia,ih,ij,age,p)] * Gz[iz,iz1] * Gyp[iy + p.ny * (ip-1) , iy1 + p.ny * (ip1-1) ] * Gs[is,is1] 
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
		itau   = rand(1:p.ntau)
		# start test loop
		for iz=1:p.nz				# individual income shock
		for ip=1:p.np 				# regional price deviation
		for iy=1:p.ny 				# regional income deviation
		for is=1:p.ns 				# regional income deviation
		mig.integrateVbar!(iz,iy,ip,is,age,Gz,Gyp,Gs,m,p)
			@test isapprox(myEV[is,iz,iy,ip,itau,ia,ih,ij] , m.EV[is,iz,iy,ip,itau,ia,ih,ij,age] ,atol=0.00001)
		end
		end
		end
		end
		# end test locationp
	end

	@testset "mylinspace!" begin

		s = zeros(13)
		lb = -1.1
		ub = 18.4
		mig.mylinspace!(s,lb,ub)
		@test isapprox(s ,collect(linspace(lb,ub,length(s))),atol=1e-5)

		s = zeros(130)
		lb = 0.01
		ub = 1.4
		mig.mylinspace!(s,lb,ub)
		@test isapprox(s ,collect(linspace(lb,ub,length(s))),atol=1e-5)
	end

end # facts






