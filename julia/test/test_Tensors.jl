

module test_Tensors

using FactCheck
using mig

facts("testing the final period tensors") do

	p = mig.Param(1)
	m = mig.Model(p)

	context("testing final period Tensor") do


		agrid = m.grids["asset_rent"]
		hgrid = m.grids["housing"]

		# compute tensor
		tens = p.omega[1] .+ p.omega[2] .* mig.E_tensors.T_FinalV(agrid,hgrid,m.gridsXD["p"])

		# loop over all states
		for ia = 1:p.na
		for ih = 1:p.nh
		for iP = 1:p.nP
		for ij = 1:p.nJ
		for ip = 1:p.np 

			tval = p.omega[1] + p.omega[2] * log(agrid[ia] + hgrid[ih] * (m.gridsXD["p"][iP,ip,ij] ) )

			@fact tens[ia,ih,iP,ij,ip] == tval => true

		end
		end
		end
		end
		end
	end

end


facts("testing EVbar integration tensor") do

	p = mig.Param(1)
	m = mig.Model(p)
# T_Evbar V[a,h,y1,p1,P1,z1,tau,j] * Gz[z,z1] * Gp[p,p1,j] * Gy[y,y1,j] * GP[P,P1] | a,h,y,p,P,z,tau,j
	
	age = p.nt-1

	indx9 = 0

	Gz = m.gridsXD["Gz"]
	Gy = m.gridsXD["Gy"]
	Gp = m.gridsXD["Gp"]
	GP = m.grids2D["GP"]

	# want to integrate this
	# it contains random numbers
	

	# result from my code
	mig.integrateVbar!(m,age)



	# testing against this
	test_evb = zeros(p.dimvec3[1:8])

	for ia   = 1:p.na
	for ih   = 1:p.nh
	for iy   = 1:p.ny
	for ip   = 1:p.np
	for iP   = 1:p.nP
	for iz   = 1:p.nz
	for itau = 1:p.ntau
	for ij   = 1:p.nJ

		# println("ia=$ia,ih=$ia,iy=$iy,ip=$ip,iP=$iP,iz=$iz,itau=$itau,ij=$ij")
		for jy = 1:p.ny
		for jp = 1:p.np
		for jP = 1:p.nP
		for jz = 1:p.nz

		# println("jy=$jy,jp=$jp,jP=$jP,jz=$jz")

		indx9 = mig.idx9(ia,ih,jy,jp,jP,jz,itau,ij,age,p)
		# println(indx9)


			test_evb[ia,ih,iy,ip,iP,iz,itau,ij] += m.vbar[indx9] * Gz[iz,jz,ij] * Gp[ip,jp,ij] * Gy[iy,jy,ij] * GP[iP,jP];
		end
		end
		end
		end


		indx9 = mig.idx9(ia,ih,iy,ip,iP,iz,itau,ij,age,p)
		@fact test_evb[ia,ih,iy,ip,iP,iz,itau,ij] - m.EV[indx9] => roughly(0.0,atol=0.00001)

	end
	end
	end
	end
	end
	end
	end
	end

end

end	# module