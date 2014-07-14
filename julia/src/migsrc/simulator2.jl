





# simulator

function simulate(m::Model2,p::Param)

	T = p.nt-1

	# set random seed
	srand(p.rseed)

	# grids
	agrid = m.grids["assets"]
	zgrid = m.gridsXD["z"]
	pgrid = m.price

	# drawing the shocks 
	# ==================

	# initial distributions
	# TODO
	# all of those should be non-uniform probably
	G0z   = Categorical([1/p.nz for i=1:p.nz])
	x     = [1/i^2 for i=1:length(m.aone:p.na)]
	x     = x / sum(x)
	G0a   = Categorical(x)

	# prepare cumsum of probability matrices
	# cumrho = cumsum(m.rho,1)	# get cumulative prob of moving along dim k
	cumGz  = cumsum(m.gridsXD["Gz"],2)

	# storage
	Dt      = zeros(Int,p.nsim*(T))	# age
	Di      = zeros(Int,p.nsim*(T))	# identity
	Dv      = zeros(p.nsim*(T))	# value
	Dc      = zeros(p.nsim*(T))	# consu
	Da      = zeros(p.nsim*(T))	# asset value
	Dz      = zeros(p.nsim*(T))	# income shock value
	Dp      = zeros(p.nsim*(T))	# house price value
	Dh      = zeros(Int,p.nsim*(T))	# housing state
	Dhh     = zeros(Int,p.nsim*(T))	# housing choice
	DiS     = zeros(Int,p.nsim*(T))	# savings index
	DS     = zeros(p.nsim*(T))	# savings values
	Diz     = zeros(Int,p.nsim*(T))	# z index

	# begin simulation loop
	# =====================

	# @inbounds begin
	for i = 1:p.nsim

		iz = rand(G0z)
		# iz   = convert(Int,floor(median([1:p.nz])))	#everybody gets median income
		ia   = rand(G0a) + m.aone - 1
		ia   =  m.aone 
		ih   = 0

		for age = 1:T

			# move to where?
			# get probabilities of moving to k

			
			# find housing choice
			ihh = m.dh[iz,ia,ih+1,age]

			# record current period state
			# ---------------------------

			Dt[age + T*(i-1)] = age
			Di[age + T*(i-1)] = i
			Dh[age + T*(i-1)] = ih
			Da[age + T*(i-1)] =	agrid[ ia ]
			Dz[age + T*(i-1)] = zgrid[iz,age]
			Dp[age + T*(i-1)] = pgrid
			Diz[age + T*(i-1)] = iz
			Dv[age + T*(i-1)] = m.v[iz,ia,ih+1,age]

			# current choices
			# ---------------

			ss = m.s[iz,ia,ih+1,age]
			
			Dc[age + T*(i-1)] = m.c[iz,ia,ih+1,age]
			Dhh[age + T*(i-1)] = ihh	# housign choice

			# transition to new state
			# -----------------------




			# catching simulation errors
			# =========================
			if ss==0
				# println(ktmp)
				ia = 1
				println("next period asset state is zero at\niz=$iz,ih=$ih,ihh=$ihh,age=$age,id=$i")
				println("age=$age")
				println("ia=$ia")
				DiS[age + T*(i-1)]=1
				DS[age + T*(i-1)]=agrid[1]
				# inc = income(m.gridsXD["y"][iy,ij],m.gridsXD["z"][iz,ij,age])
				# cash = cashFunction(Da[age + T*(i-1)],income(m.gridsXD["y"][iy,ij],m.gridsXD["z"][iz,ij,age]),ih,ihh,m.gridsXD["p"][iP,ip,ij],move,moveto,p)
				# println("current value=$(Dv[age + T*(i-1)])")
				# println("current cons=$(Dc[age + T*(i-1)])")
				# println("income = $inc")
				# println("grossA = $(grossSaving(agrid[ia],p))")
				# println("cash = $cash")
				# error("next period asset state is zero at\nik=$moveto,iy=$iy,ip=$ip,iP=$iP,iz=$iz,a=$(agrid[ia]),ih=$ih,ihh=$ihh,itau=$itau,ij=$ij,age=$age,id=$i,move=$move")
			else

				DiS[age + T*(i-1)] = ss
				ia = ss
				DS[age + T*(i-1)] = agrid[ ss ] 

			end
			# =========================



			ih = ihh
			# get new shocks in region 
			# you are moving to
			# -----------------------

			# iP = searchsortedfirst( cumGP[iP,:][:] , rand() ) 	# SLOW, all of them
			# ip = searchsortedfirst( cumGp[ip,:,moveto][:], rand() )
			# iy = searchsortedfirst( cumGy[iy,:,moveto][:], rand() )
			# if move
				# iz = searchsortedfirst( cumGzM[iz,:,moveto][:], rand() )
			# else
				iz = searchsortedfirst( cumGz[iz,:][:], rand() )
			# end
			# is = searchsortedfirst( cumGs[is,:,age][:], rand() )

		end	# age t

	end	# individual i

	# end # inbounds

	# collect all data into a dataframe
	w = (Dp .* Dh) .+ Da

	df = DataFrame(id=Di,age=Dt,age2=Dt.^2,a=Da,saveidx=DiS,save=DS,c=Dc,iz=Diz,z=Dz,h=Dh,hh=Dhh,v=Dv,wealth=w,own=PooledDataArray(convert(Array{Bool,1},Dh)))
	sort!(df,cols=[1,2]	)

	return df
end


