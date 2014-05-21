


function drawMarkov(G::Array{Float64,2},init::Array{Float64,1},N::Int,T::Int)

	out = zeros(Int,N,T)
	eps = rand(N,T)

	n = size(G,1)

	if length(init) != n
		error("length(init) must be size(G,1)")
	end

	g = Categorical(init)

	Gs = mapslices(cumsum,G,2)

	z = zeros(n)

	# initiate everybody on a random state
	out[:,1] = rand(g,N)

	for i = 1:N
		for t = 2:T

			z = Gs[ out[i,t-1], : ]

			out[i,t] = findfirst(z.>eps[i,t])

		end
	end
	out
end









# simulator

function simulate(m::Model,p::Param)

	T = p.nt-1

	# drawing the shocks 
	# ==================


	# setup random shocks


	# price/income histories
	# at macro level
	Hy = [drawMarkov(m.gridsXD["Gy"][:,:,i],Float64[1/p.ny for i=1:p.ny],1,T) for i=1:p.nJ]
	Hp = [drawMarkov(m.gridsXD["Gp"][:,:,i],Float64[1/p.np for i=1:p.np],1,T) for i=1:p.nJ]
	HP = drawMarkov(m.grids2D["GP"],Float64[1/p.nP for i=1:p.nP],1,T)

	# at individual level
	Rz = rand(m.nsim*(T))	# income shock
	Rj = rand(m.nsim*(T))	# location preference shock

	# you are of fixed type Rt:
	Gt = Categorical(p.taudist)	# type distribution
	Rt = rand(Gt,N)				# 


	# you are born in location Rj:
	# Gj = Categorical(p.popdist)
	# Rj0 = Categorical(p.popdist)


	# storage
	DT = zeros(Int,m.nsim*(T))	# age
	DI = zeros(Int,m.nsim*(T))	# identity
	DV = zeros(m.nsim*(T))	# value
	DC = zeros(m.nsim*(T))	# consu
	# iS = zeros(Int,m.nsim*(T))	# savings index
	DS = zeros(m.nsim*(T))	# saving
	DJ = zeros(Int,m.nsim*(T))	# location
	DH = zeros(Int,m.nsim*(T))	# housing
	DM = zeros(Bool,m.nsim*(T))	# move

	# current index
	jidx = 0

	# tmp vars
	tmpM = false	# move indidicator

	# draw aggregate uncertainty P and store


	for i = 1:p.nsim

		iy=0
		ip=0
		iP=0
		iz=0
		ia=0
		ih=0
		itau=Rt[i]
		ij=Rj0[i]
		age=0


		# draw initial state:
		# you need to know
		# (iy,ip,iP,iz,ia,ih=0,itau,Rj0,age=1)
	
		for age = 1:(T)

			# obtain current index
			jidx = idx9(iy,ip,iP,iz,ia,ih+1,itau,ij,age,p)

			# move?
			DM[age + T*(i-1)] = (Rj[age + T*(i-1)] < m.rho[jidx])




			# get new price/income
			# in each region!


		end

	end





end





























