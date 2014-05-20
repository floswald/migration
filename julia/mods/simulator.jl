


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

	# drawing the shocks 
	# ==================

	# the distributions you draw from
	# depend on your current location
	# so cannot precompute the histories
	# only the uniform shocks

	# setup random shocks
	# at macro level
	Ry = rand((p.nt-1)*p.nJ)	# regional income deviations
	Rp = rand((p.nt-1)*p.nJ)	# regional price deviations
	RP = rand(p.nt-1)		# aggregate price

	# at individual level
	Rz = rand(m.nsim*(p.nt-1))	# income shock
	Rj = rand(m.nsim*(p.nt-1))	# location preference shock

	Gt = Categorical(p.taudist)	# type distribution
	Rt = rand(Gt,N)				# 

	# draw 



	# storage
	V = zeros(m.nsim*(p.nt-1))	# value
	C = zeros(m.nsim*(p.nt-1))	# consu
	# iS = zeros(Int,m.nsim*(p.nt-1))	# savings index
	S = zeros(m.nsim*(p.nt-1))	# saving
	J = zeros(m.nsim*(p.nt-1))	# location
	H = zeros(m.nsim*(p.nt-1))	# housing



	jidx = idx9(iy,ip,iP,iz,ia,ih+1,itau,ij,age,p)


end
