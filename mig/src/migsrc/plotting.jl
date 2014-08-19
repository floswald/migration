


# mig module plotting functions

using PyPlot


function vhplot(m::Model,p::Param,idx)

	# plotting against dimension "a",
	# which is slot 8 in vh

	# choose a state

	ik   = idx[1]
	is   = idx[2]
	iz   = idx[3]
	iy   = idx[4]
	ip   = idx[5]
	itau = idx[6]
	ij   = idx[7]
	it   = idx[8]


	a  = m.grids["assets"]
	a11 = copy(m.grids["assets"])
	a12 = copy(m.grids["assets"])
	a21 = copy(m.grids["assets"])
	a22 = copy(m.grids["assets"])

	# current renter
	vh11   = m.vh  [1,ik,is,iz,iy,ip,itau,:,1,ij,it][:]
	vh12   = m.vh  [2,ik,is,iz,iy,ip,itau,:,1,ij,it][:]
	sh11   = m.sh  [1,ik,is,iz,iy,ip,itau,:,1,ij,it][:]
	sh12   = m.sh  [2,ik,is,iz,iy,ip,itau,:,1,ij,it][:]
	ch11   = m.ch  [1,ik,is,iz,iy,ip,itau,:,1,ij,it][:]
	ch12   = m.ch  [2,ik,is,iz,iy,ip,itau,:,1,ij,it][:]
	cash11 = m.cash[1,ik,is,iz,iy,ip,itau,:,1,ij,it][:]
	cash12 = m.cash[2,ik,is,iz,iy,ip,itau,:,1,ij,it][:]
	# current owner
	vh21   = m.vh  [1,ik,is,iz,iy,ip,itau,:,2,ij,it][:]
	vh22   = m.vh  [2,ik,is,iz,iy,ip,itau,:,2,ij,it][:]
	ch21   = m.ch  [1,ik,is,iz,iy,ip,itau,:,2,ij,it][:]
	ch22   = m.ch  [2,ik,is,iz,iy,ip,itau,:,2,ij,it][:]
	sh21   = m.sh  [1,ik,is,iz,iy,ip,itau,:,2,ij,it][:]
	sh22   = m.sh  [2,ik,is,iz,iy,ip,itau,:,2,ij,it][:]
	cash21 = m.cash[1,ik,is,iz,iy,ip,itau,:,2,ij,it][:]
	cash22 = m.cash[2,ik,is,iz,iy,ip,itau,:,2,ij,it][:]

	# envelopes, i.e. v = max(vh[1],vh[2])
	v1 = m.v    [ik,is,iz,iy,ip,itau,:,1,ij,it][:]
	v2 = m.v    [ik,is,iz,iy,ip,itau,:,2,ij,it][:]
	h1 = m.dh   [ik,is,iz,iy,ip,itau,:,1,ij,it][:]
	h2 = m.dh   [ik,is,iz,iy,ip,itau,:,2,ij,it][:]
	rho1 = m.rho[ik,is,iz,iy,ip,itau,:,1,ij,it][:]
	rho2 = m.rho[ik,is,iz,iy,ip,itau,:,2,ij,it][:]

	# get infeasible regions
	i11 = vh11.==p.myNA
	i12 = vh12.==p.myNA
	i21 = vh21.==p.myNA
	i22 = vh22.==p.myNA

	# set infeasi to NaN
	vh11[i11]   = NaN
	vh12[i12]   = NaN
	sh11[i11]   = NaN
	sh12[i12]   = NaN
	ch11[i11]   = NaN
	ch12[i12]   = NaN
	cash11[i11] = NaN
	cash11[i11] = NaN
	a11[i11]    = NaN
	a12[i12]    = NaN
	v1[v1.==p.myNA] = NaN
	v2[v2.==p.myNA] = NaN
	a1 = a[v1.!=p.myNA]
	a2 = a[v2.!=p.myNA]


	vh21[i21]   = NaN
	vh22[i22]   = NaN
	sh21[i21]   = NaN
	sh22[i22]   = NaN
	ch21[i21]   = NaN
	ch22[i22]   = NaN
	cash21[i21] = NaN
	cash22[i22] = NaN
	a21[i21]    = NaN
	a22[i22]    = NaN

	xpad = 10
	ypad = 0.5

	vscale = (minimum([vh11,vh12,vh21,vh22]) - ypad, maximum([vh11,vh12,vh21,vh22]) + ypad)
	xscale = (minimum([a11,a12,a21,a22]) - xpad, maximum([a11,a12,a21,a22]) + xpad)

	figure()

	subplot(331)
	ylim( vscale )
	xlim( xscale )
	p_v1, = plot(a11,vh11,"o-")
	p_v2, = plot(a21,vh21,"o-")
	ylabel("v_h_0")
	grid()
	title("value h(t+1)=0")
	legend((p_v1,p_v2),("renter","owner"),"lower right",borderpad=0.2,labelspacing=0.1,handlelength=0.7)

	subplot(332)
	ylim( ( minimum([sh11,sh21,0.0]) -xpad , maximum([sh11,sh21,ch11,ch21]) + xpad ) )
	xlim( xscale )
	plot(a11,sh11,"o-")
	plot(a21,sh21,"o-")
	grid()
	title("savings h(t+1)=0")
	
	subplot(333)
	ylim( ( minimum([sh11,sh21,0.0])- xpad, maximum([sh11,sh21,ch11,ch21]) + xpad) )
	xlim( xscale )
	plot(a11,ch11,"o-")
	plot(a21,ch21,"o-")
	grid()
	title("consumption h(t+1)=0")

	subplot(334)
	ylim( vscale )
	xlim( xscale )
	plot(a12,vh12,"o-")
	plot(a22,vh22,"o-")
	grid()
	ylabel("v_h_1")
	title("value h(t+1)=1")

	subplot(335)
	ylim( ( minimum([sh12,sh22,0.0])- xpad, maximum([sh12,sh22,ch12,ch22]) + xpad) )
	xlim( xscale )
	plot(a12,sh12,"o-")
	plot(a22,sh22,"o-")
	grid()
	title("savings h(t+1)=1")
	
	subplot(336)
	ylim( ( minimum([sh12,sh22,0.0])- xpad, maximum([sh12,sh22,ch12,ch22]) + xpad) )
	xlim( xscale )
	plot(a12,ch12,"o-")
	plot(a22,ch22,"o-")
	grid()
	title("consumption h(t+1)=1")

	subplot(337)
	xlim( xscale )
	ylim( vscale )
	plot(a1,v1,"o-")
	plot(a2,v2,"o-")
	grid()
	title("v = max(v0,v1)")
	xlabel("assets")


	subplot(338)
	ylim( (-0.2,1.1) )
	xlim( xscale )
	plot(a1,h1[v1.!=p.myNA]-0.1,"o-")
	plot(a2,h2[v2.!=p.myNA],"o-")
	title("housing choice")
	xlabel("assets")

	subplot(339)
	xlim( xscale )
	ylim( (-0.2,1.1) )
	plot(a1,rho1[v1.!=p.myNA],"o-")
	plot(a2,rho2[v2.!=p.myNA],"o-")
	title("probability of moving")
	xlabel("assets")
	suptitle("Model at ik=$ik,is=$is,iz=$iz,iy=$iy,ip=$ip,itau=$itau,ij=$ij,age=$it")

end

function vplot(m::Model,p::Param)


	# choose a random state

	ik   = rand(1:p.nJ)
	ih   = rand(1:p.nh)
	is   = rand(1:p.ns)
	iy   = rand(1:p.ny)
	ip   = rand(1:p.np)
	itau = rand(1:p.ntau)
	ij   = rand(1:p.nJ)
	it   = rand(1:(p.nt-1))

	subplot(231,projection="3d")
	mesh(reshape(m.v[ik,is,:,iy,ip,itau,:,ih,ij,it],p.nz,p.na))
	xlabel("z")
	ylabel("a")
	zlabel("value")
	title("value")

	subplot(232,projection="3d")
	mesh(reshape(m.sh[1,ik,is,:,iy,ip,itau,:,ih,ij,it],p.nz,p.na))
	xlabel("z")
	ylabel("a")
	zlabel("save index")
	title("savings for hh=1")

	subplot(233,projection="3d")
	mesh(reshape(m.ch[1,ik,is,:,iy,ip,itau,:,ih,ij,it],p.nz,p.na))
	xlabel("x")
	ylabel("a")
	zlabel("consumption")
	title("consumption for hh=1")

	subplot(234,projection="3d")
	mesh(reshape(m.rho[ik,is,:,iy,ip,itau,:,ih,ij,it],p.nz,p.na))
	xlabel("x")
	ylabel("a")
	zlabel("prob of moving")
	title("prob of moving j=>k:\n$ij => $ik")
	println(reshape(m.rho[ik,is,:,iy,ip,itau,:,ih,ij,it],p.nz,p.na))

	subplot(235,projection="3d")
	# contour(1:p.na,1:p.nz,reshape(m.dh[iy,:,:,ih,itau,ij,it],p.nz,p.na),	3)
	mesh(reshape(m.dh[ik,:,iy,is,ip,itau,:,ih,ij,it],p.nz,p.na))
	xlabel("x")
	ylabel("a")
	zlabel("housing choice")
	title("housing choice")

	suptitle("model at index(ik,is,iy,ip,itau,ij,it)=($ik,$is,$iy,$ip,$ih,$itau,$ij,$it)")

end



# plot simulation histories

function simplot(sim::DataFrame,n::Int)

	# choose n random individs
	nr = rand(1:maximum(sim[:id]),n)

	sim  = sim[findin(sim[:id],nr),:] 

	# fig = PyPlot.figure()

	gdf = groupby(sim,:id)

	subplot(4,4,1)
	ylim((0.0,maximum(sim[:income])+5))
	for sdf in gdf
		plot(sdf[:age],sdf[:income])
	end
	title("income")

	subplot(4,4,2)
	ylim((0.0,maximum(sim[:income])+5))
	for sdf in gdf
		plot(sdf[:age],sdf[:c])
	end
	title("cons")

	subplot(4,4,3)
	for sdf in gdf
		plot(sdf[:age],sdf[:a])
	end
	title("assets")

	subplot(4,4,4)
	for sdf in gdf
		plot(sdf[:age],sdf[:save])
	end
	title("save")

	subplot(4,4,5)
	ylim((-0.1,1.1))
	for sdf in gdf
		plot(sdf[:age],sdf[:h])
	end
	title("own")

	subplot(4,4,6)
	for sdf in gdf
		plot(sdf[:age],sdf[:wealth])
	end
	title("wealth")

	subplot(4,4,7)
	ylim((0.5,9.5))
	for sdf in gdf
		plot(sdf[:age],sdf[:j])
	end
	title("current location")

	subplot(4,4,8)
	for sdf in gdf
		plot(sdf[:age],sdf[:y])
	end
	title("region income")

	subplot(4,4,9)
	for sdf in gdf
		plot(sdf[:age],sdf[:p])
	end
	title("region price")

	subplot(4,4,10)
	for sdf in gdf
		plot(sdf[:age],sdf[:v])
	end
	title("lifetime utility(v)")

	subplot(4,4,11)
	ylim((-0.1,1.1))
	for sdf in gdf
		plot(sdf[:age],sdf[:kids])
	end
	title("kids")

	subplot(4,4,12)
	ylim((0.5,9.5))
	for sdf in gdf
		plot(sdf[:age],sdf[:moveto])
	end
	title("moveto")

	subplot(4,4,13)
	ylim((-0.1,1.1))
	for sdf in gdf
		plot(sdf[:age],sdf[:hh])
	end
	title("hchoice")

	plts = Any[]
	ids = Int[]
	idcount = 1
	subplot(4,4,14)
	ylim((0.5,n + 0.5))
	for sdf in gdf
		plt, = plot(sdf[:age],ones(length(sdf[:age])).*idcount)
		push!(plts,plt)
		push!(ids,sdf[1,:id])
		idcount += 1
	end
	legend(plts,["indiv $(ids[i])" for i=1:n],"lower right")
	title("legend")
	suptitle("ids: $nr")
end


function vhplot(m::Model2,p::Param,idx)


	# choose a state

	iz   = idx[1]
	it   = idx[2]

	a = m.grids["assets"]
	v1 = m.v[iz,:,1,it][:]
	v2 = m.v[iz,:,2,it][:]

	v1[v1.==p.myNA] = minimum(v1[v1.>p.myNA])
	v2[v2.==p.myNA] = minimum(v2[v2.>p.myNA])

	s1 = m.s[iz,:,1,it][:]
	s2 = m.s[iz,:,2,it][:]
	# s1[s1.==p.myNA] = minimum(s1[s1.>p.myNA])
	# s2[s2.==p.myNA] = minimum(s2[s2.>p.myNA])

	figure()
	subplot(141)
	plot(a,v1)
	plot(a,v2)
	ylabel("v")
	xlabel("a")
	title("value")

	subplot(142)
	plot(a,s1)
	plot(a,s2)
	title("savings")
	
	subplot(143)
	plot(a,reshape(m.c[iz,:,1,it],p.na,1))
	plot(a,reshape(m.c[iz,:,2,it],p.na,1))
	title("consumption")


	subplot(144)
	ylim((-0.1,1.1))
	plot(a,reshape(m.dh[iz,:,1,it],p.na,1))
	plot(a,reshape(m.dh[iz,:,2,it],p.na,1))
	title("housing choice")

end


# plot simulation histories

function simplot2(sim::DataFrame,n::Int)

	# choose n random individs
	nr = rand(1:maximum(sim[:id]),n)

	sim  = sim[findin(sim[:id],nr),:] 

	# fig = PyPlot.figure()

	gdf = groupby(sim,:id)

	subplot(3,3,1)
		for sdf in gdf
			plot(sdf[:age],sdf[:iz])
		end
	title("iz")
	subplot(3,3,2)
		for sdf in gdf
			plot(sdf[:age],sdf[:z])
		end
	title("z")
	subplot(3,3,3)
		for sdf in gdf
			plot(sdf[:age],sdf[:c])
		end
	title("cons")
	subplot(3,3,4)
		for sdf in gdf
			plot(sdf[:age],sdf[:a])
		end
	title("assets")
	subplot(3,3,5)
		for sdf in gdf
			plot(sdf[:age],sdf[:h])
		end
	title("own")
	subplot(3,3,6)
		for sdf in gdf
			plot(sdf[:age],sdf[:wealth])
		end
	title("wealth")
	subplot(3,3,7)
		for sdf in gdf
			plot(sdf[:age],sdf[:v])
		end
	title("lifetime utility(v)")
	subplot(3,3,8)
		for sdf in gdf
			plot(sdf[:age],sdf[:save])
		end
	title("save")
	subplot(3,3,9)
		for sdf in gdf
			plot(sdf[:age],sdf[:hh])
		end
	title("hchoice")
	suptitle("ids: $nr")
end






