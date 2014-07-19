


# mig module plotting functions


function vhplot(m::Model,p::Param,idx)


	# choose a state

	ik   = idx[1]
	is   = idx[2]
	iy   = idx[3]
	ip   = idx[4]
	iz   = idx[5]
	itau = idx[6]
	ij   = idx[7]
	it   = idx[8]

	a  = m.grids["assets"]
	a1 = copy(m.grids["assets"])
	a2 = copy(m.grids["assets"])
	v1 = m.v[ik,is,iy,ip,iz,:,1,itau,ij,it][:]
	v2 = m.v[ik,is,iy,ip,iz,:,2,itau,ij,it][:]
	s1 = m.s[ik,is,iy,ip,iz,:,1,itau,ij,it][:]
	s2 = m.s[ik,is,iy,ip,iz,:,2,itau,ij,it][:]
	c1 = m.c[ik,is,iy,ip,iz,:,1,itau,ij,it][:]
	c2 = m.c[ik,is,iy,ip,iz,:,2,itau,ij,it][:]
	h1 = m.dh[ik,is,iy,ip,iz,:,1,itau,ij,it][:]
	h2 = m.dh[ik,is,iy,ip,iz,:,2,itau,ij,it][:]

	# cash for choosign 0
	cash0_0 = m.cash0[ik,is,iy,ip,iz,:,1,itau,ij,it][:]
	cash0_1 = m.cash0[ik,is,iy,ip,iz,:,2,itau,ij,it][:]
	# cash for choosign 1
	cash1_0 = m.cash1[ik,is,iy,ip,iz,:,1,itau,ij,it][:]
	cash1_1 = m.cash1[ik,is,iy,ip,iz,:,2,itau,ij,it][:]


	# get infeasible region
	i1 = v1.==p.myNA
	i2 = v2.==p.myNA

	v1[i1] = NaN
	v2[i2] = NaN
	s1[i1] = NaN
	s2[i2] = NaN
	c1[i1] = NaN
	c2[i2] = NaN
	a1[i1] = NaN
	a2[i2] = NaN

	figure()

	subplot(231)
	p_v1, = plot(a1,v1,"o-")
	p_v2, = plot(a2,v2,"o-")
	ylabel("v")
	xlabel("a")
	title("value")
	legend((p_v1,p_v2),("renter","owner"),"lower right")

	subplot(232)
	ylim( ( minimum([s1,s2,0.0]), maximum([s1,s2,c1,c2]) ) )
	p_s1 = plot(a1,s1,"o-")
	p_s2 = plot(a2,s2,"o-")
	grid()
	title("savings")
	
	subplot(233)
	ylim( ( minimum([s1,s2,0.0]), maximum([s1,s2,c1,c2]) ) )
	p_c1 = plot(a1,c1,"o-")
	p_c2 = plot(a2,c2,"o-")
	grid()
	title("consumption")


	subplot(234)
	ylim( (-0.1,1.1) )
	p_h1 = plot(a[!i1],h1[!i1],"o-")
	p_h2 = plot(a[!i2],h2[!i2],"o-")
	title("housing choice")

	subplot(235)
	ylim( ( minimum(cash1_1[!i2]), maximum(cash0_1[!i2]) ) )
	p_ca01 = plot(a[!i1],cash0_0[!i1],"o-")
	p_ca02 = plot(a[!i2],cash0_1[!i2],"o-")
	grid()
	title("cash when choosing h(t+1)=0")

	subplot(236)
	ylim( ( minimum(cash1_1[!i2]), maximum(cash0_1[!i2]) ) )
	p_ca11 = plot(a[!i1],cash1_0[!i1],"o-")
	p_ca12 = plot(a[!i2],cash1_1[!i2],"o-")
	grid()
	title("cash when choosing h(t+1)=1")
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
	mesh(reshape(m.v[ik,is,iy,ip,:,:,ih,itau,ij,it],p.nz,p.na))
	xlabel("z")
	ylabel("a")
	zlabel("value")
	title("value")

	subplot(232,projection="3d")
	mesh(reshape(m.s[ik,is,iy,ip,:,:,ih,itau,ij,it],p.nz,p.na))
	xlabel("z")
	ylabel("a")
	zlabel("save index")
	title("savings")

	subplot(233,projection="3d")
	mesh(reshape(m.c[ik,is,iy,ip,:,:,ih,itau,ij,it],p.nz,p.na))
	xlabel("x")
	ylabel("a")
	zlabel("consumption")
	title("consumption")

	subplot(234)
	mesh(reshape(m.rho[ik,is,iy,ip,:,:,ih,itau,ij,it],p.nz,p.na))
	xlabel("x")
	ylabel("a")
	zlabel("prob of moving")
	title("prob of moving j=>k:\n$ij => $ik")
	println(reshape(m.rho[ik,is,iy,ip,:,:,ih,itau,ij,it],p.nz,p.na))

	subplot(235,projection="3d")
	# contour(1:p.na,1:p.nz,reshape(m.dh[iy,:,:,ih,itau,ij,it],p.nz,p.na),	3)
	mesh(reshape(m.dh[ik,iy,is,ip,:,:,ih,itau,ij,it],p.nz,p.na))
	xlabel("x")
	ylabel("a")
	zlabel("housing choice")
	title("housing choice")

	suptitle("index(ik,ih,iy,ip,itau,ij,it)=($ik,$ih,$iy,$ip,$itau,$ij,$it)")

end



# plot simulation histories

function simplot(sim::DataFrame,n::Int)

	# choose n random individs
	nr = rand(1:maximum(sim[:id]),n)

	sim  = sim[findin(sim[:id],nr),:] 

	# fig = PyPlot.figure()

	gdf = groupby(sim,:id)

	subplot(4,4,1)
		for sdf in gdf
			plot(sdf[:age],sdf[:income])
		end
	title("income")
	subplot(4,4,2)
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
			plot(sdf[:age],sdf[:h])
		end
	title("own")
	subplot(4,4,5)
		for sdf in gdf
			plot(sdf[:age],sdf[:wealth])
		end
	title("wealth")
	subplot(4,4,6)
		for sdf in gdf
			plot(sdf[:age],sdf[:j])
		end
	title("location")
	subplot(4,4,7)
		for sdf in gdf
			plot(sdf[:age],sdf[:y])
		end
	title("region income")
	subplot(4,4,8)
		for sdf in gdf
			plot(sdf[:age],sdf[:p])
		end
	title("region price")
	subplot(4,4,9)
		for sdf in gdf
			plot(sdf[:age],sdf[:v])
		end
	title("lifetime utility(v)")
	subplot(4,4,10)
		for sdf in gdf
			plot(sdf[:age],sdf[:kids])
		end
	title("kids")
	subplot(4,4,11)
		for sdf in gdf
			plot(sdf[:age],sdf[:save])
		end
	title("save")
	subplot(4,4,12)
		for sdf in gdf
			plot(sdf[:age],sdf[:moveto])
		end
	title("moveto")
	subplot(4,4,13)
		for sdf in gdf
			plot(sdf[:age],sdf[:hh])
		end
	title("hchoice")
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






