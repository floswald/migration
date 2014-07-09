


# mig module plotting functions


function vhplot(m::Model,p::Param,idx)


	# choose a state

	ik   = idx[1]
	iy   = idx[2]
	ip   = idx[3]
	iP   = idx[4]
	iz   = idx[5]
	itau = idx[6]
	ij   = idx[7]
	it   = idx[8]

	subplot(131)
	plot(reshape(m.v[ik,iy,ip,iP,iz,:,1,itau,ij,it],p.na,1))
	plot(reshape(m.v[ik,iy,ip,iP,iz,:,2,itau,ij,it],p.na,1))
	ylabel("v")
	xlabel("a")
	title("value")

	subplot(132)
	plot(reshape(m.s[ik,iy,ip,iP,iz,:,1,itau,ij,it],p.na,1))
	plot(reshape(m.s[ik,iy,ip,iP,iz,:,2,itau,ij,it],p.na,1))
	title("savings")
	
	subplot(133)
	plot(reshape(m.c[ik,iy,ip,iP,iz,:,1,itau,ij,it],p.na,1))
	plot(reshape(m.c[ik,iy,ip,iP,iz,:,2,itau,ij,it],p.na,1))
	title("consumption")

end

function vplot(m::Model,p::Param)


	# choose a random state

	ik   = rand(1:p.nJ)
	ih   = rand(1:p.nh)
	iy   = rand(1:p.ny)
	ip   = rand(1:p.np)
	iP   = rand(1:p.nP)
	itau = rand(1:p.ntau)
	ij   = rand(1:p.nJ)
	it   = rand(1:(p.nt-1))

	subplot(231,projection="3d")
	mesh(reshape(m.v[ik,iy,ip,iP,:,:,ih,itau,ij,it],p.nz,p.na))
	xlabel("z")
	ylabel("a")
	zlabel("value")
	title("value")

	subplot(232,projection="3d")
	mesh(reshape(m.s[ik,iy,ip,iP,:,:,ih,itau,ij,it],p.nz,p.na))
	xlabel("z")
	ylabel("a")
	zlabel("save index")
	title("savings")

	subplot(233,projection="3d")
	mesh(reshape(m.c[ik,iy,ip,iP,:,:,ih,itau,ij,it],p.nz,p.na))
	xlabel("x")
	ylabel("a")
	zlabel("consumption")
	title("consumption")

	subplot(234)
	mesh(reshape(m.rho[ik,iy,ip,iP,:,:,ih,itau,ij,it],p.nz,p.na))
	xlabel("x")
	ylabel("a")
	zlabel("prob of moving")
	title("prob of moving j=>k:\n$ij => $ik")

	subplot(235,projection="3d")
	# contour(1:p.na,1:p.nz,reshape(m.dh[iy,ip,iP,:,:,ih,itau,ij,it],p.nz,p.na),	3)
	mesh(reshape(m.dh[iy,ip,iP,:,:,ih,itau,ij,it],p.nz,p.na))
	xlabel("x")
	ylabel("a")
	zlabel("housing choice")
	title("housing choice")

	suptitle("index(ik,ih,iy,ip,iP,itau,ij,it)=($ik,$ih,$iy,$ip,$iP,$itau,$ij,$it)")

end




