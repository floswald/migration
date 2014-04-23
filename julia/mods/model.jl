

# setting up a model



type Model 

	# values
	# dimensions: a,z,psi,t,P,Y,j,k,pj,pk,yj,yk
	vstay :: Array{Float64,12}
	vsell :: Array{Float64,12}
	vbuy  :: Array{Float64,12}
	vrent :: Array{Float64,12}

	Vown  :: Array{Float64,11}
	Vrent :: Array{Float64,11}


	function Model(p::Param)

		vstay = fill(0.0,p.dimvec)
		vsell = fill(0.0,p.dimvec)
		vbuy  = fill(0.0,p.dimvec)
		vrent = fill(0.0,p.dimvec)

		Vown  = fill(0.0,p.dimvec2)
		Vrent = fill(0.0,p.dimvec2)

		return new(vstay,vsell,vbuy,vrent,Vown,Vrent)

	end



end


function show(io::IO, M::Model)
	r = round( (sizeof(M.vstay)+sizeof(M.vsell)+sizeof(M.vbuy)+sizeof(M.vrent)+sizeof(M.Vown)+sizeof(M.Vrent))/1.074e+9, 3)
	print(io, "size of M in Gb: $(r)")
end