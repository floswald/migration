


# module param


import Base.show

export Param

# define the struct

type Param
	# utility parameters
	beta  ::Float64
	gamma ::Float64

	# numerical setup

	na   ::Int 	# number of asset points
	nz   ::Int 	# number of inidividual income states
	nt   ::Int 	# number of periods
	npsi ::Int 	# number of types
	nP   ::Int 	# aggregate price levels
	nY   ::Int 	# aggregate income levels
	nJ   ::Int 	# number of origin locations
	            # nk = nj - 1 is the number of destination locations
	np   ::Int # number of price levels in each location
	ny   ::Int # number of income levels by location

	dimvec ::(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) # total number of dimensions
	dimvec2::(Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) # total number of dimensions



	# constructor assigning initial values
	function Param()

		beta  = 0.95
		gamma = 2

		na    = 10
		nz    = 4
		nt    = 10
		npsi  = 2
		nP    = 2
		nY    = 2
		nJ    = 9
		np    = 4
		ny    = 4

		dimvec = (nt, na, nz, npsi, nP, nY, nJ ,np ,ny, np, ny , (nJ-1))
		dimvec2 = (nt, na, nz, npsi, nP, nY, nJ ,np ,ny, np, ny )

		# create object

		return new(beta,gamma,na,nz,nt,npsi,nP,nY,nJ,np,ny,dimvec,dimvec2)
	end

	
end


# define member functions

function nPoints(p::Param)
	r = p.na * p.nz * p.nt * p.npsi * p.nP * p.nY * p.nJ * (p.nJ - 1) * p.np^2 * p.ny^2
	return r
end
# show(io::IO, p::Param) = print(io,"number of points=$(p.na*p.nz*p.nt)")
show(io::IO, p::Param) = print(io,"number of points:$(nPoints(p))\nnumber of dims  :$(length(p.dimvec))")




