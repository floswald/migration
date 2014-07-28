

module test_approx


using mig, FactCheck





facts("testing coefficient estimation on random data") do

	d = Dict{Int,Array{Float64,2}}()
	for i=1:3 
		d[i] = rand(i+1,i+1) 
	end
	 n1 = 2
	 n2 = 3
	 n3 = 4
	 # function values
	 y = zeros(n1*n2*n3)
	 for i=1:n1
	 	for j =1:n2
	 		for k=1:n3
	 			y[i + n1*(j-1 + n2*(k-1))] = d[1][i,1] + d[2][j,1] + d[3][k,1] + d[1][i,1]*d[2][j,1]* d[3][k,1]
	 			# println("index (i,j,k)=($i,$j,$k) maps to position $(i + n1*(j-1 + n2*(k-1)))")
	 		end
	 	end
	 end


	 ## build tensor product matrix
	 krons = kron(d[3],kron(d[2],d[1]))

	 ## notice: in kron the indexing corresponds to this:
	 ## uncomment to see
	 ##expand.grid( fastest=1:nrow(m1), middle=1:nrow(m2), slowest=1:nrow(m3) )

	 # check for equality
	 res1 = krons * y
	 myres = mig.getTensorCoef(d,y)

	 @fact sumabs(res1 - myres) => roughly(0.0,atol=0.00001)

end




facts("testing computation of coefficients") do
	
	# 3D approximation example

	# bounds
	lb = [-1.1,2.5,0.1]
	ub = [3.2,5.4,9.1]

	# number of eval points and basis functions:
	# we require square basis matrices!
	npoints = [10,6,7]

	# number of basis funcs
	nbasis = npoints

	# splien degrees
	degs = [3,2,1]

	# implies a number of knots for each spline
	nknots = {i => nbasis[i] - degs[i] + 1 for i=1:3}

	# eval points
	points = {i => linspace(lb[i],ub[i],npoints[i]) for i=1:3}

	# set up BSplines
	bsp = {i => mig.BSpline(nknots[i],degs[i],lb[i],ub[i]) for i=1:3}

	# set of basis functions
	d = Dict{Int,Array{Float64,2}}()
	for i=1:3
		d[i] = full(mig.getBasis(points[i],bsp[i]))
	end

	# set of INVERSE basis functions
	id = Dict{Int,Array{Float64,2}}()
	for k in collect(keys(d))
		id[k] = inv(d[k])
	end

	#  get a function
	function f(x,y,z) 
		(x+y).^2 + z.^z 
	end

	# compute function values so that
	# k is fastest varying index
	y = Float64[f(i,j,k) for k in points[3] , j in points[2], i in points[1]]

	yvec = y[:]

	# compute coefs by constructing tensor product
	# where note the order is important!
	 ikrons = kron(id[3],kron(id[2],id[1]))
	 coef1 = ikrons * yvec

	 # get coefs using the function
	 mycoef = mig.getTensorCoef(id,yvec)

	 # testing tolerance
	 tol = 3e-6

	 # check coefs are the same
	@fact maxabs(coef1 - mycoef) => roughly(0.0,atol=tol)

	# approximate the function values on the original grid
	# by using the basis in d. this is just reverse of getting coefs!
	pred = mig.getTensorCoef(d,mycoef)
	@fact maxabs(pred - yvec) => roughly(0.0,atol=tol)

	t1 = reshape(pred,7,6,10)
	@fact maxabs(t1 .- y) => roughly(0.0,atol=tol)

	# is that really doing what you want?
	# you want B * coefs
	B = kron(d[3],kron(d[2],d[1]))
	pred2 = B * mycoef
	@fact maxabs(pred2 - yvec) => roughly(0.0,atol=tol)

	# predict usign the predict function	
	pred = mig.evalTensor3(d[3],d[2],d[1],mycoef)
	@fact maxabs(pred - yvec) => roughly(0.0,atol=tol)

end



facts("testing 2D tensorProduct evaluating off grid") do
	
	# 2D approximation example

	ndims = 2

	# bounds
	lb = [-2.1,-2.5]
	ub = [2.2,2.6]

	# number of eval points and basis functions:
	# we require square basis matrices!
	npoints = [11,12]

	# number of basis funcs
	nbasis = npoints

	# splien degrees
	degs = [3,3]

	# implies a number of knots for each spline
	nknots = {i => nbasis[i] - degs[i] + 1 for i=1:ndims}

	# eval points
	points = {i => linspace(lb[i],ub[i],npoints[i]) for i=1:ndims}

	# set up BSplines
	bsp = {i => mig.BSpline(nknots[i],degs[i],lb[i],ub[i]) for i=1:ndims}

	# set of basis functions
	d = Dict{Int,Array{Float64,2}}()
	for i=1:ndims
		d[i] = full(mig.getBasis(points[i],bsp[i]))
	end

	# set of INVERSE basis functions
	id = Dict{Int,Array{Float64,2}}()
	for k in collect(keys(d))
		id[k] = inv(d[k])
	end

	#  get a function
	function f(x,y) 
		sin(sqrt(x^2+y^2))
	end

	# compute function values so that
	# k is fastest varying index
	y = Float64[f(i,j) for i in points[1], j in points[2]]

	yvec = y[:]

	# get coefs using the function
	mycoef = mig.getTensorCoef(id,yvec)

	 # testing tolerance
	 tol = 1e-2

	# predict values off grid

	for it in 1:10

		rval1 = lb[1] + rand()
		rval2 = lb[2] + rand()
		b2 = mig.getBasis(rval2,bsp[2])
		b1 = mig.getBasis(rval1,bsp[1])
		@fact mig.evalTensor2(b2,b1,mycoef) => roughly(f(rval1,rval2),atol=tol)

		rval1 = ub[1] - rand()
		rval2 = ub[2] - rand()
		b2 = mig.getBasis(rval2,bsp[2])
		b1 = mig.getBasis(rval1,bsp[1])
		@fact mig.evalTensor2(b2,b1,mycoef) => roughly(f(rval1,rval2),atol=tol)
	end


	 # make a plot
	 # ===========

	new_npoints = [17,16]
	# new_npoints = npoints
	new_points = {i => linspace(lb[i],ub[i],new_npoints[i]) for i=1:ndims}
	# set of new basis functions
	nd = Dict{Int,Array{Float64,2}}()
	for i=1:ndims
		nd[i] = full(mig.getBasis(new_points[i],bsp[i]))
	end

	pred = mig.evalTensor2(nd[2],nd[1],mycoef)
	
	mig.figure()
	mig.subplot(121,projection="3d")
	mig.mesh(points[2],points[1],y)	
	mig.title("true values on grid")

	mig.subplot(122,projection="3d")
	pp = reshape(pred,new_npoints[1],new_npoints[2])
	mig.mesh(new_points[2],new_points[1],pp)	
	mig.title("Approximation off grid")

	mig.suptitle("2D TEST")

end


facts("testing 3D tensorProduct approximations") do
	
	# 3D approximation example

	ndims = 3

	# bounds
	lb = [-2.1,-2.5,0.9]
	ub = [2.2,2.6,5.0]

	# number of eval points and basis functions:
	# we require square basis matrices!
	npoints = [12,16,13]

	# number of basis funcs
	nbasis = npoints

	# splien degrees
	degs = [3,3,3]

	# implies a number of knots for each spline
	nknots = {i => nbasis[i] - degs[i] + 1 for i=1:ndims}

	# eval points
	points = {i => linspace(lb[i],ub[i],npoints[i]) for i=1:ndims}

	# set up BSplines
	bsp = {i => mig.BSpline(nknots[i],degs[i],lb[i],ub[i]) for i=1:ndims}

	# set of basis functions
	d = Dict{Int,Array{Float64,2}}()
	for i=1:ndims
		d[i] = full(mig.getBasis(points[i],bsp[i]))
	end

	# set of INVERSE basis functions
	id = Dict{Int,Array{Float64,2}}()
	for k in collect(keys(d))
		id[k] = inv(d[k])
	end

	#  get a function
	function f(x,y,z) 
		sin(sqrt(x^2+y^2)) - z^2
	end

	# compute function values so that
	# k is fastest varying index
	y = Float64[f(i,j,k) for i in points[1], j in points[2], k in points[3]]

	yvec = y[:]

	# get coefs using the function
	mycoef = mig.getTensorCoef(id,yvec)

	 # testing tolerance
	 tol = 1e-2

	# predict values off grid

	for it in 1:10

		rval1 = lb[1] + rand()
		rval2 = lb[2] + rand()
		rval3 = lb[3] + rand()
		b3 = mig.getBasis(rval3,bsp[3])
		b2 = mig.getBasis(rval2,bsp[2])
		b1 = mig.getBasis(rval1,bsp[1])
		@fact mig.evalTensor3(b3,b2,b1,mycoef) => roughly(f(rval1,rval2,rval3),atol=tol)

		rval1 = ub[1] - rand()
		rval2 = ub[2] - rand()
		rval3 = ub[3] - rand()
		b3 = mig.getBasis(rval3,bsp[3])
		b2 = mig.getBasis(rval2,bsp[2])
		b1 = mig.getBasis(rval1,bsp[1])
		@fact mig.evalTensor3(b3,b2,b1,mycoef) => roughly(f(rval1,rval2,rval3),atol=tol)
	end


	 # make a plot
	 # ===========

	new_npoints = [17,16,5]
	# new_npoints = npoints
	new_points = {i => linspace(lb[i],ub[i],new_npoints[i]) for i=1:ndims}
	# set of new basis functions
	nd = Dict{Int,Array{Float64,2}}()
	for i=1:ndims
		nd[i] = full(mig.getBasis(new_points[i],bsp[i]))
	end

	pred = mig.evalTensor3(nd[3],nd[2],nd[1],mycoef)
	
	mig.figure()
	mig.subplot(221,projection="3d")
	mig.mesh(points[2],points[1],y[:,:,1])	
	mig.title("true values at lowest 3D state")

	mig.subplot(222,projection="3d")
	pp = reshape(pred,new_npoints[1],new_npoints[2],new_npoints[3])
	mig.mesh(new_points[2],new_points[1],pp[:,:,1])	
	mig.title("Approximation")

	mig.subplot(223,projection="3d")
	mig.mesh(points[2],points[1],y[:,:,npoints[3]])	
	mig.title("true values at highest 3D state")

	mig.subplot(224,projection="3d")
	pp = reshape(pred,new_npoints[1],new_npoints[2],new_npoints[3])
	mig.mesh(new_points[2],new_points[1],pp[:,:,new_npoints[3]])	
	mig.title("Approximation")
	mig.suptitle("3D TEST")

end


facts("testing 4D tensorProduct approximations") do
	
	ndims = 4

	# bounds
	lb = [-1.1,-1.5,-0.9,-1.0]
	ub = [1.2,1.6,0.9,1]

	# number of eval points and basis functions:
	# we require square basis matrices!
	npoints = [13,13,13,13]

	# number of basis funcs
	nbasis = npoints

	# splien degrees
	degs = [3,3,3,3]

	# implies a number of knots for each spline
	nknots = {i => nbasis[i] - degs[i] + 1 for i=1:ndims}

	# eval points
	points = {i => linspace(lb[i],ub[i],npoints[i]) for i=1:ndims}

	# set up BSplines
	bsp = {i => mig.BSpline(nknots[i],degs[i],lb[i],ub[i]) for i=1:ndims}

	# set of basis functions
	d = Dict{Int,Array{Float64,2}}()
	for i=1:ndims
		d[i] = full(mig.getBasis(points[i],bsp[i]))
	end

	# set of INVERSE basis functions
	id = Dict{Int,Array{Float64,2}}()
	for k in collect(keys(d))
		id[k] = inv(d[k])
	end

	#  get a function
	function f(x,y,z,w) 
		sin(sqrt(x^2+y^2)) + (z-w)^3
	end

	# compute function values so that
	# k is fastest varying index
	y = Float64[f(i,j,k,w) for i in points[1], j in points[2], k in points[3], w in points[4]]

	yvec = y[:]

	# get coefs using the function
	mycoef = mig.getTensorCoef(id,yvec)

	 # testing tolerance
	 tol = 3e-3

	# predict values off grid

	for it in 1:10

		rval1 = lb[1] + rand()
		rval2 = lb[2] + rand()
		rval3 = lb[3] + rand()
		rval4 = lb[4] + rand()
		b4 = mig.getBasis(rval4,bsp[4])
		b3 = mig.getBasis(rval3,bsp[3])
		b2 = mig.getBasis(rval2,bsp[2])
		b1 = mig.getBasis(rval1,bsp[1])
		@fact mig.evalTensor4(b4,b3,b2,b1,mycoef) => roughly(f(rval1,rval2,rval3,rval4),atol=tol)

		rval1 = ub[1] - rand()
		rval2 = ub[2] - rand()
		rval3 = ub[3] - rand()
		rval4 = ub[4] - rand()
		b4 = mig.getBasis(rval4,bsp[4])
		b3 = mig.getBasis(rval3,bsp[3])
		b2 = mig.getBasis(rval2,bsp[2])
		b1 = mig.getBasis(rval1,bsp[1])
		@fact mig.evalTensor4(b4,b3,b2,b1,mycoef) => roughly(f(rval1,rval2,rval3,rval4),atol=tol)
	end


	 # make a plot
	 # ===========

	new_npoints = [17,16,5,7]
	# new_npoints = npoints
	new_points = {i => linspace(lb[i],ub[i],new_npoints[i]) for i=1:ndims}
	# set of new basis functions
	nd = Dict{Int,Array{Float64,2}}()
	for i=1:ndims
		nd[i] = full(mig.getBasis(new_points[i],bsp[i]))
	end

	# predict everywhere
	pred = mig.evalTensor4(nd[4],nd[3],nd[2],nd[1],mycoef)
	
	mig.figure()
	mig.subplot(221,projection="3d")
	mig.mesh(points[2],points[1],y[:,:,1,1])	
	mig.title("true values at lowest state (:,:,1,1)")

	mig.subplot(222,projection="3d")
	pp = reshape(pred,new_npoints[1],new_npoints[2],new_npoints[3],new_npoints[4])
	mig.mesh(new_points[2],new_points[1],pp[:,:,1,1])	
	mig.title("Approximation")

	mig.subplot(223,projection="3d")
	mig.mesh(points[2],points[1],y[:,:,npoints[3],npoints[4]])	
	mig.title("true values at state (:,:,end,end)")

	mig.subplot(224,projection="3d")
	pp = reshape(pred,new_npoints[1],new_npoints[2],new_npoints[3],new_npoints[4])
	mig.mesh(new_points[2],new_points[1],pp[:,:,new_npoints[3],new_npoints[4]])	
	mig.title("Approximation")
	mig.suptitle("4D TEST")

end



end