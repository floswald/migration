

module test_approx


using mig, FactCheck


facts("testing tensorProduct function on random data") do

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

facts("testing tensorProduct evaluating on grid") do

	# dims
	d1 = linspace(0.1,5,10)
	d2 = linspace(-2.1,4,6)
	d3 = linspace(1.2,5.4,7)

	# set of basis functions
	d = Dict{Int,Array{Float64,2}}()
	d[1] = mig.quadraticSplineBFE(d1,(d1[1],d1[end]),length(d1)-1)
	d[2] = mig.quadraticSplineBFE(d2,(d2[1],d2[end]),length(d2)-1)
	d[3] = mig.quadraticSplineBFE(d3,(d3[1],d3[end]),length(d3)-1)

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
	y = Float64[f(i,j,k) for k in d3 , j in d2, i in d1]

	yvec = y[:]

	# compute coefs by constructing tensor product
	# where note the order is important!
	 ikrons = kron(id[3],kron(id[2],id[1]))
	 coef1 = ikrons * yvec

	 # get coefs using the function
	 mycoef = mig.getTensorCoef(id,yvec)

	 # check coefs are the same
	@fact sumabs(coef1 - mycoef) => roughly(0.0,atol=0.00001)

	# approximate the function values on the original grid
	# by using the basis in d. this is just reverse of getting coefs!
	pred = mig.getTensorCoef(d,mycoef)
	@fact sumabs(pred - yvec) => roughly(0.0,atol=0.00001)

	t1 = reshape(pred,7,6,10)
	@fact sumabs(t1 .- y) => roughly(0.0,atol=10e-5)

	# is that really doing what you want?
	# you want B * coefs
	B = kron(d[3],kron(d[2],d[1]))
	pred2 = B * mycoef
	@fact sumabs(pred2 - yvec) => roughly(0.0,atol=0.00001)

	# predict usign the predict function	
	pred = mig.evalTensor3(d[3],d[2],d[1],mycoef)
	@fact sumabs(pred - yvec) => roughly(0.0,atol=0.00001)



	# 

end

facts("test evaluating off grid") do

	m1 = rand(3,5)
	m2 = rand(3,4)

	co = [1.0:20]
	truth = kron(m1,m2) * co

	# try to get that with evalTensor2
	myres = mig.evalTensor2(m1,m2,co)

	@fact sumabs(truth - myres) => roughly(0.0,atol=0.000001)

	
end


facts("visual test of approximation") do

	# dims
	d1 = linspace(0.1,1,10)
	d2 = linspace(-2.1,4,10)

	# set of basis functions
	d = Dict{Int,Array{Float64,2}}()
	d[1] = mig.quadraticSplineBFE(d1,(d1[1],d1[end]),length(d1)-1)
	d[2] = mig.quadraticSplineBFE(d2,(d2[1],d2[end]),length(d2)-1)

	# set of INVERSE basis functions
	id = Dict{Int,Array{Float64,2}}()
	for k in collect(keys(d))
		id[k] = inv(d[k])
	end

	#  get a function
	function f(x,y) 
		(x+y).^2 
	end

	# (7,6,10)
	# (k,j,i)
	y = Float64[f(i,j) for j in d2, i in d1 ]

	yvec = y[:]

	# get coefs using the function
	 mycoef = mig.getTensorCoef(id,yvec)

	pred = mig.evalTensor2(d[2],d[1],mycoef)
	@fact sumabs(pred - yvec) => roughly(0.0,atol=0.00001)

	# predict on values off grid points
	nn1 = 10
	nn2 = 10
	newd1 = linspace(0.1,5,nn1)
	newd2 = linspace(-2.1,4,nn2)
	newb = Dict{Int,Array{Float64,2}}()
	# newb[1] = mig.quadraticSplineBFE(newd1,(d1[1],d1[end]),length(d1)-1)
	# newb[2] = mig.quadraticSplineBFE(newd2,(d2[1],d2[end]),length(d2)-1)
	newb[1] = mig.quadraticSplineBFE(newd1,(d1[1],d1[end]),length(d1)-1)
	newb[2] = mig.quadraticSplineBFE(newd2,(d2[1],d2[end]),length(d2)-1)

	# # TODO a function that evaluates tensor products of basis functions and a coefficient vector
	# pred2 = zeros(nn2,nn1)
	# for i1 = 1:nn1
	# 	for i2 = 1:nn2
	# 		t1 = squeeze(newb[1][i1,: ],1)
	# 		t2 = squeeze(newb[2][i2,:],1)
	# 		pred2[i2,i1] = mig.evalTensor2(t2,t1,mycoef)
	# 	end
	# end
	pred2 = mig.evalTensor2(newb[2],newb[1],mycoef)
	@fact sumabs(pred2 - yvec) => roughly(0.0,atol=0.00001)

	mig.subplot(121,projection="3d")
	mig.mesh(y)	
	mig.title("true values on grid")

	mig.subplot(122,projection="3d")
	tmp = reshape(pred2,(nn1,nn2))
	mig.mesh(tmp)
	mig.title("values off grid")

end




end