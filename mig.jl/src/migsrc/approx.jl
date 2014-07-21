




# approximating tensor grids
# computing the approximating coefficients on 
# a tensor product of basis functions
# NOTE: the fastest varying index in v is the one with highest 
# index in ibm
function getTensorCoef{T<:Real}(ibm::Dict{Int,Array{T,2}},v::Vector{T})

	# ibm are usually inverse basis matrices

	nall = length(v)	# length value vector
	nbm  = length(ibm)	# number of basis functions / dimensions of v

	# dim(matrices) must be same as length v
	prod = 1
	for (k,va) in ibm
		prod *= size(va,1)
		if !(size(va,1) == size(va,2))
			throw(ArgumentError("all matrices must be square"))
		end
	end
	if !(prod == nall)
		throw(ArgumentError("sizes of matrices not consistent with v"))
	end

	# compute product for first matrix
	ks = sort(collect(keys(ibm)))
	v0    = copy(v)
	stemp = ibm[ks[1]]
	n     = size(stemp,1)
	m     = nall / n
	v1    = zeros(nall)
	for i in 1:m
		v1[m*(0:(n-1)) + i] = stemp * v0[(n*(i-1)) + (1:n)]
	end

	# compute for all other matrics
	if nbm > 1
		for imat in 2:nbm
			v0    = copy(v1)
			stemp = ibm[ks[imat]]
			n     = size(stemp,1)
			m     = nall / n
			# fill!(v1,0.0)
			for i in 1:m
				v1[m*(0:(n-1)) + i] = stemp * v0[(n*(i-1)) + (1:n)]
			end
		end
	end
	return v1
end


# evaluate approxiation at several points
# a point is a tuple (a,b,c,...,dim), so bm is a dict with each
# member a basis matrix evaluated at either vectors a,b,c,...,dim
function evalTensorApprox{T<:Real}(bm::Dict{Int,Array{T}},c::Vector{T})

	if length(bm==2)
		r = evalTensor2(bm,c)
	elseif length(bm==3)
		r=evalTensor3(bm,c)
	elseif length(bm==4)
		r=evalTensor4(bm,c)
	end
 	return r
end

function evalTensor2(mat1::Array{Float64,2},mat2::Array{Float64,2},c::Vector)

	# TODO
	# sparse: if you had a type BSpline with field "nonzero" that
	# gives the index of the nonzero basis functions, this could
	# be sped up dramatically

	n1 = size(mat1,1)
	n2 = size(mat2,1)
	m1 = size(mat1,2)
	m2 = size(mat2,2)
	row_offset = 0
	col_offset = 0
	factor = 0.0

	r = zeros(n1 * n2)

	# loop over rows of 1
	for row_idx1 in 1:n1
		row_offset = (row_idx1-1) * n2

		# loop over rows of 2
		for row_idx2 in 1:n2

			# loop over cols of 1
			for col_idx1 in 1:m1
				col_offset = (col_idx1-1) * m2
				factor = mat1[row_idx1,col_idx1]

				# loop over cols of 2
				for col_idx2 in 1:m2

					# println("row_offset = $row_offset")
					# println("row_idx1 = $row_idx1")
					# println("row_idx2 = $row_idx2")
					# println("row_offset + row_idx1 = $(row_offset + row_idx1)")
					# println("factor = $factor")

					r[row_offset + row_idx2] += factor * mat2[row_idx2,col_idx2] * c[col_offset + col_idx2]
				end
			end
		end
	end
	return r
end



function evalTensor2(mat1::Array{Float64,1},mat2::Array{Float64,1},c::Vector)

	# TODO
	# sparse

	m1 = length(mat1)
	m2 = length(mat2)
	col_offset = 0
	factor = 0.0

	r = 0.0

	# loop over cols of 1
	for col_idx1 in 1:m1
		col_offset = (col_idx1-1) * m2
		factor = mat1[col_idx1]

		# loop over cols of 2
		for col_idx2 in 1:m2

			r += factor * mat2[col_idx2] * c[col_offset + col_idx2]
		end
	end
	return r
end


function evalTensor3(mat1::Array{Float64,2},mat2::Array{Float64,2},mat3::Array{Float64,2},c::Vector)

	n1 = size(mat1,1)
	n2 = size(mat2,1)
	n3 = size(mat3,1)
	m1 = size(mat1,2)
	m2 = size(mat2,2)
	m3 = size(mat3,2)
	row_offset1= 0
	col_offset1= 0
	factor1= 0.0
	row_offset2= 0
	col_offset2= 0
	factor2= 0.0

	r = zeros(n1 * n2 * n3)

	# loop over rows of 1
	for row_idx1 in 1:n1
		row_offset1 = (row_idx1-1) * n2

		# loop over rows of 2
		for row_idx2 in 1:n2
			row_offset2 = (row_offset1 + (row_idx2-1)) * n3

			# loop rows 3
			for row_idx3 in 1:n3
				# println(row_offset2 + row_idx3)

				# loop over cols of 1
				for col_idx1 in 1:m1
					col_offset1 = (col_idx1-1) * m2
					factor1 = mat1[row_idx1,col_idx1]

					# loop over cols of 2
					for col_idx2 in 1:m2
						col_offset2 = (col_offset1 + (col_idx2-1)) * m3
						factor2 = factor1 * mat2[row_idx2,col_idx2]

						# cols mat3
						for col_idx3 in 1:m3

							r[row_offset2 + row_idx3] += factor2 * mat3[row_idx3,col_idx3] * c[col_offset2 + col_idx3]
						end
					end
				end
			end
		end
	end
	return r
end


function evalTensor3(mat1::Array{Float64,1},mat2::Array{Float64,1},mat3::Array{Float64,1},c::Vector)

	m1 = length(mat1)
	m2 = length(mat2)
	m3 = length(mat3)
	col_offset1= 0
	factor1= 0.0
	col_offset2= 0
	factor2= 0.0

	r = 0.0

	# loop over cols of 1
	for col_idx1 in 1:m1
		col_offset1 = (col_idx1-1) * m2
		factor1 = mat1[col_idx1]

		# loop over cols of 2
		for col_idx2 in 1:m2
			col_offset2 = (col_offset1 + (col_idx2-1)) * m3
			factor2 = factor1 * mat2[col_idx2]

			# cols mat3
			for col_idx3 in 1:m3
				r += factor2 * mat3[col_idx3] * c[col_offset2 + col_idx3]
			end
		end
	end
	return r
end

# type BSpline
# 	degree :: Int
# 	numKnots :: Int
# 	lower :: Float64
# 	upper :: Float64
# 	vecKnots :: Array{Float64,1}

# 	function BSpline(nKnots,deg,lb,ub)
# 		new(nKnots,deg,lb,ub)
# 	end

# 	function BSpline(knots,deg,extend=false)
# 		if !issorted(knots)
# 			error("knots must be sorted")
# 		end
# 		numKnots = length(knots) - 2*deg*extend
# 		lb = knots[1]
# 		ub = knots[end]
# 		vecKnots = zeros( numKnots + 2*deg*(1-extend) )
# 		# fill knot vector
# 		for i in 1:length(knotVec)
# 			if i < deg







# function evalBasis(point,bs::BSpline)




# end

# Eigen::SparseVector<double> BSpline::calc(
#         double point, 
#         int derivative /* DEFAULT = 0 */ )  	// first line to change to use sparse vectors
# {
#     // declare sparse basis function of length num_nodes
#     int num_nodes = getNumCoefs();
#     Eigen::SparseVector<double> vec_bs( num_nodes );
#     vec_bs.reserve( d_degree+1 );   // SparseVector has d_degree+1 non-zero elements

#     // NOTE: the derivatives do not work for the case where point==upper_bound or lower_bound yet (and extrapolations either)
    
#     // calculate the zero order basis functions, which has d_numKnots - 1 elements
#     int bf_index;

#     if ( point <= d_lowerBound ) { // extrapolation below
#         bf_index = d_degree;
#         //std::cout << "Warning: point is outside interpolation interval" << std::endl;
#     }
#     else if ( point >= d_upperBound ) { // extrapolation above
#         bf_index = num_nodes - 1;
#         //std::cout << "Warning: point is outside interpolation interval" << std::endl;
#     }
#     else {
#         // determine index of first non-zero entry
#         bf_index = d_degree; // non-extended vecKnot starts here
#         while ( point > d_vecKnots(bf_index+1) ) {
#             bf_index++;
#         }
#     }
        
#     vec_bs.insert( bf_index ) = 1.0;
        
#     for (int k=1;k<=(d_degree-derivative);k++) { // loop over current degree
        
#         // maybe we should calculate the minimum and maximum, so we don't have to use the if statements below
#         for (int j=bf_index-k;j<=bf_index;j++) {
                
#             double d, e;
#             if ( j+k <= d_degree ) {
#                 d = 0.0;
#             }
#             else { 
#                 d = (point - d_vecKnots(j))/(d_vecKnots(j+k)-d_vecKnots(j))*vec_bs.coeff(j);
#             }
            
#             if ( j+1 >= vec_bs.size() ) {
#                    e = 0.0;
#             }	
#             else {
#                 e = (d_vecKnots(j+k+1) - point)/(d_vecKnots(j+k+1)-d_vecKnots(j+1))*vec_bs.coeff(j+1);
#             }
#             // insert or update element j
#             vec_bs.coeffRef(j) = d + e;

#         }
#     }

# B0(u::Vector{Float64}, i::Integer) = x::Real -> (u[i] <= x < u[i+1]) ? float(1) : float(0) ## non-continuous
# B(p::Integer,u::Vector{Float64},i::Integer) = (p==0) ? B0(u,i) : x -> float(((x-u[i])/(u[i+p]-u[i]))*B(p-1,u,i)(x) + ((u[i+p+1]-x)/(u[i+p+1]-u[i+1]))*B(p-1,u,i+1)(x))





