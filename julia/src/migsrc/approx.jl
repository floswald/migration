




# approximating tensor grids

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


# essentially the same thing, but evaluates the product also for
# single points, i.e. bm is a dict of vectors (one basis function)
# for each point
function evalTensorProduct{T<:Real}(bm::Dict{Int,Array{T}},c::Vector{T})

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
	# sparse

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
		row_offset = (row_idx1-1)
		row_offset *= n2

		# loop over rows of 2
		for row_idx2 in 1:n2

			# loop over cols of 1
			for col_idx1 in 1:m1
				col_offset = (col_idx1-1) 
				col_offset *=  m2
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
		col_offset = (col_idx1-1) 
		col_offset *=  m2
		factor = mat1[col_idx1]

		# loop over cols of 2
		for col_idx2 in 1:m2

			r += factor * mat2[col_idx2] * c[col_offset + col_idx2]
		end
	end
	return r
end











