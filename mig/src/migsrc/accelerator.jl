


# 1D linear approximation
# =======================

type Accelerator
	i::Int
end

function getAccel(a::Accelerator)
	a.i
end
function setAccel!(a::Accelerator,j::Int)
	a.i = j
	return nothing
end


# CAUTION
# this function only works in the very specific ciurcumstance where it's called.
# assumption: x is known to be larger than at least y[1]!
function searchsorted1step(y::Vector,x,j::Int,n)
	# find lowest upper bound
	while x >= y[j] && j < n
		# println("x >= y[j]: $(x >= y[j])")
		# println("j < n: $(j<n)")
		# println("y[j] = $(y[j]), j=$j, x=$x")
		j += 1
	end
	# return highest lower bound
	return j-1
end



# ================================
# INNERMOST LOOP
# SUPER HIGH OPTIMIZATION REQUIRED
# ================================

# very special approximation: the value of x is monotonically increasing! so you can 
# never go in a lower bin from any bin k that was select for a given value of x[k]
function linearapprox(x::Array{Float64,1},y::Array{Float64,1},xi::Float64,n::Int,acc::Accelerator)
	r = 0.0

	# determining bounds 
	if xi <= x[1]
		# get linear approx below
		@inbounds r = y[1] + (y[2] - y[1]) * (xi - x[1])  / (x[2] - x[1])
		return r
	elseif xi >= x[n]
		# get linear approx above
		@inbounds r = y[n] + (y[n] - y[n-1]) * (xi - x[n])  / (x[n] - x[n-1])
		return r

	# that will never happen
	# elseif xi < x[acc.i]
	# 	setAccel!(acc,searchsortedlast(x,xi,1,acc.i,Base.Forward))
	elseif xi >= x[acc.i+1]
		# setAccel!(acc,searchsortedlast(x,xi,acc.i+1,n-1,Base.Forward))
		setAccel!(acc,searchsorted1step(x,xi,acc.i+1,n))
	# else
		# hit!
	end
	@inbounds r = (y[acc.i] * (x[acc.i+1] - xi) + y[acc.i+1] * (xi - x[acc.i]) ) / (x[acc.i+1] - x[acc.i])

	return r 
end