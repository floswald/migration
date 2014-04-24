
# requires the distributions package



# implement a normal bivariate AR1 copula type
# aims:
# 1) get the density of the copula
# 2) nothing else
type Copula

	d     :: Int  # number of dimensions
	rho   :: Float64  # AR1 parameter
	sigma :: Array{Float64}  # sigma array

	# initiate copula with AR1 structure
	function Copula(ndim::Int,rho::Float64)
 		m = abs(linspace(1.0,ndim,ndim) .- linspace(1.0,ndim,ndim)')
		sig = rho.^m
		return new(ndim,rho,sig)
	end
end	  

function show(io::IO, c::Copula)
	print(io, "normal AR1 copula with $(c.d) dimensions and parameter $(c.rho)")
end



# random draws from the copula
function rnormCopula( c::Copula, ndraw::Int )
	n = Normal()
	mn = MvNormal(c.sigma)
	pdf(n, rand(mn,ndraw) )
end


# density of the copula
# TODO restrict u to be certain type?
# how to change the body so that it can take vectors and reals?
function dnormCopula(u::Array{Float64}, c::Copula)

	if length(size(u)) > 2
		error("u must be a matrix or a vector")
	end


	if ndims(u)>1
		if size(u,2) != c.d
			error("u must have as many cols as there are dims in the copula")
		end
	else
		if length(u) != c.d
			error("u must have same length as there are dims in the copula")
		end
	end

	n = Normal(0,1)
	x = quantile(n,u)
	mn = MvNormal(c.sigma)
	r = pdf(mn,x) .- sum(pdf(n,x),2) 
end





## Copyright (C) 2012 Marius Hofert and Martin Maechler
##
## This program is free software; you can redistribute it and/or modify it under
## the terms of the GNU General Public License as published by the Free Software
## Foundation; either version 3 of the License, or (at your option) any later
## version.
##
## This program is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
## FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public License along with
## this program; if not, see <http://www.gnu.org/licenses/>.


### Wrappers and auxiliary functions for dealing with elliptical (Gauss, t_nu)
### and Archimedean copulas

##' @title Copula class for the given copula object
##' @param cop copula object
##' @return "ellipCopula" or "outer_nacopula" depending on the given copula object
##' @author Marius Hofert

# dnormalCopula <- function(u, copula, log=FALSE, ...) {
#   dim <- copula@dimension
#   sigma <- getSigma(copula)
#   if(!is.matrix(u)) u <- matrix(u, ncol = dim)
#   r <- numeric(nrow(u)) # i.e. 0  by default (i.e. "outside")
#   ok <- !apply(u, 1, function(x) any(is.na(x)))
#   x <- qnorm(u[ok, , drop=FALSE])
#   ## work in log-scale [less over-/under-flow, then (maybe) transform:
#   r[ok] <- dmvnorm(x, sigma = sigma, log=TRUE) - rowSums(dnorm(x, log=TRUE))
#   ## now happens in dCopula(): -- dnormalCopula() not called directly by user
#   ## if(any(out <- !is.na(u) & (u <= 0 | u >= 1)))
#   ##   val[apply(out, 1, any)] <- -Inf
#   if(log) r else exp(r)
# }


# pnormalCopula <- function(u, copula) {
#   dim <- copula@dimension
#   i.lower <- rep.int(-Inf, dim)
#   sigma <- getSigma(copula)
#   ## now happens in pCopula(): u <- matrix(pmax(0, pmin(1, u)), ncol = dim)
#   apply(qnorm(u), 1, function(x) if(any(is.na(x))) NA_real_ else
#         pmvnorm(lower = i.lower, upper = x, sigma = sigma))
# }