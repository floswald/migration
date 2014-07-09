

# miscellaneous includes

# objective function to work with mopt
function objfunc(pd::Dict,mom::DataFrame,whichmom::Array{ASCIIString,1})

	# info("start model solution")
	time0 = time()
	p = Param(2)	# create a default param type
	update!(p,pd)	# update with values changed by the optimizer
	m = Model(p)
	mig.solve!(m,p)
	# info("end model solution after $(round(time()-time0)) seconds")

	# info("simulation/computation of moments")
	s   = simulate(m,p)
	mms = computeMoments(s,p,m)	# todo: return DataFrame(moment,model_value)

	println("time till mms = $(time()-time0)")

	mom2 = join(mom,mms,on=:moment)

	fval = sum((mom2[findin(mom2[:moment],whichmom),:data_value] - mom2[findin(mom2[:moment],whichmom),:model_value]).^2)

    mout = transpose(mom2[[:moment,:model_value]],1)

	time1 = round(time()-time0)
	ret = ["value" => fval, "params" => deepcopy(pd), "time" => time1, "status" => 1, "moments" => mout]
	return ret
end

function mywrap()
	p = mig.Param(2)
	m = mig.Model(p)
    mig.solve!(m,p)
end


function runSim()
	p = mig.Param(2)
	m = mig.Model(p)
    mig.solve!(m,p)
	s   = simulate(m,p)
	mms = computeMoments(s,p,m)	# todo: return DataFrame(moment,model_value)
end

# converts
# convert(::Type{Array{Int64,1}}, PooledDataArray{Int64,Uint32,1})

