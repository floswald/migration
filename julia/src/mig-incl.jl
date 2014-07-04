

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

    nm0 = names(mom)
    DataFrames.insert_single_column!(mom,zeros(nrow(mom)),ncol(mom)+1)
    names!(mom,[nm0,:model_value])

    mout = transpose(mom[[:moment,:model_value]],1)
	# get obj value
	# mom - mms
	# TODO: setup mms in a way that you can also subset with whichmom
	retval = mom[findin(mom[:moment],whichmom),:value] - mms["nomove"]

	time1 = round(time()-time0)
	ret = ["value" => retval, "params" => deepcopy(pd), "time" => time1, "status" => 1, "moments" => mout]
	return ret
end

function mywrap()
	p = mig.Param(2)
	m = mig.Model(p)
    mig.solve!(m,p)
end

