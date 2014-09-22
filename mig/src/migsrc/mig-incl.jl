

# miscellaneous includes

# objective function to work with mopt
function objfunc(pd::Dict,mom::DataFrame,whichmom::Array{ASCIIString,1},opts...)


	# info("start model solution")
	time0 = time()
	p = Param(2)	# create a default param type
	update!(p,pd)	# update with values changed by the optimizer
	m = Model(p)
	mig.solve!(m,p)
	s   = simulate(m,p)
	mms = computeMoments(s,p,m)	
	# mms   = simulate_parts(m,p,5)	# simulate and compute moments in 5 pars

	mom2 = join(mom,mms,on=:moment)
	insert!(mom2,6,DataArray(Float64,nrow(mom2)),:perc)
	insert!(mom2,6,DataArray(Float64,nrow(mom2)),:sqdist)

	# get subset of moments
	subset = findin(mom2[:moment],whichmom)

	# get percentage difference
	mom2[subset,:perc] = (mom2[subset,:data_value] - mom2[subset,:model_value]) ./ mom2[subset,:data_value]

	# get mean squared distance over standard edeivation
	mom2[subset,:sqdist] = ((mom2[subset,:data_value] - mom2[subset,:model_value])./ mom2[subset,:data_sd] ).^2

	fval = mean(mom2[subset,:sqdist]) / 1000
	# fval = mean(abs(mom2[subset,:perc]))

    mout = transpose(mom2[[:moment,:model_value]],1)

    if Sys.OS_NAME == :Darwin
	    showall(mom2[:,[:moment,:data_value,:model_value,:sqdist]])
	    println(p)
	    if get(opts[1],"printmoms",false) 
    		writetable("/Users/florianoswald/Dropbox/mobility/output/model/fit/moms.csv",mom2)
	    end
	    if get(opts[1],"plotsim",false) 
	   		simplot(s[!isna(s[:cohort]),:],5)
	   	end
	    println()
	end

	status = 1
	# if isnan(fval)
	# 	status = -1
	# end

	if get(opts[1],"printlevel",0) > 0
		println("objfunc runtime = $(time()-time0)")
	end
	time1 = round(time()-time0)
	ret = ["value" => fval, "params" => deepcopy(pd), "time" => time1, "status" => status, "moments" => mout]
	return ret
end

function mywrap()
	p = mig.Param(2)
	m = mig.Model(p)
    mig.solve!(m,p)
end


function runSim()
	p = Param(2)
	m = Model(p)
    solve!(m,p)
	s   = simulate(m,p)
	s = s[!isna(s[:cohort]),:]
	simplot(s,5)
	x=computeMoments(s,p,m)
	showall(x)
	return s
end


# single test run of objective
function runObj(printmoms::Bool)
	# run objective
	p2 = Dict{ASCIIString,Float64}()
	if Sys.OS_NAME == :Darwin
		indir = joinpath(ENV["HOME"],"Dropbox/mobility/output/model/data_repo/in_data_jl")
	elseif Sys.OS_NAME == :Windows
		indir = "C:\\Users\\florian_o\\Dropbox\\mobility\\output\\model\\data_repo\\in_data_jl"
	else
		indir = joinpath(ENV["HOME"],"data_repo/mig/in_data_jl")
	end
	moms = mig.DataFrame(mig.read_rda(joinpath(indir,"moments.rda"))["m"])
	objfunc_opts = ["printlevel" => 1,"printmoms"=>printmoms]
	@time x = mig.objfunc(p2,moms,array(moms[:moment]),objfunc_opts)
	return x
end

		
# asset grid scaling
function scaleGrid(lb::Float64,ub::Float64,n::Int,order::Int,cutoff::Float64,partition=0.5) 
	out = zeros(n)
	if order==1
		off = 1
		if lb<0 
			off = 1 - lb #  adjust in case of neg bound
		end
		out[1] = log(lb + off) 
		out[n] = log(ub + off) 
		out    = linspace(out[1],out[n],n)
		out    = exp(out) - off  
	elseif order==2
		off = 1
		if lb<0 
			off = 1 - lb #  adjust in case of neg bound
		end
		out[1] = log( log(lb + off) + off )
		out[n] = log( log(ub + off) + off )
		out    = linspace(out[1],out[n],n)
		out    = exp( exp(out) - off ) - off
	elseif order == 3
		npos = int(ceil(n*partition))
		nneg = n-npos
		if nneg < 1
			error("need at least one point in neg space")
		end
		nneg += 1
		# positive: log scale
		pos = exp( linspace(log(cutoff),log( ub + 1) ,npos) ) -1 
		# negative: linear scale
		neg = linspace(lb,cutoff,nneg)
		return [neg[1:(nneg-1)],pos]
	else
		error("supports only double log grid")
	end
end


# converts
function convert(::Type{DataFrame},cc::CoefTable)
	DataFrame(Variable=cc.rownms,Estimate=cc.mat[:,1],StdError=cc.mat[:,2],tval=cc.mat[:,3],pval=cc.mat[:,4])
end

# convert(::Type{Array{Int64,1}}, PooledDataArray{Int64,Uint32,1})
mylog(x::Float64) = ccall((:log, "libm"), Float64, (Float64,), x)
myexp(x::Float64) = ccall((:exp, "libm"), Float64, (Float64,), x)
mylog2(x::Float64) = ccall(:log, Cdouble, (Cdouble,), x)
myexp2(x::Float64) = ccall(:exp, Cdouble, (Cdouble,), x)


function setPaths()
# get moments from dropbox:
	if Sys.OS_NAME == :Darwin
		indir = joinpath(ENV["HOME"],"Dropbox/mobility/output/model/data_repo/in_data_jl")
		outdir = joinpath(ENV["HOME"],"Dropbox/mobility/output/model/data_repo/out_data_jl")
	elseif Sys.OS_NAME == :Windows
		indir = "C:\\Users\\florian_o\\Dropbox\\mobility\\output\\model\\data_repo\\in_data_jl"
		outdir = "C:\\Users\\florian_o\\Dropbox\\mobility\\output\\model\\data_repo\\out_data_jl"
	else
		indir = joinpath(ENV["HOME"],"data_repo/mig/in_data_jl")
		outdir = joinpath(ENV["HOME"],"data_repo/mig/out_data_jl")
	end
	return (indir,outdir)
end


function setupMC(autoload::Bool)
	indir, outdir = setPaths()

	if autoload
		# load model-generated data
		moms = readtable(joinpath(indir,"MCtrue.csv"))
	else
		# make model-generated data
		p = Param(2)
		m = Model(p)
	    solve!(m,p)
		s   = simulate(m,p)
		x=computeMoments(s,p,m)

		mom = DataFrame(read_rda(joinpath(indir,"moments.rda"))["m"])
		moms = join(mom,x,on=:moment)
		delete!(moms,[:data_value,:data_sd])
		names!(moms,[:moment,:data_value,:data_sd])

		writetable(joinpath(indir,"MCtrue.csv"),moms)
	end
	return moms
end

	