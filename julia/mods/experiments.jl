



# experiments

# show probability of moving a at a given state 
# as a function of moving cost parameters for owner
# and renter

function experMC(whichMC,nval)

	# baseline param
	p = mig.Param(2)

	# mc names
	nm = ["alpha1","alpha2","alpha3"]

	prange = linspace(0.2,1,nval)

	# collect results

	m = mig.Model(p)

	mig.solve!(m,p)

	out = zeros(p.nJ,2,nval)
	out[:,:,1] = reshape(m.rho[:,1,1,1,1,15,:,1,2,29],p.nJ,2)

	outdf = DataFrame(MC=[p.MC[whichMC] for i=1:p.nJ*2],own=[["rent" for i=1:p.nJ],["own" for i=1:p.nJ]],moveto=[1:p.nJ,1:p.nJ],prob=m.rho[:,1,1,1,1,15,:,1,2,29][:])

	for n in 2:nval
		p.MC[whichMC] = prange[n]
		m = mig.Model(p)
		mig.solve!(m,p)
		
		out[:,:,n] = reshape(m.rho[:,1,1,1,1,15,:,1,2,29],p.nJ,2)

		outdf = rbind(outdf,DataFrame(MC=[p.MC[whichMC] for i=1:p.nJ*2],own=[["rent" for i=1:p.nJ],["own" for i=1:p.nJ]],moveto=[1:p.nJ,1:p.nJ],prob=m.rho[:,1,1,1,1,15,:,1,2,29][:]))
	end


	# return values and plots
	# this actually works

	# maxval = maximum(out[:,2,:])
	# figure("owner")
	# plot(out[:,2,1],linewidth=2,linestyle="-")
	# plot(out[:,2,2],linewidth=2,linestyle="--")
	# plot(out[:,2,3],linewidth=2,linestyle="-.")
	# # plot(out[:,2,4],linewidth=2,linestyle=":")
	# title("owners reaction to changes in $(nm[whichMC])")

	# figure("renter")
	# plot(out[:,1,1],linewidth=2,linestyle="-")
	# plot(out[:,1,2],linewidth=2,linestyle="--")
	# plot(out[:,1,3],linewidth=2,linestyle="-.")
	# # plot(out[:,1,4],linewidth=2,linestyle=":")
	# title("renters reaction to changes in $(nm[whichMC])")
	# ylim((0,maxval))
	# savefig("test.pdf")

	# figure("owner")
	# savefig("test2.pdf")

	# not gadfly currently...

	# maxval = maximum(out[out[:own] .== "yes",:prob])+0.05

	# plno = Gadfly.plot(out[out[:own] .== "no",:],x="moveto",y="prob",color="MC",Geom.line,Guide.title("Renter response to changes in $(nm[whichMC])"),Scale.y_continuous(minvalue=0,maxvalue=maxval))
	# plyes = Gadfly.plot(out[out[:own] .== "yes",:],x="moveto",y="prob",color="MC",Geom.line,Guide.title("Owner response to changes in $(nm[whichMC])"),Scale.y_continuous(minvalue=0,maxvalue=maxval))

	# draw plots to disk



	return (outdf,out)
	# return (out,plno,plyes)

end




