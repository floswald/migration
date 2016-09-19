using PyPlot
using PyCall

function plotShockRegion(shock::AbstractString,region::Int,remote::AbstractString="null")

	file = string("exp_region$(region)_$(shock).jld")
	pa = setPaths()
	outdir = joinpath(pa["outdir"],"shockReg")
	if remote != "null"
		remote_in = joinpath(pa["remote_out"],"shockReg",file)
		info("downloading data from $remote")
		run(`scp $(remote):$(remote_out) $outdir`)
	end

	# load data
	d = JLD.load(joinpath(outdir,file))

	# recreate model
	p = Param(2,d["opts"])
	m = Model(p)

	dd = @select(d["elasticity"],:year,:Total_out_all,:Total_out_all_1, p_out_all=(:Total_out_all_1.-:Total_out_all)./abs(:Total_out_all), p_out_own=(:Own_out_all_1.-:Own_out_all)./abs(:Own_out_all),p_out_rent=(:Rent_out_all_1 .- :Rent_out_all)./abs(:Rent_out_all),:d_in_buy,:d_in_rent,:d_out_buy,:d_out_rent,:yshock,:pshock)

	# experiment data
	# dd = @select(d["elasticity"],:year,:d_all_y,:d_all_p,:d_own_y,:d_own_p,:d_rent_y,:d_rent_p,:d_net_rent_p,:d_net_rent_y,:d_net_own_y,:d_net_own_p,:d_out_buy_y,:d_in_buy_y,:d_out_rent_y,:d_in_rent_y,:d_net_rent,:d_own,:d_rent,:d_net_own,:Net_rent,:Net_rent_1,:Net_own,:Net_own_1,:Net,:Net_1,:yshock,:pshock,:Rent_out_all,:Own_out_all,:Rent_out_all_1,:Own_out_all_1)
	# dd = @transform(dd, d_net = :Net_1 .- :Net, p_net = (:Net_1 .- :Net)./abs(:Net), p_rent_out = (:Rent_out_all_1 - :Rent_out_all) ./  abs(:Rent_out_all), p_own_out = (:Own_out_all_1 - :Own_out_all) ./  abs(:Own_out_all))

	shockreg = m.regnames[d["opts"]["shockRegion"],:Division]
	# average income data
	base_y = @where(d["moments"]["base"]["yearly"][[:year,:meany,:Division]],:Division .== shockreg)
	names!(base_y,[:year;symbol("y_$(shockreg)");:Division])
	shock_y = @where(d["moments"][shock]["yearly"][[:year,:meany,:Division]],:Division .== shockreg)
	names!(shock_y,[:year;symbol("y_$(shockreg)_shock");:Division])

	# prepare py-data
	# y_j = m.pred_ydf[[:year,symbol(shockreg)]]
	# names!(y_j,[:year;symbol("y_$shockreg")])
	p_j = m.pred_pdf[[:year,symbol(shockreg)]]
	names!(p_j,[:year;symbol("p_$(shockreg)")])
	# println("dd = $(dd)")
	# println("base_y = $(base_y)")

	dp = join(dd,base_y,on=:year)
	dp = join(dp,shock_y,on=:year)
	dp = join(dp,p_j,on=:year)

	# dp[symbol("y_$(shockreg)_shock")] = dp[symbol("y_$(shockreg)")] .* (1-dp[:yshock])a
	# dp[:pshock][1:3] = 0.0

	dp[symbol("p_$(shockreg)_shock")] = dp[symbol("p_$(shockreg)")] .* (1-dp[:pshock])


	# plot parameters
	# ---------------

	figs = Any[]
	figsi = (10,7)
	lw = 1.5

	# print data
	# -----------
	# println(dp[[:year,:p_net,:Rent_out_all,:Rent_out_all_1,:Own_out_all,:Own_out_all_1,:d_net,:d_net_own,:d_net_own_y,:d_net_rent,:d_net_rent_y,:d_own,:d_rent]])

	# get a tick formatter
	tick20 = matplotlib[:ticker][:MultipleLocator](20)
	tick20[:tick_values](140,55)



	push!(figs, figure("Total outflows",figsize=figsi))
	@pyimport matplotlib.gridspec as gspec 
	g1 = gspec.GridSpec(1,1)
	g1[:update](top=0.95,bottom = 0.53)
	ax1 = subplot(get(g1,(0,0)))
	ax1[:grid]()
	ax1[:set_ylabel]("proportion of pop. leaving")
	ax1[:plot](dp[:year],dp[:Total_out_all],linestyle="-",color="black",linewidth=lw,label="Baseline")
	ax1[:plot](dp[:year],dp[:Total_out_all_1],linestyle="--",color="black",linewidth=lw,label="Shock")
	ax1[:legend](loc="upper right",prop=Dict("size"=>10))

	g2 = gspec.GridSpec(2,1)
	g2[:update](top=0.5,bottom = 0.05,hspace=0.03)

	ax2 = subplot(get(g2,(0,0)),sharex=ax1)
	ax2[:plot](dp[:year],dp[symbol("y_$(shockreg)_shock")],label="y_$(shockreg)_shock",linewidth=lw,linestyle="--")
	ax2[:plot](dp[:year],dp[symbol("y_$(shockreg)")],label="y_$(shockreg)",linewidth=lw,linestyle="-")
	ax2[:grid]()
	ax2[:set_ylabel]("1000 USD")
	ax2[:legend](loc="center right",prop=Dict("size"=>10))
	# set ticks
	vrange = (minimum(dp[symbol("y_$(shockreg)_shock")]),maximum(dp[symbol("y_$(shockreg)_shock")]))
	tick2 = matplotlib[:ticker][:MultipleLocator](2)
	tick2[:tick_values](vrange[1]+1,vrange[2]-1)
	ax2[:yaxis][:set_major_locator](tick2)

	ax3 = subplot(get(g2,(1,0)),sharex=ax1)
	ax3[:plot](dp[:year],dp[symbol("p_$(shockreg)_shock")],label="p_$(shockreg)_shock",linewidth=lw,linestyle="--")
	ax3[:plot](dp[:year],dp[symbol("p_$(shockreg)")],label="p_$(shockreg)",linewidth=lw,linestyle="-")
	ax3[:legend](loc="upper right",prop=Dict("size"=>10))
	ax3[:grid]()
	ax3[:set_ylabel]("1000 USD")
	vrange = (minimum(dp[symbol("p_$(shockreg)_shock")]),maximum(dp[symbol("p_$(shockreg)_shock")]))
	tick20 = matplotlib[:ticker][:MultipleLocator](20)
	tick20[:tick_values](vrange[1]+10,vrange[2]-10)
	ax3[:yaxis][:set_major_locator](tick20)

	PyPlot.setp(ax2[:get_xticklabels](),visible=false)
	PyPlot.setp(ax1[:get_xticklabels](),visible=false)
	figs[1][:canvas][:draw]() # Update the figure

	# split by renter/owner

	push!(figs, figure("outflows own/rent",figsize=figsi))
	g3 = gspec.GridSpec(1,1)
	g3[:update](top=0.95,bottom = 0.53)
	ax1 = subplot(get(g3,(0,0)))
	ax1[:grid]()
	ax1[:set_ylabel](L"%$\Delta$ in proportion of leavers")
	ax1[:plot](dp[:year],100.0*dp[:p_out_own],color="red",linewidth=lw,label="owner")
	ax1[:plot](dp[:year],100.0*dp[:p_out_rent],color="blue",linewidth=lw,label="renter")
	# ax1[:set_ylim]([-0.6;0.05])
	ax1[:legend](loc="upper right",prop=Dict("size"=>10))

	g4 = gspec.GridSpec(2,1)
	g4[:update](top=0.5,bottom = 0.05,hspace=0.03)

	ax2 = subplot(get(g4,(0,0)),sharex=ax1)
	ax2[:plot](dp[:year],dp[symbol("y_$(shockreg)_shock")],label="y_$(shockreg)_shock",linewidth=lw,linestyle="--")
	ax2[:plot](dp[:year],dp[symbol("y_$(shockreg)")],label="y_$(shockreg)",linewidth=lw,linestyle="-")
	ax2[:yaxis][:set_major_locator](tick2)
	ax2[:grid]()
	ax2[:set_ylabel]("1000 USD")
	ax2[:legend](loc="center right",prop=Dict("size"=>10))

	ax3 = subplot(get(g4,(1,0)),sharex=ax1)
	ax3[:plot](dp[:year],dp[symbol("p_$(shockreg)_shock")],label="p_$(shockreg)_shock",linewidth=lw,linestyle="--")
	ax3[:plot](dp[:year],dp[symbol("p_$(shockreg)")],label="p_$(shockreg)",linewidth=lw,linestyle="-")
	ax3[:legend](loc="upper right",prop=Dict("size"=>10))
	ax3[:grid]()
	ax3[:yaxis][:set_major_locator](tick20)
	ax3[:set_ylabel]("1000 USD")
	PyPlot.setp(ax2[:get_xticklabels](),visible=false)
	PyPlot.setp(ax1[:get_xticklabels](),visible=false)
	figs[2][:canvas][:draw]() # Update the figure

	# changes in in-buy, in-rent, out-buy, out-rent

	push!(figs, figure("changes in behaviour of movers",figsize=figsi))
	ax4 = subplot(211)
	ax4[:grid]()
	ax4[:plot](dp[:year],100.0*dp[:d_in_buy],color="red",linewidth=lw,label="in and buy")
	ax4[:plot](dp[:year],100.0*dp[:d_in_rent],color="blue",linewidth=lw,label="in and rent")
	# ax1[:set_ylim]([-0.6;0.05])
	ax4[:legend](loc="upper right",prop=Dict("size"=>10))
	ax4[:set_ylabel](L"%$\Delta$ in proportion")
	PyPlot.setp(ax4[:get_xticklabels](),visible=false)

	ax5 = subplot(212,sharex=ax1)
	ax5[:plot](dp[:year],100.0*dp[:d_out_buy],color="red",linewidth=lw,label="out and buy")
	ax5[:plot](dp[:year],100.0*dp[:d_out_rent],color="blue",linewidth=lw,label="out and rent")
	ax5[:grid]()
	ax5[:legend](loc="upper right",prop=Dict("size"=>10))
	ax5[:set_ylabel](L"%$\Delta$ in proportion")
	figs[3][:canvas][:draw]() # Update the figure

	e = @linq dp |>
		@where(:yshock.!= 0.0) |>
		@select(eall = mean(:p_out_all ./ :yshock), erent = mean(:p_out_rent ./ :yshock), eown = mean(:p_out_own ./ :yshock))

	# save dataframe
	f = open(joinpath(pa["outdir"],"elasticities.json"),"w")
	JSON.print(f,e)
	close(f)

	# save figs
	fnames = ["outflow","outflow-ownrent","in-out-rent-buy"] 
	for f in 1:length(fnames)
		fi = open(joinpath(pa["out_graphs"],string(fnames[f],".pdf")),"w")
		writemime(fi,"application/pdf",figs[f])
		close(fi)
	end

	return figs


	# subplots_adjust(hspace=0.0) # Set the vertical spacing between axes
	# fig,axes = PyPlot.subplots(1,2, gridspec_kw = Dict("width_ratios"=>[3; 1]))
	# subplot(211) # Create the 1st axis of a 3x1 array of axes
	# ax1 = gca()
	# ax1[:set_xscale]("log") # Set the x axis to a logarithmic scale
	# setp(ax1[:get_xticklabels](),visible=false) # Disable x tick labels
	# grid("on")
	# title("Title")
	# yticks(0.1:0.2:0.9) # Set the y-tick range and step size, 0.1 to 0.9 in increments of 0.2
	# ylim(0.0,1.0) # Set the y-limits from 0.0 to 1.0
	# subplot(212,sharex=ax1) # Create the 2nd axis of a 3x1 array of axes
	# ax2 = gca()
	# ax2[:set_xscale]("log") # Set the x axis to a logarithmic scale
	# setp(ax2[:get_xticklabels](),visible=false) # Disable x tick labels
	# grid("on")
	# ylabel("Log Scale")
	# yticks(0.1:0.2:0.9)
	# ylim(0.0,1.0)
	# fig[:canvas][:draw]() # Update the figure
	# suptitle("2x1 Shared Axis")
	# fig,axes = subplots(2,1)
	# ax = axes[1]
	# ax[:plot]()



	# save plots



end

# mig module plotting functions



function vhplot(m::Model,p::Param,idx)

	# plotting against dimension "a",
	# which is slot 8 in vh

	# choose a state

	ik   = idx[1]
	is   = idx[2]
	iz   = idx[3]
	iy   = idx[4]
	ip   = idx[5]
	itau = idx[6]
	ij   = idx[7]
	it   = idx[8]


	a  = m.grids["assets"]
	a11 = copy(m.grids["assets"])
	a12 = copy(m.grids["assets"])
	a21 = copy(m.grids["assets"])
	a22 = copy(m.grids["assets"])

	# current renter
	vh11   = m.vh[1,ik,is,iz,iy,ip,itau,:,1,ij,it][:]
	vh12   = m.vh[2,ik,is,iz,iy,ip,itau,:,1,ij,it][:]
	sh11   = m.sh[1,ik,is,iz,iy,ip,itau,:,1,ij,it][:]
	sh12   = m.sh[2,ik,is,iz,iy,ip,itau,:,1,ij,it][:]
	ch11   = m.ch[1,ik,is,iz,iy,ip,itau,:,1,ij,it][:]
	ch12   = m.ch[2,ik,is,iz,iy,ip,itau,:,1,ij,it][:]
	cash11 = m.cash[1,ik,is,iz,iy,ip,itau,:,1,ij,it][:]
	cash12 = m.cash[2,ik,is,iz,iy,ip,itau,:,1,ij,it][:]
	# current owner
	vh21   = m.vh[1,ik,is,iz,iy,ip,itau,:,2,ij,it][:]
	vh22   = m.vh[2,ik,is,iz,iy,ip,itau,:,2,ij,it][:]
	ch21   = m.ch[1,ik,is,iz,iy,ip,itau,:,2,ij,it][:]
	ch22   = m.ch[2,ik,is,iz,iy,ip,itau,:,2,ij,it][:]
	sh21   = m.sh[1,ik,is,iz,iy,ip,itau,:,2,ij,it][:]
	sh22   = m.sh[2,ik,is,iz,iy,ip,itau,:,2,ij,it][:]
	cash21 = m.cash[1,ik,is,iz,iy,ip,itau,:,2,ij,it][:]
	cash22 = m.cash[2,ik,is,iz,iy,ip,itau,:,2,ij,it][:]

	# envelopes, i.e. v = max(vh[1],vh[2])
	v1 = m.v[ik,is,iz,iy,ip,itau,:,1,ij,it][:]
	v2 = m.v[ik,is,iz,iy,ip,itau,:,2,ij,it][:]
	h1 = m.dh[ik,is,iz,iy,ip,itau,:,1,ij,it][:]
	h2 = m.dh[ik,is,iz,iy,ip,itau,:,2,ij,it][:]
	rho1 = m.rho[ik,is,iz,iy,ip,itau,:,1,ij,it][:]
	rho2 = m.rho[ik,is,iz,iy,ip,itau,:,2,ij,it][:]

	# get infeasible regions
	i11 = vh11.==p.myNA
	i12 = vh12.==p.myNA
	i21 = vh21.==p.myNA
	i22 = vh22.==p.myNA

	# set infeasi to NaN
	vh11[i11]   = NaN
	vh12[i12]   = NaN
	sh11[i11]   = NaN
	sh12[i12]   = NaN
	ch11[i11]   = NaN
	ch12[i12]   = NaN
	cash11[i11] = NaN
	cash11[i11] = NaN
	a11[i11]    = NaN
	a12[i12]    = NaN
	v1[v1.==p.myNA] = NaN
	v2[v2.==p.myNA] = NaN
	a1 = a[v1.!=p.myNA]
	a2 = a[v2.!=p.myNA]


	vh21[i21]   = NaN
	vh22[i22]   = NaN
	sh21[i21]   = NaN
	sh22[i22]   = NaN
	ch21[i21]   = NaN
	ch22[i22]   = NaN
	cash21[i21] = NaN
	cash22[i22] = NaN
	a21[i21]    = NaN
	a22[i22]    = NaN

	xpad = 10
	ypad = 0.5

	vscale = (minimum([vh11,vh12,vh21,vh22]) - ypad, maximum([vh11,vh12,vh21,vh22]) + ypad)
	xscale = (minimum([a11,a12,a21,a22]) - xpad, maximum([a11,a12,a21,a22]) + xpad)

	figure()

	subplot(331)
	ylim( vscale )
	xlim( xscale )
	p_v1, = plot(a11,vh11,"o-")
	p_v2, = plot(a21,vh21,"o-")
	ylabel("v_h_0")
	grid()
	title("value h(t+1)=0")
	legend((p_v1,p_v2),("renter","owner"),"lower right",borderpad=0.2,labelspacing=0.1,handlelength=0.7)

	subplot(332)
	ylim( ( minimum([sh11,sh21,0.0]) -xpad , maximum([sh11,sh21,ch11,ch21]) + xpad ) )
	xlim( xscale )
	plot(a11,sh11,"o-")
	plot(a21,sh21,"o-")
	grid()
	title("savings h(t+1)=0")
	
	subplot(333)
	ylim( ( minimum([sh11,sh21,0.0])- xpad, maximum([sh11,sh21,ch11,ch21]) + xpad) )
	xlim( xscale )
	plot(a11,ch11,"o-")
	plot(a21,ch21,"o-")
	grid()
	title("consumption h(t+1)=0")

	subplot(334)
	ylim( vscale )
	xlim( xscale )
	plot(a12,vh12,"o-")
	plot(a22,vh22,"o-")
	grid()
	ylabel("v_h_1")
	title("value h(t+1)=1")

	subplot(335)
	ylim( ( minimum([sh12,sh22,0.0])- xpad, maximum([sh12,sh22,ch12,ch22]) + xpad) )
	xlim( xscale )
	plot(a12,sh12,"o-")
	plot(a22,sh22,"o-")
	grid()
	title("savings h(t+1)=1")
	
	subplot(336)
	ylim( ( minimum([sh12,sh22,0.0])- xpad, maximum([sh12,sh22,ch12,ch22]) + xpad) )
	xlim( xscale )
	plot(a12,ch12,"o-")
	plot(a22,ch22,"o-")
	grid()
	title("consumption h(t+1)=1")

	subplot(337)
	xlim( xscale )
	ylim( vscale )
	plot(a1,v1,"o-")
	plot(a2,v2,"o-")
	grid()
	title("v = max(v0,v1)")
	xlabel("assets")


	subplot(338)
	ylim( (-0.2,1.1) )
	xlim( xscale )
	plot(a1,h1[v1.!=p.myNA]-0.1,"o-")
	plot(a2,h2[v2.!=p.myNA],"o-")
	title("housing choice")
	xlabel("assets")

	subplot(339)
	xlim( xscale )
	ylim( (-0.2,1.1) )
	plot(a1,rho1[v1.!=p.myNA],"o-")
	plot(a2,rho2[v2.!=p.myNA],"o-")
	title("probability of moving")
	xlabel("assets")
	suptitle("Model at ik=$ik,is=$is,iz=$iz,iy=$iy,ip=$ip,itau=$itau,ij=$ij,age=$it")

end

function vplot(m::Model,p::Param)


	# choose a random state

	ik   = rand(1:p.nJ)
	ih   = rand(1:p.nh)
	is   = rand(1:p.ns)
	iy   = rand(1:p.ny)
	ip   = rand(1:p.np)
	itau = rand(1:p.ntau)
	ij   = rand(1:p.nJ)
	it   = rand(1:(p.nt-1))

	subplot(231,projection="3d")
	mesh(reshape(m.v[ik,is,:,iy,ip,itau,:,ih,ij,it],p.nz,p.na))
	xlabel("z")
	ylabel("a")
	zlabel("value")
	title("value")

	subplot(232,projection="3d")
	mesh(reshape(m.sh[1,ik,is,:,iy,ip,itau,:,ih,ij,it],p.nz,p.na))
	xlabel("z")
	ylabel("a")
	zlabel("save index")
	title("savings for hh=1")

	subplot(233,projection="3d")
	mesh(reshape(m.ch[1,ik,is,:,iy,ip,itau,:,ih,ij,it],p.nz,p.na))
	xlabel("x")
	ylabel("a")
	zlabel("consumption")
	title("consumption for hh=1")

	subplot(234,projection="3d")
	mesh(reshape(m.rho[ik,is,:,iy,ip,itau,:,ih,ij,it],p.nz,p.na))
	xlabel("x")
	ylabel("a")
	zlabel("prob of moving")
	title("prob of moving j=>k:\n$ij => $ik")
	println(reshape(m.rho[ik,is,:,iy,ip,itau,:,ih,ij,it],p.nz,p.na))

	subplot(235,projection="3d")
	# contour(1:p.na,1:p.nz,reshape(m.dh[iy,:,:,ih,itau,ij,it],p.nz,p.na),	3)
	mesh(reshape(m.dh[ik,:,iy,is,ip,itau,:,ih,ij,it],p.nz,p.na))
	xlabel("x")
	ylabel("a")
	zlabel("housing choice")
	title("housing choice")

	suptitle("model at index(ik,is,iy,ip,itau,ij,it)=($ik,$is,$iy,$ip,$ih,$itau,$ij,$it)")

end


# select n individuals from simulation to export
# data to ggplot for plotting
function simexport(sim::DataFrame,n::Int,file::String)

	# choose n random individs
	nr = rand(1:maximum(sim[:id]),n)
	sim  = sim[findin(sim[:id],nr),:] 

	writetable(file,sim)
end

function simexport(sim::DataFrame,id::Array{Int,1},file::String)

	sim  =  getID(sim,id)

	writetable(file,sim)
end


# plot simulation histories

function simplot(sim::DataFrame,n::Int)

	# choose n random individs
	# get real random numbers
	srand(round(Integer,time()))

	nr = sample(unique(sim[:id]),n)


	sim  = sim[findin(sim[:id],nr),:] 

	# fig = PyPlot.figure()

	gdf = groupby(sim,:id)

	subplot(4,4,1)
	ylim((0.0,maximum(sim[:income])+5))
	for sdf in gdf
		plot(sdf[:age],sdf[:income])
	end
	title("income")

	subplot(4,4,2)
	ylim((0.0,maximum(sim[:income])+5))
	for sdf in gdf
		plot(sdf[:age],sdf[:cons])
	end
	title("cons")

	subplot(4,4,3)
	for sdf in gdf
		plot(sdf[:age],sdf[:a])
	end
	title("assets")

	subplot(4,4,4)
	for sdf in gdf
		plot(sdf[:age],sdf[:save])
	end
	title("save")

	subplot(4,4,5)
	ylim((-0.1,1.1))
	for sdf in gdf
		plot(sdf[:age],sdf[:h])
	end
	title("own")

	subplot(4,4,6)
	for sdf in gdf
		plot(sdf[:age],sdf[:wealth])
	end
	title("wealth")

	subplot(4,4,7)
	ylim((0.5,9.5))
	for sdf in gdf
		plot(sdf[:age],sdf[:j])
	end
	title("current location")

	subplot(4,4,8)
	for sdf in gdf
		plot(sdf[:age],sdf[:y])
	end
	title("region income")

	subplot(4,4,9)
	for sdf in gdf
		plot(sdf[:age],sdf[:p])
	end
	title("region price")

	subplot(4,4,10)
	for sdf in gdf
		plot(sdf[:age],sdf[:v])
	end
	title("lifetime utility(v)")

	subplot(4,4,11)
	ylim((-0.1,1.1))
	for sdf in gdf
		plot(sdf[:age],sdf[:kids])
	end
	title("kids")

	subplot(4,4,12)
	ylim((0.5,9.5))
	for sdf in gdf
		plot(sdf[:age],sdf[:moveto])
	end
	title("moveto")

	subplot(4,4,13)
	ylim((-0.1,1.1))
	for sdf in gdf
		plot(sdf[:age],sdf[:hh])
	end
	title("hchoice")

	subplot(4,4,14)
	ylim((0,46))
	for sdf in gdf
		plot(sdf[:age],sdf[:cohort])
	end
	title("Cohort")

	plts = Any[]
	ids = Int[]
	idcount = 1
	subplot(4,4,15)
	ylim((0.5,n + 0.5))
	for sdf in gdf
		plt, = plot(sdf[:age],ones(length(sdf[:age])).*idcount)
		push!(plts,plt)
		push!(ids,sdf[1,:id])
		idcount += 1
	end
	legend(plts,["indiv $(ids[i])" for i=1:n],"lower right")
	title("legend")
	suptitle("ids: $nr")
end



function simplot(sim::DataFrame,id::Array{Int,1})

	# choose id individs

	sim  =  getID(sim,id)
	n = length(id)
	nr = id

	# fig = PyPlot.figure()

	gdf = groupby(sim,:id)

	subplot(4,4,1)
	ylim((0.0,maximum(sim[:income])+5))
	for sdf in gdf
		plot(sdf[:age],sdf[:income])
	end
	title("income")

	subplot(4,4,2)
	ylim((0.0,maximum(sim[:income])+5))
	for sdf in gdf
		plot(sdf[:age],sdf[:cons])
	end
	title("cons")

	subplot(4,4,3)
	for sdf in gdf
		plot(sdf[:age],sdf[:a])
	end
	title("assets")

	subplot(4,4,4)
	for sdf in gdf
		plot(sdf[:age],sdf[:save])
	end
	title("save")

	subplot(4,4,5)
	ylim((-0.1,1.1))
	for sdf in gdf
		plot(sdf[:age],sdf[:h])
	end
	title("own")

	subplot(4,4,6)
	for sdf in gdf
		plot(sdf[:age],sdf[:wealth])
	end
	title("wealth")

	subplot(4,4,7)
	ylim((0.5,9.5))
	for sdf in gdf
		plot(sdf[:age],sdf[:j])
	end
	title("current location")

	subplot(4,4,8)
	for sdf in gdf
		plot(sdf[:age],sdf[:y])
	end
	title("region income")

	subplot(4,4,9)
	for sdf in gdf
		plot(sdf[:age],sdf[:p])
	end
	title("region price")

	subplot(4,4,10)
	for sdf in gdf
		plot(sdf[:age],sdf[:v])
	end
	title("lifetime utility(v)")

	subplot(4,4,11)
	ylim((-0.1,1.1))
	for sdf in gdf
		plot(sdf[:age],sdf[:kids])
	end
	title("kids")

	subplot(4,4,12)
	ylim((0.5,9.5))
	for sdf in gdf
		plot(sdf[:age],sdf[:moveto])
	end
	title("moveto")

	subplot(4,4,13)
	ylim((-0.1,1.1))
	for sdf in gdf
		plot(sdf[:age],sdf[:hh])
	end
	title("hchoice")

	subplot(4,4,14)
	ylim((0,46))
	for sdf in gdf
		plot(sdf[:age],sdf[:cohort])
	end
	title("Cohort")
	
	plts = Any[]
	ids = Int[]
	idcount = 1
	subplot(4,4,15)
	ylim((0.5,n + 0.5))
	for sdf in gdf
		plt, = plot(sdf[:age],ones(length(sdf[:age])).*idcount)
		push!(plts,plt)
		push!(ids,sdf[1,:id])
		idcount += 1
	end
	legend(plts,["indiv $(ids[i])" for i=1:n],"lower right")
	title("legend")
	suptitle("ids: $nr")
end



function plotSlices(s::Slice)

	kk = sort(collect(keys(s.res)))

	fig, axes = plt.subplots(3,3,figsize=(15,11))
	currfig = 0

	for k in kk
		currfig += 1
		ax = axes[currfig]
		r = get(s,k,:value)
		ax[:plot](r[:x],r[:y],"b-",lw=2)
		ax[:set_xlabel]("$k")
		ax[:set_ylabel]("Value")
		ax[:grid]()
	end

	return fig
end



