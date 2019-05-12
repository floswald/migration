
function plot_copula(m::Model)
	n = length(m.gridsXD["zmarginal"])
	contour(m.gridsXD["zmarginal"],
		m.gridsXD["zmarginal"],
		reshape(m.copdf[:dens],n,n),
		c=ColorGradient([:blue,:red]),
		xlabel="z today",
		ylabel="z tomorrow",title="Movers z copula density")
	io = setPaths()
	savefig(joinpath(io["out_graphs"],"mover-copula.pdf"))
	heatmap(m.gridsXD["zmarginal"],
		m.gridsXD["zmarginal"],
		m.cop,
		# c=ColorGradient([:blue,:red]),
		xlabel="z today",
		ylabel="z tomorrow",title="Movers Transition Matrix")
	savefig(joinpath(io["out_graphs"],"mover-trans.pdf"))
end


function own_cond_tau()
	s = runSim()
	s = s[.!ismissing.(s[:cohort]),:];

	x = @linq s |>
       @where(:year .> 1996 ) |>
       @by([:tau,:age], o=mean(:own))

    @df x plot(:age, :o,group=:tau)

end

function plotOwnerWTP()

	# load data 
	io = setPaths()
	f = open(joinpath(io["out"],"ownersWTP.json"))
	d = JSON.parse(f)
	close(f)
	
	popweights = DataFrame(FileIO.load(joinpath(io["indir"],"prop.rda"))["prop"])
	sort!(popweights,:Division)

	# into dataframe
	df = DataFrame(div=Int[],comp=Float64[])
	for (k,v) in d
        push!(df,[v[1]["region"],v[1]["data"]["2"]["4"]["y"]["comp"]])
  	end
  	sort!(df,:div)

  	# plot
  	pl = bar(popweights[:Division],df[:comp],fill=:grey,leg=false,ylabel="1000 Dollars")

	path = joinpath(io["out_graphs"],"ownersWTP")
	fiend = Plots.backend() == Plots.PGFPlotsBackend() ? ".tex" : ".pdf"
	savefig(string(path,"age",fiend))
	return pl
end



function plotShockRegion(m::Model;save=true)

	pa = setPaths()

	y = @where(m.pred_ydf,:year.>1996)[[:year,:ENC]]
	p = @where(m.pred_pdf,:year.>1996)[[:year,:ENC]]
	y[:ENC_y_shock] = copy(y[:ENC])
	p[:ENC_p_shock] = copy(p[:ENC])
	names!(y,[:year,:ENC_y,:ENC_y_shock])
	names!(p,[:year,:ENC_p,:ENC_p_shock])
	y[y[:year].>1999,:ENC_y_shock] = y[y[:year].>1999,:ENC_y] .* 0.9
	p[p[:year].>1999,:ENC_p_shock] = p[p[:year].>1999,:ENC_p] .* 0.94

	py = @df y plot(:year,[:ENC_y,:ENC_y_shock],line=(2,[:solid :dash],:black),title="Q",legend=false)
	pp = @df p plot(:year,[:ENC_p,:ENC_p_shock],line=(2,[:solid :dash],:black),title="P",legend=false)

	if save
		pgfplots()
		fi = joinpath(pa["outdir"],"shockregPaths.tex")
		pl = plot([py,pp]...)
		savefig(fi)
	else
		pl = plot([py,pp]...)
	end
	return pl

end

