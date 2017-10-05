

# model reporting
# ===============


# report some solution characteristics
function solReport()
	p = Param(2)
	m = Model(p)
	mig.solve!(m,p)

	# report

	# look at rho at different aggregate states
end

# illustrate rho function
function plotRho()
	f(x,y)=exp(x - log(exp(x) + exp(y)))
	df = DataFrame(x=linspace(-3.0,4.0,100),P1 = f(linspace(-3.0,4.0,100),1.0))
	ticks = [-3.0,-1.3,0.0,1.0,2.0,4.4]
	plot(df,x="x",y="P1",xintercept=[1.0],Geom.line,Geom.vline(color="red"),Guide.xticks(ticks=ticks),Theme(default_color=color("black")),Guide.title("Choice Probabilities"),Guide.xlabel("v1 - v2"),Guide.ylabel("Prob(v1|v2=1.0)") )
end	


function simReport(s::DataFrame)

	s = @where(s,(.!isna.(:cohort) .& (:year.>1997)));
	mp = @select(s,prob=mean(:cumprob),)
	mp_own = @by(s,:own,prob=mean(:cumprob))

    # mp_ass = transform(s,assets = 0.0)
    # qt_a = quantile(s[:a],(0:20)./20)
    # for i in 1:size(mp_ass,1)
    #     mp_ass[i,:assets] = searchsortedfirst(qt_a,mp_ass[i,:a])
    # end
    cut_ass = (0:20)./20
    mp_ass = @linq s |>
    	@transform(assets=cut(:a,round(quantile(:a,^(cut_ass)),1))) |>
        # @transform(assets=searchsortedlast(:a,quantile(:a,(0:20)./20)))
        @by([:assets,:own],prob=mean(:cumprob)) |>
        @transform(Type="Owner")
    mp_ass[!mp_ass[:own],:Type] = "Renter"
    mp_ass[:quantile] = 0
    mp_ass[mp_ass[:own],:quantile] = 1:size(mp_ass[mp_ass[:own],:],1)


    mp_ass_age = @linq s |>
        @transform(assets=cut(:a,round(quantile(:a,(0:20)./20),1))) |>
        @by([:assets,:own,:age],prob=mean(:cumprob))

    mp_wealth = @linq s |>
        @where(:wealth.>-100)|>
        @transform(wealth=cut(:wealth,round(quantile(:wealth,(0:100)./100),1)))|>
        @by([:wealth,:own],prob=mean(:cumprob,Weights(:density)),m_prob=mean(:move,Weights(:density)),m_sum=sum(:move),n_all=length(:move))|>
        @transform(Type="Owner")

    mp_wealth[!mp_wealth[:own],:Type] = "Renter"
    writetable(joinpath(ENV["HOME"],"Dropbox/research/mobility/output/model/fit/mp_wealth.csv"),mp_wealth)

    mp_wealth2 = @linq s |>
        @where(:wealth.>-100)|>
        @transform(wealth=cut(:wealth,round(quantile(:wealth,(0:100)./100),1)))|>
        @by(:wealth,prob=mean(:cumprob,Weights(:density)),m_prob=mean(:move,Weights(:density)),m_sum=sum(:move),n_all=length(:move),n_own=mean(:h,Weights(:density)))

    writetable(joinpath(ENV["HOME"],"Dropbox/research/mobility/output/model/fit/mp_wealth2.csv"),mp_wealth)
    mp_wealth[!mp_wealth[:own],:Type] = "Renter"

    mp_wealth = @linq s |>
        @where(:wealth2.>-100) |>
        @transform(wealth=cut(:wealth2,round(quantile(:wealth2,(0:100)./100),1))) |>
        @by([:wealth,:own],prob=mean(:cumprob,Weights(:density)),m_prob=mean(:move,Weights(:density)),m_sum=sum(:move),n_all=length(:move)) |>
        @transform(Type="Owner")
    mp_wealth[!mp_wealth[:own],:Type] = "Renter"

    mp_inc = @linq s |>
    	@transform(inc_bin=cut(:income,round(quantile(:income,(0:20)./20),1))) |>
    	@by([:inc_bin,:own],prob=mean(:cumprob))

	mp_z = @linq s |>
    	@transform(z_bin=cut(:z,round(quantile(:z,(0:20)./20),1))) |>
    	@by([:z_bin,:own],prob=mean(:cumprob))

    zdist_move = @linq s |>
        @transform(z_bin=cut(:z,round(quantile(:z,(0:20)./20),1))) |>
        @by([:move,:own],z10=quantile(:z,0.1),z30=quantile(:z,0.3),z50=median(:z),z70=quantile(:z,0.7),z90=quantile(:z,0.9))

    # qregs_own = Dict()
    # qregs_age = Dict()
    # qregs_own_move = Dict()
    qregs = Dict()
    qreg_coefs = DataFrame(τ=Float64[],own=Float64[],move=Float64[],age=Float64[])
    for tau in 0.1:0.1:0.9
        info("tau = $tau")
        qregs["τ$tau"] = qreg(@formula(z ~ own + move + age),s,tau) 
        ct = coeftable(qregs["τ$tau"])
        est = coef(qregs["τ$tau"])
        nms = ct.rownms
        push!(qreg_coefs,vcat(tau, [est[nms .== coef] for coef in ["own: true","move: true","age"]]...))
    end

	mp_p = @linq s |>
    	@transform(p_bin=cut(:p,round(quantile(:p,(0:20)./20),1))) |>
    	@by([:p_bin,:own],prob=mean(:cumprob))

    mp_age = @by(s,:age,prob=mean(:cumprob))
    m_age = @by(s,:age,prob=mean(:move))

    mp_ass = mp_ass[2:end,:]
    mp_inc = mp_inc[2:end,:]

    writetable(joinpath(ENV["HOME"],"Dropbox/research/mobility/output/model/fit/mp_ass.csv"),mp_ass)
    writetable(joinpath(ENV["HOME"],"Dropbox/research/mobility/output/model/fit/mp_inc.csv"),mp_inc)
    writetable(joinpath(ENV["HOME"],"Dropbox/research/mobility/output/model/fit/mp_z.csv"),mp_z)

    # plots
    # =====

    # myt = Theme(line_width=0.5mm,major_label_color=color("black"),minor_label_color=color("black"))
    # pinc = plot(mp_inc,x="inc_bin",y="prob",Geom.line,color="own",myt,Guide.title("Probability of Moving by income"))
    # pz = plot(mp_z,x="z_bin",y="prob",Geom.line,color="own",myt,Guide.title("Probability of Moving by income shock"),Scale.discrete_color_manual("red","blue"))
    # pass=plot(mp_ass,x="assets",y="prob",Geom.line,color="Type",myt,Guide.title("Probability of moving by assets"),Scale.discrete_color_manual("blue","red"))
    # pwealth=plot(mp_wealth,x="wealth",y="prob",Geom.point,color="n_own",myt,Guide.title("Probability of moving by wealth"))
    # pwealth=plot(mp_wealth,x="wealth",y="prob",Geom.line,color="n_own",myt,Guide.title("Probability of moving by wealth"),Scale.discrete_color_manual("blue","red"))
    # pass0=plot(@where(mp_ass_age,:own),x="assets",y="prob",Geom.line,color="age",myt,Guide.title("Probability of Moving by assets"))
    # pass1=plot(@where(mp_ass_age,!:own),x="assets",y="prob",Geom.line,color="age",myt,Guide.title("Probability of Moving by assets"))
    # pp  =plot(mp_p,x="p_bin",y="prob",Geom.line,color="own",myt,Guide.title("Probability of Moving by House Price"))

    # wealth_hist = plot(@where(s,:wealth.>10.0),x="wealth",color="own",Geom.histogram)

    # draw(PDF(joinpath(ENV["HOME"],"Dropbox/research/mobility/output/model/properties/mp_assets.pdf"),6inch,4inch),pass)
    # draw(PDF(joinpath(ENV["HOME"],"Dropbox/research/mobility/output/model/properties/mp_income.pdf"),6inch,4inch),pinc)
    # draw(PDF(joinpath(ENV["HOME"],"Dropbox/research/mobility/output/model/properties/mp_pz.pdf"),6inch,4inch),pz)

    # (pinc,pz,pass,pp)
    (mp_z,mp_ass,mp_inc,qregs,qreg_coefs)
end





	











