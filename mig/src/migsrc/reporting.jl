

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

	s = @where(s,!isna(:cohort))

	mp = @> begin
       s
       @select(prob=mean(:cumprob),)
    end

	mp_own = @> begin
       s
       @by(:own,prob=mean(:cumprob))
    end

    mp_ass = @> begin
    	s
    	@transform(assets=cut(:a,round(quantile(:a,(0:20)./20),1)))
    	@by([:assets,:own],prob=mean(:cumprob))
    end

    mp_inc = @> begin
    	s
    	@transform(inc_bin=cut(:income,round(quantile(:income,(0:20)./20),1)))
    	@by([:inc_bin,:own],prob=mean(:cumprob))
    end

	mp_z = @> begin
    	s
    	@transform(z_bin=cut(:z,round(quantile(:z,(0:20)./20),1)))
    	@by([:z_bin,:own],prob=mean(:cumprob))
    end

	mp_p = @> begin
    	s
    	@transform(p_bin=cut(:p,round(quantile(:p,(0:20)./20),1)))
    	@by([:p_bin,:own],prob=mean(:cumprob))
    end

    mp_age = @> begin
    	s
    	@by(:age,prob=mean(:cumprob))
    end
    m_age = @> begin
    	s
    	@by(:age,prob=mean(:move))
    end

    mp_ass = mp_ass[2:end,:]
    mp_inc = mp_inc[2:end,:]

    writetable("/Users/florianoswald/Dropbox/mobility/output/model/fit/mp_ass.csv",mp_ass)
    writetable("/Users/florianoswald/Dropbox/mobility/output/model/fit/mp_inc.csv",mp_inc)
    writetable("/Users/florianoswald/Dropbox/mobility/output/model/fit/mp_z.csv",mp_z)

    myt = Theme(line_width=0.5mm,major_label_color=color("black"),minor_label_color=color("black"))
    pinc = plot(mp_inc,x="inc_bin",y="prob",Geom.line,color="own",myt,Guide.title("Probability of Moving by income"))
    pz = plot(mp_z,x="z_bin",y="prob",Geom.line,color="own",myt,Guide.title("Probability of Moving by income shock"))
    pass=plot(mp_ass,x="assets",y="prob",Geom.line,color="own",myt,Guide.title("Probability of Moving by assets"))
    pp  =plot(mp_p,x="p_bin",y="prob",Geom.line,color="own",myt,Guide.title("Probability of Moving by House Price"))

    draw(PDF("/Users/florianoswald/Dropbox/mobility/output/model/fit/mp_assets.pdf",6inch,4inch),pass)
    draw(PDF("/Users/florianoswald/Dropbox/mobility/output/model/fit/mp_income.pdf",6inch,4inch),pinc)
    draw(PDF("/Users/florianoswald/Dropbox/mobility/output/model/fit/mp_pz.pdf",6inch,4inch),pz)

    (pinc,pz,pass,pp)
end



function compareMC_hfriction()

	s0 = runObj(false)
	s0 = s0["moments"]

	# no owner MC
    # alpha_3 = 0
	p1 = Dict{ASCIIString,Float64}()
	p1["MC3"] = 0.0
	s1 = runObj(false,p1)
	s1 = s1["moments"]

	# no owner MC and no transaction cost :
    # alpha_3 = 0, phi = 0
	p1["phi"] = 0.0
	s2 = runObj(false,p1)
	s2 = s2["moments"]

	# no transaction cost: phi = 0
	pop!(p1,"MC3")
	s3 = runObj(false,p1)
	s3 = s3["moments"]

    pfun(x,y) = 100 * (x-y) / y

    d = Dict()
    d["own"] = ["base" => s0[:mean_own][1], "alpha" => s1[:mean_own][1], "phi" => s3[:mean_own][1], "alpha_phi" => s2[:mean_own][1]]

    d["move"] = ["base" => pfun(s0[:mean_move][1],s0[:mean_move][1]), "alpha" => pfun(s1[:mean_move][1],s0[:mean_move][1]), "phi" => pfun(s3[:mean_move][1],s0[:mean_move][1]), "alpha_phi" => pfun(s2[:mean_move][1],s0[:mean_move][1])]

    d["move_rent"] = ["base" => pfun(s0[:mean_move_ownFALSE][1],s0[:mean_move_ownFALSE][1]), "alpha" => pfun(s1[:mean_move_ownFALSE][1],s0[:mean_move_ownFALSE][1]), "phi" => pfun(s3[:mean_move_ownFALSE][1],s0[:mean_move_ownFALSE][1]), "alpha_phi" => pfun(s2[:mean_move_ownFALSE][1],s0[:mean_move_ownFALSE][1])]
   
    d["move_own"] = ["base" => pfun(s0[:mean_move_ownTRUE][1],s0[:mean_move_ownTRUE][1]), "alpha" => pfun(s1[:mean_move_ownTRUE][1],s0[:mean_move_ownTRUE][1]), "phi" => pfun(s3[:mean_move_ownTRUE][1],s0[:mean_move_ownTRUE][1]), "alpha_phi" => pfun(s2[:mean_move_ownTRUE][1],s0[:mean_move_ownTRUE][1])]

    indir, outdir = mig.setPaths()
    f = open(joinpath(outdir,"MC_hfriction.json"),"w")
    JSON.print(f,d)
    close(f)

	return(d)
end



	











