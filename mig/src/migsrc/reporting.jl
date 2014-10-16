

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
    mp_z = mp_z[2:end,:]

    myt = Theme(line_width=0.5mm,major_label_color=color("black"),minor_label_color=color("black"))
    pinc = plot(mp_inc,x="inc_bin",y="prob",Geom.line,color="own",myt,Guide.title("Probability of Moving by income"))
    pz = plot(mp_z,x="z_bin",y="prob",Geom.line,color="own",myt,Guide.title("Probability of Moving by income shock"))
    pass=plot(mp_ass,x="assets",y="prob",Geom.line,color="own",myt,Guide.title("Probability of Moving by assets"))

    draw(PDF("/Users/florianoswald/Dropbox/mobility/output/model/fit/mp_assets.pdf",6inch,4inch),pass)
    draw(PDF("/Users/florianoswald/Dropbox/mobility/output/model/fit/mp_income.pdf",6inch,4inch),pinc)
    draw(PDF("/Users/florianoswald/Dropbox/mobility/output/model/fit/mp_pz.pdf",6inch,4inch),pz)

    (mp,mp_own,mp_ass,mp_age,m_age)
end














