


reg_vs_state_p <- function(){

	data(FHFA_states,package="EconData")
	data(FHFA_Div,package="EconData")

	yr = FHFA_states$yr
	qt = FHFA_states$qtr

	dyr = FHFA_Div$yr
	dqt = FHFA_Div$qtr

	# get long division denomination 
	dyr[,Div := "USA"]
	dyr[Division=="DV_PAC",Div := "Pacific"]
	dyr[Division=="DV_ENC",Div := "East North Central"]
	dyr[Division=="DV_ESC",Div := "East South Central"]
	dyr[Division=="DV_WNC",Div := "West North Central"]
	dyr[Division=="DV_WSC",Div := "West South Central"]
	dyr[Division=="DV_NE",Div := "New England"]
	dyr[Division=="DV_MT",Div := "Mountain"]
	dyr[Division=="DV_SA",Div := "South Atlantic"]
	dyr[Division=="DV_MA",Div := "Middle Atlantic"]
	dqt[,Div := "USA"]
	dqt[Division=="DV_PAC",Div := "Pacific"]
	dqt[Division=="DV_ENC",Div := "East North Central"]
	dqt[Division=="DV_ESC",Div := "East South Central"]
	dqt[Division=="DV_WNC",Div := "West North Central"]
	dqt[Division=="DV_WSC",Div := "West South Central"]
	dqt[Division=="DV_NE",Div := "New England"]
	dqt[Division=="DV_MT",Div := "Mountain"]
	dqt[Division=="DV_SA",Div := "South Atlantic"]
	dqt[Division=="DV_MA",Div := "Middle Atlantic"]

	dyr[,Division := Div]
	dqt[,Division := Div]

	dyr[,Div := NULL]
	dqt[,Div := NULL]

	# merge division ID into state level data
	setkey(yr, state)
	setkey(qt, state)

	data(US_states,package="EconData")
	setkey(US_states,state)
	yr = US_states[yr]
	qt = US_states[qt]

	# change names of price columns
	setnames(yr,"index_nsa","p_state")
	setnames(qt,"index_nsa","p_state")
	setnames(dyr,"index_nsa","p_division")
	setnames(dqt,"index_nsa","p_division")

	# merge division level prices into state level
	setkey(yr,Division,yr)
	setkey(qt,Division,Date)
	setkey(dyr,Division,yr)
	setkey(dqt,Division,Date)

    yr = dyr[yr]
    qt = dqt[qt]

    # clean up
    qt[,c("Reg_ID","Region","Div_ID","index_sa","FIPS","PSID","STATE","quarter.1","note") := NULL]
    yr[,c("Reg_ID","Region","Div_ID","index_sa","FIPS","PSID","STATE") := NULL]

    divs = unique(yr[,Division])

    m_yr = lapply(divs, function(x){ lm(p_state ~ p_division,data=yr[Division==x])})
    names(m_yr) = divs
    m_qt = lapply(divs, function(x){ lm(p_state ~ p_division,data=qt[Division==x])})
    names(m_qt) = divs

    s_yr = lapply(m_yr,summary)
    s_qt = lapply(m_qt,summary)

    rs_yr = lapply(s_yr,function(x) x$r.squared)
    rs_qt = lapply(s_qt,function(x) x$r.squared)

    df_rs = data.frame(yearly=unlist(rs_yr),quarterly=unlist(rs_qt))
    return(df_rs)


}





reg_vs_state_y <- function(){

	data(PersonalIncome,package="EconData",envir=environment())
	data(Population,package="EconData",envir=environment())

	setkey(pers_income_current,state,year)
	setkey(population,state,year)
	population[pers_income_current]
	pers_income_current[population]
	py = pers_income_current[population]
	py[,pcy := income / population]


	# real per capita income
	cpi = getCPI(base="2012",freq="yearly")
	setkey(py,year)
	py = py[cpi]
	py[,y_state := pcy / cpi2012]
	py
	py = py[complete.cases(py)]

	# merge divisoin
	data(US_states,package="EconData",envir=environment())
	US_states = US_states[,list(state,Division)]
	setkey(US_states,state)

	setkey(py,state)
	py = US_states[py]
	py[,Division := abbreviate(Division,minlength=3)]
	py[,wgt := population / .SD[,sum(population)],by=list(Division,year)]

	regs = py[,list(y_division = weighted.mean(y_state,wgt)),by=list(Division,year)]
	setkey(regs,year,Division)
	setkey(py,year,Division)

	p = regs[py]

    divs = unique(p[,Division])

    m_yr = lapply(divs, function(x){ lm(y_state ~ y_division,data=p[Division==x])})
    names(m_yr) = divs

    s_yr = lapply(m_yr,summary)

    rs_yr = lapply(s_yr,function(x) x$r.squared)

    df_rs = data.frame(yearly=unlist(rs_yr))

                                  

	yr = FHFA_states$yr
	qt = FHFA_states$qtr

	dyr = FHFA_Div$yr
	dqt = FHFA_Div$qtr

	# get long division denomination 
	dyr[,Div := "USA"]
	dyr[Division=="DV_PAC",Div := "Pacific"]
	dyr[Division=="DV_ENC",Div := "East North Central"]
	dyr[Division=="DV_ESC",Div := "East South Central"]
	dyr[Division=="DV_WNC",Div := "West North Central"]
	dyr[Division=="DV_WSC",Div := "West South Central"]
	dyr[Division=="DV_NE",Div := "New England"]
	dyr[Division=="DV_MT",Div := "Mountain"]
	dyr[Division=="DV_SA",Div := "South Atlantic"]
	dyr[Division=="DV_MA",Div := "Middle Atlantic"]
	dqt[,Div := "USA"]
	dqt[Division=="DV_PAC",Div := "Pacific"]
	dqt[Division=="DV_ENC",Div := "East North Central"]
	dqt[Division=="DV_ESC",Div := "East South Central"]
	dqt[Division=="DV_WNC",Div := "West North Central"]
	dqt[Division=="DV_WSC",Div := "West South Central"]
	dqt[Division=="DV_NE",Div := "New England"]
	dqt[Division=="DV_MT",Div := "Mountain"]
	dqt[Division=="DV_SA",Div := "South Atlantic"]
	dqt[Division=="DV_MA",Div := "Middle Atlantic"]

	dyr[,Division := Div]
	dqt[,Division := Div]

	dyr[,Div := NULL]
	dqt[,Div := NULL]

	# merge division ID into state level data
	setkey(yr, state)
	setkey(qt, state)

	data(US_states,package="EconData")
	setkey(US_states,state)
	yr = US_states[yr]
	qt = US_states[qt]

	# change names of price columns
	setnames(yr,"index_nsa","p_state")
	setnames(qt,"index_nsa","p_state")
	setnames(dyr,"index_nsa","p_division")
	setnames(dqt,"index_nsa","p_division")

	# merge division level prices into state level
	setkey(yr,Division,yr)
	setkey(qt,Division,Date)
	setkey(dyr,Division,yr)
	setkey(dqt,Division,Date)

    yr = dyr[yr]
    qt = dqt[qt]

    # clean up
    qt[,c("Reg_ID","Region","Div_ID","index_sa","FIPS","PSID","STATE","quarter.1","note") := NULL]
    yr[,c("Reg_ID","Region","Div_ID","index_sa","FIPS","PSID","STATE") := NULL]

    divs = unique(yr[,Division])

    m_yr = lapply(divs, function(x){ lm(p_state ~ p_division,data=yr[Division==x])})
    names(m_yr) = divs
    m_qt = lapply(divs, function(x){ lm(p_state ~ p_division,data=qt[Division==x])})
    names(m_qt) = divs

    s_yr = lapply(m_yr,summary)
    s_qt = lapply(m_qt,summary)

    rs_yr = lapply(s_yr,function(x) x$r.squared)
    rs_qt = lapply(s_qt,function(x) x$r.squared)

    df_rs = data.frame(yearly=unlist(rs_yr),quarterly=unlist(rs_qt))


}