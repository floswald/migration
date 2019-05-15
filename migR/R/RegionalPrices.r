

#' Compare State vs Division Level p Indices
#' 
#' @details produces table B.6 in online appendix
#' @name reg_vs_state_p
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


	# change names of price columns
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

    # clean up
    qt[,c("Reg_ID","Region","Div_ID","index_sa","FIPS","PSID","STATE","note") := NULL]
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
    return(list(mods=m_yr,rsq=df_rs))
}




#' Compare State vs Division Level q Indices
#' 
#' @details produces table B.7 in online appendix
#' @name reg_vs_state_y
reg_vs_state_y <- function(){

	data(PersonalIncome,package="EconData",envir=environment())
	data(Population,package="EconData",envir=environment())

	setkey(pers_income,state,year)
	setkey(population,state,year)
	population[pers_income]
	pers_income[population]
	py = pers_income[population]
	py[,pcy := income / population]


	# real per capita income
	cpi = getCPI(base="2012",freq="yearly")
	setkey(py,year)
	py = py[cpi]
	py[,y_state := pcy / cpi2012]
	py
	py = py[complete.cases(py)]

	# merge divisoin
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
    return(list(mods=m_yr,rsq=df_rs))
}

#' Produce Table 6 in Main Text
#' 
#' Collects R2 from all regressions in \code{\link{reg_vs_state_y}} and \code{\link{reg_vs_state_p}}
#' @name both_prices_output
both_prices_output <- function(){
  p = reg_vs_state_p()
  y = reg_vs_state_y()
  
  loc = "~/Dropbox/research/mobility/output/data/FHFA"
  
  # save reg output
  texreg(p$mods,file=file.path(loc,"p-P-mods.tex"),table=FALSE,dcolumn=TRUE,booktabs=TRUE,use.packages=FALSE,stars=c(0.1,0.05,0.01),digits = 4,custom.model.names = abbreviate(names(p$mods),minlength = 3L))
  
  texreg(y$mods,file=file.path(loc,"q-Q-mods.tex"),table=FALSE,dcolumn=TRUE,booktabs=TRUE,use.packages=FALSE,stars=c(0.1,0.05,0.01),digits = 4,custom.model.names = abbreviate(names(y$mods),minlength = 3L))
  
  o = cbind(subset(p$rsq,select=yearly),y$rsq)
  names(o) <- c("$R^2: p_{st} \\sim p_{dt}$","$R^2: q_{st} \\sim q_{dt}$")
  print(xtable(o),sanitize.text.function=function(x){x},file=file.path(loc,"rsquareds.tex"),floating=FALSE,booktabs=TRUE)
  xtable(o)
}