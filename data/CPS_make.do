

** stata script to select variables from all march supplements in outdata/

clear
cap log close
set more off
cd ~/datasets/CPS/outdata

foreach yr in 03 04  {

	use cpsmar`yr'.dta,clear
	keep h_seq h_year hhinc h_tenure hh5to18 gestfips geco hmssz hmsa_r hccc_r ffpos fpersons fownu18 ftot_r fsup_wgt a_exprrp a_age a_maritl a_hga prdtrace a_lfsr a_mjind migsame mig_st placdscp gediv mig_div mig_mtr1 mig_mtr3 nxtres prdtrace a_clswkr

	** drop all  non-reference persons
	keep if a_exprrp < 3
	rename geco gtco
    rename hmssz gtcbsasz
	rename hmsa_r gtmetsta
	rename hccc_r gtcbsast

	save selected_cpsmar`yr'.dta,replace
}

foreach yr in 05 06 07 08 09 {

	use cpsmar`yr'.dta,clear
	keep h_seq h_year hhinc h_tenure hh5to18 gestfips gtco gtcbsasz gtmetsta gtcbsast ffpos fpersons fownu18 ftot_r fsup_wgt a_exprrp a_age a_maritl a_hga prdtrace a_lfsr a_mjind migsame mig_st mig_dscp gediv mig_div mig_mtr1 mig_mtr3 nxtres prdtrace a_clswkr

	keep if a_exprrp < 3

	save selected_cpsmar`yr'.dta,replace

}

foreach yr in 10 11 12 13 {

	use cpsmar`yr'.dta,clear
	keep h_seq h_year hhinc h_tenure hh5to18 gestfips gtco gtcbsasz gtmetsta gtcbsast ffpos fpersons fownu18 ftot_r fsup_wgt a_exprrp a_age a_maritl a_hga prdtrace a_lfsr a_mjind migsame mig_st mig_dscp gediv mig_div mig_mtr1 mig_mtr3 nxtres prdtrace a_clswkr

	keep if a_exprrp < 3

	save selected_cpsmar`yr'.dta,replace
}


use selected_cpsmar03.dta,clear
save selected.dta,replace

foreach yr in 04 05 06 07 08 09 10 11 12 13 {
	use selected.dta,clear
	append using selected_cpsmar`yr'.dta
	save selected.dta,replace
}
