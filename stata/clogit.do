


use "/Users/florianoswald/Dropbox/mobility/output/model/BBL/logit.dta",clear


*** if first time, do this
insheet using "/Users/florianoswald/Dropbox/mobility/output/model/BBL/logit.csv",comma
gen cchoice = 0
replace cchoice = 1 if choice=="TRUE"
gen sstay = 0
replace sstay = 1 if stay=="TRUE"

clogit choice distance logHHincome wealth age HValue96, group(upid)

matrix X=e(b)
matrix V1=e(V)

