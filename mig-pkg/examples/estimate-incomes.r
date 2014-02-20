
# make summary statistics SIPP

library(migration)

if (Sys.info()['login'] == "florianoswald" ){
	path <- "~/Dropbox/mobility"
} else {
	path <- "C:/Users/florian_o/Dropbox/mobility/"
}
load(file.path(path,"SIPP/SippIncome.RData"))


RE.HHincome(dat=income,path=file.path(path,"output/model/BBL")
