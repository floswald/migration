

# Analyse the 2008 SIPP panel.
library(data.table)
library(ggplot2)



setwd("~/git/migration/data/")

if( !file.exists( "SIPP2008.RData" ) ){

	library(foreign)
	d <- data.table(read.dta("~/datasets/SIPP/2008/dta/core_and_topical/core_top.dta"))


