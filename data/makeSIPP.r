

# get a SIPP panel from the database file for each panel available
# constructed with http://www.asdfree.com/2013/02/analyze-survey-of-income-and-program.html


require(survey)
require(RSQLite)

setwd("~/datasets/SIPP/R")


# Setup up 2008 SIPP panel
# ========================

# note: I am not interested in variance estimation, so I don't
# bother with replicate weights. I want an accurate count of 
# observational units in each wave.

# unit of observation
# ===================

# I will focus on households
# on the reference person in particular

db <- dbConnect( SQLite(), "SIPP08.db")


# make a character vector containing the variables that should be kept from the core file (core keep variables)
core.kv <- 
	c( 'ssuid','shhadid','srefmon','rhcalmn','rhcalyr','tfipsst','tmovrflg','eoutcome','eppintvw','rhnf','errp','tmetro','etenure','epubhse','tmthrnt','rfid','efrefper','rfnkids','whfnwgt','tftotinc','epppnum','wpfinwgt','eenlevel','eeducate','eentaid','tage','esex','erace','ebornus','eafnow','ems','epdjbthn','ersnowrk','east3e')	

		# note: the 'wpfinwgt' column does not need to be kept for calendar year analyses --
		# all weights will come from external files, not from inside the core files
	
		# variables specific to this analysis:
		

# each core wave data file contains data at the person-month level.  in general, there are four records per respondent in each core wave data set.

# in order to create a file containing every individual's monthly observations throughout the calendar year,
# query each of the waves designated above, removing each record containing an rhcalyr (calendar year) matching the year designated above

#############################################
# access the appropriate core waves of data #

# which waves would you like to pull?  to pull an entire calendar year, you must pull each interview that overlaps the year you want
# to see which waves correspond with what months, see http://www.census.gov/sipp/usrguide/ch2_nov20.pdf
# pages 5, 6, 7, and 8 have sipp panels 2008, 2004, 2001, and 1996, respectively

# here's an example using the 2008 panel

# uncomment this line to pull waves necessary for 2009 calendar year
# and comment all other "waves <-" lines
# waves <- 2:5 ; year <- 2009 ; mainwgt <- 'lgtcy1wt' ; yrnum <- 1

# uncomment this line to pull waves necessary for 2010 calendar year:
# and comment all other "waves <-" lines
#waves <- 5:8 ; year <- 2010 ; mainwgt <- 'lgtcy2wt' ; yrnum <- 2

# uncomment this line to pull waves necessary for 2011 calendar year:
# and comment all other "waves <-" lines
# waves <- 8:11 ; year <- 2011 ; mainwgt <- 'lgtcy3wt' ; yrnum <- 3

waves <- 1:14

sql.string <- paste0( "select " , paste( core.kv , collapse = "," ) , " from w" , waves , " where srefmon == 4" )
x <- dbGetQuery( db , sql.string )
	
head(x)

# topical data migration
tm.kv <- c('ssuid','eentaid','epppnum','tprstate','eprevres','tbrstate','tmovyryr','toutinyr','tmovest','eprevten')

sql.string <- paste0( "select " , paste( tm.kv , collapse = "," ) , " from tm" , 2 )

tmig <- dbGetQuery( db, sql.string)

head(tmig)

y <- merge(x,tmig)
head(y)
