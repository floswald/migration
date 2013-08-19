


# build dataset to explore.

# catalogue 91
#    V19349   'AGE OF 1991 HEAD        '
#    V20198   'COMPLETED ED-HD 91      '
#    V20178   'TOTAL HEAD LABOR Y 90   '   TLOC=  2178- 2183                    
#    V19372   'A15 OWN/RENT OR WHAT    '
#    V19024   '1991 HOUSE VALUE  (A16) '  
#    V19026   '1991 REM MORT PRIN (A20)'   
#    V19028   'TOTAL ANN MRTG PMT (A21)'   
#    V19033   'ANN RENT (A27)          '   
#    V19393   'B1 EMPLOYMENT STATUS-HD '   
#    V19355   'A3 MARITAL STATUS'
#    V19352   '# CHILDREN IN FU        '   
#    V20190   'STATE CODE (FIPS)       '  
#    V20091   'L8-9 STATE GREW UP    HD'   
#    V19387   'A38 MOVED SINCE SPG 90? '   
#    V19389   'A40 WHY MOVED           '   
#    V19391   'A42 LIKELIHOOD OF MOVING'   


# 92
#    V20651   'AGE OF 1992 HEAD        '   TLOC=   874-  875    MD=99           
#    V21504   'COMPLETED ED-HD 92      '   TLOC=  2254- 2255    MD=99  
#    V21484   'TOTAL HEAD LABOR Y 91   '   TLOC=  2191- 2196                    
#    V20672   'A19 OWN/RENT OR WHAT    '   TLOC=   903                          
#    V20324   '1992 HOUSE VALUE  (A20) '   TLOC=    44-   49                    
#    V20326   '1992 REM MORT PRIN (A24)'   TLOC=    51-   56                    
#    V20328   'TOTAL ANN MRTG PMT (A25)'   TLOC=    58-   62         
#    V20333   'ANN RENT (A31)          '   TLOC=    74-   78                    
#    V20693   'B1 EMPLOYMENT STATUS-HD '   TLOC=   926
#    V20657   'A3 MARITAL STATUS       '
#    V20654   '# CHILDREN IN FU        '   TLOC=   879-  880                    
#    V20303   'CURRENT STATE           '   TLOC=     9-   10    MD=99           
#    V21397   'M8-9 STATE GREW UP    HD'   TLOC=  2019- 2020    MD=99    
#    V20687   'A42 MOVED SINCE SPG 91? '   TLOC=   919    MD=9                  
#    V20689   'A44 WHY MOVED           '   TLOC=   922    MD=9                  
#    V20691   'A46 LIKELIHOOD OF MOVING'   TLOC=   924    MD=9              

# 93
# V22406    "AGE OF 1993 HEAD"
# V23333    "COMPLETED ED-HD 1993"
# V23323    "HD 1992 TOTAL LABOR INCOME"
# V21610    "A20 1993 HOUSE VALUE"
# V21612    "A24 REMAINING MORTGAGE PRINCIPAL"
# V21615    "TOTAL ANNUAL MORTGAGE PAYMENT (A25)"
# V21622    "ANNUAL RENT (A31)"
# V22448    "B1 HEAD 1993 EMPLOYMENT STATUS"
# V22412    "A3 MARITAL STATUS"
# V22409    "# CHILDREN IN FAMILY UNIT"
# V21603    "CURRENT STATE" 
# V23254    "L8 STATE HD GREW UP"
# V22441    "A42 WTR MOVED SINCE SPRING 1992"
# V22444    "A44 WHY MOVED"
# V22446    "A46 LIKELIHOOD OF MOVING"

# 94
# ER2007    "AGE OF HEAD "
# ER4158    "COMPLETED ED-HD"
# ER4140    "LABOR INCOME OF HEAD-1993"
# ER2033    "A20 HOUSE VALUE"
# ER2037    "A24 REM PRINCIPAL MOR 1"
# ER2039    "A25 MNTHLY PMTS MOR   1"
# ER2049    "A31 DOLLARS RENT"
# ER2050    "A31 DOLLLARS PER WHAT"
# ER2068    "B1 EMPLOYMENT STATUS-HD"
# ER2014    "HEAD MARITAL STATUS"
# ER2010    "# CHILDREN IN FU"
# ER4157    "CURRENT STATE"
# NA        "STATE HD GREW UP"
# ER2062    "A42 MOVED SINCE SPG?"
# ER2065    "A44 WHY MOVED 1ST"
# ER4004    "N3 PCT CHN MOVE"


# 95
ER5006 "Age of Head"
ER6998    "COMPLETED ED-HD"
ER6980    "LABOR INCOME OF HEAD-1994"
ER5032    "A20 HOUSE VALUE"
ER5036    "A24 REM PRINCIPAL MOR 1"
ER5038    "A25 MNTHLY PMTS MOR   1"
ER5048    "A31 DOLLARS RENT"
ER5049    "per what"
ER5067    "B1 EMPLOYMENT STATUS-HD"
ER5013    "HEAD MARITAL STATUS"
ER5009    "# CHILDREN IN FU"
ER6997    "CURRENT STATE"
NA        "STATE HD GREW UP"
ER5061    "A42 MOVED SINCE SPG?"
ER5064    "Why moved"
NA        "A46 LIKELIHOOD OF MOVING"





library(psidR)

yrs <- c(1991:1997,seq(1999,2011,by=2))

famv <- data.frame(year = yrs,
				  age         = c("V19349"),
				  educ        = c("V20198"),
				  income      = c("V20178"),
				  own         = c("V19372"),
				  value       = c("V19024"),
				  mortg       = c("V19026"),
				  mortpay     = c("V19028"),
				  rent        = c("V19033"),
				  rentper     = c(NA),
				  empstat     = c("V19393"),
				  marstat     = c("V20217"), # marital status at time of interview
				  numkids     = c("V19352"),
				  state       = c("V20190"),	# current state of residence
				  home        = c("V20083"),	# state where head grew up
				  moved       = c("V19387"),	# whether moved since last year
				  why.moved   = c("V19389"),
				  likely.move = c("V19391"),
				  wealth      = c(NA)) # constructed wealth w/o equity


indv <- data.frame(year = yrs,
				   weight = c())


d <- 



