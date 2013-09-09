Empirical look at interstate migration PSID
========================================================

This is about PSID data 1994-2011.




## Overview

Sample Characteristic | Value
--------------------- | -------------------
Number of unique inds | 10802
Number of years       | 11
range of years        | [1994, 2011]
Number of ind-years   | 71808

## Statistics by year

All in current dollars.


```
##          y prop.own HValue MDebt  moved mvd.state mvd.region
## 1994 47847   0.6714  76593 25686 0.1746   0.00000    0.00000
## 1995 48769   0.6708  77930 26261 0.1532   0.01844    0.01380
## 1996 49321   0.6637  79844 26248 0.1507   0.02311    0.01536
## 1997 49496   0.6483  80527 28763 0.1614   0.02650    0.01724
## 1999 57109   0.6555  91556 33315 0.2427   0.03960    0.02723
## 2001 64205   0.6622 108274 38092 0.2388   0.03963    0.02592
## 2003 62455   0.6660 126874 45325 0.2940   0.03868    0.02701
## 2005 68687   0.6548 160764 53420 0.3067   0.04405    0.02937
## 2007 71328   0.6406 179269 59472 0.3148   0.03972    0.02675
## 2009 75343   0.6197 158137 59809 0.3054   0.04022    0.02718
## 2011 73846   0.6352 154284 59292 0.2714   0.04595    0.03140
```


## Median Wealth and consumption by year

Current Dollars.





```
##     year assets  cons
##  1: 1994  25000     0
##  2: 1995      0     0
##  3: 1996      0     0
##  4: 1997      0     0
##  5: 1999  23500 42399
##  6: 2001  26500 47460
##  7: 2003  26000 49038
##  8: 2005  25500 62893
##  9: 2007  26000 68649
## 10: 2009  23365 67644
## 11: 2011  22500     0
```







## how many periods do people stay in the sample?

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 



## Moving patterns by state

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 


## Moving patterns by Region

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 



## Return Migration

What is the empirical distribution of frequency of moves?

What is the average time between moves

what is the proportion of movers moving back home

What is the proportion of movers moving back to a previous location?


## Determinants of interstate moves

### Sample Proportion Interstate Move by Age and Tenure

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 


### Sample Proportion InterREGIONAL Move by Age and Tenure

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 



### Self-reported reasons of ALL moves. % of population


```
##              own     FALSE      TRUE
## why.moved                           
## not moved        19.451984 57.114902
## new job           1.280649  0.459859
## more housing      0.730247  0.261221
## less housing      2.473172  1.439914
## want to own       1.302831  0.373488
## better area       1.543192  2.846634
## forced            1.484980  0.532980
## ambiguous         2.561424  0.493280
## homeless          4.175704  1.424385
## DK                0.040786  0.008369
```


### Self-reported reasons of interstate moves. % of population


```
##              own   FALSE    TRUE
## why.moved                       
## not moved         4.3038  8.0854
## new job          14.9134  6.8071
## more housing      3.3102  1.4795
## less housing      3.3190  1.9000
## want to own       2.5850  0.8047
## better area       3.4917  4.5927
## forced            8.4843  3.9380
## ambiguous         8.0663  2.9054
## homeless         13.4794  7.3448
## DK                0.1891  0.0000
```


### Self-reported reasons of interREGIONAL moves. % of population


```
##              own   FALSE    TRUE
## why.moved                       
## not moved         3.6738  8.5942
## new job          15.5235  7.0934
## more housing      3.4126  1.3552
## less housing      3.4106  1.5559
## want to own       2.7867  0.6475
## better area       3.2886  3.6758
## forced            9.7100  3.8862
## ambiguous         7.0894  2.8027
## homeless         13.7825  7.4405
## DK                0.2709  0.0000
```


### Interstate Moves vs employment. % of population


```
##               inter     FALSE      TRUE
## empstat                                
## working             64.291292  2.285202
## temp laid off        0.472519  0.002642
## unemployed           3.544628  0.199220
## retired             20.896243  0.615282
## disabled             3.403545  0.105615
## house keeping        2.641095  0.048870
## student              0.814008  0.078207
## other                0.565766  0.035865
```


### Interregional Moves vs employment. % of population


```
##               interreg     FALSE      TRUE
## empstat                                   
## working                65.088114  1.488380
## temp laid off           0.473555  0.001607
## unemployed              3.603011  0.140838
## retired                21.051473  0.460052
## disabled                3.439419  0.069741
## house keeping           2.656990  0.032975
## student                 0.831556  0.060658
## other                   0.573417  0.028214
```



### Interstate Moves vs marital status. % of population


```
##               inter   FALSE    TRUE
## marstat                            
## married             49.8964  1.6016
## never married       18.0436  0.9073
## widowed             11.6273  0.2368
## divorced            14.4272  0.4931
## separated            2.6319  0.1348
```


### Interregional Moves vs marital status. % of population


```
##               interreg    FALSE     TRUE
## marstat                                 
## married                50.44535  1.05266
## never married          18.32198  0.62889
## widowed                11.69029  0.17387
## divorced               14.58477  0.33552
## separated               2.67226  0.09441
```


## Probit: Determinants of all moves. 


```
## 
## ===========================================
##              estimates     marginal.effects
## -------------------------------------------
## (Intercept)      1.16 ***      0.39 ***    
##                 (0.03)        (0.01)       
## age             -0.03 ***     -0.01 ***    
##                 (0.00)        (0.00)       
## I(age^2)         0.00 ***      0.00 ***    
##                 (0.00)        (0.00)       
## ownTRUE         -0.93 ***     -0.31 ***    
##                 (0.02)        (0.01)       
## educ             0.00          0.00        
##                 (0.00)        (0.00)       
## incomeFAM        0.00          0.00        
##                 (0.00)        (0.00)       
## dnumkids         0.12 ***      0.04 ***    
##                 (0.01)        (0.00)       
## wealth           0.00          0.00        
##                 (0.00)        (0.00)       
## divorceTRUE      0.60 ***      0.23 ***    
##                 (0.07)        (0.03)       
## -------------------------------------------
## Deviance     50112.88      50112.88        
## Dispersion       1.01          1.01        
## Num. obs.    53645         53645           
## ===========================================
## *** p < 0.001, ** p < 0.01, * p < 0.05
```

```
## Rsquared:  0.2051
```


## Probit: Determinants of interstate moves. 


```
## 
## ===========================================
##              estimates     marginal.effects
## -------------------------------------------
## (Intercept)     -1.35 ***     -0.11 ***    
##                 (0.04)        (0.00)       
## age             -0.01 ***      0.00 ***    
##                 (0.00)        (0.00)       
## I(age^2)         0.00 ***      0.00 ***    
##                 (0.00)        (0.00)       
## ownTRUE         -0.47 ***     -0.04 ***    
##                 (0.03)        (0.00)       
## educ             0.00 ***      0.00 ***    
##                 (0.00)        (0.00)       
## incomeFAM        0.00 ***      0.00 ***    
##                 (0.00)        (0.00)       
## dnumkids         0.03          0.00        
##                 (0.02)        (0.00)       
## wealth           0.00          0.00        
##                 (0.00)        (0.00)       
## divorceTRUE      0.29 **       0.03 *      
##                 (0.10)        (0.01)       
## -------------------------------------------
## Deviance     16610.65      16610.65        
## Dispersion       0.99          0.99        
## Num. obs.    53645         53645           
## ===========================================
## *** p < 0.001, ** p < 0.01, * p < 0.05
```

```
## Rsquared:  0.04491
```


## Probit: Determinants of interregional moves. 


```
## 
## ===========================================
##              estimates     marginal.effects
## -------------------------------------------
## (Intercept)     -1.57 ***     -0.09 ***    
##                 (0.05)        (0.00)       
## age              0.00 ***      0.00 ***    
##                 (0.00)        (0.00)       
## I(age^2)         0.00 ***      0.00 **     
##                 (0.00)        (0.00)       
## ownTRUE         -0.46 ***     -0.03 ***    
##                 (0.03)        (0.00)       
## educ             0.00 **       0.00 **     
##                 (0.00)        (0.00)       
## incomeFAM        0.00 ***      0.00 ***    
##                 (0.00)        (0.00)       
## dnumkids         0.03          0.00        
##                 (0.02)        (0.00)       
## wealth           0.00          0.00        
##                 (0.00)        (0.00)       
## divorceTRUE      0.27 *        0.02        
##                 (0.12)        (0.01)       
## -------------------------------------------
## Deviance     12364.03      12364.03        
## Dispersion       1.00          1.00        
## Num. obs.    53645         53645           
## ===========================================
## *** p < 0.001, ** p < 0.01, * p < 0.05
```

```
## Rsquared:  0.03897
```


## Average wage growth

Wage growth is higher for movers.

Sample |  growth movers (owner) | growth movers (renter)
------ | ------------------------- | -------------------------
all    | 0.0376 | 0.0334
inter state    | 0.0436 | 0.097
all region   | 0.0272 | 0.1272

## Linear Wage Equation

$$ \ln y_{it} = \beta_0 + \beta_1 \ln y_{it-1} + \beta_2 age_{it} + \beta_3 age^2_{it} + \beta_4 own_{it} + \beta_5 interreg_{it} + \gamma Z_{it} $$







## Price movement over time




![plot of chunk unnamed-chunk-24](figure/unnamed-chunk-24.png) 


## Price ranking among states

![plot of chunk unnamed-chunk-25](figure/unnamed-chunk-25.png) 

