
library(ggplot2)
library(reshape2)

# plot euler equation

# under what restrictions is euler equation
# 1) finite
# 2) straddling 0?

# want a condition on V'(a) and u'(c) that determines if euler equation can be solved 

# for simplicity take V(a) = log(a) and CRRA period utility
# you basically need that V'(a) decreases fast as a -> min(a)


# 2 equivalent ways of definining the problem

# a) as a function of savings:
# V(a) = max_{alow<a'<a} u(a - a') + beta EV(R*a')
# foc: -u'(c) + beta*R*EV'(R*a') = 0

# b) as a function of consumption:
# V(a) = max_{0<c<a} u( c ) + beta EV( R*(a - c) )
# foc: u'(c) - beta*R*EV'( R*(a-c) )

mu <- function(x,gamma) { 1/(x^gamma) }

# dummy future value function.
# i just assume it's shape here.
vprime <- function(x,zeta) { 1/(x^zeta) }

obja <- function(assets, saving, beta, R, gamma, zeta){

	return( mu( assets - saving, gamma) - beta * R * vprime( R* saving,zeta) )

}

objb <- function(assets, cons, beta, R, gamma, zeta){

	saving <- R*(assets - cons)

	return( mu( cons, gamma) - beta * R * vprime(saving,zeta) )

}

# define parameters
gam <- 1.4
zet <- 1.0
R <- 1.04
beta <- 0.95


d <- data.frame(res=0,x=0,value=0,control="savings")
assets <- 1:10
for (i in 1:10){
	dtmp <- data.frame(res=assets[i],x=seq(0.05,assets[i]-0.01,length=50))
	dtmp$value = obja(assets[i],dtmp$x,beta,R,gam,zet)
	dtmp$control  = "savings"
	d <- rbind(d,dtmp)
}

for (i in 1:10){
	dtmp <- data.frame(res=assets[i],x=seq(0.05,assets[i]-0.01,length=50))
	dtmp$value = objb(assets[i],dtmp$x,beta,R,gam,zet)
	dtmp$control  = "consumption"
	d <- rbind(d,dtmp)
}

d <- d[-1,]
ggplot(d,aes(x=x,y=value,color=factor(res))) + geom_line() + facet_wrap(~control,scales="free_y")


