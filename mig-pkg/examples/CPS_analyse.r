



d <- Clean.CPS()
des <- CPS.makeDesign(d)

tlist <- lapply(names(des$des),function(x) tabfun(x,des))
names(tlist) <- names(des$des)

tabs <- CPS.makeTabs(tlist)

# print to tables
CPS.print.tabs(tabs,yrs=2013)

# plot to file
CPS.plot.tabs(tabs)


# run a probit model analysis
# CAUTION: this model is not easy to interpret!
# this is an interview that asks CURRENT owners whether they moved last year.
# you cannot say that this is the effect of owning on moving, it could be that the 
# person moved (as a renter say), and then bought a house in the new place.

# DO NOT USE THIS MODEL!!!!

mod <- lapply(des$des, function(x) svyglm(S2S.move ~ age + age2 + tenure + numkids + educ + a_maritl + clswkr  +  race, family = binomial(link = "probit"), design = x))

# print output
texreg(mod[paste0("year",seq(from=2003,to=2013,by=2))],file="~/Dropbox/mobility/output/data/CPS/probit-all.tex",omit.coef="maritl|race|Self-employed|clswkrWithout",booktabs=TRUE,dcolumn=TRUE,use.packages=FALSE,table=FALSE,custom.model.names=paste0("year",seq(from=2003,to=2013,by=2)),digits=3,include.aic=FALSE,include.bic=FALSE,include.deviance=FALSE,include.loglik=FALSE,include.dispersion=FALSE)

texreg(mod["year2013"],file="~/Dropbox/mobility/output/data/CPS/probit-2013.tex",omit.coef="maritl|race|Self-employed|clswkrWithout|clswkr",booktabs=TRUE,dcolumn=TRUE,use.packages=FALSE,table=FALSE,custom.model.names="Pr(move states)",digits=3,include.aic=FALSE,include.bic=FALSE,include.deviance=FALSE,include.loglik=FALSE,include.dispersion=FALSE)

# get a good of fit measure: fraction of true predictions
dat <- copy(des$des$year2013$variables)
dat[,probmove := predict(mod$year2013,type="response")]
dat[,pred.move := runif(n=nrow(dat))<probmove]
dat[,list(actual.moves=sum(as.numeric(S2S.move)-1),predicted=sum(pred.move))]



# make newdata predictions
nd=expand.grid(age=20:60,tenure=factor(c("own","rent"),levels=d[,levels(tenure)]))
nd$age2 = nd$age^2
nd$numkids=0
nd$educ = factor("some.college",levels=d[,levels(educ)])
nd$clswkr = factor("Private",levels=d[,levels(clswkr)])
nd$a_maritl = factor("Married - civilian spouse",levels=d[,levels(a_maritl)])
nd$race = factor("white",levels=d[,levels(race)])

pp <- predict(mod$year2013,newdata=nd,type="response")
nd$prob <- coef(pp)
nd$SE   <- vcov(pp)

truedat <- d[age %in% 20:60 & h_year==2013,list(trueprob=mean(S2S.move)),by=list(age,tenure)][order(tenure,age)]
truedat <- rbind(truedat[tenure=="own"],truedat[tenure=="rent"])

nd <- cbind(nd,truedat)

 
ggplot(nd,aes(x=age,y=prob,color=tenure)) + geom_point(aes(x=age,y=trueprob),size=2) + geom_line(size=1.5) + theme_bw() + ggtitle('CPS Probit prediction and raw data')
ggsave("~/Dropbox/mobility/output/data/CPS/probit-predict.pdf",width=9,height=6.5)



