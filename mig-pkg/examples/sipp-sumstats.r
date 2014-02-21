

# make summary statistics SIPP

library(migration)
load("~/Dropbox/mobility/SIPP/SippFull.RData")

l <- Sipp.SumStats(merged,saveto="~/Dropbox/mobility/SIPP/sumstat.RData")

print(l)
