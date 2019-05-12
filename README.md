
# migration

[![Build Status](https://travis-ci.com/floswald/migration.svg?token=yCXmyQ4r4F8RyxxzHZFG&branch=master)](https://travis-ci.com/floswald/migration)

This is the replication kit for *The Effect of Homeownership on the Option Value of Regional Migration*.

## Requirements for Replication

1. julia version [0.6.x (*not* the latest version of julia)](https://julialang.org/downloads/oldreleases.html) is needed to replicate all results in the paper.
2. R if you want to rebuild the data inputs to the julia package.

## Installation and Usage

### Julia Package `mig.jl`



## experiments

```bash
# go to root of this package
# for me: 
# cd ~/.julia/v0.6/mig

# 1. migration shutdown - baseline prices
julia --color=yes run.jl experiment noMove --nworkers=10

# 2. migration shutdown - small shock
julia --color=yes run.jl experiment noMove --pshock=0.99 --yshock=0.99 --nworkers=10

# 3. migration shutdown - bust
julia --color=yes run.jl experiment noMove --pshock=0.9 --yshock=0.95 --nworkers=10

# 4. migration shutdown - price increase
julia --color=yes run.jl experiment noMove --pshock=1.05 --nworkers=10
```
