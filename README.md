
# migration

[![Build status](https://badge.buildkite.com/087d2bb0abb0513511e10e9cd735b0a9a86d7d6ba6cacdd5cc.svg)](https://buildkite.com/sciencespoecon/migration)

## Run Code

* install latest stable julia version
* install this package [HOW]

## experiments

```bash
# go to root of this package
# for me: 
# cd ~/.julia/v0.6/mig

# 1. migration shutdown - baseline prices
julia --color=yes run.jl experiment noMove --nworkers=6

# 2. migration shutdown - small shock
julia --color=yes run.jl experiment noMove --pshock=0.99 --yshock=0.99 --nworkers=6

# 3. migration shutdown - bust
julia --color=yes run.jl experiment noMove --pshock=0.9 --yshock=0.95 --nworkers=6
```
