
# migration

[![Build Status](https://travis-ci.com/floswald/migration.svg?token=yCXmyQ4r4F8RyxxzHZFG&branch=master)](https://travis-ci.com/floswald/migration)

This is the replication kit for *The Effect of Homeownership on the Option Value of Regional Migration*.

## Requirements for Replication

1. julia version [0.6.x (*not* the latest version of julia)](https://julialang.org/downloads/oldreleases.html) is needed to replicate all results in the paper.
2. R if you want to rebuild the data inputs to the julia package.

## Installation and Usage

In order to use the code, you should clone this repository to your computer by typing in your terminal

```bash
git clone https://github.com/floswald/migration.git
```

or by clicking on the green button top right (clone or download) to download. Once you have the code on your computer, you need to build the `julia` package to use it.

### Installing `julia` Package `mig`

All results from the structural model are produce with the package `mig`, which is a sub-folder of this repository. Here is how to build the package:

1. Go to where you cloned the package to, e.g. `~/migration`.
2. Calling `julia6` the julia v0.6.x executable, run
    ```
    julia6 mig/install.jl
    ```
    to download all dependencies and precompile the package.
3. Run `julia6 -e 'Pkg.test("mig")';` to run the unit tests on your computer.
4. Observe that the badge on top of the repo indicates that the same tests run on MacOS and Linux on [Travis-CI](https://travis-ci.com/floswald/migration).

### Installing `R` package `mig-pkg`


 
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
