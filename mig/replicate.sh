# script to replicate all results in the paper

julia6 run.jl --help   # show options
julia6 run.jl --version
julia6 --color=yes run.jl experiment moneyMC
julia6 --color=yes run.jl experiment decomp
