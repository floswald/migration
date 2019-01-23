# script to replicate all results in the paper

julia6 run.jl --help   # show options
julia6 run.jl --version
julia6 --color=yes run.jl experiment elasticity --nworkers=9
julia6 --color=yes run.jl experiment ownersWTP2 --nworkers=9
julia6 --color=yes run.jl experiment noMove --nworkers=9
julia6 --color=yes run.jl experiment noMove --nworkers=9 --yshock=0.99 --pshock=0.99
julia6 --color=yes run.jl experiment noMove --nworkers=9 --yshock=0.95 --pshock=0.9