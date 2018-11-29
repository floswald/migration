#!/bin/bash
#SBATCH --job-name=jl-mig
#SBATCH --output=magi.out
#SBATCH --error=magi.err
#SBATCH --partition=COMPUTE
#SBATCH --nodes=2
#SBATCH --ntasks-per-node=5

srun -l julia --color=yes run.jl experiment ownersWTP2 --nworkers=9
