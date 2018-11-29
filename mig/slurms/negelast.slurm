#!/bin/bash
#SBATCH --job-name=nomove
#SBATCH --output=elast.out
#SBATCH --error=elast.err
#SBATCH --partition=COMPUTE
#SBATCH --nodes=2
#SBATCH --ntasks-per-node=15

srun -l julia --color=yes run.jl experiment elasticity --nworkers=30
