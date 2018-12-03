#!/bin/bash
#SBATCH --job-name=nomove
#SBATCH --output=slurms/elast.out
#SBATCH --error=slurms/elast.err
#SBATCH --partition=COMPUTE
#SBATCH --nodes=2
#SBATCH --ntasks-per-node=15
#SBATCH --chdir=..

srun -l julia --color=yes run.jl experiment elasticity --nworkers=30
