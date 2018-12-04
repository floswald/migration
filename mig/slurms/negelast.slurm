#!/bin/bash
#SBATCH --job-name=negelast
#SBATCH --output=slurms/negelast.out
#SBATCH --error=slurms/negelast.err
#SBATCH --partition=COMPUTE
#SBATCH --nodes=2
#SBATCH --ntasks-per-node=15
#SBATCH --chdir=..

srun -l julia --color=yes run.jl experiment elasticity --nworkers=30 --neg
