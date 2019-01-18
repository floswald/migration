#!/bin/bash
#SBATCH --job-name=nomove
#SBATCH --output=slurms/nomove.out
#SBATCH --error=slurms/nomove.err
#SBATCH --partition=COMPUTE
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=10
#SBATCH --chdir=..

srun -l julia --color=yes run.jl experiment noMove --nworkers=9
