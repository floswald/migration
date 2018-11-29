#!/bin/bash
#SBATCH --job-name=nomove
#SBATCH --output=nomove.out
#SBATCH --error=nomove.err
#SBATCH --partition=COMPUTE
#SBATCH --nodes=2
#SBATCH --ntasks-per-node=5

srun -l julia --color=yes run.jl experiment noMove --nworkers=9
