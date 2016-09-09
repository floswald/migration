#   This is the most basic QSUB file needed for this cluster.
#   Further examples can be found under /share/apps/examples
#   Most software is NOT in your PATH but under /share/apps
#
#   For further info please read http://hpc.cs.ucl.ac.uk
#
#   NOTE hash dollar is a scheduler diredctive not a comment.


# These are flags you must include - Two memory and one runtime.
# Runtime is either seconds or hours:min:sec

#$ -l tmem=6G
#$ -l h_vmem=6G
#$ -l h_rt=0:23:0

#$ -pe mpich 40
#$ -R y

#These are optional flags but you problably want them in all jobs

#$ -S /bin/bash
#$ -j y
#$ -e /home/uctpfos/git/migration/mig/src/cluster/examples/estim.err
#$ -o /home/uctpfos/git/migration/mig/src/cluster/examples/estim.out
#$ -N migestim
#$ -cwd

#The code you want to run now goes here.

#submit this from cluster/ with
#qsub subs/sub-uclcs.sh

hostname
date
awk '{ for (i=0; i < $2; ++i) { print $1} }' $PE_HOSTFILE > hosts

echo "your hosts"
cat hosts
julia5 --machinefile hosts --depwarn=no examples/estimation.jl

