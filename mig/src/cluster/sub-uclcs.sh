#   This is the most basic QSUB file needed for this cluster.
#   Further examples can be found under /share/apps/examples
#   Most software is NOT in your PATH but under /share/apps
#
#   For further info please read http://hpc.cs.ucl.ac.uk
#
#   NOTE hash dollar is a scheduler diredctive not a comment.


# These are flags you must include - Two memory and one runtime.
# Runtime is either seconds or hours:min:sec

#$ -l tmem=4G
#$ -l h_vmem=4G
#$ -l h_rt=300

#$ -pe mpich 40
#$ -R y

#These are optional flags but you problably want them in all jobs

#$ -S /bin/bash
#$ -j y
#$ -e /home/uctpfos/git/migration/src/cluster/examples/slices.err
#$ -o /home/uctpfos/git/migration/src/cluster/examples/slices.out
#$ -j y
#$ -N migslice
#$ -cwd

#The code you want to run now goes here.

#submit this from cluster/ with
#qsub subs/sub-uclcs.sh

hostname
date
awk '{ for (i=0; i < $2; ++i) { print $1} }' $PE_HOSTFILE > hosts

echo "your hosts"
cat hosts
/home/uctpfos/thirdparty/juliabin-0.4.6/bin/julia --machinefile hosts examples/slices.jl

