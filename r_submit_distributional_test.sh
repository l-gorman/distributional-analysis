#!/bin/bash

#SBATCH --mail-user=[lg14410@bristol.ac.uk]
#SBATCH --mail-type=END,FAIL,TIME_LIMIT_80
#SBATCH --job-name=distributional-analysis
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=4
#SBATCH --time=08:00:00
#SBATCH --mem=12G


cd "${SLURM_SUBMIT_DIR}"

echo "Running on host $(hostname) \n"
echo "Time is $(date) \n"
echo "Directory is $(pwd) \n"
echo "Slurm job ID is ${SLURM_JOBID} \n"
echo "This jobs runs on the following machines: \n"
echo "${SLURM_JOB_NODELIST}"

echo "Keep track of job by entering sacct -j ${SLURM_JOBID}  \n"
echo "Cancel your job by entering scancel ${SLURM_JOBID}  \n"
echo "More details on submitting jobs here https://www.acrc.bris.ac.uk/protected/hpc-docs/job_types/ \n"

module add languages/r/4.1.0

# d=$(date +%Y-%m-%d)

export OMP_NUM_THREADS=4

for SAMPLE in 100, 500, 1000, 2000
do
  for ITER in 2000 5000 10000 20000 50000
  do
    Rscript skew-normal-fit.R -n $SAMPLE -i $ITER -d $d
  done
done





