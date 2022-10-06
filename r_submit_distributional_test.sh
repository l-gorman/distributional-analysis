#!/bin/bash

#SBATCH --job-name=test_job
#SBATCH --partition=test
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=4
#SBATCH --cpus-per-task=4
#SBATCH --time=00:20:00
#SBATCH --mem=2G


cd "${SLURM_SUBMIT_DIR}"

echo Running on host "$(hostname)" \n
echo Time is "$(date)" \n
echo Directory is "$(pwd)" \n
echo Slurm job ID is "${SLURM_JOBID}" \n
echo This jobs runs on the following machines: \n
echo "${SLURM_JOB_NODELIST}"

echo Keep track of job by entering "sacct -j ${SLURM_JOBID} " \n
echo Cancel your job by entering "scancel ${SLURM_JOBID} " \n
echo More details on submitting jobs here https://www.acrc.bris.ac.uk/protected/hpc-docs/job_types/ \n

module add languages/r/4.1.0

export OMP_NUM_THREADS=4

Rscript skew-normal-fit.R