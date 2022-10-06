#!/bin/bash

#SBATCH --job-name=test_job
#SBATCH --partition=test
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=4
#SBATCH --time=00:10:00
#SBATCH --mem=500M


cd "${SLURM_SUBMIT_DIR}"

echo Running on host "$(hostname)"
echo Time is "$(date)"
echo Directory is "$(pwd)"
echo Slurm job ID is "${SLURM_JOBID}"
echo This jobs runs on the following machines:
echo "${SLURM_JOB_NODELIST}"

echo Keep track of job by entering "sacct -j ${SLURM_JOBID}$ "
echo Cancel your job by entering "scancel ${SLURM_JOBID}$ "
echo More details on submitting jobs here https://www.acrc.bris.ac.uk/protected/hpc-docs/job_types/

module add languages/r/4.1.0

export OMP_NUM_THREADS=1

Rscript skew-normal-fit.R