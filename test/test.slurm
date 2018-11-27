#!/bin/bash

#SBATCH --mail-user=r.tao@vanderbilt.edu  # email address
#SBATCH --mail-type=NONE  # Alerts sent when job begins, ends, or aborts
#SBATCH --nodes=1   # Number of nodes required
#SBATCH --ntasks=1   # Number of nodes required
#SBATCH --mem=5G  # Total memory (RAM) required, per node
#SBATCH --time=00-01:00:00  # Wall Clock time (dd-hh:mm:ss) [max of 14 days]
#SBATCH --array=1-100
#SBATCH --output=res/SY2011_%a.slog  # output and error messages go to this file
#SBATCH --job-name=test # job name
#SBATCH --constraint="sandybridge|haswell|skylake"

echo "SLURM_JOBID: " $SLURM_JOBID
echo "SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID
echo "SLURM_ARRAY_JOB_ID: " $SLURM_ARRAY_JOB_ID

Rscript test.R $SLURM_ARRAY_TASK_ID
