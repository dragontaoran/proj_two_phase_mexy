#!/bin/bash

dir="results"

mkdir -p ${dir}

echo "#!/bin/bash" > tmp.slurm
echo "" >> tmp.slurm
echo "#SBATCH --mail-user=taor@live.unc.edu  # email address" >> tmp.slurm
echo "#SBATCH --mail-type=NONE  # Alerts sent when job begins, ends, or aborts" >> tmp.slurm
echo "#SBATCH --nodes=1   # Number of nodes required" >> tmp.slurm
echo "#SBATCH --ntasks=1   # Number of nodes required" >> tmp.slurm
echo "#SBATCH --mem=4G  # Total memory (RAM) required, per node" >> tmp.slurm
echo "#SBATCH --time=02-00:00:00  # Wall Clock time (dd-hh:mm:ss) [max of 14 days]" >> tmp.slurm
echo "#SBATCH --output=SY2011.slog  # output and error messages go to this file" >> tmp.slurm
echo "#SBATCH --job-name=SY # job name" >> tmp.slurm
echo "" >> tmp.slurm
echo "echo \"SLURM_JOBID: \" \$SLURM_JOBID" >> tmp.slurm
echo "" >> tmp.slurm
echo "R CMD BATCH --no-save --no-restore --slave \"--args ${dir}\" sim_SY2011.R ${dir}/sim_SY2011.Rout" >> tmp.slurm

# cat tmp.slurm
sbatch tmp.slurm
rm -rf tmp.slurm			
