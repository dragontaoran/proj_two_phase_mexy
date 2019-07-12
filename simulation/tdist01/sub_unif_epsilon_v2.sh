#!/bin/bash

rho="0.3"
p="0.6"
hn="0.1"
nsieve="20"
dir="res"
boundary_set=("0.5" "1" "2")

mkdir -p ${dir}

for boundary in ${boundary_set[@]};
do
	subdir=${dir}/unif_boundary${boundary}
	mkdir -p ${subdir}
	
	echo "#!/bin/bash" > tmp.slurm
	echo "" >> tmp.slurm
	echo "#SBATCH --mail-user=r.tao@vanderbilt.edu  # email address" >> tmp.slurm
	echo "#SBATCH --mail-type=NONE  # Alerts sent when job begins, ends, or aborts" >> tmp.slurm
	echo "#SBATCH --nodes=1   # Number of nodes required" >> tmp.slurm
	echo "#SBATCH --ntasks=1   # Number of nodes required" >> tmp.slurm
	echo "#SBATCH --mem=4G  # Total memory (RAM) required, per node" >> tmp.slurm
	echo "#SBATCH --time=02-00:00:00  # Wall Clock time (dd-hh:mm:ss) [max of 14 days]" >> tmp.slurm
	echo "#SBATCH --array=1-10" >> tmp.slurm
	echo "#SBATCH --output=${subdir}/%A_%a.slog  # output and error messages go to this file" >> tmp.slurm
	echo "#SBATCH --job-name=SMLE # job name" >> tmp.slurm
	echo "#SBATCH --constraint=\"sandybridge|haswell|skylake\"" >> tmp.slurm
	echo "" >> tmp.slurm
	echo "echo \"SLURM_JOBID: \" \$SLURM_JOBID" >> tmp.slurm
	echo "echo \"SLURM_ARRAY_TASK_ID: \" \$SLURM_ARRAY_TASK_ID" >> tmp.slurm
	echo "echo \"SLURM_ARRAY_JOB_ID: \" \$SLURM_ARRAY_JOB_ID" >> tmp.slurm
	echo "" >> tmp.slurm
	echo "Rscript sim_unif_epsilon_v2.R \$SLURM_ARRAY_TASK_ID ${rho} ${p} ${hn} ${nsieve} ${subdir} ${boundary}" >> tmp.slurm
	
	# cat tmp.slurm
	sbatch tmp.slurm
	rm -rf tmp.slurm
done
