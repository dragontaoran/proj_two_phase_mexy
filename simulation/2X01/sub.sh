#!/bin/bash

p_set=("0.6" "1")
rho_set=("0.3" "0.5")
design_set=("srs" "ssrs")
sigma_set=("0.5" "1")
hn_set=("0.1")
nsieve_set=("10")
dir="res"

mkdir -p ${dir}

for nsieve in ${nsieve_set[@]};
do
	for hn in ${hn_set[@]}; 
	do
		for p in ${p_set[@]};
		do
			for rho in ${rho_set[@]};
			do
				for sigma in ${sigma_set[@]};
				do
					for design in ${design_set[@]};
					do
						subdir=${dir}/nsieve${nsieve}_hn${hn}_${design}_sigma${sigma}_rho${rho}_p${p}
						mkdir -p ${subdir}
						
						echo "#!/bin/bash" > tmp.slurm
						echo "" >> tmp.slurm
						echo "#SBATCH --mail-user=taor@live.unc.edu  # email address" >> tmp.slurm
						echo "#SBATCH --mail-type=NONE  # Alerts sent when job begins, ends, or aborts" >> tmp.slurm
						echo "#SBATCH --nodes=1   # Number of nodes required" >> tmp.slurm
						echo "#SBATCH --ntasks=1   # Number of nodes required" >> tmp.slurm
						echo "#SBATCH --mem=4G  # Total memory (RAM) required, per node" >> tmp.slurm
						echo "#SBATCH --time=02-00:00:00  # Wall Clock time (dd-hh:mm:ss) [max of 14 days]" >> tmp.slurm
						echo "#SBATCH --array=1-10" >> tmp.slurm
						echo "#SBATCH --output=${subdir}/README.slog  # output and error messages go to this file" >> tmp.slurm
						echo "#SBATCH --job-name=SMLE # job name" >> tmp.slurm
						echo "" >> tmp.slurm
						echo "echo \"SLURM_JOBID: \" \$SLURM_JOBID" >> tmp.slurm
						echo "echo \"SLURM_ARRAY_TASK_ID: \" \$SLURM_ARRAY_TASK_ID" >> tmp.slurm
						echo "echo \"SLURM_ARRAY_JOB_ID: \" \$SLURM_ARRAY_JOB_ID" >> tmp.slurm
						echo "" >> tmp.slurm
						echo "R CMD BATCH --no-save --no-restore --slave \"--args \$SLURM_ARRAY_TASK_ID ${rho} ${p} ${hn} ${nsieve} ${subdir} ${sigma} ${design}\" sim.R ${subdir}/\$SLURM_ARRAY_TASK_ID.Rout" >> tmp.slurm
						
						# cat tmp.slurm
						sbatch tmp.slurm
						rm -rf tmp.slurm
					done
				done
			done
		done
	done
done
