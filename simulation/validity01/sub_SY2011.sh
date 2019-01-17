#!/bin/bash

n2_set=("300")
p_s_set=("0.05" "0.1" "0.2" "0.3")
rho_u_ustar_set=("-0.5" "0" "0.5")
dir="res"

mkdir -p ${dir}

for n2 in ${n2_set[@]}; 
do
	for p_s in ${p_s_set[@]};
	do
		for rho_u_ustar in ${rho_u_ustar_set[@]};
		do
			subdir=${dir}/SY2011_n2_${n2}_p_s_${p_s}_rho_u_ustar_${rho_u_ustar}
			mkdir -p ${subdir}
			
			echo "#!/bin/bash" > tmp.slurm
			echo "" >> tmp.slurm
			echo "#SBATCH --mail-user=r.tao@vanderbilt.edu  # email address" >> tmp.slurm
			echo "#SBATCH --mail-type=NONE  # Alerts sent when job begins, ends, or aborts" >> tmp.slurm
			echo "#SBATCH --nodes=1   # Number of nodes required" >> tmp.slurm
			echo "#SBATCH --ntasks=1   # Number of nodes required" >> tmp.slurm
			echo "#SBATCH --mem=4G  # Total memory (RAM) required, per node" >> tmp.slurm
			echo "#SBATCH --time=00-02:00:00  # Wall Clock time (dd-hh:mm:ss) [max of 14 days]" >> tmp.slurm
			echo "#SBATCH --array=1-100" >> tmp.slurm
			echo "#SBATCH --output=${subdir}/%a.slog  # output and error messages go to this file" >> tmp.slurm
			echo "#SBATCH --job-name=validity01 # job name" >> tmp.slurm
			echo "#SBATCH --constraint=\"sandybridge|haswell|skylake\"" >> tmp.slurm
			echo "" >> tmp.slurm
			echo "echo \"SLURM_JOBID: \" \$SLURM_JOBID" >> tmp.slurm
			echo "echo \"SLURM_ARRAY_TASK_ID: \" \$SLURM_ARRAY_TASK_ID" >> tmp.slurm
			echo "echo \"SLURM_ARRAY_JOB_ID: \" \$SLURM_ARRAY_JOB_ID" >> tmp.slurm
			echo "" >> tmp.slurm
			echo "Rscript sim_SY2011.R \$SLURM_ARRAY_TASK_ID ${n2} ${p_s} ${rho_u_ustar}" >> tmp.slurm

			sbatch tmp.slurm
			rm -rf tmp.slurm			
		done
	done
done
