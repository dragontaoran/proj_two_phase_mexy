software="/proj/epi/CVDGeneNas/sol/taor/SUGEN_github/SUGEN/SUGEN"
# phenotype="/nas/depts/004/CVD_BP2/SOL_metabo/pheno/chrX/pheno_women.txt"        
phenotype="sum2_validation.tab"  

id_col="DBGAP_SUBJECT_ID"
fam_col="fid"
vcf="/proj/epi/Genetic_Data_Center/calico/sol_gwas_uwash/vcf/vcf/SOL_imputed3_chr"

# quant_traits=("sbp")
quant_start=1            
quant_end=5
                
                            
length=${#quant_traits[@]}
 
for ((i=${quant_start}; i<${quant_end}; i++));
do
	quant_trait=$(awk -v i="$i" 'NR==2{print $i}' ${phenotype})
	quant_formula=(${quant_trait}"=age+CEN_C+CEN_M+CEN_S+EV1+EV2+EV3+EV4+EV5+gengrp6+bmi") 
	for ((j=23; j<24; j++));
    do
        echo "sbatch -t 11-00:00:00 --mem 30g -J chr${j}trait${i} --mail-type END --mail-user noraf@unc.edu -o ${quant_trait}_chr${j}.lsflog --wrap=\"${software} --pheno ${phenotype} --id-col ${id_col} --family-col ${fam_col} --vcf ${vcf}${j}.vcf.gz --formula ${quant_formula} --unweighted --model linear --out-prefix ./${quant_trait}_chr${j} --dosage\" "                                
	done
done
