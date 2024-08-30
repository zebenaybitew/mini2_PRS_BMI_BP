


#!/bin/bash
#SBATCH --job-name=GRS_PLINK_dareng
#SBATCH --output=GRS_PLINK_dareng.txt
#SBATCH --nodes=2
#SBATCH --cpus-per-task=1
#SBATCH --mem=100000M
#SBATCH --time=1:00:00
#SBATCH --account=sscm013902
#SBATCH --partition=mrcieu


module load gcc/10.5.0
module load bgen/1.1.7
module load apps/plink2/2.00a6LM



cd /user/work/cr23646/PRS/females

bgen_pattern=/mnt/storage/private/mrcieu/data/ukbiobank/genetic/variants/arrays/imputed/released/2018-09-18/data/dosage_bgen/data.chrCHROM.bgen

bgen_index_pattern=/mnt/storage/private/mrcieu/data/ukbiobank/genetic/variants/arrays/imputed/released/2018-09-18/data/dosage_bgen/data.chrCHROM.bgen.bgi

snp_list="/user/work/cr23646/PRS/females/snp_list_f.txt"
snp_list_alleles="/user/work/cr23646/PRS/females/allele_effect_f.txt"
temp_geno_prefix=temp_genos
for chrom in {01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,X}; do
  inbgen=${bgen_pattern/CHROM/$chrom}
  inbgenidx=${bgen_index_pattern/CHROM/$chrom}
  bgenix -g $inbgen -i $inbgenidx -incl-rsids $snp_list > $temp_geno_prefix.$chrom.bgen  
done

cmd=""
for chrom in {01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,X}; do
  cmd="${cmd} ${temp_geno_prefix}.${chrom}.bgen"
done
cat-bgen -g ${cmd} -og instruments.bgen
# Remove temp genos
rm $temp_geno_prefix*

plink2 --bgen instruments.bgen ref-first --score $snp_list_alleles --out GRSscore 
module rm bgen/1.1.7
module rm  apps/plink2/2.00a6LM