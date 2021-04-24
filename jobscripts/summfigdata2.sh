#!/bin/bash
#SBATCH --nodes=1
#SBATCH --partition=sesync
#SBATCH --nodelist=pn46
#SBATCH --job-name=summfigdata2

cd /research-home/qread/virtualland
Rscript --vanilla figs/figs_v2_summarydata_parallel2.R


