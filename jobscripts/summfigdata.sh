#!/bin/bash
#SBATCH --nodes=1
#SBATCH --partition=sesync
#SBATCH --nodelist=pn47
#SBATCH --job-name=summfigdata

cd /research-home/qread/virtualland
Rscript --vanilla figs/figs_v2_summarydata_parallel.R


