#!/bin/bash
#SBATCH --nodes=1
#SBATCH --job-name=cdl_byyear

# Job script to get USDA cropland tallies by region for all years that we have CDL
outdir="/nfs/qread-data/raw_data/landuse/output_csvs"

# Get year beginning with 2009 from task id
((year=SLURM_ARRAY_TASK_ID+2008))

# Define file names
countytncvector="/nfs/qread-data/cfs_io_analysis/county_tnc_aea_intersect.gpkg"
cdlraster="/nfs/qread-data/raw_data/landuse/USDAcropland/CDL/cdl${year}.vrt"

# Do zonal stats on each of the 3 shapefiles
python3 /research-home/qread/virtualland/NLCD/tabulateraster.py ${countytncvector} ${cdlraster} ${outdir}/CDL_${year}_countyTNC.csv


