#!/bin/bash
#SBATCH --nodes=1
#SBATCH --partition=sesyncshared
#SBATCH --job-name=cdl_byyear

# Job script to get USDA cropland tallies by region for all years that we have CDL
outdir="/nfs/qread-data/raw_data/landuse/output_csvs"

# Define file names
bcrvector="/nfs/qread-data/raw_data/landuse/ecoregions/bcr_usa_combined.shp"
fafvector="/nfs/qread-data/raw_data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/faf_aea.shp"
tncvector="/nfs/qread-data/raw_data/landuse/ecoregions/tnc_usa_aea.shp"
cfstncvector="/nfs/qread-data/cfs_io_analysis/cfs_tnc_aea_intersect.gpkg"

# Do zonal stats on each of the 3 shapefiles
calc_stats () {
	local year=$1
	cdlraster="/nfs/qread-data/raw_data/landuse/USDAcropland/CDL/cdl${year}.vrt"
	python3 /research-home/qread/fwe/NLCD/tabulateraster.py ${bcrvector} ${cdlraster} ${outdir}/CDL_${year}_BCR.csv
	python3 /research-home/qread/fwe/NLCD/tabulateraster.py ${fafvector} ${cdlraster} ${outdir}/CDL_${year}_FAF.csv
	python3 /research-home/qread/fwe/NLCD/tabulateraster.py ${tncvector} ${cdlraster} ${outdir}/CDL_${year}_TNC.csv
	python3 /research-home/qread/fwe/NLCD/tabulateraster.py ${cfstncvector} ${cdlraster} ${outdir}/CDL_${year}_CFSTNC.csv
}

for year in $(seq 2009 2018); do calc_stats $year & done
wait

