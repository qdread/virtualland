#!/bin/bash
#SBATCH --nodes=1
#SBATCH --partition=sesyncshared
#SBATCH --job-name=countpxls

# Arguments with the file name of the two inputs and the output (to be created) passed with --export flag of sbatch
python3 /research-home/qread/virtualland/NLCD/zonalstats_to_geojson.py ${vector_file} ${raster_file} ${output_file} 
