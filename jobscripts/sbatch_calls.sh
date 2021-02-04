EXEC="countpixels.sh"
outdir="/nfs/qread-data/raw_data/landuse/output_csvs"
cd ~/virtualland/jobscripts

# Extract NLCD2016 by combined BCR, FAF, and TNC 
sbatch --export=vector_file=/nfs/qread-data/raw_data/landuse/ecoregions/bcr_usa_combined.shp,raster_file=/nfs/qread-data/raw_data/landuse/NLCD/nlcd2016landcover.vrt,output_file=${outdir}/NLCD_2016_BCR.csv ${EXEC}
sbatch --export=vector_file=/nfs/qread-data/raw_data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/faf_aea.shp,raster_file=/nfs/qread-data/raw_data/landuse/NLCD/nlcd2016landcover.vrt,output_file=${outdir}/NLCD_2016_FAF.csv ${EXEC}
sbatch --export=vector_file=/nfs/qread-data/raw_data/landuse/ecoregions/tnc_usa_aea.shp,raster_file=/nfs/qread-data/raw_data/landuse/NLCD/nlcd2016landcover.vrt,output_file=${outdir}/NLCD_2016_TNC.csv ${EXEC}
# Added 17 Aug 2020: extract NLCD 2016 by the FAF and TNC intersected shapefile
sbatch --export=vector_file=/nfs/qread-data/cfs_io_analysis/cfs_tnc_aea_intersect.gpkg,raster_file=/nfs/qread-data/raw_data/landuse/NLCD/nlcd2016landcover.vrt,output_file=${outdir}/NLCD_2016_CFSTNC.csv ${EXEC}
# Added 12 Jan 2021: NLCD 2016 by county x TNC intersected shapefile
sbatch -J nlcdcountytnc --partition=sesync --export=vector_file=/nfs/qread-data/cfs_io_analysis/county_tnc_aea_intersect.gpkg,raster_file=/nfs/qread-data/raw_data/landuse/NLCD/nlcd2016landcover.vrt,output_file=${outdir}/NLCD_2016_countyTNC.csv ${EXEC}

# Use the historic 1700 raster.
sbatch --export=vector_file=/nfs/qread-data/raw_data/landuse/ecoregions/bcr_usa_combined.shp,raster_file=/nfs/qread-data/raw_data/landuse/historic/historic_landcover_hdeg/aea_historic_landcover_hd_1700.tif,output_file=${outdir}/BCR1700.csv ${EXEC}
sbatch --export=vector_file=/nfs/qread-data/raw_data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/faf_aea.shp,raster_file=/nfs/qread-data/raw_data/landuse/historic/historic_landcover_hdeg/aea_historic_landcover_hd_1700.tif,output_file=${outdir}/FAF1700.csv ${EXEC}
sbatch --export=vector_file=/nfs/qread-data/raw_data/landuse/ecoregions/tnc_usa_aea.shp,raster_file=/nfs/qread-data/raw_data/landuse/historic/historic_landcover_hdeg/aea_historic_landcover_hd_1700.tif,output_file=${outdir}/TNC1700.csv ${EXEC}

# Extract USDA Cropland Data Layer 2018 for BCR, FAF, and TNC
cdlraster="/nfs/qread-data/raw_data/landuse/USDAcropland/CDL/cdl2018.vrt"
sbatch --export=vector_file=/nfs/qread-data/raw_data/landuse/ecoregions/bcr_usa_combined.shp,\
	raster_file=${cdlraster},\
	output_file=${outdir}/CDL_2018_BCR.csv ${EXEC}
sbatch --export=vector_file=/nfs/qread-data/raw_data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/faf_aea.shp,\
	raster_file=${cdlraster},\
	output_file=${outdir}/CDL_2018_FAF.csv ${EXEC}
sbatch --export=vector_file=/nfs/qread-data/raw_data/landuse/ecoregions/tnc_usa_aea.shp,\
	raster_file=${cdlraster},\
	output_file=${outdir}/CDL_2018_TNC.csv ${EXEC}

# Extract the rasterized TNC by CFS and by US states
tncraster="/nfs/qread-data/raw_data/landuse/ecoregions/tnc_usa_aea_gridded.tif"
sbatch --export=vector_file=/nfs/qread-data/raw_data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/faf_aea.shp,\
	raster_file=${tncraster},\
	output_file=${outdir}/TNCcount_by_CFS.csv ${EXEC}

sbatch -J countbystate --export=vector_file=/nfs/qread-data/raw_data/landuse/USA/USA_adm1_aea.shp,\
	raster_file=${tncraster},\
	output_file=${outdir}/TNCcount_by_state.csv ${EXEC}

# Extract the rasterized BCR by CFS and by US states
bcrraster="/nfs/qread-data/raw_data/landuse/ecoregions/bcr_usa_aea_gridded.tif"
sbatch --export=vector_file=/nfs/qread-data/raw_data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/faf_aea.shp,\
	raster_file=${bcrraster},\
	output_file=${outdir}/BCRcount_by_CFS.csv ${EXEC}

sbatch -J countbystate --export=vector_file=/nfs/qread-data/raw_data/landuse/USA/USA_adm1_aea.shp,\
	raster_file=${bcrraster},\
	output_file=${outdir}/BCRcount_by_state.csv ${EXEC}

# Extract cropland data layer for all years (2018 already done)
sbatch --array=1-9 countcdlbyyear.sh

#### Added 16 Sept 2020
# Tabulate global cropland dominance and global cropland mask by intersected TNC ecoregion X country boundaries.
# Global pastureland is done in a separate Python script.
tnccountryvector="/nfs/qread-data/cfs_io_analysis/countries_tnc_intersect.gpkg"
rasterdir="/nfs/qread-data/raw_data/landuse/global_aglands"

sbatch -J countcropd --export=vector_file=${tnccountryvector},\
	raster_file=${rasterdir}/cropdominance_equalarea.vrt,\
	output_file=${outdir}/global_count_cropdominance.csv ${EXEC}
	
sbatch -J countcropm --export=vector_file=${tnccountryvector},\
	raster_file=${rasterdir}/cropmask_equalarea.vrt,\
	output_file=${outdir}/global_count_cropmask.csv ${EXEC}

#### Added 04 Feb 2021
# NLCD for Alaska and Hawaii

# Download Hawaii NLCD2001 landcover
cd /nfs/qread-data/raw_data/landuse/NLCD
wget https://s3-us-west-2.amazonaws.com/mrlc/HI_landcover_wimperv_9-30-08_se5.zip
unzip HI_landcover_wimperv_9-30-08_se5.zip
# Download Alaska NLCD2016 landcover
wget https://s3-us-west-2.amazonaws.com/mrlc/NLCD_2016_Land_Cover_AK_20200724.zip
unzip NLCD_2016_Land_Cover_AK_20200724.zip
# (moved to their own directories)

# Build VRTs from Alaska and Hawaii rasters
gdalbuildvrt nlcd2016landcover_ak.vrt NLCD_2016_Land_Cover_AK_20200724.img
gdalbuildvrt nlcd2001landcover_hi.vrt hi_landcover_wimperv_9-30-08_se5.img

# Aggregate the Hawaii NLCD2001 and the Alaska NLCD2016 by different shapefiles.
# BCR, FAF, TNC, FAFxTNC, countyxTNC
EXEC="countpixels.sh"
outdir="/nfs/qread-data/raw_data/landuse/output_csvs"
akraster="/nfs/qread-data/raw_data/landuse/NLCD/NLCD2016_Alaska/nlcd2016landcover_ak.vrt"
hiraster="/nfs/qread-data/raw_data/landuse/NLCD/NLCD2001_Hawaii/nlcd2001landcover_hi.vrt"
cd ~/virtualland/jobscripts

# Extract AK NLCD2016 by all the vector files.
sbatch -J akbcr --export=vector_file=/nfs/qread-data/raw_data/landuse/ecoregions/bcr_usa_combined.shp,raster_file=${akraster},output_file=${outdir}/AK_NLCD_2016_BCR.csv ${EXEC}
sbatch -J akfaf --export=vector_file=/nfs/qread-data/raw_data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/faf_aea.shp,raster_file=${akraster},output_file=${outdir}/AK_NLCD_2016_FAF.csv ${EXEC}
sbatch -J aktnc --export=vector_file=/nfs/qread-data/raw_data/landuse/ecoregions/tnc_usa_aea.shp,raster_file=${akraster},output_file=${outdir}/AK_NLCD_2016_TNC.csv ${EXEC}
sbatch -J akfaftnc --export=vector_file=/nfs/qread-data/cfs_io_analysis/cfs_tnc_aea_intersect.gpkg,raster_file=${akraster},output_file=${outdir}/AK_NLCD_2016_CFSTNC.csv ${EXEC}
sbatch -J akcountytnc --export=vector_file=/nfs/qread-data/cfs_io_analysis/county_tnc_aea_intersect.gpkg,raster_file=${akraster},output_file=${outdir}/AK_NLCD_2016_countyTNC.csv ${EXEC}

sbatch -J hibcr --export=vector_file=/nfs/qread-data/raw_data/landuse/ecoregions/bcr_usa_combined.shp,raster_file=${hiraster},output_file=${outdir}/HI_NLCD_2016_BCR.csv ${EXEC}
sbatch -J hifaf --export=vector_file=/nfs/qread-data/raw_data/commodity_flows/FAF/Freight_Analysis_Framework_Regions/faf_aea.shp,raster_file=${hiraster},output_file=${outdir}/HI_NLCD_2016_FAF.csv ${EXEC}
sbatch -J hitnc --export=vector_file=/nfs/qread-data/raw_data/landuse/ecoregions/tnc_usa_aea.shp,raster_file=${hiraster},output_file=${outdir}/HI_NLCD_2016_TNC.csv ${EXEC}
sbatch -J hifaftnc --export=vector_file=/nfs/qread-data/cfs_io_analysis/cfs_tnc_aea_intersect.gpkg,raster_file=${hiraster},output_file=${outdir}/HI_NLCD_2016_CFSTNC.csv ${EXEC}
sbatch -J hicountytnc --export=vector_file=/nfs/qread-data/cfs_io_analysis/county_tnc_aea_intersect.gpkg,raster_file=${hiraster},output_file=${outdir}/HI_NLCD_2016_countyTNC.csv ${EXEC}