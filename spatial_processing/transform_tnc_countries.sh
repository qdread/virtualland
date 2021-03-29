cd /nfs/qread-data/raw_data/landuse/ecoregions

# Transform TNC ecoregions clipped to USA extent to Albers equal area.
tncusaproj=`gdalsrsinfo /nfs/qread-data/raw_data/landuse/ecoregions/tnc_usa.prj -o proj4 | xargs`
nlcdproj=`gdalsrsinfo /nfs/qread-data/raw_data/landuse/NLCD/nlcd2016landcover.vrt -o proj4 | xargs`

ogr2ogr -f "ESRI Shapefile" -t_srs "${nlcdproj}" -s_srs "${tncusaproj}" tnc_usa_aea.shp tnc_usa.shp
ogr2ogr -f "GPKG" -t_srs "${nlcdproj}" -s_srs "${tncusaproj}" tnc_usa_aea.gpkg tnc_usa.shp

# Mollweide projection for global analysis that preserves area.
proj_moll="+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"

# Transform global TNC ecoregions to Mollweide
tncproj=`gdalsrsinfo tnc_terr_ecoregions.prj -o proj4 | xargs`

ogr2ogr -f "GPKG" -t_srs "${proj_moll}" -s_srs "${tncproj}" tnc_global_equalarea.gpkg tnc_terr_ecoregions.shp

# Country boundaries obtained from https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip (2018 version)

# Transform global country boundaries to Mollweide
countryproj=`gdalsrsinfo ne_50m_admin_0_countries.prj -o proj4 | xargs`

ogr2ogr -f "GPKG" -t_srs "${proj_moll}" -s_srs "${countryproj}" countries_global_equalarea.gpkg ne_50m_admin_0_countries.shp

# Transform both the cropland and pastureland raster layers to the Mollweide projection.

cd /nfs/qread-data/raw_data/landuse/global_aglands

pastureproj=`gdalsrsinfo pasture.tif -o proj4 | xargs`
cropdproj=`gdalsrsinfo GFSAD1KCD.2010.001.2016348142525.tif -o proj4 | xargs`
cropmproj=`gdalsrsinfo GFSAD1KCM.2010.001.2016348142550.tif -o proj4 | xargs`

gdalwarp -t_srs "${proj_moll}" -s_srs "${pastureproj}" -of "VRT" pasture.tif pasture_equalarea.vrt
gdalwarp -t_srs "${proj_moll}" -s_srs "${cropdproj}" -of "VRT" GFSAD1KCD.2010.001.2016348142525.tif cropdominance_equalarea.vrt
gdalwarp -t_srs "${proj_moll}" -s_srs "${cropmproj}" -of "VRT" GFSAD1KCM.2010.001.2016348142550.tif cropmask_equalarea.vrt

# Also transform cropland as TIF so that it can be read in R.
gdalwarp -t_srs "${proj_moll}" -s_srs "${cropdproj}" GFSAD1KCD.2010.001.2016348142525.tif cropdominance_equalarea.tif
gdalwarp -t_srs "${proj_moll}" -s_srs "${cropmproj}" GFSAD1KCM.2010.001.2016348142550.tif cropmask_equalarea.tif