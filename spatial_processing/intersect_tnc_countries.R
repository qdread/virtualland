# Intersect TNC global ecoregions with global country boundaries

library(sf)
library(tidyverse)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_eco <- file.path(fp, 'raw_data/landuse/ecoregions')
fp_out <- file.path(fp, 'cfs_io_analysis')


# Load country boundary and TNC polygons
tncmap <- st_read(dsn = file.path(fp_eco, 'tnc_global_equalarea.gpkg')) # 814 regions
countrymap <- st_read(dsn = file.path(fp_eco, 'countries_global_equalarea.gpkg')) # 241 regions

# Intersect the two
country_tnc <- st_intersection(tncmap, countrymap) 
# This results in 1718 polygons combining the two
# Plot a subset of it to see if it looks correct.
country_tnc_nearctic <- filter(country_tnc, WWF_REALM2 %in% "Nearctic")
plot(country_tnc_nearctic['ECO_CODE']) # Looks correct

# Tabulate both the global cropland layer and the global pastureland layer by this intersected polygon layer.
# This is done outside of R.

# Areas of the intersected regions
country_tnc$area <- st_area(country_tnc)

# Write result as geopackage
st_write(country_tnc, dsn = file.path(fp_out, 'countries_tnc_intersect.gpkg'))
