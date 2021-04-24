# Update to use exactextractr and to project the polygons each time, not the raster.
library(raster)
library(sf)
library(tidyverse)
library(exactextractr)

# Read shapefile and the three grids: L48, HI, and AK
county_tnc <- st_read('data/cfs_io_analysis/county_tnc_aea_intersect.gpkg')
usgrid <- raster(file.path('data/raw_data/Census/uspopgrid/geotiff', 'uspop10.tif'))
higrid <- raster(file.path('data/raw_data/Census/uspopgrid/geotiff', 'hipop10.tif'))
akgrid <- raster(file.path('data/raw_data/Census/uspopgrid/geotiff', 'akpop10.tif'))

# Return shapefile to longlat. Each of the three uspopgrids is in the same longlat projection.
county_tnc_longlat <- county_tnc %>% st_transform(st_crs(usgrid))

# Extract population totals by intersected polygon
county_tnc_pop <- exact_extract(usgrid, county_tnc_longlat, fun = 'sum')
hi_pop <- exact_extract(higrid, county_tnc_longlat, fun = 'sum')
ak_pop <- exact_extract(akgrid, county_tnc_longlat, fun = 'sum')

# Combine all three sections into one data frame. 
county_tnc_pop_all <- rowSums(cbind(county_tnc_pop, hi_pop, ak_pop))

county_tnc_pop_df <- county_tnc %>%
  mutate(pop = county_tnc_pop_all$pop)

county_tnc_df <- county_tnc %>% 
  mutate(pop = county_tnc_pop_all) %>%
  st_drop_geometry %>%
  dplyr::select(fips_state, fips_county, ECO_CODE, pop) %>%
  setNames(c('state_fips', 'county_fips', 'TNC', 'pop'))

write_csv(county_tnc_df, 'data/cfs_io_analysis/population_county_x_TNC_longform.csv')
