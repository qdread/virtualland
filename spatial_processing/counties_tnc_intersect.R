# Conversion of USA counties to ecoregions, using cropland map to determine how much land is in each ecoregion portion of each county.
# Tabulate 2012 CDL by combination of county polygon and TNC ecoregion polygon
# Also tabulate 2010 US 1 km population grid to get area weightings 

# QDR / Virtualland / 11 Jan 2021
# (copied and modified from cfs_tnc_intersect.R)
# Modified 29 Jan 2021: Use "fixed" county_aea that has the fixed county names. (the one created in county_aea.R)

# Perform intersect -------------------------------------------------------

# First we need to create a combined polygon layer that contains the intersection of every FAF polygon and TNC ecoregion.

library(sf)

fp_out <- 'data/cfs_io_analysis'

# Load county and TNC polygons
tncmap <- st_read(dsn = 'data/raw_data/landuse/ecoregions', layer = 'tnc_usa_aea')
county_aea <- st_read('data/raw_data/landuse/USA/USA_county_2014_aea.gpkg')

# Intersect the two
county_tnc <- st_intersection(county_aea, tncmap) 
# This results in 4690 polygons combining the two

# Write result as geopackage
st_write(county_tnc, dsn = file.path(fp_out, 'county_tnc_aea_intersect.gpkg'))

# Calculate areas of all intersected polygons
county_tnc_area <- st_area(county_tnc)

# Extract attributes from the geometry, and include area column, to make a long and wide form of the area table
county_tnc_area_df <- county_tnc %>%
  st_drop_geometry %>%
  select(ID_1, ID_2, NAME_1, NAME_2, ECO_ID_U, ECO_CODE, region) %>%
  mutate(area = county_tnc_area)

# Create a table by state and county code
county_tnc_area_mat <- county_tnc_area_df %>%
  select(ID_1, ID_2, ECO_CODE, area) %>%
  mutate(area = as.numeric(area)) %>%
  pivot_wider(names_from = ECO_CODE, values_from = area, values_fill = list(area = 0))

# The tabulation of the CDL raster by this intersected polygon layer is done in Python from a shell script


# Wrangle tabulated CDL raster by intersected layer -----------------------

library(tidyverse)
library(sf)
cdlcountytnc2012 <- read_csv(file.path('data/raw_data/landuse/output_csvs', 'CDL_2012_countyTNC.csv')) %>%
  rename(region = X1) %>%
  pivot_longer(-region, names_to = 'cdl_class', values_to = 'n_pixels')

# The region names are just 0 to n in this output so we need to match it with the correct names.
countytncmap <- st_read(file.path(fp_out, 'county_tnc_aea_intersect.gpkg'))

region_lookup <- countytncmap %>%
  st_drop_geometry %>%
  select(ID_1, ID_2, NAME_1, NAME_2, ECODE_NAME, ECO_CODE) %>%
  setNames(c('id_state', 'id_county', 'name_state', 'name_county', 'TNC_Region', 'TNC_Code')) %>%
  mutate(region = 0:(nrow(.)-1))

cdlcountytnc2012 <- cdlcountytnc2012 %>%
  left_join(region_lookup) %>%
  mutate(n_pixels = if_else(is.na(n_pixels), 0, n_pixels))

# Get the actual names of the CDL cover classes for this data frame.
cdlclasses <- read_table('data/raw_data/landuse/USDAcropland/CDL/cdlclasses.txt') %>%
  mutate(CDL_Code = map_chr(CDL_Code, str_extract, "[0-9]+"))

cdlcountytnc2012 <- cdlcountytnc2012 %>%
  left_join(cdlclasses, by = c('cdl_class' = 'CDL_Code'))

write_csv(cdlcountytnc2012, file.path(fp_out, 'CDL_x_county_x_TNC_counts.csv'))


# Get population counts by county TNC intersect ------------------------------

library(raster)

usgrid <- raster(file.path('data/raw_data/Census/uspopgrid/geotiff', 'uspop10.tif'))

# Transform usgrid to AEA
usgrid_aea <- projectRaster(usgrid, crs = crs(countytncmap))

# Extract population totals by intersected polygon
# The additional wrapper around sum is necessary for na.rm (I think)
county_tnc_pop <- raster::extract(usgrid_aea, countytncmap, fun = function(x, ...) sum(x, na.rm = TRUE), df = TRUE)

# Do the same for Hawaii and the two sections of Alaska, for completeness
higrid <- raster(file.path('data/raw_data/Census/uspopgrid/geotiff', 'hipop10.tif'))
akgrid <- raster(file.path('data/raw_data/Census/uspopgrid/geotiff', 'akpop10.tif'))
akeastgrid <- raster(file.path('data/raw_data/Census/uspopgrid/geotiff', 'ehpop10.tif'))

# Transform the three other grids to AEA
higrid_aea <- projectRaster(higrid, crs = crs(countytncmap))
akgrid_aea <- projectRaster(akgrid, crs = crs(countytncmap))
akeastgrid_aea <- projectRaster(akeastgrid, crs = crs(countytncmap))

hi_pop <- raster::extract(higrid_aea, countytncmap, fun = function(x, ...) sum(x, na.rm = TRUE), df = TRUE)
ak_pop <- raster::extract(akgrid_aea, countytncmap, fun = function(x, ...) sum(x, na.rm = TRUE), df = TRUE)
akeast_pop <- raster::extract(akeastgrid_aea, countytncmap, fun = function(x, ...) sum(x, na.rm = TRUE), df = TRUE)

# Combine all three sections into one data frame. (no AK east)
county_tnc_pop_all <- Reduce(left_join, list(county_tnc_pop, hi_pop, ak_pop))

# Check that there is only one non-NA per row
table(apply(county_tnc_pop_all[,-1], 1, function(x) sum(!is.na(x)))) 

county_tnc_pop_all <- county_tnc_pop_all %>%
  group_by(ID) %>%
  summarize(pop = sum(uspop10, hipop10, akpop10, na.rm = TRUE))

county_tnc_pop_df <- countytncmap %>%
  mutate(pop = county_tnc_pop_all$pop)

ggplot(county_tnc_pop_df) +
  geom_sf(aes(fill = pop), color = NA) +
  scale_fill_viridis_c()

write_csv(county_tnc_pop_df %>% st_drop_geometry, file.path(fp_out, 'population_county_x_TNC.csv'))


# Calculate county to ecoregion weights --------------------------------------

# Use the previously created population dataframe.
# We want to get the proportion of each ecoregion's population in each county, and vice versa

# Create a FAF x TNC matrix with the populations.

countytncpop <- county_tnc_pop_df %>% 
  st_drop_geometry %>%
  dplyr::select(STATEFP, COUNTYFP, NAME, ECO_CODE, pop) %>%
  setNames(c('state_fips', 'county_fips', 'name_county', 'TNC', 'pop'))

# Make into a big sparse matrix
countytncmat <- countytncpop %>%
  pivot_wider(id_cols = c(state_fips, county_fips, name_county), names_from = TNC, values_from = pop, values_fill = 0)

# This could now be normalized by row totals or by column totals to get the incoming or outgoing weights.
# Save just the three column matrix which will not take up as much space.

write_csv(countytncpop, file.path(fp_out, 'population_county_x_TNC_longform.csv'))
