# Conversion of FAF regions to ecoregions, using cropland map to determine how much land is in each ecoregion portion of each FAF region.
# Tabulate 2012 CDL by combination of FAF polygon and TNC ecoregion
# Also tabulate 2010 US 1 km population grid to get area weightings (added 14 Aug 2020)

# QDR / Virtualland


# Perform intersect -------------------------------------------------------

# First we need to create a combined polygon layer that contains the intersection of every FAF polygon and TNC ecoregion.

library(sf)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_cfs <- file.path(fp, 'raw_data/commodity_flows/CFS')
fp_faf <- file.path(fp, 'raw_data/commodity_flows/FAF')
fp_crosswalk <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'crossreference_tables')
fp_out <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'cfs_io_analysis')
fp_fwe <- ifelse(is_local, '~/Documents/GitHub/fwe', '~/fwe')

# Load CFS(FAF) and TNC polygons
cfsmap <- st_read(dsn = file.path(fp_faf, 'Freight_Analysis_Framework_Regions/cfs_aea.gpkg'))
tncmap <- st_read(dsn = file.path(fp, 'raw_data/landuse/ecoregions'), layer = 'tnc_usa_aea')

# Intersect the two
cfs_tnc <- st_intersection(cfsmap, tncmap) 
# This results in 453 polygons combining the two
plot(cfs_tnc['Code']) # Looks correct

# Write result as geopackage
st_write(cfs_tnc, dsn = file.path(fp_out, 'cfs_tnc_aea_intersect.gpkg'))

# Calculate areas of all intersected polygons
cfs_tnc_area <- st_area(cfs_tnc)

# Extract attributes from the geometry, and include area column, to make a long and wide form of the area table
cfs_tnc_area_df <- cfs_tnc %>%
  as_tibble %>%
  select(FAF_Region, Code, State, ECO_ID_U, ECO_CODE, region) %>%
  mutate(area = cfs_tnc_area)

# Create a table by FAF code 

cfs_tnc_area_mat <- cfs_tnc_area_df %>%
  select(Code, ECO_CODE, area) %>%
  mutate(area = as.numeric(area)) %>%
  pivot_wider(names_from = ECO_CODE, values_from = area, values_fill = list(area = 0))

# The tabulation of the CDL raster by this intersected polygon layer is done in Python from a shell script


# Wrangle tabulated CDL raster by intersected layer -----------------------

library(tidyverse)
library(sf)
cdlcfstnc2012 <- read_csv(file.path(fp, 'raw_data/landuse/output_csvs', 'CDL_2012_CFSTNC.csv')) %>%
  rename(region = X1) %>%
  pivot_longer(-region, names_to = 'cdl_class', values_to = 'n_pixels')

# The region names are just 0 to 452 in this output so we need to match it with the correct names.
cfstncmap <- st_read(file.path(fp_out, 'cfs_tnc_aea_intersect.gpkg'))

region_lookup <- cfstncmap %>%
  as_tibble %>%
  select(FAF_Region, ECODE_NAME, Code, ECO_CODE) %>%
  setNames(c('FAF_Region', 'TNC_Region', 'FAF_Code', 'TNC_Code')) %>%
  mutate(region = 0:452)

cdlcfstnc2012 <- cdlcfstnc2012 %>%
  left_join(region_lookup) %>%
  mutate(n_pixels = if_else(is.na(n_pixels), 0, n_pixels))

# Get the actual names of the CDL cover classes for this data frame.
cdlclasses <- read_table(file.path(fp, 'raw_data/landuse/USDAcropland/CDL/cdlclasses.txt')) %>%
  mutate(CDL_Code = map_chr(CDL_Code, str_extract, "[0-9]+"))

cdlcfstnc2012 <- cdlcfstnc2012 %>%
  left_join(cdlclasses, by = c('cdl_class' = 'CDL_Code'))

write_csv(cdlcfstnc2012, file.path(fp_out, 'CDL_x_FAF_x_TNC_counts.csv'))


# Get population counts by CFS TNC intersect ------------------------------

library(raster)

usgrid <- raster(file.path(fp, 'raw_data/Census/uspopgrid/geotiff', 'uspop10.tif'))

# Transform usgrid to AEA
usgrid_aea <- projectRaster(usgrid, crs = crs(cfs_tnc))

# Extract population totals by intersected polygon
# The additional wrapper around sum is necessary for na.rm (I think)
cfs_tnc_pop <- raster::extract(usgrid_aea, cfs_tnc, fun = function(x, ...) sum(x, na.rm = TRUE), df = TRUE)

# Do the same for Hawaii and the two sections of Alaska, for completeness
higrid <- raster(file.path(fp, 'raw_data/Census/uspopgrid/geotiff', 'hipop10.tif'))
akgrid <- raster(file.path(fp, 'raw_data/Census/uspopgrid/geotiff', 'akpop10.tif'))
akeastgrid <- raster(file.path(fp, 'raw_data/Census/uspopgrid/geotiff', 'ehpop10.tif'))

# Transform the three other grids to AEA
higrid_aea <- projectRaster(higrid, crs = crs(cfs_tnc))
akgrid_aea <- projectRaster(akgrid, crs = crs(cfs_tnc))
akeastgrid_aea <- projectRaster(akeastgrid, crs = crs(cfs_tnc))

hi_pop <- raster::extract(higrid_aea, cfs_tnc, fun = function(x, ...) sum(x, na.rm = TRUE), df = TRUE)
ak_pop <- raster::extract(akgrid_aea, cfs_tnc, fun = function(x, ...) sum(x, na.rm = TRUE), df = TRUE)
akeast_pop <- raster::extract(akeastgrid_aea, cfs_tnc, fun = function(x, ...) sum(x, na.rm = TRUE), df = TRUE)

# Combine all four sections into one data frame.
cfs_tnc_pop_all <- Reduce(left_join, list(cfs_tnc_pop, hi_pop, ak_pop, akeast_pop))

# Check that there is only one non-NA per row
table(apply(cfs_tnc_pop_all[,-1], 1, function(x) sum(!is.na(x)))) # one has both.

which(apply(cfs_tnc_pop_all[,-1], 1, function(x) sum(!is.na(x))) > 1)
cfs_tnc[79,] # OK because one is zero.

cfs_tnc_pop_all <- cfs_tnc_pop_all %>%
  group_by(ID) %>%
  summarize(pop = sum(uspop10, hipop10, akpop10, ehpop10, na.rm = TRUE))

cfs_tnc_pop_df <- cfs_tnc %>%
  mutate(pop = cfs_tnc_pop_all$pop)

ggplot(cfs_tnc_pop_df) +
  geom_sf(aes(fill = pop)) +
  scale_fill_viridis_c()

st_geometry(cfs_tnc_pop_df) <- NULL

write_csv(cfs_tnc_pop_df, file.path(fp_out, 'population_FAF_x_TNC.csv'))


# Calculate FAF to ecoregion weights --------------------------------------

# Use the previously created population dataframe.
# We want to get the proportion of each ecoregion's population in each CFS region, and vice versa

# Create a FAF x TNC matrix with the populations.

faftncpop <- cfs_tnc_pop_df %>% 
  select(Code, ECO_CODE, pop) %>%
  setNames(c('FAF', 'TNC', 'pop'))

# Make into a big sparse matrix
faftncmat <- faftncpop %>%
  pivot_wider(id_cols = FAF, names_from = TNC, values_from = pop, values_fill = 0)

# This could now be normalized by row totals or by column totals to get the incoming or outgoing weights.
# Save just the three column matrix which will not take up as much space.

write_csv(faftncpop, file.path(fp_out, 'population_FAF_x_TNC_3column.csv'))
