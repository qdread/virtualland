# Incorporate foreign-origin virtual land transfers into the FAF dataset
# QDR / 15 Sept 2020

# Procedure: 
# 1. Sum up the country-level virtual land transfers by region (FAF only has ~8 regions outside USA)
# 2. Divide them up again proportionally based on the relative amount of incoming imports at each FAF region
# (This makes the simplifying assumption that all ports receive an equal mix of imports from all countries)
# 3. Do the same reassignment from FAF region to TNC ecoregion that we did for domestic transfers

# List of countries by UN region R-friendly version available in package poldham/places

library(tidyverse)
library(places)
data(unregions)

faf4regions <- read_csv('/nfs/qread-data/raw_data/commodity_flows/FAF/faf4_foreign_lookup.csv')


# Get VLT by FAF foreign region -------------------------------------------



# Convert UN regions and subregions into FAF regions

unregions <- unregions %>%
  mutate(FAF_region = case_when(
    un_country_or_area %in% c('Canada', 'United States of America', 'Mexico') ~ un_country_or_area,
    un_region_name %in% c('Europe', 'Africa') ~ un_region_name,
    un_region_name %in% 'Americas' ~ 'Rest of Americas',
    un_sub_region_name %in% c('Southern Asia', 'Central Asia', 'Western Asia') ~ 'SW & Central Asia',
    un_sub_region_name %in% 'Eastern Asia' ~ 'Eastern Asia',
    un_region_name %in% 'Oceania' | un_sub_region_name %in% 'South-eastern Asia' ~ 'SE Asia & Oceania'
  )) %>%
  left_join(faf4regions, by = c('FAF_region' = 'FAF Region'))

# Select only country code, name, and FAF foreign region
unregions_faf <- unregions %>% 
  select(un_m49_code, un_country_or_area, FAF_region, Code) %>%
  setNames(c('un_m49_code', 'country_name', 'FAF_foreign_region', 'FAF_foreign_region_code'))

# Load virtual land transfers

VLT_all <- read_csv('/nfs/qread-data/cfs_io_analysis/fao_VLT_provisional.csv') %>%
  filter(VLT_crop > 0, VLT_pasture > 0)

# The codes do not match so we need to join it by name.
# Check names that don't match.
setdiff(VLT_all$country_name, unregions_faf$country_name) # The two Chinas.

unregions_faf$country_name[unregions_faf$country_name %in% c('China', 'China, Hong Kong Special Administrative Region')] <- 
  c('China, mainland', 'China, Hong Kong SAR')

VLT_all <- VLT_all %>%
  left_join(unregions_faf)

# Sum up by foreign region
VLT_by_foreign_region <- VLT_all %>%
  group_by(FAF_foreign_region, FAF_foreign_region_code) %>%
  summarize(VLT_crop = sum(VLT_crop), VLT_pasture = sum(VLT_pasture))

# Write the CSVs
write_csv(VLT_all, '/nfs/qread-data/cfs_io_analysis/foreign_VLT_by_country.csv')
write_csv(VLT_by_foreign_region, '/nfs/qread-data/cfs_io_analysis/foreign_VLT_by_region.csv')
