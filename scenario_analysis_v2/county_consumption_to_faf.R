# Aggregate BEA county consumption to FAF region
# Aggregate and/or downscale BEA county production to FAF region
# QDR / Virtualland / 17 Dec 2020

# Load data ---------------------------------------------------------------

library(tidyverse)
library(sf)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_out <- file.path(fp, 'cfs_io_analysis')   
fp_crosswalk <- file.path(fp, 'crossreference_tables')
fp_faf <- file.path(fp, 'raw_data/commodity_flows/FAF/Freight_Analysis_Framework_Regions')

# Consumption by county
county_totaldemand2012 <- read_csv(file.path(fp_out, 'county_totaldemand2012.csv'))
# Production by county
receipts_bea_sctg_x_state <- read_csv(file.path(fp_out, 'receipts_bea_sctg_x_state.csv')) # Created in sctg_to_bea.R

# FAF region shapefile is split by counties already
faf_regions <- st_read(file.path(fp_faf, 'Freight_Analysis_Framework_Regions.shp'))


# Join consumption and faf region data ------------------------------------

faf_county_lookup <- faf_regions %>%
  st_drop_geometry() %>%
  select(ANSI_ST_CO, CNTY_NAME, CFS12_NAME)

faf_counties <- faf_county_lookup$ANSI_ST_CO
demand_counties <- sprintf('%05d', as.integer(names(county_totaldemand2012)[-1]))

setdiff(x=faf_counties,y=demand_counties)
setdiff(y=faf_counties,x=demand_counties)
# These are all the same except that DC is listed at 11001 in FAF and 11000 in the demand table.

county_demand_long <- county_totaldemand2012 %>%
  pivot_longer(cols = -BEA_code, names_to = 'county_fips', values_to = 'demand') %>%
  mutate(county_fips = sprintf('%05d', as.integer(county_fips))) %>%
  mutate(county_fips = if_else(county_fips == '11000', '11001', county_fips)) %>%
  left_join(faf_county_lookup, by = c('county_fips' = 'ANSI_ST_CO'))

faf_demand_long <- county_demand_long %>%
  group_by(CFS12_NAME, BEA_code) %>%
  summarize(demand = sum(demand))
# For now leave in long form.

write_csv(faf_demand_long, file.path(fp_out, 'faf_demand2012.csv'))

