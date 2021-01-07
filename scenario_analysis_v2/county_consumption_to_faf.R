# Aggregate BEA county consumption to FAF region
# Aggregate and/or downscale BEA county production to FAF region
# QDR / Virtualland / 17 Dec 2020

# Modified 07 Jan 2021: also write out county level production because we don't necessarily need to aggregate to FAF.

# Load data ---------------------------------------------------------------

library(tidyverse)
library(sf)

fp_out <- 'data/cfs_io_analysis'
fp_crosswalk <- 'data/crossreference_tables'
fp_faf <- 'data/raw_data/commodity_flows/FAF/Freight_Analysis_Framework_Regions'

# Consumption by county
county_totaldemand2012 <- read_csv(file.path(fp_out, 'county_totaldemand2012.csv'))
# Production by county
receipts_bea_sctg_x_state <- read_csv(file.path(fp_out, 'receipts_bea_sctg_x_state.csv')) # Created in sctg_to_bea.R

# FAF region shapefile is split by counties already
faf_regions <- st_read(file.path(fp_faf, 'Freight_Analysis_Framework_Regions.shp'))

# Read county level weighting data for downscaling state production to county
county_weightings <- read_csv(file.path(fp_out, 'county_weightings_for_downscale.csv'), col_types = c('cccciiii'))

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


# Downscale state level production to county ------------------------------

# Pivot the receipts data to long form and then join the county weightings to it
# Also need to convert the full state names to FIPS codes

fips_uppercase <- unique(tidycensus::fips_codes[, c('state', 'state_code', 'state_name')]) %>%
  mutate(state_name = toupper(state_name))

production_states <- receipts_bea_sctg_x_state %>%
  pivot_longer(-c(SCTG_Code, BEA_Code), names_to = 'state_name', values_to = 'production') %>%
  left_join(fips_uppercase)

production_counties <- inner_join(county_weightings, production_states, by = c('state_fips' = 'state_code', 'BEA_code' = 'BEA_Code')) %>%
  group_by(state_fips, state_name, SCTG_Code, BEA_code) %>%
  select(-n_employees, -q1_payroll, -annual_payroll) %>%
  mutate(production_county_downscaled = production * n_establishments/sum(n_establishments)) %>%
  replace_na(list(production_county_downscaled= 0)) %>%
  mutate(county_fips = paste0(state_fips, county_fips))

# Write county production to CSV
write_csv(production_counties, file.path(fp_out, 'county_production2012.csv'))

# Reaggregate downscaled county production to faf -------------------------

faf_counties <- faf_county_lookup$ANSI_ST_CO

setdiff(x=faf_counties,y=production_counties$county_fips)
setdiff(y=faf_counties,x=production_counties$county_fips)
# Not sure if this will matter. Not every county needs to be accounted for since we are aggregating back up to FIPS.
# The two counties not accounted for are Aleutian islands 02010 and Oglala Lakota 46102
faf_counties_extra <- data.frame(ANSI_ST_CO = c('02010', '46102'),
                                 CNTY_NAME = c('Aleutian Islands', 'Oglala Lakota County'),
                                 CFS12_NAME = c('Remainder of Alaska', 'Remainder of South Dakota'))

faf_county_lookup <- bind_rows(faf_county_lookup, faf_counties_extra)

production_counties_joined <- production_counties %>%
  left_join(faf_county_lookup, by = c('county_fips' = 'ANSI_ST_CO'))

faf_production_long <- production_counties_joined %>%
  group_by(CFS12_NAME, BEA_code) %>%
  summarize(production = sum(production_county_downscaled)) %>% 
  filter(!is.na(CFS12_NAME))

write_csv(faf_production_long, file.path(fp_out, 'faf_production2012.csv'))
