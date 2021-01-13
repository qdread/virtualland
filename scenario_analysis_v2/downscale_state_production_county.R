# Downscale state level production by BEA to county level
# Copied and modified from county_consumption_to_faf.R

# Load data ---------------------------------------------------------------

library(tidyverse)

fp_out <- 'data/cfs_io_analysis'

# Production by state
receipts_bea_sctg_x_state <- read_csv(file.path(fp_out, 'receipts_bea_sctg_x_state.csv')) # Created in sctg_to_bea.R

# Read county level weighting data for downscaling state production to county
county_weightings <- read_csv(file.path(fp_out, 'county_weightings_for_downscale.csv'), col_types = c('cccciiii'))

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
