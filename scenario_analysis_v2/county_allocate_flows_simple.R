# Allocate county-level consumption from each scenario to producing counties
# Use the very basic assumption that distance from producer to consumer does not matter
# Each county sends goods to all other counties exactly proportional to how much they consume
# QDR / Virtualland / 07 Jan 2021


# Load consumption and production data ------------------------------------

library(tidyverse)

fp_out <- 'data/cfs_io_analysis'

# Consumption data for all scenarios (2012)
county_consumption <- read_csv(file.path(fp_out, 'county_totaldemand2012_allscenarios.csv'))
# Production downscaled (2012)
county_production <- read_csv(file.path(fp_out, 'county_production2012.csv'))

# Get vector of primary agricultural goods.
bea_codes <- unique(county_consumption$BEA_code)
ag_goods <- bea_codes[substr(bea_codes, 1, 3) %in% c('111', '112')]

# Prepare data ------------------------------------------------------------

# Normalize production for each BEA code, retaining only primary agricultural goods
county_production_ag <- county_production %>%
  select(county_fips, BEA_code, production_county_downscaled) %>%
  filter(BEA_code %in% ag_goods) %>%
  group_by(BEA_code) %>%
  mutate(production_norm = production_county_downscaled/sum(production_county_downscaled))

# Reshape consumption data to long, retaining only ag goods
# Also add missing digits to the county fips code
county_consumption_ag <- county_consumption %>%
  filter(BEA_code %in% ag_goods) %>%
  pivot_longer(-c(BEA_code, scenario), names_to = 'county_fips', values_to = 'consumption') %>%
  mutate(county_fips = if_else(nchar(county_fips) == 4, paste0('0', county_fips), county_fips))


# Join data ---------------------------------------------------------------

# Harmonize production codes with consumption codes

# Check county codes match
prod_unmatched <- setdiff(x = county_production_ag$county_fips, y = county_consumption_ag$county_fips)
cons_unmatched <- setdiff(y = county_production_ag$county_fips, x = county_consumption_ag$county_fips)
# There are quite a few that aren't in production but I think it's because those do not produce anything. 

# The mismatches are:
# We have Maui 15009 which is combined with Kalawao 15005 and renamed 15901. 
# We have 23 counties in Virginia which were merged with an independent city they contain, and renamed. 
# In all cases it is a one to one correspondence so we can use our harmonization crosswalk to rename.

fips_harmonization <- read_csv('data/crossreference_tables/fips_harmonization.csv')
fips_tojoin <- fips_harmonization %>%
  filter(grepl('^15', FIPS_data) | grepl('^51', FIPS_data)) %>% # keep VA and HI
  select(FIPS_data, FIPS_map1) %>%
  setNames(c('county_fips_replacement', 'county_fips'))

# Join production with the codes to rename, and rename where appropriate.
county_production_ag <- county_production_ag %>% 
  left_join(fips_tojoin) %>%
  mutate(county_fips = if_else(is.na(county_fips_replacement), county_fips, county_fips_replacement)) %>%
  select(-county_fips_replacement)

# Join consumption and production
# Fill NA values in with zeroes.
county_cons_prod_ag <- left_join(county_consumption_ag, county_production_ag) %>%
  replace_na(list(production_county_downscaled = 0, production_norm = 0))

# Allocate flows of agricultural goods ------------------------------------

# We assume that each producing region of the USA sends goods with equal probability around the country
# proportional only to the demand of the consuming region (i.e. ignores how far apart the two are)

# For each region and good, multiply the total consumption times the relative proportion of production.
# This function will be applied to each scenario and good separately.
allocate_consumption <- function(data) {
  mat <- data$production_norm %*% t(data$consumption)
  dimnames(mat)[[2]] <- data$BEA_code
  as_tibble(mat)
}

county_consumption_allocated_wide <- county_cons_prod_ag %>%
  group_by(BEA_code, scenario) %>%
  nest %>%
  mutate(consumption_allocated = map(data, allocate_consumption))

# convert allocated wideform data, currently in list columns, to a data frame in longform.
# These start to be very large data frames. 
county_consumption_allocated_wide <- county_consumption_allocated_wide %>% 
  mutate(consumption_allocated = map2(data, consumption_allocated, ~ cbind(county_fips = .x$county_fips, .y))) %>%
  select(-data)

# bind everything together and rename things where appropriate
county_consumption_allocated_wide_df <- county_consumption_allocated_wide %>%
  unnest(cols = consumption_allocated) %>%
  setNames(c('BEA_code', 'scenario', 'county_fips', county_consumption_allocated_wide$consumption_allocated[[1]]$county_fips))

# To make this slightly more feasible, split by scenario and write to csvs individually per scenario (wideform)
county_consumption_allocated_wide_df %>%
  ungroup %>%
  group_split(scenario, .keep = TRUE) %>%
  walk(~ write_csv(., file.path(fp_out, 'county_consumption_csvs', paste0(.$scenario[1], '_wide.csv'))))

# Do not run the following code as longform takes up at least 3x disk space of wideform.  
# Pivot to longform and write to CSVs as well (to facilitate later reading)
# pivot_long_write <- function(dat) {
#   dat_long <- dat %>% 
#     pivot_longer(-c(BEA_code, scenario, county_fips), values_to = 'consumption', names_to = 'county_to') %>%
#     rename(county_from = county_fips)
#   write_csv(dat_long, file.path(fp_out, 'county_consumption_csvs', paste0(dat_long$scenario[1], '_long.csv')))
# }
# 
# county_consumption_allocated_wide_df %>%
#   ungroup %>%
#   group_split(scenario, .keep = TRUE) %>%
#   walk(pivot_long_write)
