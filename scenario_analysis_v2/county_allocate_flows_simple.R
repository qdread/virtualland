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

# Crosswalk table which has info for which codes are in the food system (primary agricultural goods only)
bea_table <- read_csv('data/crossreference_tables/naics_crosswalk_final.csv')

ag_goods <- bea_table %>% filter(substr(BEA_389_code,1,3) %in% c('111','112'))

# Prepare data ------------------------------------------------------------

# Normalize production for each BEA code, retaining only primary agricultural goods
county_production_ag <- county_production %>%
  select(county_fips, BEA_code, production_county_downscaled) %>%
  filter(BEA_code %in% ag_goods$BEA_389_code) %>%
  group_by(BEA_code) %>%
  mutate(production_norm = production_county_downscaled/sum(production_county_downscaled))

# Reshape consumption data to long, retaining only ag goods
# Also add missing digits to the county fips code
county_consumption_ag <- county_consumption %>%
  filter(BEA_code %in% ag_goods$BEA_389_code) %>%
  pivot_longer(-c(BEA_code, scenario), names_to = 'county_fips', values_to = 'consumption') %>%
  mutate(county_fips = if_else(nchar(county_fips) == 4, paste0('0', county_fips), county_fips))


# Join data ---------------------------------------------------------------

# Check county codes match
setdiff(x = county_production_ag$county_fips, y = county_consumption_ag$county_fips)
setdiff(y = county_production_ag$county_fips, x = county_consumption_ag$county_fips)
# There are quite a few that aren't in production but I think it's because those do not produce anything. Most are cities in VA and some places in AK.
# The two counties not accounted for in consumption are Aleutian islands 02010 and Oglala Lakota 46102
# 02010 was split into an east and west region 02013 and 02016
# 46102 was changed from 46113.

# To fix, just change 46113 in consumption to 46102. Then add the consumption from 02013 and 02016 together.
aleutians_consumption <- county_consumption_ag %>% 
  filter(county_fips %in% c('02013','02016')) %>%
  group_by(BEA_code, scenario) %>%
  summarize(consumption = sum(consumption)) %>%
  mutate(county_fips = '02010')

county_consumption_ag <- county_consumption_ag %>%
  mutate(county_fips = if_else(county_fips == '46113', '46102', county_fips)) %>%
  filter(!county_fips %in% c('02013','02016')) %>%
  bind_rows(aleutians_consumption)

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

# # Pivot to longform (currently not being done)
# # Do the pivoting operation in chunks to save memory.
# county_consumption_allocated_long <- county_consumption_allocated_wide %>% 
#   mutate(consumption_allocated = map(consumption_allocated, function(dat) {
#     names(dat) <- c('county_fips', dat$county_fips)
#     pivot_longer(dat, -county_fips)
#   }))
  
# county_consumption_allocated_long_df <- county_consumption_allocated_long %>%
#   unnest(cols = consumption_allocated) %>%
#   setNames(c('BEA_code', 'scenario', 'to', 'from', 'consumption'))
  
# write_csv(county_consumption_allocated_wide_df, file.path(fp_out, 'county_consumption_allocated_wide.csv'))
# write_csv(county_consumption_allocated_long_df, file.path(fp_out, 'county_consumption_allocated_long.csv'))

# To make this slightly more feasible, split by scenario and write to csvs individually per scenario (longform)
county_consumption_allocated_wide_df %>%
  group_split(scenario, .keep = TRUE) %>%
  walk(~ write_csv(., file.path(fp_out, 'county_consumption_csvs', paste0(.$scenario[1], '_wide.csv'))))
  