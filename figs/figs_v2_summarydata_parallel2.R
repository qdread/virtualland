# Summary flows (inbound, outbound, and net) by scenario, for scenario analysis V2
# Sum land flows by county (2 of 2)
# QDR / Virtualland / 20 Jan 2021 (aka FDT Day)

# Modified 23 Apr 2021: split into two scripts

library(tidyverse)
library(furrr) # To parallelize.

options(mc.cores = 8)
plan(multicore)

# Land flows by county ----------------------------------------------------

fp_landcounties <- 'data/cfs_io_analysis/county_land_consumption_csvs'

scenario_combos <- expand_grid(diet = c('baseline','planetaryhealth','medstyle','usstyle','vegetarian'),
                               waste = c('baseline','preconsumer','consumer','allavoidable'))

sum_land_flows_county <- function(diet, waste) {
  flows <- read_csv(glue::glue('{fp_landcounties}/D_{diet}_WR_{waste}_landconsumption.csv'))
  message(glue::glue('{diet} by {waste} read'))
  # Longform data so outbound and inbound sums are grouped separately
  flows_outbound <- flows %>%
    group_by(scenario, land_type, county_from) %>%
    summarize(flow_outbound = sum(land_consumption, na.rm = TRUE)) %>%
    ungroup %>%
    rename(county = county_from)
  
  flows_inbound <- flows %>%
    group_by(scenario, land_type, county_to) %>%
    summarize(flow_inbound = sum(land_consumption, na.rm = TRUE)) %>%
    ungroup %>%
    rename(county = county_to)

  # Join and split scenario column up
  full_join(flows_inbound, flows_outbound, by = c('scenario', 'land_type', 'county')) %>%
    separate(scenario, into = c('D', 'scenario_diet', 'W', 'scenario_waste'), sep = '_') %>%
    mutate(land_type = gsub('_exchange', '', land_type, fixed = TRUE)) %>%
    select(scenario_diet, scenario_waste, land_type, county, flow_inbound, flow_outbound)
  
}

county_land_flows <- future_pmap_dfr(scenario_combos, sum_land_flows_county)

write_csv(county_land_flows, 'data/cfs_io_analysis/scenarios/landflows_county_sums_all_scenarios.csv')
