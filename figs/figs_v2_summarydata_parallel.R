# Summary flows (inbound, outbound, and net) by scenario, for scenario analysis V2
# Sum goods flows by county (1 of 2)
# QDR / Virtualland / 20 Jan 2021 (aka FDT Day)

# Modified 23 Apr 2021: split into two scripts

library(tidyverse)
library(furrr) # To parallelize.

options(mc.cores = 8)
plan(multicore)

# Goods flows by county ---------------------------------------------------

fp_goods <- 'data/cfs_io_analysis/county_consumption_csvs'

scenario_combos <- expand_grid(diet = c('baseline','planetaryhealth','medstyle','usstyle','vegetarian'),
                               waste = c('baseline','preconsumer','consumer','allavoidable'))

sum_goods_flows_county <- function(diet, waste) {
  flows <- read_csv(glue::glue('{fp_goods}/D_{diet}_WR_{waste}_wide.csv'))
  message(glue::glue('{diet} by {waste} read'))
  # Outbound: each row represents origin county, sum across all destination counties (columns)
  flows_outbound <- cbind(flows[, 1:3], flow_outbound = apply(flows[, -(1:3)], 1, sum)) %>%
    rename(county = county_fips)

  # Inbound: exclude column denoting origin county then sum by group and reshape
  flows_inbound <- flows %>%
    select(-county_fips) %>%
    group_by(BEA_code, scenario) %>%
    summarize_all(sum) %>%
    ungroup %>%
    pivot_longer(-c(BEA_code, scenario), names_to = 'county', values_to = 'flow_inbound')

    # Join and split scenario column up
  full_join(flows_inbound, flows_outbound, by = c('scenario', 'BEA_code', 'county')) %>%
    separate(scenario, into = c('D', 'scenario_diet', 'W', 'scenario_waste'), sep = '_') %>%
    select(scenario_diet, scenario_waste, BEA_code, county, flow_inbound, flow_outbound)

}

county_goods_flows <- future_pmap_dfr(scenario_combos, sum_goods_flows_county)

write_csv(county_goods_flows, 'data/cfs_io_analysis/scenarios/goodsflows_county_sums_all_scenarios.csv')
