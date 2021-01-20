# Summary flows (inbound, outbound, and net) by scenario, for scenario analysis V2
# Sum goods flows by county, land flows by county, land flows by ecoregion, and species extinction flows by ecoregion
# QDR / Virtualland / 20 Jan 2021 (aka FDT Day)

library(tidyverse)


# Goods flows by county ---------------------------------------------------

fp_goods <- 'data/cfs_io_analysis/county_consumption_csvs'

scenario_combos <- expand_grid(diet = c('baseline','planetaryhealth','medstyle','usstyle','vegetarian'),
                               waste = c('baseline','presconsumer','consumer','allavoidable'))

sum_goods_flows_county <- function(diet, waste) {
  flows <- read_csv(glue::glue('{fp_goods}/D_{diet}_WR_{waste}_wide.csv'))
  
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

county_goods_flows <- pmap_dfr(scenario_combos, sum_goods_flows_county)

write_csv(county_goods_flows, 'data/cfs_io_analysis/scenarios/goodsflows_county_sums_all_scenarios.csv')

# Land flows by county ----------------------------------------------------

fp_landcounties <- 'data/cfs_io_analysis/county_land_consumption_csvs'

sum_land_flows_county <- function(diet, waste) {
  flows <- read_csv(glue::glue('{fp_landcounties}/D_{diet}_WR_{waste}_landconsumption.csv'))
  
  # Longform data so outbound and inbound sums are grouped separately
  flows_outbound <- flows %>%
    group_by(scenario, land_type, county_from) %>%
    summarize(flow_outbound = sum(land_consumption, na.rm = TRUE)) %>%
    ungroup %>%
    rename(county = county_from)
  
  flows_inbound <- flows %>%
    group_by(scenario, land_type, county_fips) %>%
    summarize(flow_inbound = sum(land_consumption, na.rm = TRUE)) %>%
    ungroup %>%
    rename(county = county_fips)

  # Join and split scenario column up
  full_join(flows_inbound, flows_outbound, by = c('scenario', 'land_type', 'county')) %>%
    separate(scenario, into = c('D', 'scenario_diet', 'W', 'scenario_waste'), sep = '_') %>%
    mutate(land_type = gsub('_exchange', '', land_type, fixed = TRUE)) %>%
    select(scenario_diet, scenario_waste, land_type, county, flow_inbound, flow_outbound)
  
}

county_land_flows <- pmap_dfr(scenario_combos, sum_land_flows_county)

write_csv(county_land_flows, 'data/cfs_io_analysis/scenarios/landflows_county_sums_all_scenarios.csv')


# Land flows by ecoregion -------------------------------------------------

tnc_landflows <- read_csv('data/cfs_io_analysis/scenarios/landflows_tnc_x_tnc_all_scenarios.csv')

flows_outbound <- tnc_landflows %>%
  select(-TNC_to) %>%
  group_by(scenario, TNC_from) %>%
  summarize_all(sum) %>%
  ungroup %>%
  pivot_longer(-c(scenario, TNC_from), names_to = 'land_type', values_to = 'flow_outbound') %>%
  rename(TNC = TNC_from) %>%
  mutate(land_type = gsub('_flow', '', land_type, fixed = TRUE)) %>%
  separate(scenario, into = c('D', 'scenario_diet', 'W', 'scenario_waste'), sep = '_') %>%
  select(scenario_diet, scenario_waste, land_type, TNC, flow_outbound)

flows_inbound <- tnc_landflows %>%
  select(-TNC_from) %>%
  group_by(scenario, TNC_to) %>%
  summarize_all(sum) %>%
  ungroup %>%
  pivot_longer(-c(scenario, TNC_to), names_to = 'land_type', values_to = 'flow_inbound') %>%
  rename(TNC = TNC_to) %>%
  mutate(land_type = gsub('_flow', '', land_type, fixed = TRUE)) %>%
  separate(scenario, into = c('D', 'scenario_diet', 'W', 'scenario_waste'), sep = '_') %>%
  select(scenario_diet, scenario_waste, land_type, TNC, flow_inbound)

tnc_land_flows <- full_join(flows_outbound, flows_inbound)

write_csv(tnc_land_flows, 'data/cfs_io_analysis/scenarios/landflows_tnc_sums_all_scenarios.csv')


# Biodiversity flows by ecoregion -----------------------------------------

intensity_values <- c('low', 'med', 'high')

for (intensity in intensity_values) {
  
  tnc_specieslost <- read_csv(glue::glue('data/cfs_io_analysis/scenarios/species_lost_all_scenarios_occ_{intensity}.csv'))
  
  flows_outbound <- tnc_specieslost %>%
    filter(statistic == 'mean') %>%
    group_by(scenario, TNC_from, land_use, taxon) %>%
    summarize(flow_outbound = sum(species_lost, na.rm = TRUE)) %>%
    ungroup %>%
    rename(TNC = TNC_from) %>%
    separate(scenario, into = c('D', 'scenario_diet', 'W', 'scenario_waste'), sep = '_') %>%
    select(scenario_diet, scenario_waste, land_use, taxon, TNC, flow_outbound)
  
  flows_inbound <- tnc_specieslost %>%
    filter(statistic == 'mean') %>%
    group_by(scenario, TNC_to, land_use, taxon) %>%
    summarize(flow_inbound = sum(species_lost, na.rm = TRUE)) %>%
    ungroup %>%
    rename(TNC = TNC_to) %>%
    separate(scenario, into = c('D', 'scenario_diet', 'W', 'scenario_waste'), sep = '_') %>%
    select(scenario_diet, scenario_waste, land_use, taxon, TNC, flow_inbound)
  
  tnc_extinction_flows <- full_join(flows_outbound, flows_inbound)
  
  write_csv(tnc_extinction_flows, glue::glue('data/cfs_io_analysis/scenarios/species_lost_tnc_sums_all_scenarios_{intensity}.csv'))
  message(paste(intensity, ' written.'))
}