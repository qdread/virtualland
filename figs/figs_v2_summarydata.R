# Summary flows (inbound, outbound, and net) by scenario, for scenario analysis V2
# Sum goods flows by county, land flows by county, land flows by ecoregion, and species extinction flows by ecoregion
# QDR / Virtualland / 20 Jan 2021 (aka FDT Day)

# Modified 24 Mar 2021: remove deprecated biodiversity flow summaries.

library(tidyverse)
library(vroom) # To speed up

# Land flows by ecoregion -------------------------------------------------

tnc_landflows <- vroom('data/cfs_io_analysis/scenarios/landflows_tnc_x_tnc_all_scenarios.csv')

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

# Total demand sums -------------------------------------------------------

### County-level consumption and production
county_production <- vroom('data/cfs_io_analysis/county_production2012.csv')
county_consumption <- vroom('data/cfs_io_analysis/county_consumption2012_allscenarios.csv') # 320 MB
county_totaldemand <- vroom('data/cfs_io_analysis/county_totaldemand2012_allscenarios.csv') # 430 MB

source('figs/figs_v2_lookups.R')

# Need to reshape county_totaldemand, sum across counties, and possibly calculate relative to baseline.
totaldemand_sums <- cbind(county_totaldemand[,c('BEA_code', 'scenario')], demand = rowSums(county_totaldemand[,-(1:2)])) %>%
  separate(scenario, into = c('d', 'scenario_diet', 'w', 'scenario_waste'), sep = '_') %>% 
  select(-d, -w) %>%
  mutate(scenario_diet = factor(scenario_diet, levels = diet_levels_ordered),
         scenario_waste = factor(scenario_waste, levels = waste_levels_ordered)) %>% 
  right_join(ag_names_lookup, by = c('BEA_code' = 'BEA_389_code'))

# Correct such that greenhouse crops are added to peanuts and sugar.
totaldemand_sums <- totaldemand_sums %>%
  mutate(BEA_code = if_else(BEA_code == '111400', '111900', BEA_code),
         short_name = fct_collapse(short_name, `peanuts, sugar, etc.` = c('greenhouse crops', 'peanuts, sugar, etc.'))) %>%
  group_by(BEA_code, short_name, kingdom, scenario_diet, scenario_waste) %>%
  summarize(demand = sum(demand))

write_csv(totaldemand_sums, 'data/cfs_io_analysis/scenarios/totaldemand_sums_all_scenarios.csv')
