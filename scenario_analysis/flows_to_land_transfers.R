# Code to convert a set of BEA flows to virtual land transfers
# This generalizes the original code written just for the baseline case.
# Generalization of code in faf_land_transfers.R and faf_land_transfer_to_tnc.R
# QDR / Virtualland / 01 Oct 2020

# Modified 19 Nov 2020: Separate annual and permanent cropland.

# FIXME starts with domestic only; later foreign will be added. (We remove all trade type 2 and 3, fr_orig, and fr_dest)
# FIXME cropland flows in non-raw material shipments? So far all 3x codes are removed.

# Load data ---------------------------------------------------------------

library(tidyverse)
library(units)
library(sf)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_cfs <- file.path(fp, 'raw_data/commodity_flows/CFS')
fp_faf <- file.path(fp, 'raw_data/commodity_flows/FAF')
fp_crosswalk <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'crossreference_tables')
fp_out <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'cfs_io_analysis')
fp_github <- ifelse(is_local, '~/Documents/GitHub/foodwaste/virtualland', '~/virtualland')

# Read flows for each scenario.
flows_baseline <- read_csv(file.path(fp_out, 'scenarios/flows_baseline_domestic.csv'))
flows_diet <- read_csv(file.path(fp_out, 'scenarios/flows_dietshift_domestic_provisional.csv'))
flows_waste <- read_csv(file.path(fp_out, 'scenarios/flows_wastereduced_domestic_provisional.csv'))
flows_transport <- read_csv(file.path(fp_out, 'scenarios/flows_optimaltransport_domestic_provisional.csv'))

# FAF names lookup table
faf_lookup <- read_csv(file.path(fp_out, 'faf_region_lookup.csv'))

# load FAF by BEA cropland and pastureland
nass_bea_land <- read_csv(file.path(fp_out, 'nass_bea_state_x_faf_land_totals.csv'))

# NLCD pixel counts by the intersection of FAF region and TNC region in the USA
nlcd_faf_tnc <- read_csv(file.path(fp_out, 'NLCDcrop_FAF_x_TNC.csv'))

# Population counts by the intersection of FAF region and TNC region in the USA
pop_faf_tnc <- read_csv(file.path(fp_out, 'population_FAF_x_TNC_3column.csv'))

# For each origin, assume all ag land is converted to goods, to convert the weight or value flow to a land flow
nass_bea_land <- nass_bea_land %>%
  left_join(faf_lookup) %>%
  select(Code, FAF_Region, BEA_code, annual_cropland_normalized, permanent_cropland_normalized, pastureland_normalized) %>%
  setNames(c('FAF_Code', 'FAF_Region', 'BEA_Code', 'annual_cropland', 'permanent_cropland', 'pastureland'))


# Standardize input data (flows) ------------------------------------------

# Each scenario input data needs the same data structure
# We should sum up across modes and give everything the same name
# Modified 05 Oct 2020: we need to create a column of the weight change factor, relative to baseline
# That way we can scale the land flows accordingly.

flows_baseline_std <- flows_baseline %>%
  filter(trade_type == 1, BEA_Code %in% unique(nass_bea_land$BEA_Code)) %>%
  group_by( dms_orig, dms_dest, BEA_Code) %>%
  summarize(tons = sum(tons_2012, na.rm = TRUE))

flows_diet_std <- flows_diet %>%
  filter(trade_type == 1) %>%
  group_by(dms_orig, dms_dest, BEA_Code) %>%
  summarize(tons = sum(tons_reduced, na.rm = TRUE))

flows_waste_std <- flows_waste %>%
  filter(trade_type == 1) %>%
  group_by(dms_orig, dms_dest, BEA_Code) %>%
  summarize(tons = sum(tons_reduced, na.rm = TRUE))

flows_transport_std <- flows_transport %>%
  group_by(dms_orig, dms_dest, BEA_Code) %>%
  summarize(tons = sum(mass_optimal, na.rm = TRUE))

# Join the individual scenario dataframes into one.
flows_allscenarios <- bind_rows(list(baseline = flows_baseline_std, 
                                     diet = flows_diet_std, 
                                     waste = flows_waste_std, 
                                     transport = flows_transport_std), 
                                .id = 'scenario')

# Reshape to get the scaling factors
flows_allscenarios_wide <- flows_allscenarios %>%
  pivot_wider(names_from = scenario, values_from = tons) %>%
  mutate(diet_scalingfactor = diet/baseline,
         waste_scalingfactor = waste/baseline,
         transport_scalingfactor = transport/baseline)

# Reshape back again so we can scale the land totals for diet and waste.
flows_allscenarios <- flows_allscenarios_wide %>%
  pivot_longer(baseline:transport_scalingfactor, names_to = 'scenario') %>%
  separate(scenario, into = c('scenario', 'scalingfactor'), sep = '_') %>%
  mutate(scalingfactor = if_else(is.na(scalingfactor), 'tons', scalingfactor)) %>%
  pivot_wider(names_from = scalingfactor, values_from = value)
  
# Join tonnage flows with land areas --------------------------------------

# Join flows with NASS land values
join_with_land <- function(flows) {
  flows %>% left_join(nass_bea_land %>% select(-FAF_Region), by = c('dms_orig' = 'FAF_Code', 'BEA_Code' = 'BEA_Code'))
}

# Join then scale the diet and waste cropland and pastureland values by the scaling factor calculated previously.
flows_allscenarios <- flows_allscenarios %>% 
  join_with_land %>%
  mutate_at(vars(c(annual_cropland, permanent_cropland, pastureland)), ~ if_else(scenario %in% c('diet', 'waste'), . * scalingfactor, .))

# Convert to land flows ---------------------------------------------------

# Functions to get the proportional crop and pasture flow originating from each FAF region,
# and convert all land stocks and flows from acres to square kilometers.

# Modified 05 Oct 2020: We need to reduce the diet and waste land flows based on how far 

to_km2 <- function(x) x %>% set_units(acre) %>% set_units(km^2) %>% as.numeric

land_flows_by_weight <- function(flows) {
  flows %>%
    ungroup %>%
    filter(!is.na(annual_cropland), !is.na(permanent_cropland), !is.na(pastureland)) %>%
    group_by(scenario, dms_orig) %>%
    mutate(tons_proportion = tons / sum(tons, na.rm = TRUE)) %>%
    ungroup %>%
    mutate(annual_cropland_flow = tons_proportion * annual_cropland,
           permanent_cropland_flow = tons_proportion * permanent_cropland,
           pastureland_flow = tons_proportion * pastureland) %>%
    mutate_at(vars(contains('land')), to_km2)
}

flows_allscenarios <- flows_allscenarios %>% land_flows_by_weight

# Calculate summaries for plots -------------------------------------------

calculate_flow_totals <- function(flows) {
  # Domestic outbound
  land_outbound <- flows %>%
    filter(dms_orig != dms_dest) %>%
    group_by(dms_orig) %>%
    summarize(annual_cropland_flow = sum(annual_cropland_flow), 
              permanent_cropland_flow = sum(permanent_cropland_flow), 
              pastureland_flow = sum(pastureland_flow)) %>%
    add_row(dms_orig = '111', annual_cropland_flow = 0, permanent_cropland_flow = 0, pastureland_flow = 0) %>% # Washington DC
    rename(region = dms_orig)
  
  # Domestic inbound
  land_inbound <- flows %>%
    filter(dms_orig != dms_dest) %>%
    group_by(dms_dest) %>%
    summarize(annual_cropland_flow = sum(annual_cropland_flow), 
              permanent_cropland_flow = sum(permanent_cropland_flow), 
              pastureland_flow = sum(pastureland_flow)) %>%
    rename(region = dms_dest)
  
  # Domestic net
  land_netdomestic <- left_join(land_outbound, land_inbound, by = 'region') %>%
    mutate(annual_cropland_flow = annual_cropland_flow.y - annual_cropland_flow.x,
           permanent_cropland_flow = permanent_cropland_flow.y - permanent_cropland_flow.x,
           pastureland_flow = pastureland_flow.y - pastureland_flow.x) %>%
    select(region, annual_cropland_flow, permanent_cropland_flow, pastureland_flow) 
  
  # Domestic within region
  land_within <- flows %>%
    filter(dms_orig == dms_dest) %>%
    group_by(dms_orig) %>%
    summarize(annual_cropland_flow = sum(annual_cropland_flow), 
              permanent_cropland_flow = sum(permanent_cropland_flow), 
              pastureland_flow = sum(pastureland_flow)) %>%
    add_row(dms_orig = '111', annual_cropland_flow = 0, permanent_cropland_flow = 0, pastureland_flow = 0) %>% # Washington DC
    rename(region = dms_orig)
  
  return(bind_rows(list(outbound = land_outbound, inbound = land_inbound, netdomestic = land_netdomestic, within = land_within), .id = 'flow_type'))
}

flowtotals_allscenarios <- flows_allscenarios %>% group_by(scenario) %>% group_modify(~ calculate_flow_totals(.))

# Write outputs -----------------------------------------------------------

write_csv(flows_allscenarios, file.path(fp_out, 'scenarios/landflows_allscenarios_provisional.csv'))
write_csv(flowtotals_allscenarios, file.path(fp_out, 'scenarios/landflows_totals_allscenarios_provisional.csv'))

# For each FAF, split transfers by ecoregion ------------------------------

# Below this point is code derived from faf_land_transfer_to_tnc.R

# We can sum up the land transfers by ecoregion (across codes). 
# Then, for all outgoing transfers, we can assign proportions of them to each 
# ecoregion that the goods came from (based on crop and pastureland percentages in the ecoregions in each FAF.)

# Do not care which foreign region goods are going to, also do not care about transportation mode

flow_totals_pairwise <- function(flows) {
  flows %>%
    select(-BEA_Code) %>%
    group_by(scenario, dms_orig, dms_dest) %>%
    summarize_all(sum)
}

pairwiseflows <- flows_allscenarios %>% flow_totals_pairwise

# Only take essential rows from NLCD tally dataframe
nlcd_faf_tnc_reduced <- nlcd_faf_tnc %>% select(Code, ECO_CODE, crop, other, pasture, water)

# For each of the pairwise flows scenarios, check that all origin FAF regions besides Washington DC (111) are present
nlcdfafcodes <- unique(nlcd_faf_tnc_reduced$Code) 

pairwiseflows %>%
  group_by(scenario) %>%
  group_map(~ nlcdfafcodes[!nlcdfafcodes %in% .$dms_orig])
# All return only 111 (OK).

# Get the proportion crop and pasture in each ecoregion in each FAF before joining.
nlcd_faf_tnc_reduced <- nlcd_faf_tnc_reduced %>%
  group_by(Code) %>%
  mutate(cropland_ecoregion_proportion = crop / sum(crop, na.rm = TRUE),
         pastureland_ecoregion_proportion = pasture / sum(pasture, na.rm = TRUE)) %>%
  ungroup

flows_tnc_joined <- full_join(pairwiseflows, nlcd_faf_tnc_reduced, by = c('dms_orig' = 'Code'))

# Use the cropland and pastureland proportions to get the cropland and pastureland flows out of each ecoregion into each FAF.
# Multiply cropland flow by cropland proportion, and pastureland flow by pastureland proportion
multiply_tnc_flows <- function(flows) {
  flows %>%
    mutate(annual_cropland_flow = annual_cropland_flow * cropland_ecoregion_proportion,
           permanent_cropland_flow = permanent_cropland_flow * cropland_ecoregion_proportion,
           pastureland_flow = pastureland_flow * pastureland_ecoregion_proportion)
}

flows_tnc_joined <- flows_tnc_joined %>% multiply_tnc_flows %>% filter(!is.na(scenario))

# Check grand totals
pairwiseflows %>% group_by(scenario) %>% summarize(crop = sum(annual_cropland_flow, na.rm = TRUE))
flows_tnc_joined %>% group_by(scenario) %>% summarize(crop = sum(annual_cropland_flow, na.rm = TRUE))
# Good.

# Save totals to CSVs
write_csv(flows_tnc_joined, file.path(fp_out, 'landflows_faf_x_tnc_scenarios_provisional.csv'))

# Use population weights to get TNC x TNC transfers -----------------------

# Get the proportion population in each ecoregion in each FAF before joining.
pop_faf_tnc <- pop_faf_tnc %>%
  group_by(FAF) %>%
  mutate(pop_proportion = pop / sum(pop, na.rm = TRUE)) %>%
  ungroup

# Join the flows with population weights
flows_tnc_pop <- flows_tnc_joined %>% full_join(pop_faf_tnc, by = c('dms_dest' = 'FAF'))

# Domestic:
# Convert flows based on population proportion
weight_flows_by_pop <- function(flows) {
  flows %>%
    mutate(annual_cropland_flow = annual_cropland_flow * pop_proportion,
           permanent_cropland_flow = permanent_cropland_flow * pop_proportion,
           pastureland_flow = pastureland_flow * pop_proportion) %>%
    rename(TNC_orig = ECO_CODE,
           TNC_dest = TNC)
}

flows_tnc_pop <- flows_tnc_pop %>% weight_flows_by_pop

# Aggregate to only TNC x TNC flows
aggregate_tnc_flows <- function(flows) {
  flows %>%
    group_by(scenario, TNC_orig, TNC_dest) %>%
    summarize(annual_cropland_flow = sum(annual_cropland_flow, na.rm = TRUE),
              permanent_cropland_flow = sum(permanent_cropland_flow, na.rm = TRUE),
              pastureland_flow = sum(pastureland_flow, na.rm = TRUE))
}

flows_tnc_agg <- flows_tnc_pop %>% aggregate_tnc_flows

# Save outputs to CSVs

# Intermediate result (standardized ag flows across scenarios)
write_csv(flows_allscenarios, file.path(fp_out, 'scenarios/agflows_scenarios_provisional.csv'))

# Final result
write_csv(flows_tnc_pop, file.path(fp_out, 'scenarios/landflows_faf_tnc_x_tnc_scenarios_provisional.csv'))
write_csv(flows_tnc_agg, file.path(fp_out, 'scenarios/landflows_tnc_x_tnc_scenarios_provisional.csv'))
