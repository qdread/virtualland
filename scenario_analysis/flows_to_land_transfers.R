# Code to convert a set of BEA flows to virtual land transfers
# This generalizes the original code written just for the baseline case.
# Generalization of code in faf_land_transfers.R and faf_land_transfer_to_tnc.R
# QDR / Virtualland / 01 Oct 2020

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
  select(Code, FAF_Region, BEA_code, cropland_normalized, pastureland_normalized) %>%
  setNames(c('FAF_Code', 'FAF_Region', 'BEA_Code', 'cropland', 'pastureland'))


# Standardize input data (flows) ------------------------------------------

# Each scenario input data needs the same data structure
# We should sum up across modes and give everything the same name

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


# Join tonnage flows with land areas --------------------------------------

# Join flows with NASS land values
join_with_land <- function(flows) {
  flows %>% left_join(nass_bea_land %>% select(-FAF_Region), by = c('dms_orig' = 'FAF_Code', 'BEA_Code' = 'BEA_Code'))
}

flows_baseline_std <- flows_baseline_std %>% join_with_land
flows_diet_std <- flows_diet_std %>% join_with_land
flows_waste_std <- flows_waste_std %>% join_with_land
flows_transport_std <- flows_transport_std %>% join_with_land


# Convert to land flows ---------------------------------------------------

# Functions to get the proportional crop and pasture flow originating from each FAF region,
# and convert all land stocks and flows from acres to square kilometers.
to_km2 <- function(x) x %>% set_units(acre) %>% set_units(km^2) %>% as.numeric

land_flows_by_weight <- function(flows) {
  flows %>%
    filter(!is.na(cropland), !is.na(pastureland)) %>%
    group_by(dms_orig) %>%
    mutate(tons_proportion = tons / sum(tons, na.rm = TRUE)) %>%
    ungroup %>%
    mutate(cropland_flow = tons_proportion * cropland,
           pastureland_flow = tons_proportion * pastureland) %>%
    mutate_at(vars(contains('land')), to_km2)
}

flows_baseline_std <- flows_baseline_std %>% land_flows_by_weight
flows_diet_std <- flows_diet_std %>% land_flows_by_weight
flows_waste_std <- flows_waste_std %>% land_flows_by_weight
flows_transport_std <- flows_transport_std %>% land_flows_by_weight


# Calculate summaries for plots -------------------------------------------

calculate_flow_totals <- function(flows) {
  # Domestic outbound
  land_outbound <- flows %>%
    filter(dms_orig != dms_dest) %>%
    group_by(dms_orig) %>%
    summarize(cropland_flow = sum(cropland_flow), pastureland_flow = sum(pastureland_flow)) %>%
    add_row(dms_orig = '111', cropland_flow = 0, pastureland_flow = 0) %>% # Washington DC
    rename(region = dms_orig)
  
  # Domestic inbound
  land_inbound <- flows %>%
    filter(dms_orig != dms_dest) %>%
    group_by(dms_dest) %>%
    summarize(cropland_flow = sum(cropland_flow), pastureland_flow = sum(pastureland_flow)) %>%
    rename(region = dms_dest)
  
  # Domestic net
  land_netdomestic <- left_join(land_outbound, land_inbound, by = 'region') %>%
    mutate(cropland_flow = cropland_flow.y - cropland_flow.x,
           pastureland_flow = pastureland_flow.y - pastureland_flow.x) %>%
    select(region, cropland_flow, pastureland_flow) 
  
  # Domestic within region
  land_within <- flows %>%
    filter(dms_orig == dms_dest) %>%
    group_by(dms_orig) %>%
    summarize(cropland_flow = sum(cropland_flow), pastureland_flow = sum(pastureland_flow)) %>%
    add_row(dms_orig = '111', cropland_flow = 0, pastureland_flow = 0) %>% # Washington DC
    rename(region = dms_orig)
  
  return(bind_rows(list(land_outbound, land_inbound, land_netdomestic, land_within), .id = 'flow_type'))
}

flowtotals_baseline <- flows_baseline_std %>% calculate_flow_totals
flowtotals_diet <- flows_diet_std %>% calculate_flow_totals
flowtotals_waste <- flows_waste_std %>% calculate_flow_totals
flowtotals_transport <- flows_transport_std %>% calculate_flow_totals


# Write outputs -----------------------------------------------------------

write_csv(flows_baseline_std, file.path(fp_out, 'scenarios/landflows_baseline_provisional.csv'))
write_csv(flows_diet_std, file.path(fp_out, 'scenarios/landflows_diet_provisional.csv'))
write_csv(flows_waste_std, file.path(fp_out, 'scenarios/landflows_waste_provisional.csv'))
write_csv(flows_transport_std, file.path(fp_out, 'scenarios/landflows_transport_provisional.csv'))

write_csv(flowtotals_baseline, file.path(fp_out, 'scenarios/landflows_totals_baseline_provisional.csv'))
write_csv(flowtotals_diet, file.path(fp_out, 'scenarios/landflows_totals_diet_provisional.csv'))
write_csv(flowtotals_waste, file.path(fp_out, 'scenarios/landflows_totals_waste_provisional.csv'))
write_csv(flowtotals_transport, file.path(fp_out, 'scenarios/landflows_totals_transport_provisional.csv'))


# For each FAF, split transfers by ecoregion ------------------------------

# Below this point is code derived from faf_land_transfer_to_tnc.R

# We can sum up the land transfers by ecoregion (across codes). 
# Then, for all outgoing transfers, we can assign proportions of them to each 
# ecoregion that the goods came from (based on crop and pastureland percentages in the ecoregions in each FAF.)

# Do not care which foreign region goods are going to, also do not care about transportation mode

flow_totals_pairwise <- function(flows) {
  flows %>%
    select(-BEA_Code) %>%
    group_by(dms_orig, dms_dest) %>%
    summarize_all(sum)
}

pairwiseflows_baseline <- flows_baseline_std %>% flow_totals_pairwise
pairwiseflows_diet <- flows_diet_std %>% flow_totals_pairwise
pairwiseflows_waste <- flows_waste_std %>% flow_totals_pairwise
pairwiseflows_transport <- flows_transport_std %>% flow_totals_pairwise

# Only take essential rows from NLCD tally dataframe
nlcd_faf_tnc_reduced <- nlcd_faf_tnc %>% select(Code, ECO_CODE, crop, other, pasture, water)

# For each of the pairwise flows dataframes, check that all origin FAF regions besides Washington DC (111) are present
nlcdfafcodes <- unique(nlcd_faf_tnc_reduced$Code) 
nlcdfafcodes[!nlcdfafcodes %in% pairwiseflows_domestic$dms_orig]
nlcdfafcodes[!nlcdfafcodes %in% pairwiseflows_diet$dms_orig]
nlcdfafcodes[!nlcdfafcodes %in% pairwiseflows_waste$dms_orig]
nlcdfafcodes[!nlcdfafcodes %in% pairwiseflows_transport$dms_orig]
# All return only 111 (OK).

# Get the proportion crop and pasture in each ecoregion in each FAF before joining.
nlcd_faf_tnc_reduced <- nlcd_faf_tnc_reduced %>%
  group_by(Code) %>%
  mutate(cropland_ecoregion_proportion = crop / sum(crop, na.rm = TRUE),
         pastureland_ecoregion_proportion = pasture / sum(pasture, na.rm = TRUE)) %>%
  ungroup

flows_tnc_joined_baseline <- full_join(pairwiseflows_baseline, nlcd_faf_tnc_reduced, by = c('dms_orig' = 'Code'))
flows_tnc_joined_diet <- full_join(pairwiseflows_diet, nlcd_faf_tnc_reduced, by = c('dms_orig' = 'Code'))
flows_tnc_joined_waste <- full_join(pairwiseflows_waste, nlcd_faf_tnc_reduced, by = c('dms_orig' = 'Code'))
flows_tnc_joined_transport <- full_join(pairwiseflows_transport, nlcd_faf_tnc_reduced, by = c('dms_orig' = 'Code'))

# Use the cropland and pastureland proportions to get the cropland and pastureland flows out of each ecoregion into each FAF.
# Multiply cropland flow by cropland proportion, and pastureland flow by pastureland proportion
multiply_tnc_flows <- function(flows) {
  flows %>%
    mutate(cropland_flow = cropland_flow * cropland_ecoregion_proportion,
           pastureland_flow = pastureland_flow * pastureland_ecoregion_proportion)
}

flows_tnc_joined_baseline <- flows_tnc_joined_baseline %>% multiply_tnc_flows
flows_tnc_joined_diet <- flows_tnc_joined_diet %>% multiply_tnc_flows
flows_tnc_joined_waste <- flows_tnc_joined_waste %>% multiply_tnc_flows
flows_tnc_joined_transport <- flows_tnc_joined_transport %>% multiply_tnc_flows

# Check grand totals
map(list(pairwiseflows_baseline, flows_tnc_joined_baseline,
         pairwiseflows_diet, flows_tnc_joined_diet,
         pairwiseflows_waste, flows_tnc_joined_waste,
         pairwiseflows_transport, flows_tnc_joined_transport),
    ~ sum(.$cropland_flow, na.rm = TRUE))
# Good.

# Save totals to CSVs
write_csv(flows_tnc_joined_baseline, file.path(fp_out, 'landflows_faf_x_tnc_baseline_provisional.csv'))
write_csv(flows_tnc_joined_diet, file.path(fp_out, 'landflows_faf_x_tnc_diet_provisional.csv'))
write_csv(flows_tnc_joined_waste, file.path(fp_out, 'landflows_faf_x_tnc_waste_provisional.csv'))
write_csv(flows_tnc_joined_transport, file.path(fp_out, 'landflows_faf_x_tnc_transport_provisional.csv'))


# Use population weights to get TNC x TNC transfers -----------------------

# Get the proportion population in each ecoregion in each FAF before joining.
pop_faf_tnc <- pop_faf_tnc %>%
  group_by(FAF) %>%
  mutate(pop_proportion = pop / sum(pop, na.rm = TRUE)) %>%
  ungroup

# Join the flows with population weights
flows_tnc_pop_baseline <- flows_tnc_joined_baseline %>% full_join(pop_faf_tnc, by = c('dms_dest' = 'FAF'))
flows_tnc_pop_diet <- flows_tnc_joined_diet %>% full_join(pop_faf_tnc, by = c('dms_dest' = 'FAF'))
flows_tnc_pop_waste <- flows_tnc_joined_waste %>% full_join(pop_faf_tnc, by = c('dms_dest' = 'FAF'))
flows_tnc_pop_transport <- flows_tnc_joined_transport %>% full_join(pop_faf_tnc, by = c('dms_dest' = 'FAF'))

# Domestic:
# Convert flows based on population proportion
weight_flows_by_pop <- function(flows) {
  flows %>%
    mutate(cropland_flow = cropland_flow * pop_proportion,
           pastureland_flow = pastureland_flow * pop_proportion) %>%
    rename(TNC_orig = ECO_CODE,
           TNC_dest = TNC)
}

flows_tnc_pop_baseline <- flows_tnc_pop_baseline %>% weight_flows_by_pop
flows_tnc_pop_diet <- flows_tnc_pop_diet %>% weight_flows_by_pop
flows_tnc_pop_waste <- flows_tnc_pop_waste %>% weight_flows_by_pop
flows_tnc_pop_transport <- flows_tnc_pop_transport %>% weight_flows_by_pop

# Aggregate to only TNC x TNC flows
aggregate_tnc_flows <- function(flows) {
  flows %>%
    group_by(TNC_orig, TNC_dest) %>%
    summarize(cropland_flow = sum(cropland_flow, na.rm = TRUE),
              pastureland_flow = sum(pastureland_flow, na.rm = TRUE))
}

flows_tnc_agg_baseline <- flows_tnc_pop_baseline %>% aggregate_tnc_flows
flows_tnc_agg_diet <- flows_tnc_pop_diet %>% aggregate_tnc_flows
flows_tnc_agg_waste <- flows_tnc_pop_waste %>% aggregate_tnc_flows
flows_tnc_agg_transport <- flows_tnc_pop_transport %>% aggregate_tnc_flows

# Save outputs to CSVs
write_csv(flows_tnc_pop_baseline, file.path(fp_out, 'scenarios/landflows_faf_tnc_x_tnc_baseline_provisional.csv'))
write_csv(flows_tnc_pop_diet, file.path(fp_out, 'scenarios/landflows_faf_tnc_x_tnc_diet_provisional.csv'))
write_csv(flows_tnc_pop_waste, file.path(fp_out, 'scenarios/landflows_faf_tnc_x_tnc_waste_provisional.csv'))
write_csv(flows_tnc_pop_transport, file.path(fp_out, 'scenarios/landflows_faf_tnc_x_tnc_transport_provisional.csv'))

write_csv(flows_tnc_agg_baseline, file.path(fp_out, 'scenarios/landflows_tnc_x_tnc_baseline_provisional.csv'))
write_csv(flows_tnc_agg_diet, file.path(fp_out, 'scenarios/landflows_tnc_x_tnc_diet_provisional.csv'))
write_csv(flows_tnc_agg_waste, file.path(fp_out, 'scenarios/landflows_tnc_x_tnc_waste_provisional.csv'))
write_csv(flows_tnc_agg_transport, file.path(fp_out, 'scenarios/landflows_tnc_x_tnc_transport_provisional.csv'))

