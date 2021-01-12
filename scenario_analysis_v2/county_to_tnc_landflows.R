# County by county land flows to TNC ecoregion flows
# QDR / Virtualland / 12 Jan 2021

library(tidyverse)
fp_out <- 'data/cfs_io_analysis'

# NLCD pixel counts by the intersection of county and TNC region in the USA
nlcd_faf_tnc <- read_csv(file.path(fp_out, 'NLCDcrop_county_x_TNC.csv'))

# Population counts by the intersection of FAF region and TNC region in the USA
pop_faf_tnc <- read_csv(file.path(fp_out, 'population_FAF_x_TNC_3column.csv'))


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
write_csv(flows_tnc_joined, file.path(fp_out, 'landflows_faf_x_tnc_2x2x2_factorial_provisional.csv'))

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
write_csv(flows_tnc_pop, file.path(fp_out, 'scenarios/landflows_faf_tnc_x_tnc_2x2x2_factorial_provisional.csv'))
write_csv(flows_tnc_agg, file.path(fp_out, 'scenarios/landflows_tnc_x_tnc_2x2x2_factorial_provisional.csv'))