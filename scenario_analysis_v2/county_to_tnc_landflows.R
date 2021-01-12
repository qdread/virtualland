# County by county land flows to TNC ecoregion flows
# QDR / Virtualland / 12 Jan 2021

# Do in parallel across each scenario.

library(tidyverse)
fp_out <- 'data/cfs_io_analysis'

# NLCD pixel counts by the intersection of county and TNC region in the USA
nlcd_county_tnc <- read_csv(file.path(fp_out, 'NLCDcrop_county_x_TNC.csv'))

# Population counts by the intersection of county and TNC region in the USA
pop_county_tnc <- read_csv(file.path(fp_out, 'population_county_x_TNC_longform.csv'))


# Process weighting factors -----------------------------------------------

# Join NLCD and population together, then calculate proportions

pop_county_tnc <- pop_county_tnc %>%
  mutate(county_fips = paste0(state_fips, county_fips)) %>%
  select(county_fips, TNC, pop)

county_tnc_weights <- nlcd_county_tnc %>% 
  mutate(county_fips = paste0(STATEFP, COUNTYFP)) %>%
  rename(TNC = ECO_CODE) %>%
  select(county_fips, TNC, crop, pasture, water, other) %>%
  left_join(pop_county_tnc) %>%
  group_by(county_fips) %>%
  mutate(cropland_ecoregion_proportion = crop / sum(crop, na.rm = TRUE),
         pastureland_ecoregion_proportion = pasture / sum(pasture, na.rm = TRUE),
         pop_ecoregion_proportion = pop / sum(pop, na.rm = TRUE)) %>%
  ungroup %>%
  select(county_fips, TNC, contains('proportion'))


# DEFINE FUNCTION TO CONVERT FLOWS ----------------------------------------

# Applied in parallel to each scenario

county_flows_to_tnc_flows <- function(diet, waste) {
  
  # Calculate flows from ecoregion to county 
  
  flows <- read_csv(paste('/nfs/qread-data/cfs_io_analysis/county_land_consumption_csvs/D', diet, 'WR', waste, 'landconsumption.csv', sep = '_'))
  
  # Widen county to county land flows data
  flows <- flows %>% 
    mutate(land_type = gsub('_exchange', '', land_type)) %>%
    rename(county_to = county_fips) %>%
    select(-state_from) %>%
    pivot_wider(id_cols = c(scenario, county_to, county_from), names_from = land_type, values_from = land_consumption)
  
  # Join county to county land flows data frame with the NLCD cropland and pastureland proportions.
  # Join by originating county (county_from)
  
  flows <- flows %>%
    left_join(county_tnc_weights, by = c('county_from' = 'county_fips'))
  
  # Then multiply cropland flows from county to county by the proportion of cropland in that county belonging to each ecoregion
  # (Most counties will belong only to one ecoregion but some belong to multiple)
  
  flows <- flows %>%
    mutate(annual_cropland = annual_cropland * cropland_ecoregion_proportion,
           permanent_cropland = permanent_cropland * cropland_ecoregion_proportion,
           pastureland = pastureland * pastureland_ecoregion_proportion)
  
  # Then, sum grouped by target county and originating ecoregion
  
  flows_tnc_to_county <- flows %>%
    rename(TNC_from = TNC) %>%
    group_by(scenario, county_to, TNC_from) %>%
    summarize(annual_cropland_flow = sum(annual_cropland, na.rm = TRUE),
              permanent_cropland_flow = sum(permanent_cropland, na.rm = TRUE),
              pastureland_flow = sum(pastureland, na.rm = TRUE)) %>%
    ungroup
  
  # Save totals to CSV
  write_csv(flows_tnc_to_county, paste('/nfs/qread-data/cfs_io_analysis/ecoregion_landflow_csvs/D', diet, 'WR', waste, 'landflows_tnc_to_county.csv'))
  
  # Use population weights to get TNC x TNC transfers 
  
  # Join the flows with population weights
  flows_tnc_pop <- flows_tnc_to_county %>% full_join(county_tnc_weights %>% select(county_fips, TNC, pop_ecoregion_proportion), by = c('county_to' = 'county_fips'))
  
  # Domestic:
  # Convert flows based on population proportion
  weight_flows_by_pop <- function(flows) {
    flows %>%
      mutate(annual_cropland_flow = annual_cropland_flow * pop_ecoregion_proportion,
             permanent_cropland_flow = permanent_cropland_flow * pop_ecoregion_proportion,
             pastureland_flow = pastureland_flow * pop_ecoregion_proportion) %>%
      rename(TNC_to = TNC)
  }
  
  flows_tnc_pop <- flows_tnc_pop %>% weight_flows_by_pop
  
  # Aggregate to only TNC x TNC flows
  aggregate_tnc_flows <- function(flows) {
    flows %>%
      group_by(scenario, TNC_from, TNC_to) %>%
      summarize(annual_cropland_flow = sum(annual_cropland_flow, na.rm = TRUE),
                permanent_cropland_flow = sum(permanent_cropland_flow, na.rm = TRUE),
                pastureland_flow = sum(pastureland_flow, na.rm = TRUE))
  }
  
  flows_tnc_agg <- flows_tnc_pop %>% aggregate_tnc_flows
  
  
  # Save outputs to CSVs
  write_csv(flows_tnc_to_county, paste('/nfs/qread-data/cfs_io_analysis/ecoregion_landflow_csvs/D', diet, 'WR', waste, 'landflows_tnc_to_county.csv'))
  
}


# APPLY FUNCTION ACROSS SCENARIOS -----------------------------------------

scenario_combos <- expand_grid(diet = c('baseline','planetaryhealth','usstyle','medstyle','vegetarian'),
                               waste = c('baseline','preconsumer','consumer','allavoidable'))

library(rslurm)

sjob <- slurm_apply(land_consumption_by_scenario, scenario_combos, 
                    jobname = 'convert_flows', nodes = 4, cpus_per_node = 2, 
                    global_objects = c('county_tnc_weights'),
                    slurm_options = list(partition = 'sesync'))

cleanup_files(sjob)

