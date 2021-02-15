# Biodiversity impacts, modeled by Chaudhary & Kastner, for diet x waste scenarios in USA for each county x county virtual land transfer
# QDR / Virtualland / 14 Feb 2021

# 14 Feb 2021: forked from the TNC x TNC one (now requires parallelization)
# modified 21 Jan 2021: correct bug in case_when

# We have the biodiversity model results with characterization factors for amphibians, birds, mammals, and reptiles.
# Annual crops, permanent crops, pasture, managed forests, and urban landuse

# We have 20 scenarios: 5 diet scenarios x 4 waste reduction scenarios

# For each scenario we have square meters of virtual land transfers,
# and the CFs are in species lost per meter squared.
# So we just need to join them, then multiply the virtual land transfers (area) by the CFs (species lost per area)

# Process Chaudhary data --------------------------------------------------

library(data.table)
library(rslurm)

# Updated characterization factors from Chaudhary and Brooks 2018
chaudsi2018 <- fread('data/raw_data/biodiversity/chaudhary2015SI/chaud2018si_CFs.csv', colClasses = rep(c('character', 'double'), c(9, 1)))

# Process Chaudhary 2018 CFs data in preparation for joining with VLT data
# Plantation = permanent cropland, Crop = annual cropland
chaudsi_processed <- chaudsi2018[region_type %in% 'ecoregion' & land_use %in% c('crop', 'pasture', 'plantation') & unit %in% 'potential species loss y m-2']
chaudsi_processed[, land_use := fcase(land_use == 'crop', 'annual',
                                      land_use == 'plantation', 'permanent',
                                      land_use == 'pasture', 'pasture')]
chaudsi_processed[, c('region_type', 'unit', 'ecoregion_name') := NULL]   

# Pre-filter to include only the medium intensity and occupation CFs
chaudsi_processed <- chaudsi_processed[intensity %in% 'med' & CF_type %in% 'occupation' & statistic %in% 'mean']

# Function to read VLT, join with Chaud, and calc extinctions -------------

extinctions_by_scenario <- function(diet, waste) {
  # Read VLT values for scenario
  VLT <- fread(glue::glue('/nfs/qread-data/cfs_io_analysis/ecoregion_landflow_csvs/D_{diet}_WR_{waste}_county_x_county_landtncweights.csv'), 
               colClasses = rep(c('character', 'double'), c(4, 3)))
  
  # Join land transfers and characterization factors
  setnames(VLT, old = c('annual_cropland', 'permanent_cropland', 'pastureland'), new = c('annual', 'permanent', 'pasture'))
  VLT <- melt(VLT, measure.vars = c('annual', 'permanent', 'pasture'), variable.name = 'land_use', value.name = 'VLT')
  
  VLT_CF <- merge(VLT, chaudsi_processed, 
                  by.x = c('TNC_from', 'land_use'), by.y = c('ecoregion_code', 'land_use'), 
                  all.x = TRUE, all.y = FALSE, allow.cartesian = TRUE)
  
  setnames(VLT_CF, old = 'value', new = 'CF')
  VLT_CF[, species_lost := VLT * CF]
  
  # Sum the mean species lost across all the TNC regions so we only have pairwise county extinctions
  VLT_CF <- VLT_CF[, .(species_lost = sum(species_lost, na.rm = TRUE)), by = .(scenario, county_from, county_to, land_use, taxon)]
  VLT_CF <- VLT_CF[!is.na(taxon)]
  
  # The county x county is too big to write so do the following: 
  # Sum to state x state for network map purposes
  # Sum to county-level inbound and outbound sums for choropleth map purposes
  
  VLT_CF[, state_from := substr(county_from, 1, 2)]
  VLT_CF[, state_to := substr(county_to, 1, 2)]
  
  extinctions_state <- VLT_CF[, .(species_lost = sum(species_lost)), by = .(scenario, state_from, state_to, land_use, taxon)]
  county_inbound <- VLT_CF[, .(species_lost = sum(species_lost)), by = .(scenario, county_to, land_use, taxon)]
  county_outbound <- VLT_CF[, .(species_lost = sum(species_lost)), by = .(scenario, county_from, land_use, taxon)]
  
  # Combine county inbound and outbound to one data frame and join
  setnames(county_inbound, old = c('county_to', 'species_lost'), new = c('county', 'extinction_inbound'))
  setnames(county_outbound, old = c('county_from', 'species_lost'), new = c('county', 'extinction_outbound'))
  county_flows <- county_outbound[county_inbound, on = .(scenario, county, land_use, taxon)]
  county_flows[is.na(extinction_outbound), extinction_outbound := 0]

  fwrite(extinctions_state, glue::glue('/nfs/qread-data/cfs_io_analysis/county_state_extinction_csvs/D_{diet}_WR_{waste}_state_x_state_extinctions.csv'))
  fwrite(county_flows, glue::glue('/nfs/qread-data/cfs_io_analysis/county_state_extinction_csvs/D_{diet}_WR_{waste}_county_extinction_sums.csv'))
  
}


# Apply function across scenarios -----------------------------------------

scenario_combos <- expand.grid(diet = c('baseline','planetaryhealth','usstyle','medstyle','vegetarian'),
                               waste = c('baseline','preconsumer','consumer','allavoidable'), stringsAsFactors = FALSE)

sjob_extinctions <- slurm_apply(extinctions_by_scenario, scenario_combos, 
                                 jobname = 'extinctions_county', nodes = 5, cpus_per_node = 1, 
                                 global_objects = c('chaudsi_processed'),
                                 slurm_options = list(partition = 'sesync'))

cleanup_files(sjob_extinctions)


# Load and concatenate and write ------------------------------------------

extinctions_state_all <- purrr::pmap_dfr(scenario_combos, function(diet, waste) fread(glue::glue('/nfs/qread-data/cfs_io_analysis/county_state_extinction_csvs/D_{diet}_WR_{waste}_state_x_state_extinctions.csv')))
county_flows_all <- purrr::pmap_dfr(scenario_combos, function(diet, waste) fread(glue::glue('/nfs/qread-data/cfs_io_analysis/county_state_extinction_csvs/D_{diet}_WR_{waste}_county_extinction_sums.csv')))

fwrite(extinctions_state_all, file.path(fp_out, 'scenarios/species_lost_state_x_state_all_scenarios_med.csv'))
fwrite(county_flows_all, file.path(fp_out, 'scenarios/species_lost_county_sums_all_scenarios_med.csv'))

