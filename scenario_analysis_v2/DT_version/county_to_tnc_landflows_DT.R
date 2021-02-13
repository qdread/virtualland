# County by county land flows to TNC ecoregion flows
# QDR / Virtualland / 12 Jan 2021

# Do in parallel across each scenario.
# Correct for missing values and for the Alaskan county code

library(data.table)
fp_out <- 'data/cfs_io_analysis'

# NLCD pixel counts by the intersection of county and TNC region in the USA
nlcd_county_tnc <- fread(file.path(fp_out, 'NLCDcrop_county_x_TNC.csv'), colClasses = rep(c('character', 'integer'), c(2, 4)))

# Population counts by the intersection of county and TNC region in the USA
pop_county_tnc <- fread(file.path(fp_out, 'population_county_x_TNC_longform.csv'), colClasses = rep(c('character', 'double'), c(4, 1)))


# Process weighting factors -----------------------------------------------

# Join NLCD and population together, then calculate proportions

pop_county_tnc[, county_fips := paste0(state_fips, county_fips)]
pop_county_tnc <- pop_county_tnc[ , .(county_fips, TNC, pop)]

setnames(nlcd_county_tnc, old = c('ECO_CODE', 'county'), new = c('TNC', 'county_fips'))

county_tnc_weights <- pop_county_tnc[nlcd_county_tnc, on = c('TNC', 'county_fips')]
proportion_cols <- c('cropland_ecoregion_proportion', 'pastureland_ecoregion_proportion', 'pop_ecoregion_proportion')
county_tnc_weights[, (proportion_cols) := list(crop / sum(crop, na.rm = TRUE), pasture / sum(pasture, na.rm = TRUE), pop / sum(pop, na.rm = TRUE)), by = county_fips]
county_tnc_weights[, (proportion_cols) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = proportion_cols]
county_tnc_weights[county_fips == '02013', county_fips := '02010']
county_tnc_weights <- county_tnc_weights[, c('county_fips', 'TNC', proportion_cols), with = FALSE]

# DEFINE FUNCTION TO CONVERT FLOWS ----------------------------------------

# Applied in parallel to each scenario

county_flows_to_tnc_flows <- function(diet, waste) {
  
  # Calculate flows from ecoregion to county 
  
  flows <- fread(glue::glue('/nfs/qread-data/cfs_io_analysis/county_land_consumption_csvs/D_{diet}_WR_{waste}_landconsumption.csv'), colClasses = rep(c('character', 'double'), c(5, 1)))
  
  flows[, land_type := gsub('_exchange', '', land_type)][, state_from := NULL]
        
  # Widen county to county land flows data
  flows <- dcast(flows, scenario + county_to + county_from ~ land_type, value.var = 'land_consumption')  

  # Join county to county land flows data frame with the NLCD cropland and pastureland proportions.
  # Join by originating county (county_from)
  flows <- merge(flows, county_tnc_weights, by.x = 'county_from', by.y = 'county_fips', all.x = TRUE, allow.cartesian = TRUE)

  # Then multiply cropland flows from county to county by the proportion of cropland in that county belonging to each ecoregion
  # (Most counties will belong only to one ecoregion but some belong to multiple)
  flows[, annual_cropland := annual_cropland * cropland_ecoregion_proportion]
  flows[, permanent_cropland := permanent_cropland * cropland_ecoregion_proportion]
  flows[, pastureland := pastureland * pastureland_ecoregion_proportion]

  # Then, sum grouped by target county and originating ecoregion
  setnames(flows, old = 'TNC', new = 'TNC_from')
  
  flow_cols <- c('annual_cropland_flow', 'permanent_cropland_flow', 'pastureland_flow')
  #flows_tnc_to_county <- flows[, (flow_cols) = lapply(list(annual_cropland, permanent_cropland, pastureland), sum, na.rm = TRUE), by = .(scenario, county_to, TNC_from)]
  flows_tnc_to_county <- flows[
    , .(annual_cropland_flow = sum(annual_cropland, na.rm = TRUE),
        permanent_cropland_flow = sum(permanent_cropland, na.rm = TRUE),
        pastureland_flow = sum(pastureland, na.rm = TRUE)),
    by = .(scenario, county_to, TNC_from)
  ]
  
  # Save totals to CSV
  fwrite(flows_tnc_to_county, glue::glue('/nfs/qread-data/cfs_io_analysis/ecoregion_landflow_csvs/D_{diet}_WR_{waste}_landflows_tnc_to_county.csv'))
  
  # Use population weights to get TNC x TNC transfers 
  
  # Join the flows with population weights
  county_tnc_weights <- county_tnc_weights[, .(county_fips, TNC, pop_ecoregion_proportion)]
  flows_tnc_pop <- merge(flows_tnc_to_county, county_tnc_weights, by.x = 'county_to', by.y = 'county_fips', all.x = TRUE, all.y = TRUE, allow.cartesian = TRUE)

  # Domestic:
  # Convert flows based on population proportion
  flows_tnc_pop[ , (flow_cols) := lapply(.SD, `*`, pop_ecoregion_proportion), .SDcols = flow_cols]
  setnames(flows_tnc_pop, old = 'TNC', new = 'TNC_to')

  # Aggregate to only TNC x TNC flows
  flows_tnc_agg <- flows_tnc_pop[, lapply(.SD, sum, na.rm = TRUE), by = .(scenario, TNC_from, TNC_to), .SDcols = flow_cols]

  # Save outputs to CSVs
  fwrite(flows_tnc_agg, glue::glue('/nfs/qread-data/cfs_io_analysis/ecoregion_landflow_csvs/D_{diet}_WR_{waste}_landflows_tnc_to_tnc.csv'))
}


# APPLY FUNCTION ACROSS SCENARIOS -----------------------------------------

scenario_combos <- expand.grid(diet = c('baseline','planetaryhealth','usstyle','medstyle','vegetarian'),
                               waste = c('baseline','preconsumer','consumer','allavoidable'), stringsAsFactors = FALSE)

library(rslurm)

sjob_convertflows <- slurm_apply(county_flows_to_tnc_flows, scenario_combos, 
                                 jobname = 'convert_flows', nodes = 5, cpus_per_node = 1, 
                                 global_objects = c('county_tnc_weights'),
                                 slurm_options = list(partition = 'sesync'))

cleanup_files(sjob_convertflows)


# Combine ecoregion flows into single file --------------------------------

flows_tnc_all <- purrr::pmap_dfr(scenario_combos, function(diet, waste) fread(glue::glue('{fp_out}/ecoregion_landflow_csvs/D_{diet}_WR_{waste}_landflows_tnc_to_tnc.csv')))

fwrite(flows_tnc_all, file.path(fp_out, 'scenarios/landflows_tnc_x_tnc_all_scenarios.csv'))
