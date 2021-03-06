# Estimation of land footprint from goods consumption footprint for all scenarios
# QDR / Virtualland / 07 Jan 2021

# Modified 27 Jan 2021: Correct mislabeling of exporting and importing counties.

# Data needed:
# We have the county-level direct and indirect consumption, for all scenarios, with the county it originates from. (see county_level_consumption.R)
# We have the land exchanges by state. (see eeio_landdata.R and impute_exchanges.R)


# Load data ---------------------------------------------------------------

library(tidyverse)

fp_out <- 'data/cfs_io_analysis'

# Load land_exch_tables
load(file.path(fp_out, 'state_land_exchange_tables.RData'))

# Loop through scenarios and get land consumption -------------------------

# We need to get the total land consumption for all counties in all scenarios, attributable to each good.
# Define function to do this for a single scenario.
land_consumption_by_scenario <- function(diet, waste) {
  
  consumption <- read_csv(file.path('/nfs/qread-data/cfs_io_analysis/county_consumption_csvs', paste('D', diet, 'WR', waste, 'wide.csv', sep = '_')), col_types = paste0(strrep('c',3), strrep('d',3141)))
  
  # Consumption: convert to USD (currently in million USD)
  consumption <- consumption %>% mutate(across(where(is.numeric), ~ . * 1e6))
  
  # Pivot consumption matrix to long form
  consumption_fromcty <- consumption %>%
    rename(county_from = county_fips) %>%
    pivot_longer(-c(BEA_code, scenario, county_from), names_to = 'county_to', values_to = 'consumption') %>%
    mutate(state_from = substr(county_from, 1, 2)) 
  
  # Convert this long form consumption matrix to a list of vectors
  consumption_vectors <- consumption_fromcty %>%
    group_by(scenario, county_to, state_from, county_from) %>%
    nest %>%
    mutate(consumption = map(data, ~ setNames(as.numeric(.$consumption), .$BEA_code))) %>%
    select(-data)
  
  # Join consumption vectors with the land exchange table for the appropriate state
  consumption_vectors <- consumption_vectors %>%
    left_join(land_exch_tables, by = c('state_from' = 'state_fips'))
  
  # Get rid of null entries for production (DC)
  consumption_vectors <- consumption_vectors %>%
    filter(!map_lgl(land_exchange, is.null))
  
  # Function to get properly formatted land consumption for each row
  get_land_consumption = function(consumption, land_exchange) {
    consumption <- consumption[dimnames(land_exchange)[[2]]] # Ensures both are sorted the same.
    p <- land_exchange %*% consumption
    data.frame(land_type = dimnames(land_exchange)[[1]], land_consumption = p)
  }

  # Do the matrix multiplication for each row to get the land consumption!
  consumption_vectors <- consumption_vectors %>%
    mutate(land_consumption = map2(consumption, land_exchange, get_land_consumption))
  
  # Unnest list column
  land_consumption <- consumption_vectors %>%
    select(scenario, county_to, state_from, county_from, land_consumption) %>%
    unnest(land_consumption) 
  
  write_csv(land_consumption, file.path('/nfs/qread-data/cfs_io_analysis/county_land_consumption_csvs', paste('D', diet, 'WR', waste, 'landconsumption.csv', sep = '_')))
  
}

scenario_combos <- expand_grid(diet = c('baseline','planetaryhealth','usstyle','medstyle','vegetarian'),
                               waste = c('baseline','preconsumer','consumer','allavoidable'))

library(rslurm)

sjob <- slurm_apply(land_consumption_by_scenario, scenario_combos, 
                    jobname = 'county_land', nodes = 4, cpus_per_node = 2, 
                    global_objects = c('land_exch_tables'),
                    slurm_options = list(partition = 'sesync'))

cleanup_files(sjob)
