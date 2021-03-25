# Estimation of land footprint from goods consumption footprint for all scenarios
# DATA TABLE VERSION
# QDR / Virtualland 

# Forked from tidyverse version on 11 Feb 2021
# Modified 25 Mar 2021: remove correction because units of consumption from new IO model are already in USD

# Data needed:
# We have the county-level direct and indirect consumption, for all scenarios, with the county it originates from. (see county_level_consumption.R)
# We have the land exchanges by state. (see eeio_landdata.R and impute_exchanges.R)


# Load data ---------------------------------------------------------------

library(data.table)
library(tidyverse)
library(Rutilitybelt)

fp_out <- 'data/cfs_io_analysis'

# Load land_exch_tables
load(file.path(fp_out, 'state_land_exchange_tables.RData'))

# Loop through scenarios and get land consumption -------------------------

# We need to get the total land consumption for all counties in all scenarios, attributable to each good.
# Define function to do this for a single scenario.
land_consumption_by_scenario <- function(diet, waste) {
  
  consumption <- fread(glue::glue('/nfs/qread-data/cfs_io_analysis/county_consumption_csvs/D_{diet}_WR_{waste}_wide.csv'),
                       colClasses = rep(c('character','double'), c(3, 3141)))
  
  # Pivot consumption matrix to long form
  setnames(consumption, "county_fips", "county_from")
  consumption_fromcty <- melt(consumption, id.vars = c('BEA_code', 'scenario', 'county_from'), variable.name = 'county_to', value.name = 'consumption')
  consumption_fromcty[, state_from := substr(county_from, 1, 2)]  
    
  # Convert this long form consumption matrix to a list of vectors
  consumption_vectors <- group_nest_dt(consumption_fromcty, scenario, county_to, state_from, county_from)

  # Join consumption vectors with the land exchange table for the appropriate state
  consumption_vectors <- merge(consumption_vectors, land_exch_tables, by.x = 'state_from', by.y = 'state_fips', all.x = TRUE)

  # Get rid of null entries for production (DC)
  consumption_vectors <- consumption_vectors[!map_lgl(land_exchange, is.null)]
  
  # Function to get properly formatted land consumption for each row
  get_land_consumption = function(data, land_exchange) {
    consumption <- setNames(as.numeric(data$consumption), data$BEA_code)
    consumption <- consumption[dimnames(land_exchange)[[2]]] # Ensures both are sorted the same.
    p <- land_exchange %*% consumption
    data.frame(land_type = dimnames(land_exchange)[[1]], land_consumption = p)
  }

  # Do the matrix multiplication for each row to get the land consumption!
  consumption_vectors[, land_consumption := map2(data, land_exchange, get_land_consumption)]
  
  # Unnest list column
  land_consumption <- consumption_vectors[, .(scenario, county_to, state_from, county_from, land_consumption)]
  land_consumption <- unnest_dt(land_consumption, col = land_consumption, id = .(scenario, county_to, state_from, county_from))

  fwrite(land_consumption, glue::glue('/nfs/qread-data/cfs_io_analysis/county_land_consumption_csvs/D_{diet}_WR_{waste}_landconsumption.csv'))
}

scenario_combos <- expand_grid(diet = c('baseline','planetaryhealth','usstyle','medstyle','vegetarian'),
                               waste = c('baseline','preconsumer','consumer','allavoidable'))

library(rslurm)

sjob <- slurm_apply(land_consumption_by_scenario, scenario_combos, 
                    jobname = 'county_land', nodes = 5, cpus_per_node = 1, # Only 1 per node because of the large memory requirements.
                    global_objects = c('land_exch_tables'),
                    slurm_options = list(partition = 'sesync'))

cleanup_files(sjob)
