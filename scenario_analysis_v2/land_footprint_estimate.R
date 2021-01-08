# Estimation of land footprint from goods consumption footprint for all scenarios
# QDR / Virtualland / 07 Jan 2021

# Data needed:
# We have the county-level direct and indirect consumption, for all scenarios, with the county it originates from. (see county_level_consumption.R)
# We have the land exchanges by state. (see eeio_landdata.R and impute_exchanges.R)


# Load data ---------------------------------------------------------------

library(tidyverse)

fp_out <- 'data/cfs_io_analysis'

land_exch_imputed <- read_csv(file.path(fp_out, 'land_imputed_exchanges_wide.csv'))

ag_codes <- read_csv('data/crossreference_tables/naics_crosswalk_final.csv') %>%
  filter(substr(BEA_389_code, 1, 3) %in% c('111', '112')) %>%
  pull(BEA_389_code)

# Convert land exchanges to a satellite table -----------------------------

# N rows = 3 (subset land exchanges for only annual crops, pasture, and permanent crops)
# There are only 10 BEA codes accounted for in those land use types. (other crops are split between annual and permanent)
# So each satellite table will be 3x10 for each state.
# Then, multiply the appropriate state's satellite table by each county's vector (again subset for the 10 codes)

land_exch_tables <- land_exch_imputed %>%
  filter(Landuse_type %in% c('Annual crops', 'Permanent crops', 'Pasture')) %>%
  pivot_longer(-c(Landuse_type, Activity_Name, Code), names_to = 'state', values_to = 'exchange') %>%
  select(-Activity_Name) %>%
  group_by(state) %>%
  pivot_wider(names_from = Landuse_type, values_from = exchange, values_fill = 0) %>% # pivot and unpivot to add the zeroes
  pivot_longer(-c(state, Code), names_to = 'Landuse_type', values_to = 'exchange') %>%
  group_by(state, Landuse_type) %>%
  pivot_wider(names_from = Code, values_from = exchange)

# Get FIPS code of each state
data(fips_codes,package='tidycensus')

state_fips_table <- unique(fips_codes[,c('state','state_code')]) %>%
  mutate(state = paste0('US_', state))

# Convert land exchange tables to a list of matrices
# Ensure the matrix's row names are sorted the same way as the demand vectors.
land_exch_matrices <- land_exch_tables %>%
  group_by(state) %>% nest %>%
  mutate(data = map(data, function(x) {
    dat <- as.matrix(x[,-1])
    row.names(dat) <- x$Landuse_type
    dat[, ag_codes]
  }))


# Loop through scenarios and get land consumption -------------------------

# We need to get the total land consumption for all counties in all scenarios, attributable to each good.
# Define function to do this for a single scenario.
land_consumption_by_scenario <- function(diet, waste) {
  
  consumption <- read_csv(file.path('/nfs/qread-data/cfs_io_analysis/county_consumption_csvs', paste('D', diet, 'WR', waste, 'wide.csv', sep = '_')))
  
  # Consumption: convert to USD (currently in million USD)
  consumption <- consumption %>% mutate(across(where(is.numeric), ~ . * 1e6))
  
  # Pivot consumption matrix to long form
  # Sum up incoming consumption of each county by state (land exchanges are only resolved at state level)
  
  consumption_fromstate <- consumption %>%
    pivot_longer(-c(BEA_code, scenario, county_fips), names_to = 'county_from', values_to = 'consumption') %>%
    mutate(state_from = substr(county_from, 1, 2)) %>%
    group_by(BEA_code, scenario, county_fips, state_from) %>%
    summarize(consumption = sum(consumption))
  
  # Convert this long form consumption matrix to a list of vectors
  consumption_vectors <- consumption_fromstate %>%
    group_by(scenario, county_fips, state_from) %>%
    nest %>%
    mutate(data = map(data, ~ setNames(as.numeric(.$consumption), .$BEA_code)))
  
  # Get state abbreviation for the fips codes
  consumption_vectors <- consumption_vectors %>% left_join(state_fips_table, by = c('state_from' = 'state_code'))
  
  # Join consumption vectors with the land exchange table for the appropriate state
  consumption_vectors <- consumption_vectors %>%
    left_join(land_exch_matrices, by = 'state')
  
  # Get rid of null entries for production (DC)
  consumption_vectors <- consumption_vectors %>%
    filter(!map_lgl(data.y, is.null))
  
  # Here, data.x are the total consumption vectors and data.y are the exchange tables
  
  # Function to get properly formatted land consumption for each row
  get_land_consumption = function(data.x, data.y) {
    data.x <- data.x[dimnames(data.y)[[2]]]
    p <- data.y %*% data.x
    data.frame(land_type = dimnames(data.y)[[1]], land_consumption = p)
  }
  
  
  # Do the matrix multiplication for each row to get the land consumption!
  consumption_vectors <- consumption_vectors %>%
    mutate(land_consumption = map2(data.x, data.y, get_land_consumption))
  
  # Unnest list column
  land_consumption <- consumption_vectors %>%
    select(scenario, county_fips, state_from, land_consumption) %>%
    unnest(land_consumption) 
  
  write_csv(land_consumption, file.path('/nfs/qread-data/cfs_io_analysis/county_land_consumption_csvs', paste('D', diet, 'WR', waste, 'landconsumption.csv', sep = '_')))
  
}

scenario_combos <- expand_grid(diet = c('baseline','planetaryhealth','usstyle','medstyle','vegetarian'),
                               waste = c('baseline','preconsumer','consumer','allavoidable'))

library(rslurm)

sjob <- slurm_apply(land_consumption_by_scenario, scenario_combos, 
                    jobname = 'county_land', nodes = 4, cpus_per_node = 2, 
                    global_objects = c('land_exch_matrices', 'state_fips_table'),
                    slurm_options = list(partition = 'sesync'))


# Read in results and combine to one DF -----------------------------------

output_files <- dir('data/cfs_io_analysis/county_land_consumption_csvs', full.names = TRUE)

land_consumption_allscenarios <- map_dfr(output_files, read_csv)

write_csv(land_consumption_allscenarios, 'data/cfs_io_analysis/county_land_consumption_allscenarios.csv')

