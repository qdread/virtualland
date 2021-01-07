# Estimation of land footprint from goods consumption footprint for all scenarios
# QDR / Virtualland / 07 Jan 2021

# Data needed:
# We have the county-level direct and indirect consumption, for all scenarios, with the county it originates from. (see county_level_consumption.R)
# We have the land exchanges. (see eeio_landdata.R and impute_exchanges.R)


# Load data ---------------------------------------------------------------

library(tidyverse)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_out <- file.path(fp, 'cfs_io_analysis')

land_exch_imputed <- read_csv(file.path(fp_out, 'land_imputed_exchanges_wide.csv'))
county_totaldemand2012 <- read_csv(file.path(fp_out, 'county_totaldemand2012.csv'))

# Convert land exchanges to a satellite table -----------------------------

# N rows = 3 (subset land exchanges for only annual crops, pasture, and permanent crops)
# There are only 10 BEA codes accounted for in those land use types. (other crops are split between annual and permanent)
# So each satellite table will be 3x11 for each state.
# Then, multiply the appropriate state's satellite table by each county's vector (again subset for the 11 codes)

land_exch_tables <- land_exch_imputed %>%
  filter(Landuse_type %in% c('Annual crops', 'Permanent crops', 'Pasture')) %>%
  pivot_longer(-c(Landuse_type, Activity_Name, Code), names_to = 'state', values_to = 'exchange') %>%
  select(-Activity_Name) %>%
  group_by(state) %>%
  pivot_wider(names_from = Landuse_type, values_from = exchange, values_fill = 0) %>% # pivot and unpivot to add the zeroes
  pivot_longer(-c(state, Code), names_to = 'Landuse_type', values_to = 'exchange') %>%
  group_by(state, Landuse_type) %>%
  pivot_wider(names_from = Code, values_from = exchange)

# state_fips <- unique(fips_codes[,c('state','state_code')]) %>%
#   mutate(state = paste0('US_', state))
# 
# land_exch_tables <- land_exch_tables %>%
#   left_join(state_fips) %>%
#   select(state,state_code,everything())


# Multiply state satellite tables by county vectors -----------------------

# Get FIPS code of each state
data(fips_codes,package='tidycensus')

state_fips_table <- unique(fips_codes[,c('state','state_code')]) %>%
  mutate(state = paste0('US_', state))

# Convert county total demand data frame to a list of vectors
county_agdemand_2012 <- county_totaldemand2012 %>%
  filter(BEA_code %in% names(land_exch_tables)) 

county_demand_vectors <- county_agdemand_2012 %>%
  select(-BEA_code) %>% as.list

# Convert land exchange tables to a list of matrices
# Ensure the matrix's row names are sorted the same way as the demand vectors.
land_exch_matrices <- land_exch_tables %>%
  group_by(state) %>% nest %>%
  mutate(data = map(data, function(x) {
    dat <- as.matrix(x[,-1])
    row.names(dat) <- x$Landuse_type
    dat[, county_agdemand_2012$BEA_code]
  }))

get_county_land_consumption <- function(demand, county_code) {
  # find the state the county belongs to
  state_fips <- sprintf('%02d', as.integer(substr(county_code, 1, nchar(county_code) - 3)))
  state_abbr <- state_fips_table$state[state_fips_table$state_code == state_fips]
  # multiply the appropriate matrix by the demand vector
  result <- land_exch_matrices$data[[which(land_exch_matrices$state == state_abbr)]] %*% demand
  tibble(county_code = sprintf('%05d', as.integer(county_code)),
         landuse_type = row.names(result),
         consumption = result[,1])
}

# Actually do the multiplication to find county land consumption!
county_land_consumption <- map2_dfr(county_demand_vectors, names(county_totaldemand2012)[-1], get_county_land_consumption)

# Write the result to CSV
write_csv(county_land_consumption, file.path(fp_out, 'county_landconsumption2012.csv'))
####FIXME THIS IS NOT CORRECT! Obviously the county level exchange is not right.
#### We need to instead begin with county-level consumption of goods, figure out where those were produced, and only then get the land footprint.