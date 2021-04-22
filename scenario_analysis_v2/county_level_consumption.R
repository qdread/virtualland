# County-level final consumer expenditure (PCE) on BEA-classified goods
# Find by multiplying the total PCE for each BEA category from the input-output table (2012) times the percentage of personal income
# that each county consists of, out of the USA total.

# Modification 19 Apr 2021: Replace old county income data with the one downloaded directly from BEA
# Modification 24 Mar 2021: Replace old PCE and DRC with the new ones from USEEIOv2.0 alpha.
# Modification 15 Jan 2021: correct Bedford City being merged with Bedford County, VA
# Modification 06 Jan 2021: add the alternative scenarios as well as the baseline.


# Read data ---------------------------------------------------------------

# Read the county-level income data from BEA
# Remove outdated counties with no income for 2012.
# Combine Aleutians east and west into a single data point.
library(tidyverse)

county_income <- read_csv('data/raw_data/BEA/countypersonalincome2012.csv', skip = 4, n_max = 3138, col_types = 'ccn', na = '(NA)') 
county_income$`2012`[county_income$GeoFips == '02010'] <- sum(county_income$`2012`[county_income$GeoFips %in% c('02013', '02016')]) #Aleutians correction
county_income <- county_income %>%
  filter(!is.na(`2012`), !GeoFips %in% c('02013', '02016'))

# Load the personal consumption expenditure for each good, and the DRC tables, extracted from USEEIO2012v2.0
load('data/cfs_io_analysis/useeio2012v2.0_pce_drc.RData')

# Alternative scenarios ---------------------------------------------------

# Get multiplicative factors for each of the alternative scenarios
scenario_factors_bea <- read_csv('data/cfs_io_analysis/bea_consumption_factors_diet_waste_scenarios.csv')

# Multiply the appropriate BEA code by its production factor for each scenario.
# Only the 35 food BEA codes will be multiplied, so create an expanded vector for each scenario with 0 in all other elements.
# This will remove the nonfood codes from the total demand.

expand_vec <- function(BEA_389_code, value) {
  vec <- setNames(rep(0, length(pce2012)), names(pce2012))
  vec[BEA_389_code] <- value
  vec
}

scenario_vectors_bea <- scenario_factors_bea %>%
  pivot_longer(-c(BEA_389_code, BEA_389_def), names_to = 'scenario') %>%
  nest(data = c(BEA_389_code, BEA_389_def, value)) %>%
  mutate(consumption_factor = map(data, ~ expand_vec(.$BEA_389_code, .$value)))

# Normalize the county income 2012 vector
county_income_norm2012 <- setNames(county_income$`2012`/sum(county_income$`2012`), county_income$GeoFips)

# County consumption for each scenario: personal consumption vector multiplied elementwise by the consumption factor for the scenario
# then take the outer product with the county income normalized vector.
county_consumption2012 <- scenario_vectors_bea %>%
  select(-data) %>%
  mutate(county_consumption = map(consumption_factor, ~ pce2012 * . %*% t(county_income_norm2012)))
# Each element of the consumption list is a 411 x 3112 matrix, each row is a good and each column a county.
# It represents the consumption in each of the 20 scenarios.
# Units are USD.

# Add column for the BEA code and concatenate the list of matrices into a data frame.
county_consumption2012_df <- county_consumption2012 %>%
  select(-consumption_factor) %>%
  mutate(county_consumption = map(county_consumption, as_tibble)) %>%
  unnest 

county_consumption2012_df <- tibble(BEA_code = rep(names(pce2012), nrow(county_consumption2012)), county_consumption2012_df)

write_csv(county_consumption2012_df, 'data/cfs_io_analysis/county_consumption2012_allscenarios.csv')


# Direct and indirect demand resulting from consumer expenditures ---------

# We can use the PCE vector for each county as a final demand vector.
# Multiply the direct requirements coefficients matrix * this vector.
# The product will be the total direct and indirect demand for each county.

# Use the domestic DRC since we are looking at domestic transactions.

all(dimnames(drc2012)[[1]] == dimnames(drc2012)[[2]]) # Already sorted OK.
all(county_consumption2012_df$BEA_code[1:411] == dimnames(drc2012)[[1]]) # Already sorted OK

# Take Leontief inverse
leontief_inverse2012 <- solve(diag(nrow(drc2012_domestic)) - drc2012_domestic)

# Columnwise, multiply the matrix drc2012 times the consumption vectors for each county.
county_demand2012 <- county_consumption2012 %>%
  mutate(totaldemand = map(county_consumption, ~ apply(.x, 2, function(f) leontief_inverse2012 %*% f))) 

# Combine everything into a single data frame for each demand type.
county_totaldemand2012 <- cbind(county_consumption2012_df[,c('BEA_code', 'scenario')], do.call(rbind, county_demand2012$totaldemand))

# Here, make the assumption that any demand greater than baseline for wild-caught fish is applied to animal farms and aquaculture instead.
# Wild-caught fish 114000, animal farms and aquaculture 112A00

county_totaldemand2012_long <- county_totaldemand2012 %>%
  pivot_longer(-c(BEA_code, scenario), names_to = 'county', values_to = 'demand')

county_totaldemand2012_base <- county_totaldemand2012_long %>% 
  filter(scenario %in% 'D_baseline_WR_baseline') %>%
  select(-scenario) %>%
  rename(demand_base = demand)

county_fishdemandincrease <- county_totaldemand2012_long %>%
  left_join(county_totaldemand2012_base) %>%
  mutate(fish_demand_increase = pmax(0, demand - demand_base)) %>%
  filter(BEA_code %in% '114000') %>%
  rename(fish_demand_base = demand_base) %>%
  select(-BEA_code, -demand)

county_totaldemand2012_long <- county_totaldemand2012_long %>%
  left_join(county_fishdemandincrease) %>%
  mutate(demand = case_when(
    BEA_code %in% '112A00' ~ demand + fish_demand_increase,
    BEA_code %in% '114000' ~ pmin(demand, fish_demand_base),
    TRUE ~ demand))

county_totaldemand2012_aquaculture_corrected <- county_totaldemand2012_long %>%
  select(-fish_demand_base, -fish_demand_increase) %>%
  pivot_wider(names_from = county, values_from = demand)

write_csv(county_totaldemand2012_aquaculture_corrected, 'data/cfs_io_analysis/county_totaldemand2012_allscenarios.csv')
