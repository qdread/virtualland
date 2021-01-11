# County-level final consumer expenditure (PCE) on BEA-classified goods
# Find by multiplying the total PCE for each BEA category from the input-output table (2012) times the percentage of personal income
# that each county consists of, out of the USA total.

# Modification 06 Jan 2021: add the alternative scenarios as well as the baseline.

# Final consumer expenditures ---------------------------------------------

# Read the county-level income data from Lin et al. (provided by Landon 9 Dec 2020)

library(tidyverse)
library(readxl)

fp_lin <- 'data/raw_data/commodity_flows/Lin_supp_info'
fp_bea <- 'data/raw_data/BEA/formatted'

county_income <- read_xlsx(file.path(fp_lin, 'County Personal Income.xlsx'), sheet = 'Total County Income', skip = 5)

# This is a complex spreadsheet with formulas but it seems like just skipping the rows is OK.
names(county_income) <- c('FIPS', 'state_FIPS', 'county_FIPS', 'state_name', 'county_name', paste('per_capita_income', 2012:2014, sep = '_'), 'per_capita_rank_2014', 'percent_change_2013', 'percent_change_2014', 'percent_change_rank_2014', 'total_income_2012')

# Remove state total rows
# Modified 17 Dec 2020: do not remove DC.
county_income <- county_income %>% filter(county_FIPS != 0 | state_FIPS == 11) %>% select(-state_name)

# Read the input-output BEA table to get the personal consumption expenditure for each good.
use2012 <- read_csv(file.path(fp_bea, 'use2012.csv'))
# Get the PCE for the 389 BEA goods from this table.
# F01000 is the code for personal consumption expenditures.
pce2012 <- setNames(use2012$F01000[1:389], use2012$X1[1:389])


# Alternative scenarios ---------------------------------------------------

# Get multiplicative factors for each of the alternative scenarios
scenario_factors_bea <- read_csv('data/cfs_io_analysis/bea_consumption_factors_diet_waste_scenarios.csv')

# Multiply the appropriate BEA code by its production factor for each scenario.
# Only the 36 food BEA codes will be multiplied, so create an expanded vector for each scenario with 1 in all other elements.

expand_vec <- function(BEA_389_code, value) {
  vec <- setNames(rep(1, length(pce2012)), names(pce2012))
  vec[BEA_389_code] <- value
  vec
}

scenario_vectors_bea <- scenario_factors_bea %>%
  pivot_longer(-c(BEA_389_code, BEA_389_def), names_to = 'scenario') %>%
  nest(data = c(BEA_389_code, BEA_389_def, value)) %>%
  mutate(consumption_factor = map(data, ~ expand_vec(.$BEA_389_code, .$value)))

# Normalize the county income 2012 vector
county_income_norm2012 <- setNames(county_income$total_income_2012/sum(county_income$total_income_2012), county_income$FIPS)

# County consumption for each scenario: personal consumption vector multiplied elementwise by the consumption factor for the scenario
# then take the cross product with the county income normalized vector.
county_consumption2012 <- scenario_vectors_bea %>%
  select(-data) %>%
  mutate(county_consumption = map(consumption_factor, ~ pce2012 * . %*% t(county_income_norm2012)))
# Each element of the consumption list is a 389 x 3143 matrix, each row is a good and each column a county.
# It represents the consumption in each of the 20 scenarios.
# Units are million USD.

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

fp_useeio <- file.path(ifelse(dir.exists('Q:/'), '~/Documents/GitHub/foodwaste', '~'), 'USEEIO/useeiopy/Model Builds/USEEIO2012')

drc2012 <- read.csv(file.path(fp_useeio, 'USEEIO2012_DRC.csv'), row.names = 1, check.names = FALSE) # Use this to get row names

# Convert DRC2012 row and column names to the six digit code with uppercase letters
dimnames(drc2012) <- map(dimnames(drc2012), ~ toupper(substr(., 1, 6)))


# There is one extra column in county consumption not present in the DRC2012
setdiff(names(pce2012), dimnames(drc2012)[[1]])
# This is the scrap column. Ignore this column and ensure the vector and matrix are sorted the same way.

all(dimnames(drc2012)[[1]] == dimnames(drc2012)[[2]]) # Already sorted OK.

# Remove the scrap category
county_consumption2012 <- filter(county_consumption2012_df, BEA_code %in% dimnames(drc2012)[[1]])

all(county_consumption2012$BEA_code[1:388] == dimnames(drc2012)[[1]]) # Already sorted OK

drc2012_mat <- as.matrix(drc2012)

# Take Leontief inverse
leontief_inverse2012 <- solve(diag(nrow(drc2012_mat)) - drc2012_mat)

# Convert county consumption data frame back to a list column of matrices.
county_demand2012 <- county_consumption2012 %>%
  group_by(scenario) %>%
  nest(directdemand = -scenario)

# Columnwise, multiply the matrix drc2012 times the consumption vectors for each county.
# Then sum indirect and total demand.
county_demand2012 <- county_demand2012 %>%
  mutate(directdemand = map(directdemand, ~ as.matrix(.x[, -1]))) %>%
  mutate(totaldemand = map(directdemand, ~ apply(.x, 2, function(f) leontief_inverse2012 %*% f))) 

# Combine everything into a single data frame for each demand type.

county_totaldemand2012 <- cbind(county_consumption2012[,c('BEA_code', 'scenario')], do.call(rbind, county_demand2012$totaldemand))

write_csv(county_totaldemand2012, 'data/cfs_io_analysis/county_totaldemand2012_allscenarios.csv')
