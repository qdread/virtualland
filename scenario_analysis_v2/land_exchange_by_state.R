# Calculation of land exchanges for states
# QDR / Virtualland / 08 Jan 2021

# Load data ---------------------------------------------------------------

library(tidyverse)
fp_out <- 'data/cfs_io_analysis'

nass_bea <- read_csv(file.path(fp_out, 'nass_workers_receipts_3landtypes_bea.csv'))

# We need receipts per unit cropland and pastureland. Do this proportionally if a code has both crop and pastureland.
# The input data is already split into annual vs permanent cropland, and mapped from the NASS NAICS classification to BEA
# Data are available at state level
# Units of cropland are given in acres.


# Convert units -----------------------------------------------------------

# Land is given in acres. Convert to square meters.
acre_to_m2 <- 4046.873

nass_bea <- nass_bea %>%
  mutate(across(contains('land'), ~ . * acre_to_m2))

# Calculate ratio ---------------------------------------------------------

nass_bea <- nass_bea %>%
  mutate(total_land = annual_cropland + permanent_cropland + pastureland,
         land_exchange = total_land / receipts,
         annual_cropland_exchange = annual_cropland / receipts,
         permanent_cropland_exchange = permanent_cropland / receipts,
         pastureland_exchange = pastureland / receipts)

# Replace NA with zeroes
nass_bea <- nass_bea %>%
  mutate(across(contains('exchange'), ~ if_else(is.nan(.), 0, .)))

# Reshape data to desired format ------------------------------------------

# For each state we need a 3x10 matrix where rows are the three land types (annual crops, permanent crops, and pastureland)
# and columns are the exchanges (for each of the ten BEA codes)

data_to_mat <- function(dat) {
  mat <- t(as.matrix(dat[,-1]))
  dimnames(mat)[[2]] <- dat$BEA_code
  mat
}

land_exch_tables <- nass_bea %>%
  select(state_fips, state_abbrev, BEA_code, annual_cropland_exchange, permanent_cropland_exchange, pastureland_exchange) %>%
  arrange(state_fips, state_abbrev, BEA_code) %>%
  group_by(state_fips, state_abbrev) %>%
  nest %>%
  mutate(land_exchange = map(data, data_to_mat)) %>%
  select(-data)

# Save to .RData object
save(land_exch_tables, file = 'data/cfs_io_analysis/state_land_exchange_tables.RData')
