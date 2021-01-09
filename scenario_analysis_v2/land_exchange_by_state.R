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
         land_exchange = total_land/receipts,
         annual_cropland_exchange = annual_cropland / receipts,
         permanent_cropland_exchange = permanent_cropland / receipts,
         pastureland_exchange = pastureland / receipts)

# Reshape data to desired format ------------------------------------------

# For each state we need a 3x10 matrix where rows are the three land types (annual crops, permanent crops, and pastureland)
# and columns are the exchanges ()
