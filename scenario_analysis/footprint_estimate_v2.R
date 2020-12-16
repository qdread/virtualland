# Revised approach to estimating production and consumption footprints
# QDR / 16 Dec 2020

#       Method
# ==================
# For each region (county or CFS region), estimate both production and consumption of goods in all categories.
# Use BEA codes. 
# For consumption, use the county BEA code table. Convert LAFA categories to BEA categories, then 
# Then, use the I-O model to get the indirect consumption required to satisfy that direct consumption.
# This can be converted to land area required to satisfy consumption in each county.
# We also have the land area of production in each county (NLCD sums).
# Scale the production and consumption land areas so they add up to the same.
# Next, use the FAF dataset (harmonization between BEA and SCTG codes) to work backwards and get 


# We have the county-level direct and indirect consumption. (see county_level_consumption.R)
# We have the land exchanges. (see eeio_landdata.R and impute_exchanges.R)


# Load data ---------------------------------------------------------------

library(tidyverse)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_out <- file.path(fp, 'cfs_io_analysis')


land_exchanges <- read_csv(file.path(fp, 'raw_data/IO_tables/output_csvs/land_exchanges_bytype.csv'))
land_exch_imputed <- read_csv(file.path(fp_out, 'land_imputed_exchanges_wide.csv'))

# Convert land exchanges to a satellite table -----------------------------

# N rows = 7 (number of land use types in the land_exchanges data)
# N columns = 388 (BEA codes)
# Need to create a separate satellite table for each state. 
# (Each matrix will be relatively sparse, with around 22K nonzero entries total out of a potential 136K.)
# Then, multiply the appropriate state's satellite table by each county's vector

