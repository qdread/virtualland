# County-level final consumer expenditure (PCE) on BEA-classified goods
# Find by multiplying the total PCE for each BEA category from the input-output table (2012) times the percentage of personal income
# that each county consists of, out of the USA total.

# Read the county-level income data from Lin et al. (provided by Landon 9 Dec 2020)

library(tidyverse)
library(readxl)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_lin <- file.path(fp, 'raw_data/commodity_flows/Lin_supp_info')
fp_bea <- file.path(fp, 'raw_data/BEA/formatted')

county_income <- read_xlsx(file.path(fp_lin, 'County Personal Income.xlsx'), sheet = 'Total County Income', skip = 5)

# This is a complex spreadsheet with formulas but it seems like just skipping the rows is OK.
names(county_income) <- c('FIPS', 'state_FIPS', 'county_FIPS', 'state_name', 'county_name', paste('per_capita_income', 2012:2014, sep = '_'), 'per_capita_rank_2014', 'percent_change_2013', 'percent_change_2014', 'percent_change_rank_2014', 'total_income_2012')

# Remove state total rows
county_income <- county_income %>% filter(county_FIPS != 0) %>% select(-state_name)

# Read the input-output BEA table to get the personal consumption expenditure for each good.
use2012 <- read_csv(file.path(fp_bea, 'use2012.csv'))
# Get the PCE for the 389 BEA goods from this table.
# F01000 is the code for personal consumption expenditures.
pce2012 <- setNames(use2012$F01000[1:389], use2012$X1[1:389])

# Cross product of PCE 2012 vector and the normalized county income 2012 vector
county_income_norm2012 <- setNames(county_income$total_income_2012/sum(county_income$total_income_2012), county_income$FIPS)

county_consumption2012 <- pce2012 %*% t(county_income_norm2012) # 389 x 3142 matrix, each row is a good and each column a county.
# Units are million USD.
                    