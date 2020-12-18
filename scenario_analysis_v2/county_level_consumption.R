# County-level final consumer expenditure (PCE) on BEA-classified goods
# Find by multiplying the total PCE for each BEA category from the input-output table (2012) times the percentage of personal income
# that each county consists of, out of the USA total.


# Final consumer expenditures ---------------------------------------------

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
# Modified 17 Dec 2020: do not remove DC.
county_income <- county_income %>% filter(county_FIPS != 0 | state_FIPS == 11) %>% select(-state_name)

# Read the input-output BEA table to get the personal consumption expenditure for each good.
use2012 <- read_csv(file.path(fp_bea, 'use2012.csv'))
# Get the PCE for the 389 BEA goods from this table.
# F01000 is the code for personal consumption expenditures.
pce2012 <- setNames(use2012$F01000[1:389], use2012$X1[1:389])

# Cross product of PCE 2012 vector and the normalized county income 2012 vector
county_income_norm2012 <- setNames(county_income$total_income_2012/sum(county_income$total_income_2012), county_income$FIPS)

county_consumption2012 <- pce2012 %*% t(county_income_norm2012) # 389 x 3143 matrix, each row is a good and each column a county.
# Units are million USD.

# Add column for the BEA code
county_consumption2012 <- cbind(BEA_code = names(pce2012), as.data.frame(county_consumption2012))

write_csv(county_consumption2012, file.path(fp, 'cfs_io_analysis/county_consumption2012.csv'))                    


# Direct and indirect demand resulting from consumer expenditures ---------

# We can use the PCE vector for each county as a final demand vector.
# Multiply the direct requirements coefficients matrix * this vector.
# The product will be the total direct and indirect demand for each county.

county_consumption2012 <- read_csv(file.path(fp, 'cfs_io_analysis/county_consumption2012.csv'))

fp_useeio <- file.path(ifelse(is_local, '~/Documents/GitHub/foodwaste', '~'), 'USEEIO/useeiopy/Model Builds/USEEIO2012')

drc2012 <- read.csv(file.path(fp_useeio, 'USEEIO2012_DRC.csv'), row.names = 1, check.names = FALSE) # Use this to get row names

# Convert DRC2012 row and column names to the six digit code with uppercase letters
dimnames(drc2012) <- map(dimnames(drc2012), ~ toupper(substr(., 1, 6)))

# There is one extra column in county consumption not present in the DRC2012
setdiff(county_consumption2012$BEA_code, dimnames(drc2012)[[1]])
# This is the scrap column. Ignore this column and ensure the vector and matrix are sorted the same way.

all(dimnames(drc2012)[[1]] == dimnames(drc2012)[[2]]) # Already sorted OK.

county_consumption2012 <- filter(county_consumption2012, BEA_code %in% dimnames(drc2012)[[1]])

all(county_consumption2012$BEA_code == dimnames(drc2012)[[1]]) # Already sorted OK

# Columnwise, multiply the matrix drc2012 times the consumption vectors for each county.

drc2012_mat <- as.matrix(drc2012)

county_indirectdemand2012 <- apply(county_consumption2012[, -1], 2, function(f) drc2012_mat %*% f)

county_totaldemand2012 <- county_consumption2012[,-1] + county_indirectdemand2012

county_indirectdemand2012 <- data.frame(BEA_code = county_consumption2012$BEA_code, county_indirectdemand2012, check.names = FALSE)
county_totaldemand2012 <- data.frame(BEA_code = county_consumption2012$BEA_code, county_totaldemand2012, check.names = FALSE)

write_csv(county_indirectdemand2012, file.path(fp, 'cfs_io_analysis/county_indirectdemand2012.csv'))    
write_csv(county_totaldemand2012, file.path(fp, 'cfs_io_analysis/county_totaldemand2012.csv'))    
