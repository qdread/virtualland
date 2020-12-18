# NASS extract county-level data
# QDR / Virtualland / 17 Dec. 2020

library(tidyverse)
library(mice)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')

# Read text file ---------------------------------------------------

cdqt_file <- '/nfs/qread-data/raw_data/USDA/2012_cdqt_data.txt'

# Columns are delimited by tabs? Probably
# Read all as characters to begin with.
cdqt <- read_delim(cdqt_file, delim = '\t', col_names = FALSE, col_types = strrep('c', 15))


# NAICS by county if possible ---------------------------------------------

cdqt_naics_county <- cdqt %>%
  filter(grepl('^NAICS', X14), X8 == 'COUNTY')

# Number of farm operations by county can be used to downscale the NAICS production by value from state to county.


# CBP county level data ---------------------------------------------------

# Use this for non-agricultural codes.

cbp12co <- read_csv(file.path(fp, 'raw_data/Census/CBP/cbp12co.txt'))

### Use CBP for the other non-agricultural codes
# Variables: empflag shows if data are withheld, emp_nf is employee noise flag, emp is total employees, qp1 is quarter 1 payroll. ap is annual payroll, est is number of establishments and then the number of establishments with different numbers of employees.

# We have CBP in NAICS codes so that would have to be converted to BEA
