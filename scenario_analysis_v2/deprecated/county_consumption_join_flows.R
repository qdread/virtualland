# Join county-level food flows from Lin et al. with county-level consumption
# QDR / Virtualland / 17 Dec 2020

# Method:
# Combine Lin et al's SCTG level county food flows with the SCTG-BEA mapping matrix (crosswalk)
# The conversion of SCTG to BEA cannot be done by simply dividing evenly.
# It needs to be based on the relative dollar amounts produced in each relevant BEA category in each county (NASS data)
# This results in flows, by weight, of BEA products from county to county.
# Then load the total demand for raw agricultural products from each county.
# Use this to determine the counties where the raw ag products consumed in each county are produced (by calculating proportions)

# Load data ---------------------------------------------------------------

library(tidyverse)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_out <- file.path(fp, 'cfs_io_analysis')   
fp_crosswalk <- file.path(fp, 'crossreference_tables')
fp_lin <- file.path(fp, 'raw_data/commodity_flows/Lin_supp_info')

county_flows <- read_csv(file.path(fp_lin, 'Lin_et_al_supp_data_county_flows.csv')) # County flows
load(file.path(fp_crosswalk, 'NAICS_BEA_SCTG_crosswalk.RData')) # SCTG to BEA mapping
receipts_bea_sctg_x_state <- read_csv(file.path(fp_out, 'receipts_bea_sctg_x_state.csv')) # Created in sctg_to_bea.R

susb_nass_bea <- read_csv(file.path(fp_out, 'susb_nass_workers_receipts_bea.csv'))

faf_flows <- read_csv(file.path(fp_out, 'FAF_all_flows_x_BEA.csv'))

# Process receipts data ---------------------------------------------------

# We need to convert the state names to fips codes to assign each county to a state
# We only need SCTG codes 1-7 which should correspond to the 10 BEA codes for primary ag production

receipts_ag <- receipts_bea_sctg_x_state %>% filter(SCTG_Code %in% paste0('0', 1:7))
  

