# Combine FAF data with cropland by faf & bea matrix, to get virtual land transfers from one FAF region to another.
# QDR / FWE / 19 May 2020

# Load data ---------------------------------------------------------------

library(tidyverse)
library(units)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_cfs <- file.path(fp, 'raw_data/commodity_flows/CFS')
fp_faf <- file.path(fp, 'raw_data/commodity_flows/FAF')
fp_crosswalk <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'crossreference_tables')
fp_out <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'cfs_io_analysis')
fp_github <- ifelse(is_local, '~/Documents/GitHub/foodwaste/virtualland', '~/virtualland')

# Load FAF data
load(file.path(fp_out, 'faf_by_bea.RData'))
faf_lookup <- read_csv(file.path(fp_out, 'faf_region_lookup.csv'))

# load FAF by BEA cropland and pastureland
nass_bea_land <- read_csv(file.path(fp_out, 'nass_bea_state_x_faf_land_totals.csv'))

# Scale transfers by land -------------------------------------------------

# For each origin, assume all ag land is converted to goods, to convert the weight or value flow to a land flow

nass_bea_land <- nass_bea_land %>%
  left_join(faf_lookup) %>%
  select(Code, FAF_Region, BEA_code, cropland_normalized, pastureland_normalized) %>%
  setNames(c('FAF_Code', 'FAF_Region', 'BEA_Code', 'cropland', 'pastureland'))

# Join FAF with NASS land values
faf_by_bea <- faf_by_bea %>%
  left_join(nass_bea_land %>% select(-FAF_Region), by = c('dms_orig' = 'FAF_Code', 'BEA_Code' = 'BEA_Code'))

# Convert each tonnage flow to a cropland flow and a pastureland flow.
# For each origin, get the fraction of the total tonnage represented by each shipment, then multiply it by the cropland to get the acreage represented by each shipment
# Exclude shipments with foreign origin (trade type 2) since we don't really have any way of knowing what land is involved there

faf_by_bea <- faf_by_bea %>%
  filter(trade_type != '2', !is.na(cropland), !is.na(pastureland)) %>%
  group_by(dms_orig) %>%
  mutate(tons_proportion = tons_2012 / sum(tons_2012, na.rm = TRUE)) %>%
  ungroup %>%
  mutate(cropland_flow = tons_proportion * cropland,
         pastureland_flow = tons_proportion * pastureland)

# Convert units from acres to square km
to_km2 <- function(x) x %>% set_units(acre) %>% set_units(km^2) %>% as.numeric

faf_by_bea <- faf_by_bea %>%
  mutate_at(vars(contains('land')), to_km2)

# Calculate summary totals ------------------------------------------------

# Domestic
land_outbound <- faf_by_bea %>%
  filter(trade_type == 1, dms_orig != dms_dest) %>%
  group_by(dms_orig) %>%
  summarize(cropland_flow = sum(cropland_flow), pastureland_flow = sum(pastureland_flow)) %>%
  add_row(dms_orig = '111', cropland_flow = 0, pastureland_flow = 0) # Washington DC

land_inbound <- faf_by_bea %>%
  filter(trade_type == 1, dms_orig != dms_dest) %>%
  group_by(dms_dest) %>%
  summarize(cropland_flow = sum(cropland_flow), pastureland_flow = sum(pastureland_flow)) 

land_netdomestic <- left_join(land_outbound, land_inbound, by = c('dms_orig' = 'dms_dest')) %>%
  mutate(cropland_flow = cropland_flow.y - cropland_flow.x,
         pastureland_flow = pastureland_flow.y - pastureland_flow.x) %>%
  select(dms_orig, cropland_flow, pastureland_flow) %>%
  rename(region = dms_orig)

# Foreign
land_export <- faf_by_bea %>%
  filter(trade_type == 3) %>%
  group_by(dms_orig) %>%
  summarize(cropland_flow = sum(cropland_flow), pastureland_flow = sum(pastureland_flow)) %>%
  add_row(dms_orig = '111', cropland_flow = 0, pastureland_flow = 0) # Washington DC


