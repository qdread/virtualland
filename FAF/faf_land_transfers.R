# Combine FAF data with cropland by faf & bea matrix, to get virtual land transfers from one FAF region to another.
# QDR / FWE / 19 May 2020

# Modified 15 Sep 2020: Include foreign-origin transfers.

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

# Load cropland and pastureland by FAF foreign origin
vlt_foreign <- read_csv(file.path(fp_out, 'foreign_VLT_by_region.csv'))

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
# Separate shipments with foreign origin (trade type 2) 

# Divide the virtual crop and pasture transfers evenly based on foreign transfers of agricultural products.
crop_codes <- sprintf('%02d', c(2,3,6,7,8,9))
pasture_codes <- sprintf('%02d', c(1,4,5))

faf_by_bea_foreign <- faf_by_bea %>%
  filter(trade_type == '2') %>%
  mutate(land_type = case_when(SCTG_Code %in% crop_codes ~ 'cropland',
                               SCTG_Code %in% pasture_codes ~ 'pastureland',
                               TRUE ~ 'none')) %>%
  filter(land_type %in% c('cropland', 'pastureland')) %>%
  group_by(fr_orig, dms_orig, dms_dest, fr_dest, fr_inmode, fr_outmode, land_type) %>%
  summarize(tons_2012 = sum(tons_2012), 
            value_2012 = sum(value_2012),
            tmiles_2012 = sum(tmiles_2012),
            wgt_dist = sum(wgt_dist))

# Join foreign shipments with land transfers by foreign region. Split by value.
# Convert foreign by value because different categories are lumped together.
faf_by_bea_foreign <- faf_by_bea_foreign %>%
  select(-tons_2012, -tmiles_2012, -wgt_dist) %>%
  group_by(fr_orig, dms_orig, dms_dest, fr_dest, fr_inmode, fr_outmode) %>%
  pivot_wider(names_from = land_type, values_from = value_2012) %>%
  left_join(vlt_foreign %>% mutate(FAF_foreign_region_code = factor(FAF_foreign_region_code)), by = c('fr_orig' = 'FAF_foreign_region_code')) %>%
  ungroup %>%
  group_by(fr_orig) %>%
  mutate(cropland_proportion = cropland / sum(cropland, na.rm = TRUE),
         pastureland_proportion = pastureland / sum(pastureland, na.rm = TRUE)) %>%
  ungroup %>%
  mutate(cropland_flow = VLT_crop * cropland_proportion,
         pastureland_flow = VLT_pasture * pastureland_proportion)

# Convert domestic by weight.
faf_by_bea <- faf_by_bea %>%
  filter(trade_type != '2', !is.na(cropland), !is.na(pastureland)) %>%
  group_by(dms_orig) %>%
  mutate(tons_proportion = tons_2012 / sum(tons_2012, na.rm = TRUE)) %>%
  ungroup %>%
  mutate(cropland_flow = tons_proportion * cropland,
         pastureland_flow = tons_proportion * pastureland)

# Convert units of domestic land transfers from acres to square km
to_km2 <- function(x) x %>% set_units(acre) %>% set_units(km^2) %>% as.numeric

faf_by_bea <- faf_by_bea %>%
  mutate_at(vars(contains('land')), to_km2)

# Convert units of foreign import transfers from hectares to square km
faf_by_bea_foreign <- faf_by_bea_foreign %>%
  mutate(cropland_flow = cropland_flow * 0.01,
         pastureland_flow = pastureland_flow * 0.01)

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

# Foreign exports
land_export <- faf_by_bea %>%
  filter(trade_type == 3) %>%
  group_by(dms_orig) %>%
  summarize(cropland_flow = sum(cropland_flow), pastureland_flow = sum(pastureland_flow)) %>%
  add_row(dms_orig = '111', cropland_flow = 0, pastureland_flow = 0) # Washington DC

# Foreign imports
land_import <- faf_by_bea_foreign %>%
  group_by(dms_dest) %>%
  summarize(cropland_flow = sum(cropland_flow, na.rm = TRUE), pastureland_flow = sum(pastureland_flow, na.rm = TRUE))

# Net foreign import-export balance
land_netforeign <- left_join(land_export, land_import, by = c('dms_orig' = 'dms_dest')) %>%
  mutate(cropland_flow = cropland_flow.y - cropland_flow.x,
         pastureland_flow = pastureland_flow.y - pastureland_flow.x) %>%
  select(dms_orig, cropland_flow, pastureland_flow) %>%
  rename(region = dms_orig)


# Net land transfers, including both domestic and foreign.

land_netall <- left_join(land_netdomestic, land_netforeign, by = 'region') %>%
  mutate(cropland_flow = cropland_flow.y + cropland_flow.x,
         pastureland_flow = pastureland_flow.y + pastureland_flow.x) %>%
  select(region, cropland_flow, pastureland_flow)
