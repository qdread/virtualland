# Convert FAF land transfers to ecoregion land transfers
# We still want the land transfers from FAF to FAF, but just split them up based on what ecoregion the land is coming from

# Needed: FAF to FAF cropland and pastureland transfers
# Totals of cropland and pastureland by ecoregion by FAF

# Modified 01 Sep 2020: Move creation of tabulated nlcd faf x tnc CSV to another script
# Modified 17 Sep 2020: Include foreign ecoregions (using data processed in fao_transfers_by_ecoregion.R)

# Prepare data ------------------------------------------------------------

source('FAF/faf_land_transfers.R')

# Code to write outputs of faf_land_transfers.R was, logically, moved to that script.
# Code to create nlcd_faf_tnc was moved to another script.

# For each FAF, split transfers by ecoregion ------------------------------

faf_lookup <- read_csv(file.path(fp_out, 'faf_region_lookup.csv'))
faf_flows_all <- read_csv(file.path(fp_out, 'FAF_all_flows_x_BEA.csv'))
faf_flows_foreign <- read_csv(file.path(fp_out, 'FAF_foreign_flows_x_BEA.csv'))
nlcd_faf_tnc <- read_csv(file.path(fp_out, 'NLCDcrop_FAF_x_TNC.csv'))
foreign_vlt_eco <- read_csv(file.path(fp_out, 'foreign_VLT_by_country_x_TNC.csv'))
foreign_vlt_eco_sum <- read_csv(file.path(fp_out, 'foreign_VLT_by_region_x_TNC.csv'))
foreign_tnc_land_by_faf <- read_csv(file.path(fp_out, 'foreign_ecoregion_land_by_FAF.csv'))

# We can sum up the transfers by BEA code. Then, for all outgoing transfers, we can assign proportions of them to each 
# ecoregion that the goods came from (based on crop and pastureland percentages in the ecoregions in each FAF.)

# Do not care which foreign region goods are going to, also do not care about transportation mode

faf_flows_by_regions <- faf_flows_all %>%
  select(-SCTG_Code, -BEA_Code, -fr_dest, -fr_inmode, -dms_mode, -fr_outmode, -annual_cropland, -permanent_cropland, -pastureland) %>%
  group_by(fr_orig, dms_orig, dms_dest, trade_type) %>%
  summarize_all(sum)
  
# Next, join this dataframe with the ecoregions within each originating FAF.
# This will be a full join because there are potentially multiple ecoregions per faf.

# Only take essential rows from NLCD tally dataframe
nlcd_faf_tnc_reduced <- nlcd_faf_tnc %>% select(Code, ECO_CODE, crop, other, pasture, water)

# Make sure they are all accounted for in faf flows dataframe
nlcdfafcodes <- unique(nlcd_faf_tnc_reduced$Code) 
nlcdfafcodes[!nlcdfafcodes %in% faf_flows_by_regions$dms_orig]
faf_lookup %>% filter(Code == '111') # It is Washington DC. It can be excluded since there is negligible crop and pastureland there.

# Get the proportion crop and pasture in each ecoregion in each FAF before joining.
nlcd_faf_tnc_reduced <- nlcd_faf_tnc_reduced %>%
  group_by(Code) %>%
  mutate(cropland_ecoregion_proportion = crop / sum(crop, na.rm = TRUE),
         pastureland_ecoregion_proportion = pasture / sum(pasture, na.rm = TRUE)) %>%
  ungroup

faf_flows_tnc_joined <- full_join(faf_flows_by_regions, nlcd_faf_tnc_reduced, by = c('dms_orig' = 'Code'))

# Use the cropland and pastureland proportions to get the cropland and pastureland flows out of each ecoregion into each FAF.
# Multiply cropland flow by cropland proportion, and pastureland flow by pastureland proportion
# Use the same proportion for annual and permanent cropland.
faf_flows_tnc_joined <- faf_flows_tnc_joined %>%
  mutate(annual_cropland_flow = annual_cropland_flow * cropland_ecoregion_proportion,
         permanent_cropland_flow = permanent_cropland_flow * cropland_ecoregion_proportion,
         pastureland_flow = pastureland_flow * pastureland_ecoregion_proportion)

# Check grand totals
sum(faf_flows_tnc_joined$annual_cropland_flow, na.rm = TRUE)
sum(faf_flows_all$annual_cropland_flow, na.rm = TRUE) # A few got removed due to NA but otherwise fine

### Foreign transfers: join the TNC x FAF foreign region land transfer data with the FAF flows (proportional)
# Proportionally multiply out the FAF flows by proportional ecoregion flow data from the global cropland/pastureland stuff.
faf_flows_foreign_tnc_joined <- faf_flows_foreign %>%
  group_by(FAF_foreign_region, fr_orig, dms_orig, dms_dest) %>%
  summarize(annual_cropland_flow = sum(annual_cropland_flow, na.rm = TRUE),
            permanent_cropland_flow = sum(permanent_cropland_flow, na.rm = TRUE),
            pastureland_flow = sum(pastureland_flow, na.rm = TRUE)) %>%
  left_join(foreign_tnc_land_by_faf, by = c('FAF_foreign_region' = 'FAF_foreign_region', 'fr_orig' = 'FAF_foreign_region_code')) %>%
  ungroup %>%
  mutate(annual_cropland_flow = annual_cropland_flow * crop_proportion_by_FAF_region,
         permanent_cropland_flow = permanent_cropland_flow * crop_proportion_by_FAF_region,
         pastureland_flow = pastureland_flow * pasture_proportion_by_FAF_region) %>%
  group_by(FAF_foreign_region, fr_orig, dms_orig, dms_dest, ECO_CODE, ECO_NAME) %>%
  summarize(annual_cropland_flow = sum(annual_cropland_flow),
            permanent_cropland_flow = sum(permanent_cropland_flow),
            pastureland_flow = sum(pastureland_flow)) %>%
  ungroup %>%
  filter(annual_cropland_flow > 0 | permanent_cropland_flow > 0 | pastureland_flow > 0)

# Check grand totals
sum(faf_flows_foreign_tnc_joined$annual_cropland_flow, na.rm = TRUE) 

# Save totals to a CSV

write_csv(faf_flows_tnc_joined, file.path(fp_out, 'FAF_all_flows_x_TNC.csv'))
write_csv(faf_flows_foreign_tnc_joined, file.path(fp_out, 'FAF_foreign_flows_x_TNC.csv'))

# Use population weights to get transfers to TNC --------------------------

# So far, we have transfers from TNC to FAF. We can also get TNC to TNC transfers by weighting by population grid.

faf_lookup <- read_csv(file.path(fp_out, 'faf_region_lookup.csv'))
faf_flows_all <- read_csv(file.path(fp_out, 'FAF_all_flows_x_BEA.csv'))
nlcd_faf_tnc <- read_csv(file.path(fp_out, 'NLCDcrop_FAF_x_TNC.csv'))
faf_flows_tnc_joined <- read_csv(file.path(fp_out, 'FAF_all_flows_x_TNC.csv'))
faf_flows_foreign_tnc_joined <- read_csv(file.path(fp_out, 'FAF_foreign_flows_x_TNC.csv'))
pop_faf_tnc <- read_csv(file.path(fp_out, 'population_FAF_x_TNC_3column.csv'))

# Get the proportion population in each ecoregion in each FAF before joining.
pop_faf_tnc <- pop_faf_tnc %>%
  group_by(FAF) %>%
  mutate(pop_proportion = pop / sum(pop, na.rm = TRUE)) %>%
  ungroup

faf_flows_tnc_pop_joined <- faf_flows_tnc_joined %>%
 full_join(pop_faf_tnc, by = c('dms_dest' = 'FAF'))

faf_flows_foreign_tnc_pop_joined <- faf_flows_foreign_tnc_joined %>%
  full_join(pop_faf_tnc, by = c('dms_dest' = 'FAF'))

# Domestic:
# Convert flows based on population proportion
faf_flows_tnc_pop_joined <- faf_flows_tnc_pop_joined %>%
  mutate(annual_cropland_flow = annual_cropland_flow * pop_proportion,
         permanent_cropland_flow = permanent_cropland_flow * pop_proportion,
         pastureland_flow = pastureland_flow * pop_proportion) %>%
  rename(TNC_orig = ECO_CODE,
         TNC_dest = TNC)

# Aggregate to only TNC x TNC flows
tnc_flows_agg <- faf_flows_tnc_pop_joined %>%
  group_by(TNC_orig, TNC_dest, trade_type) %>%
  summarize(annual_cropland_flow = sum(annual_cropland_flow, na.rm = TRUE),
            permanent_cropland_flow = sum(permanent_cropland_flow, na.rm = TRUE),
            pastureland_flow = sum(pastureland_flow, na.rm = TRUE))

# Foreign imports:
faf_flows_foreign_tnc_pop_joined <- faf_flows_foreign_tnc_pop_joined %>%
  mutate(annual_cropland_flow = annual_cropland_flow * pop_proportion,
         permanent_cropland_flow = permanent_cropland_flow * pop_proportion,
         pastureland_flow = pastureland_flow * pop_proportion) %>%
  rename(TNC_orig = ECO_CODE,
         TNC_dest = TNC)

tnc_flows_foreign_agg <- faf_flows_foreign_tnc_pop_joined %>%
  group_by(TNC_orig, TNC_dest) %>%
  summarize(annual_cropland_flow = sum(annual_cropland_flow, na.rm = TRUE),
            permanent_cropland_flow = sum(permanent_cropland_flow, na.rm = TRUE),
            pastureland_flow = sum(pastureland_flow, na.rm = TRUE))

# Write outputs.
# Domestic
write_csv(faf_flows_tnc_pop_joined, file.path(fp_out, 'FAF_all_flows_TNC_x_TNC.csv'))
write_csv(tnc_flows_agg, file.path(fp_out, 'TNC_x_TNC_all_flows.csv'))

# Foreign
write_csv(faf_flows_foreign_tnc_pop_joined, file.path(fp_out, 'FAF_all_foreign_flows_TNC_x_TNC.csv'))
write_csv(tnc_flows_foreign_agg, file.path(fp_out, 'TNC_x_TNC_all_foreign_flows.csv'))

