# Scenarios

# Waste reduction: We need the waste rates by BEA code from LAFA. 50% waste reduction at each of the 3 levels tracked by LAFA
# Then we just reduce the shipments by the proportional amount across the board

# Load data ---------------------------------------------------------------


library(tidyverse)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_cfs <- file.path(fp, 'raw_data/commodity_flows/CFS')
fp_faf <- file.path(fp, 'raw_data/commodity_flows/FAF')
fp_crosswalk <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'crossreference_tables')
fp_out <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'cfs_io_analysis')
fp_chaud <- file.path(fp, 'raw_data/biodiversity/chaudhary2015SI')
fp_github <- ifelse(is_local, '~/Documents/GitHub/foodwaste/virtualland', '~/virtualland')

# Load BEA waste rates
waste_rates_bea <- read_csv(file.path(fp_crosswalk, 'waste_rates_bea.csv'))

# Load the FAF flows (domestic only)
faf_flows <- read_csv(file.path(fp_out, 'FAF_all_flows_x_BEA.csv'))
# Load the FAO foreign flows (not yet split by FAF region) for crops and animals
faf_flows_foreign <- read_csv(file.path(fp_out, 'FAF_foreign_flows_x_BEA.csv'))
fao_crop_trade <- read_csv(file.path(fp_out, 'fao_production_trade_crops.csv'))
fao_animal_trade <- read_csv(file.path(fp_out, 'fao_production_trade_animals.csv'))

# Modify waste rates ------------------------------------------------------

# We need to augment the waste_rates_bea data so we have a waste rate for soybeans (1111A0) and cattle (1121A0)
# Also we need wheat, dairy, miscellaneous crops, and wild fish/game since they have no primary loss rate in LAFA
# Also, we need to use only primary loss rate for 1x codes, and primary*retail*consumer loss rates for 3x codes

# soybean: oilseed, cattle: meat, wheat: cereals, dairy: milk, misc crops: sugar, wild fish: fresh fish

fao_flw_rates <- read_csv('~/halvingfoodwaste/data/flw_rates.csv')
bea_crosswalk <- read_csv(file.path(fp_crosswalk, 'naics_crosswalk_final.csv'))

# The primary loss rate will be ag production x handling and storage x processing and packaging x distribution
fao_cumulative_rates <- as.matrix(fao_flw_rates[,-(1:2)])
fao_cumulative_rates[is.na(fao_cumulative_rates)] <- 0
fao_cumulative_rates <- cbind(fao_flw_rates[,1:2], 1 - t(apply(1 - fao_cumulative_rates, 1, cumprod)))

# Create data frame with two rows for oilseeds and meat to use for soybeans and cattle.
fao_flw_use <- fao_cumulative_rates[match(c('oilseeds and pulses', 'meat', 'cereals', 'milk', 'sugar', 'fish and seafood, fresh'), fao_cumulative_rates$category),]
bea_codes_use <- c('1111A0', '1121A0', '1111B0', '112120', '111900', '114000')
bea_names_use <- bea_crosswalk$BEA_389_def[match(bea_codes_use, bea_crosswalk$BEA_389_code)]

bea_flw_added <- data.frame(BEA_389_code = bea_codes_use,
                            BEA_389_def = bea_names_use,
                            primary_loss_mass = fao_flw_use$loss_distribution * 100)

waste_rates_bea <- waste_rates_bea %>%
  filter(!BEA_389_code %in% bea_codes_use) %>%
  bind_rows(bea_flw_added)

# Calculate demand reduction ----------------------------------------------



# If BEA waste rates are decreased by 50% at all levels (primary, retail, and consumer) by mass, what is the new waste rate?

waste_rates_bea <- waste_rates_bea %>%
  mutate(overall_loss_mass = if_else(substr(BEA_389_code, 1, 1) == '1', 
                                     primary_loss_mass/100, 
                                     1 - (1 - primary_loss_mass/100) * (1 - retail_loss_mass/100) * (1 - avoidable_consumer_loss_mass/100)),
         overall_loss_mass_reduced = if_else(substr(BEA_389_code, 1, 1) == '1',
                                             primary_loss_mass/200,
                                             1 - (1 - primary_loss_mass/200) * (1 - retail_loss_mass/200) * (1 - avoidable_consumer_loss_mass/200)))

# Given those reduced waste rates, what is the proportional reduction in production required to satisfy consumption
waste_rates_bea <- waste_rates_bea %>%
  mutate(production_new = (1 - overall_loss_mass)/(1 - overall_loss_mass_reduced))

# We can multiply each shipment times the modified waste rate to get the reduced ones
flows_wastereduced_domestic <- faf_flows %>%
  left_join(waste_rates_bea, by = c('BEA_Code' = 'BEA_389_code')) %>%
  mutate(tons_reduced = tons_2012 * production_new)

fao_all_trade <- bind_rows(
  fao_crop_trade %>% select(country_code, country_name, BEA_code, export_qty),
  fao_animal_trade %>% rename(country_code = area_code, country_name = area) %>% select(country_code, country_name, BEA_code, export_qty)
)

flows_wastereduced_foreign <- fao_all_trade %>%
  left_join(waste_rates_bea, by = c('BEA_code' = 'BEA_389_code')) %>%
  mutate(tons_reduced = export_qty * production_new)

# FIXME Additionally, we are going to need to reduce the feed shipments by a proportional amount to reflect the decrease in meat production. 
# So we need to identify the shipments in categories that are feed, then decrease them by the proportion that the meat production is decreased by.
# We will use the very simple number for Cassidy et al. that assumes .667 of calories grown in USA are for feed, and .570 of weight of crops is feed.
# Later fix it with the more detailed FAO food balance sheet values.

save(flows_wastereduced_domestic, flows_wastereduced_foreign, file = file.path(fp_out, 'scenarios/flows_wastereduced.RData'))

# Additional reduction due to decrease in meat production
# If meat production decreases by p, then p less feed is needed.
# Flows of oilseeds and cereals are going to decrease by 0.570p, in addition to the rate at which they already declined.
# p is the average decrease in production across all animal product codes (primary loss only)

animal_product_codes <- c('112300', '112A00', '311513', '311514', '31151A', '311520', '311615', '31161A', '311700', '1121A0', '112120')
animal_production_change <- flows_wastereduced_domestic %>%
  filter(BEA_Code %in% animal_product_codes) %>%
  group_by(BEA_Code) %>%
  summarize(wt_old = sum(tons_2012, na.rm = TRUE),
            wt_new = sum(tons_reduced, na.rm = TRUE))
total_old <- sum(animal_production_change$wt_old)
total_new <- sum(animal_production_change$wt_new)
1-total_new/total_old # Meat production has decreased by 8.7%

# Production of oilseeds and grains can decrease by an additional factor of 0.57p
crop_production_decrease <- 0.57 * (1-total_new/total_old) # About 5%

flows_wastereduced_domestic <- flows_wastereduced_domestic %>%
  mutate(crop_production_decrease = if_else(BEA_Code %in% c('1111A0', '1111B0'), 1-crop_production_decrease, 1),
         tons_reduced = tons_reduced * crop_production_decrease)

write_csv(flows_wastereduced_domestic, file.path(fp_out, 'scenarios/flows_wastereduced_domestic_provisional.csv'))
