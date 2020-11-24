# Define functions to apply each of the three scenarios to an arbitrary dataset.
# This way, more than one scenario can be applied in a factorial design.
# QDR/Virtualland/23 Nov 2020

# Load data ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(transport)
library(units)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_cfs <- file.path(fp, 'raw_data/commodity_flows/CFS')
fp_faf <- file.path(fp, 'raw_data/commodity_flows/FAF')
fp_crosswalk <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'crossreference_tables')
fp_out <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'cfs_io_analysis')
fp_chaud <- file.path(fp, 'raw_data/biodiversity/chaudhary2015SI')
fp_github <- ifelse(is_local, '~/Documents/GitHub/foodwaste/virtualland', '~/virtualland')

# Load data needed for diet shift scenario:
# LAFA calories by weight
lafa_calories_weight <- read_csv(file.path(fp_out, 'lafa_calories_weight.csv'))

# Load data needed for waste reduction scenario:
# BEA waste rates
waste_rates_bea <- read_csv(file.path(fp_crosswalk, 'waste_rates_bea.csv'))
# North America waste rates from FAO to augment BEA rates
fao_flw_rates <- read_csv('~/halvingfoodwaste/data/flw_rates.csv')
# Crosswalk table for harmonizing waste rates
bea_crosswalk <- read_csv(file.path(fp_crosswalk, 'naics_crosswalk_final.csv'))


# Load data needed for optimal transport scenario:
# Map of CFS regions
cfsmap <- st_read(dsn = file.path(fp_faf, 'Freight_Analysis_Framework_Regions/cfs_aea.gpkg'))

# Load FAF flows (domestic only)
# FIXME later add foreign
faf_flows <- read_csv(file.path(fp_out, 'FAF_all_flows_x_BEA.csv'))

# Load crosswalk to replace BEA codes with informative names
bea_codes <- read_csv(file.path(fp_crosswalk, 'all_codes.csv')) %>%
  select(sector_code_uppercase, sector_desc_demand) %>%
  setNames(c('BEA_Code', 'BEA_Name')) %>%
  filter(BEA_Code %in% faf_flows$BEA_Code)

# Use shorter names
bea_codes$BEA_Name <- c('soybeans & oilseeds', 'grains', 'vegetables & potatoes', 'fruits & nuts', 'greenhouse crops',
                        'tobacco, cotton, sugar, peanuts, etc.', 'dairy', 'cattle', 'poultry', 'other animals')# Standardize format of faf_flows so that it can be input and output from all scenario functions.
# Include baseline and reduced columns, which are equivalent at first before the scenario functions are applied.
# For now domestic only.
# Also ignore domestic transport mode (sum across them)
faf_flows_domestic <- faf_flows %>%
  filter(trade_type == 1) %>%
  select(dms_orig, dms_dest, BEA_Code, tons_2012) %>%
  group_by(dms_orig, dms_dest, BEA_Code) %>%
  summarize_all(sum) %>%
  rename(tons_baseline = tons_2012) %>%
  mutate(tons_reduced = tons_baseline)
    

# Initial processing: diet shift ------------------------------------------

# Do naive conversion by calories.

# A few aggregated groups are included. All say total. Remove.
lafa_calories_weight <- lafa_calories_weight %>%
  mutate(grams_available = calories_available/calories_per_gram) %>%
  filter(!grepl('Total', category, ignore.case = TRUE))

# Sum up calories by group
cal_group_sums <- lafa_calories_weight %>% 
  group_by(animal_product) %>%
  summarize(calories_available = sum(calories_available))
veg_calories <- cal_group_sums$calories_available[cal_group_sums$animal_product %in% 'no']

lafa_calories_weight <- lafa_calories_weight %>%
  mutate(calories_reduced = if_else(animal_product %in% 'no', 0, calories_available * 0.5),
         grams_reduced = calories_reduced/calories_per_gram) 

calories_reduced_total <- sum(lafa_calories_weight$calories_reduced) # about 600 calories per day decreased, as ~1200 come from animals.

# Divide up the increased calories assuming all non animal foods are increased by a proportional amount
lafa_calories_weight <- lafa_calories_weight %>%
  mutate(calories_increased = if_else(animal_product %in% 'no', calories_available * calories_reduced_total/veg_calories, 0),
         grams_increased = calories_increased/calories_per_gram)


# Initial processing: waste reduction -------------------------------------

# We need to augment the waste_rates_bea data so we have a waste rate for soybeans (1111A0) and cattle (1121A0)
# Also we need wheat, dairy, miscellaneous crops, and wild fish/game since they have no primary loss rate in LAFA
# Also, we need to use only primary loss rate for 1x codes, and primary*retail*consumer loss rates for 3x codes

# soybean: oilseed, cattle: meat, wheat: cereals, dairy: milk, misc crops: sugar, wild fish: fresh fish

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

# Initial processing: optimal transport -----------------------------------

# Sort cfsmap by the code
cfsmap <- cfsmap %>% arrange(Code)

# Centroids of CFS map
cfs_centroid <- st_centroid(cfsmap)

# Distance between pairs
cfs_centroid_dist <- st_distance(cfs_centroid, by_element = FALSE)
# Coerce distance matrix to plain numeric matrix
cfs_centroid_distmat <- drop_units(cfs_centroid_dist)


# Define scenario function: diet shift ------------------------------------

# All three scenario functions must return modified flows in the same format.

diet_shift <- function(flows) {
  # Assign each of the 10 BEA codes to animal product to determine whether they will be increased or decreased.
  # Basically the animal codes decrease by 50% and the plant codes increase by 37% (this equalizes calories)
  # Then the feedstock codes decrease such that 43% increase by 37% and 57% decrease by 50% (net 12% decrease)
  animal_product_codes <- c('112120', '1121A0', '112300', '112A00')
  plant_product_codes <- c('111200', '111300', '111400', '111900')
  feed_codes <- c('1111A0', '1111B0')
  
  # We can multiply each shipment times the modified waste rate to get the reduced ones
  # Return modified flow objects
  flows %>%
    mutate(tons_reduced = case_when(
      BEA_Code %in% animal_product_codes ~ tons_reduced * 0.5,
      BEA_Code %in% plant_product_codes ~ tons_reduced * (1 + calories_reduced_total/veg_calories),
      BEA_Code %in% feed_codes ~ tons_reduced * (((1-0.57) * (1+calories_reduced_total/veg_calories)) + (0.57 * 0.5))
    ))
}

# Define scenario function: waste reduction -------------------------------

waste_reduction <- function(flows) {
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
  flows_out <- flows %>%
    left_join(waste_rates_bea, by = c('BEA_Code' = 'BEA_389_code')) %>%
    mutate(tons_reduced = tons_reduced * production_new)
  
  # Additional reduction due to decrease in meat production
  # If meat production decreases by p, then p less feed is needed.
  # Flows of oilseeds and cereals are going to decrease by 0.570p, in addition to the rate at which they already declined.
  # p is the average decrease in production across all animal product codes (primary loss only)
  
  animal_product_codes <- c('112300', '112A00', '311513', '311514', '31151A', '311520', '311615', '31161A', '311700', '1121A0', '112120')
  animal_production_change <- flows %>%
    filter(BEA_Code %in% animal_product_codes) %>%
    group_by(BEA_Code) %>%
    summarize(wt_old = sum(tons_baseline, na.rm = TRUE),
              wt_new = sum(tons_reduced, na.rm = TRUE))
  total_old <- sum(animal_production_change$wt_old)
  total_new <- sum(animal_production_change$wt_new)

  # Production of oilseeds and grains can decrease by an additional factor of 0.57p
  crop_production_decrease <- 0.57 * (1-total_new/total_old) # About 5%
  
  flows_out <- flows_out %>%
    mutate(crop_production_decrease = if_else(BEA_Code %in% c('1111A0', '1111B0'), 1-crop_production_decrease, 1),
           tons_reduced = tons_reduced * crop_production_decrease)
  
  # Remove the temporarily added column names
  flows_out %>% select(all_of(names(flows)))
}

# Define scenario function: optimal transport -----------------------------

optimal_transport <- function(flows) {
  # Sum for all BEA_codes
  # Sum tonnage by all origins and all destinations
  # Only use domestic for now
  # FIXME trade_type filter is commented out.
  
  faf_sums_origin <- flows %>%
    #filter(trade_type == 1) %>%
    group_by(dms_orig, BEA_Code) %>%
    summarize(tons = sum(tons_reduced, na.rm = TRUE))
  
  faf_sums_destination <- flows %>%  
    #filter(trade_type == 1) %>%
    group_by(dms_dest, BEA_Code) %>%
    summarize(tons = sum(tons_reduced, na.rm = TRUE))
  
  faf_sums_origin_wide <- faf_sums_origin %>%
    pivot_wider(names_from = BEA_Code, values_from = tons, values_fill = 0)
  
  faf_sums_destination_wide <- faf_sums_destination %>%
    pivot_wider(names_from = BEA_Code, values_from = tons, values_fill = 0)
  
  faf_sums_origin_wide <- faf_sums_origin_wide %>%
    ungroup %>%
    add_row(dms_orig = '111') %>%
    arrange(dms_orig) %>%
    mutate_if(is.numeric, ~ if_else(is.na(.), 0, .))

  goods <- unique(faf_sums_origin$BEA_Code)
  
  # Optimal transport plan for each of the 10 goods
  transport_plans <- imap_dfr(goods, ~ data.frame(BEA_Code = .,
                                                  transport(a = deframe(faf_sums_origin_wide[,.]),
                                                            b = deframe(faf_sums_destination_wide[,.]),
                                                            costm = cfs_centroid_distmat,
                                                            method = 'revsimplex')))
  
  transport_optimal <- transport_plans %>%
    mutate(dms_orig = faf_sums_origin_wide$dms_orig[from],
           dms_dest = faf_sums_origin_wide$dms_orig[to]) %>%
    rename(mass_optimal = mass)

  # Standardize the output to the same format as the other outputs.
  transport_optimal <- transport_optimal %>%
    select(dms_orig, dms_dest, BEA_Code, mass_optimal) %>%
    rename(tons_reduced = mass_optimal)
  
  # This requires a different procedure than the other ones because there may be some flows that are zero in baseline
  # but increase in the alternative scenario.
  
  flows %>%
    select(-tons_reduced) %>%
    full_join(transport_optimal) %>%
    select(all_of(names(flows))) %>%
    mutate_at(vars(tons_reduced), coalesce, 0) # fill NA entries with 0 after join.

}

# Apply scenario functions ------------------------------------------------

# Apply all scenarios in 2x2x2 factorial design

scenario_grid <- cbind(scenario = 1:8,
                       expand_grid(diet_shift = c(FALSE, TRUE),
                                   waste_reduction = c(FALSE, TRUE),
                                   optimal_transport = c(FALSE, TRUE)))

# Function to apply functions only if TRUE
apply_scenarios <- function(diet_shift, waste_reduction, optimal_transport, flows) {
  if (diet_shift) flows <- diet_shift(flows)
  if (waste_reduction) flows <- waste_reduction(flows)
  flows <- flows %>% mutate(scalingfactor = tons_reduced/tons_baseline)
  if (optimal_transport) flows <- optimal_transport(flows)
  return(flows)
}

scenarios <- scenario_grid %>%
  group_by_all %>%
  group_modify(~ apply_scenarios(.$diet_shift, .$waste_reduction, .$optimal_transport, flows = faf_flows_domestic), .keep = TRUE)

# Write output
write_csv(scenarios, file.path(fp_out, 'scenarios/flows_2x2x2_factorial_domestic.csv'))
