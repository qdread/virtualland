# Extract data from FAO food balance sheets to get proportions of each crop in each country used for animal feed.
# QDR / Virtualland / 30 Sep 2020

library(tidyverse)
fp_fao <- '/nfs/qread-data/raw_data/FAOSTAT/31aug2020'

fbs <- read_csv(file.path(fp_fao, "FoodBalanceSheets_E_All_Data_(Normalized).csv"))

# Name repair of fbs
fbs <- fbs %>% rename_with(function(x) gsub(' ', '_', tolower(x)))

# Use only the most recent data with complete years
fbs <- fbs %>% 
  filter(year == 2016) %>%
  select(-year_code)

# Widen to get a separate column for each of the weights

fbs_weights_wide <- fbs %>% 
  filter(unit == '1000 tonnes') %>%
  select(-flag, -year, -unit, -element_code) %>%
  pivot_wider(names_from = element, values_from = value) %>% 
  rename_with(function(x) gsub(' ', '_', tolower(x)))

# For now we just need the United States values.
fbs_usa <- fbs_weights_wide %>%
  filter(area %in% 'United States of America')

# Just extract oilcrops and grains
fbs_usa %>% 
  filter(item %in% c('Oilcrops', 'Cereals - Excluding Beer')) %>%
  mutate(feed_prop_wt = feed / domestic_supply_quantity) %>%
  select(-area_code,-area,-item_code,-seed,-losses,-`other_uses_(non-food)`,-tourist_consumption,-residuals)

# Use the broader categories to find the proportions that correspond with BEA codes
items_use <- c('Cereals - Excluding Beer', 'Starchy Roots', 'Sugar Crops', 'Pulses', 'Oilcrops', '')
# FIXME do this later.