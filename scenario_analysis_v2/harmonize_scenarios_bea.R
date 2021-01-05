# Harmonize LAFA food production baseline and scenario values with BEA and supplement missing categories with FAO waste rates.
# QDR / Virtualland / 05 Jan 2021

library(tidyverse)

fp_diet <- 'data/raw_data/food_consumption/diet_guidelines'
fp_crosswalk <- 'data/crossreference_tables'
fp_out <- 'data/cfs_io_analysis'

# LAFA data with the different scenario production factors calculated
lafa_df <- read_csv(file.path(fp_out, 'lafa_with_production_factors_diet_x_waste.csv'))


# Process lafa scenario input data ----------------------------------------

# Get produced food weights for all the scenarios by first converting all to same units and then reshaping

# Primary weight lb/y is the produced weight in baseline case.
# Multiply the primary weight by the appropriate production factor. (first select only the weights and production factors)
scenario_production_weights <- lafa_df %>% 
  select(Category, primary_weight_lb_y, planetary_health, us_style, med_style, vegetarian, preconsumer_waste_reduction_prod_factor, consumer_waste_reduction_prod_factor, allavoidable_waste_reduction_prod_factor, starts_with('planetary_health_x'), starts_with('us_style_x'), starts_with('med_style_x'), starts_with('vegetarian_x')) %>%
  mutate_at(vars(planetary_health:vegetarian_x_allavoidable), ~ . * primary_weight_lb_y)

# Rename the columns to consistent names for the 20 scenarios (5x diets x 4x waste)

diet_scenarios <- c('baseline', 'planetaryhealth', 'usstyle', 'medstyle', 'vegetarian')
waste_scenarios <- c('baseline', 'preconsumer', 'consumer', 'allavoidable')

new_names <- c(paste('D', diet_scenarios, 'WR_baseline', sep = '_'), paste('D_baseline', 'WR', waste_scenarios[-1], sep = '_'), outer(waste_scenarios[-1], diet_scenarios[-1], function(w, d) paste('D', d, 'WR', w, sep = '_')))

names(scenario_production_weights)[-1] <- new_names


# Harmonize LAFA categories with BEA --------------------------------------

# Procedure: 
# - Map LAFA categories to QFAHPD categories
# - Convert the weights from LAFA to dollars using the prices per pound from QFAHPD
# - Map QFAHPD categories to BEA categories

# Some code modified from the script lafa_rate_conversion.R from the foodwasteinterventions project

# Load the two necessary crosswalks and LAFA category structure lookup table
bea2qfahpd <- read_csv(file.path(fp_crosswalk, 'bea_qfahpd_crosswalk.csv'))
qfahpd2lafa <- read_csv(file.path(fp_crosswalk, 'qfahpd_lafa_crosswalk.csv'))
lafa_struct <- read_csv(file.path(fp_crosswalk, 'lafa_category_structure.csv'))

# Also load the QFAHPD data so that we can get the prices.
qfahpd2 <- read_csv('~/foodwasteinterventions/data/intermediate_output/qfahpd2.csv')

# Convert the comma-separated string columns to list columns.
bea2qfahpd <- bea2qfahpd %>%
  mutate(QFAHPD_code = strsplit(QFAHPD_code, ';'))
qfahpd2lafa <- qfahpd2lafa %>%
  mutate(LAFA_names = strsplit(LAFA_names, ';'))

# Create an aggregated version of QFAHPD to get the final price values for each code
# Weighted average across all market groups, years, and quarters
qfahpd_agg <- qfahpd2 %>%
  group_by(foodgroup) %>%
  summarize(price = weighted.mean(price, aggweight, na.rm = TRUE))

# Using unweighted average, calculate the average price per pound of each LAFA food, averaged across the QFAHPD foods that make it up.
lafa_priceperpound <- qfahpd2lafa %>% 
  unnest(cols = LAFA_names) %>%
  left_join(qfahpd_agg, by = c('QFAHPD_name' = 'foodgroup')) %>%
  group_by(LAFA_names) %>%
  summarize(price = mean(price, na.rm = TRUE))

# Use the category structure table of LAFA to assign prices from parent LAFA categories to the individual LAFA foods
lafa_struct  <- mutate(lafa_struct, 
                       price = pmap_dbl(lafa_struct, function(Food, subgroup1, subgroup2, subgroup3, subgroup4, ...) {
                         possible_names <- c(Food, subgroup1, subgroup2, subgroup3, subgroup4)
                         price <- c(lafa_priceperpound$price[lafa_priceperpound$LAFA_names %in% possible_names], NA)[1]
                       })
)

# Calculate mean prices per pound for the aggregated categories.
# Use the most specific possible aggregated category to assign prices to the individual foods that don't have prices.
lafa_price_agg1 <- lafa_struct %>% group_by(subgroup1) %>%
  summarize(price=mean(price, na.rm = TRUE))
lafa_struct <- left_join(lafa_struct, lafa_price_agg1, by = c('Food' = 'subgroup1')) %>%
  mutate(price = coalesce(price.x, price.y)) %>%
  select(-price.x, -price.y)
lafa_price_agg2 <- lafa_struct %>% group_by(subgroup2) %>%
  summarize(price=mean(price, na.rm = TRUE))
lafa_struct <- left_join(lafa_struct, lafa_price_agg2, by = c('Food' = 'subgroup2')) %>%
  mutate(price = coalesce(price.x, price.y)) %>%
  select(-price.x, -price.y)
lafa_price_agg3 <- lafa_struct %>% group_by(subgroup3) %>%
  summarize(price=mean(price, na.rm = TRUE))
lafa_struct <- left_join(lafa_struct, lafa_price_agg3, by = c('Food' = 'subgroup3')) %>%
  mutate(price = coalesce(price.x, price.y)) %>%
  select(-price.x, -price.y)
lafa_price_agg4 <- lafa_struct %>% group_by(subgroup4) %>%
  summarize(price=mean(price, na.rm = TRUE))
lafa_struct <- left_join(lafa_struct, lafa_price_agg4, by = c('Food' = 'subgroup4')) %>%
  mutate(price = coalesce(price.x, price.y)) %>%
  select(-price.x, -price.y)

# Assign the prices
for (i in 1:nrow(lafa_struct)) {
  if (is.na(lafa_struct$price[i])) {
    possible_prices <- lafa_struct$price[match(c(lafa_struct$subgroup1[i], lafa_struct$subgroup2[i], lafa_struct$subgroup3[i], lafa_struct$subgroup4[i]), lafa_struct$Food)]
    possible_prices <- possible_prices[!is.na(possible_prices)]
    if (length(possible_prices) > 0) lafa_struct$price[i] <- possible_prices[1]
  }
}

lafa_priceperpound_all <- lafa_struct %>% 
  filter(Food %in% scenario_production_weights$Category) %>%
  select(Food, price)

setdiff(scenario_production_weights$Category, lafa_priceperpound_all$Food)
# Four LAFA foods still do not have a price per pound.
# Assign the two missing flour categories a price equal to the average flour price.
# Assign the missing fresh and processed vegetables categories the average fresh and processed vegetable prices, respectively.
# FIXME

# Pivot longer the lafa scenario weights and join with price per pound
scenario_weights_long <- pivot_longer(scenario_production_weights, -Category, names_to = 'scenario', values_to = 'weight') %>%
  left_join(lafa_priceperpound_all, by = c('Category' = 'Food'))

# FIXME Add FAO waste rates for the categories present in BEA but not LAFA (beverages)

