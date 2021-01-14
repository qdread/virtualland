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

# Load the necessary crosswalks and LAFA category structure lookup table
qfahpd2lafa <- read_csv(file.path(fp_crosswalk, 'qfahpd_lafa_crosswalk.csv'))
lafa2bea <- read_csv(file.path(fp_crosswalk, 'lafa_qfahpd_naics_crosswalk.csv'))
lafa_struct <- read_csv(file.path(fp_crosswalk, 'lafa_category_structure.csv'))
bea2lafa <- read_csv(file.path(fp_crosswalk, 'bea_lafa_crosswalk.csv'))

# Also load the QFAHPD data so that we can get the prices.
qfahpd2 <- read_csv('~/foodwasteinterventions/data/intermediate_output/qfahpd2.csv')

# Convert the comma-separated string columns to list columns.
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
extra_prices <- data.frame(Food = setdiff(scenario_production_weights$Category, lafa_priceperpound_all$Food),
                           price = c(rep(qfahpd_agg$price[qfahpd_agg$foodgroup == 'Whole grain flour and mixes'], 2), 
                                     lafa_priceperpound_all$price[lafa_priceperpound_all$Food == 'Fresh cabbage'],
                                     lafa_priceperpound_all$price[lafa_priceperpound_all$Food == 'Potato chips']))

lafa_priceperpound_all <- rbind(lafa_priceperpound_all, extra_prices)

# Pivot longer the lafa scenario weights and join with price per pound
# These are the dollars per capita per year spent.
# Add a category number
scenario_weights_long <- scenario_production_weights %>%
  mutate(LAFA_number = 1:nrow(.)) %>%
  pivot_longer(-c(Category, LAFA_number), names_to = 'scenario', values_to = 'weight') %>%
  left_join(lafa_priceperpound_all, by = c('Category' = 'Food')) %>%
  mutate(value = weight * price)

# Determine the LAFA categories to average, to create each BEA category.

# Create list column
string_to_number_vector <- function(x) {
 if (x == 'all') return(1:nrow(lafa_df))
 if (x == 'beverage') return('beverage')
 eval(parse(text = paste0('c(', gsub(';', ',', x, fixed = TRUE), ')')))
}

bea2lafa_long <- bea2lafa %>% 
  mutate(LAFA_codes = map(LAFA_codes, string_to_number_vector)) 

bea2lafa_notbeverage <- bea2lafa_long %>% filter(map_lgl(LAFA_codes, is.numeric))
bea2lafa_beverage <- bea2lafa_long %>% filter(!map_lgl(LAFA_codes, is.numeric))

# Calculate weighted average of production rates for each BEA category other than beverages.
scenario_values_bea_notbeverage <- bea2lafa_notbeverage %>%
  unnest(cols = LAFA_codes) %>%
  left_join(scenario_weights_long, by = c('LAFA_codes' = 'LAFA_number')) %>%
  group_by(BEA_389_code, BEA_389_def, scenario) %>%
  summarize(value = sum(value))
  
# Add production factors for the categories present in BEA but not LAFA (beverages)
# For waste scenarios, use the waste rates we compiled for the Stoten paper.
# For diet scenarios, use the production factors for sugar for soft drinks and flavored drinks. For coffee, tea, and alcohol, assume unchanged with diet.

stoten_waste_rates <- read_csv('~/halvingfoodwaste/data/flw_rates.csv') # Waste rates
diet_lancet_proportion <- read_csv('data/cfs_io_analysis/proportion_diet_lancet.csv') # Diet proportions for planetary health
diet_usa_proportion <- read_csv('data/cfs_io_analysis/proportion_diet_usaguidelines.csv') # Diet proportions for USA guidelines

# Do similar computations as in waste_reduction_simulation.R but for the beverages, and all relative to baseline of 1.
beverage_preconsumer_waste_rates <- as.numeric(stoten_waste_rates[stoten_waste_rates$category == 'beverages', c('loss_processing_packaging', 'loss_distribution')])
beverage_preconsumer_waste <-  1 - prod(1 - beverage_preconsumer_waste_rates) # Appx 9%
beverage_consumer_waste <- as.numeric(stoten_waste_rates[stoten_waste_rates$category == 'beverages', c('loss_consumption')]) # 8%
beverage_allavoidable_waste <- 1 - prod(1 - c(beverage_preconsumer_waste, beverage_consumer_waste)) # 16.5%

beverage_waste_factors <- c(baseline = 1, 
                            preconsumer = (1 - beverage_preconsumer_waste)/(1 - beverage_preconsumer_waste/2),
                            consumer = (1 - beverage_consumer_waste)/(1 - beverage_consumer_waste/2),
                            allavoidable = (1 - beverage_allavoidable_waste)/(1 - beverage_allavoidable_waste/2))

# Diet proportions
beverage_diet_factors <- c(baseline = 1,
                           planetaryhealth = diet_lancet_proportion$planetary_health[diet_lancet_proportion$name == 'added sugars'],
                           usstyle = diet_usa_proportion$us_style[diet_usa_proportion$name == 'calories_other'],
                           medstyle = diet_usa_proportion$med_style[diet_usa_proportion$name == 'calories_other'],
                           vegetarian = diet_usa_proportion$vegetarian[diet_usa_proportion$name == 'calories_other'])

# Product
beverage_waste_x_diet <- outer(beverage_waste_factors, beverage_diet_factors)
# Names
beverage_waste_x_diet_names <- outer(names(beverage_waste_factors), names(beverage_diet_factors), function(w, d) paste('D', d, 'WR', w, sep = '_')) %>%
  as.vector

softdrink_factors <- as.vector(beverage_waste_x_diet)

# Non soft drinks: diet factors are 1
harddrinks_diet_factors <- rep(1, 5)
harddrinks_waste_x_diet <- outer(beverage_waste_factors, harddrinks_diet_factors)
harddrink_factors <- as.vector(harddrinks_waste_x_diet)

# rbind the non-beverage with beverage.
softdrink_codes <- c('311930', '312110')
harddrink_codes <- c('311920', '312120', '312130', '312140')
softdrink_names <- bea2lafa$BEA_389_def[bea2lafa$BEA_389_code %in% softdrink_codes]
harddrink_names <- bea2lafa$BEA_389_def[bea2lafa$BEA_389_code %in% harddrink_codes]

scenario_values_bea_softdrinks <- map2_dfr(softdrink_codes, softdrink_names, ~ data.frame(BEA_389_code = .x,
                                                                                          BEA_389_def = .y,
                                                                                          scenario = beverage_waste_x_diet_names,
                                                                                          value  = softdrink_factors))
scenario_values_bea_harddrinks <- map2_dfr(harddrink_codes, harddrink_names, ~ data.frame(BEA_389_code = .x,
                                                                                          BEA_389_def = .y,
                                                                                          scenario = beverage_waste_x_diet_names,
                                                                                          value  = harddrink_factors))

scenario_values_bea <- bind_rows(scenario_values_bea_notbeverage, scenario_values_bea_softdrinks, scenario_values_bea_harddrinks)

# Reshape to wide then divide everything by the baseline to get the normalized consumption factors.
scenario_values_bea_wide <- scenario_values_bea %>%
  pivot_wider(id_cols = c(BEA_389_code, BEA_389_def), names_from = scenario) %>%
  select(BEA_389_code, BEA_389_def, D_baseline_WR_baseline, everything())

scenario_factors_bea <- scenario_values_bea_wide %>%
  mutate(across(where(is.numeric), ~ ./D_baseline_WR_baseline))

write_csv(scenario_factors_bea, 'data/cfs_io_analysis/bea_consumption_factors_diet_waste_scenarios.csv')
