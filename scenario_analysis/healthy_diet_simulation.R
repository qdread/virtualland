# Diet shift simulation version 2
# QDR / Virtualland / 07 Dec 2020

# The original way of simulating this was naive, just getting rid of 50% of meat and replacing with evenly divided veg food
# Instead of doing this, and instead of manually doing the linear programming, 
# let's just get the lancet planetary health diet and USA healthy diet guidelines diets.
# See Lancet report (summary), Dietary Guidelines report, and Blackstone et al. which gave me the idea of the approach.


# Read dietary guidelines -----------------------------------

library(tidyverse)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_diet <- file.path(fp, 'raw_data/food_consumption/diet_guidelines')
fp_crosswalk <- file.path(fp, 'crossreference_tables')
fp_out <- file.path(fp, 'cfs_io_analysis')

# Read Lancet dietary guidelines
diet_lancet <- read_csv(file.path(fp_diet, 'lancet_planetary_health_diet.csv'))
# Read USA dietary guidelines
diet_usa <- read_csv(file.path(fp_diet, 'us_dietary_guidelines_long.csv'))

# Harmonization of LAFA category names with dietary guidelines food pattern equivalent, and Lancet food categories
# Created from lafa_category_structure.csv
# Note that LAFA does not distinguish between whole and refined grains. So it is listed as just grains for both lancet and usa.
# Also note that we are not going to distinguish between saturated and unsaturated oil/fat because LAFA is by food category not fat type.
lafa_cat_lookup <- read_csv(file.path(fp_crosswalk, 'lafa_dietary_guidelines_crosswalk.csv'))

# Read LAFA
source('~/virtualland/download_data/read_lafa2019.R')
# Remove eggnog and half and half from fats (they are already included in dairy)
fat <- filter(fat, !Category %in% c('Eggnog', 'Half and half'))

# Names in meat have a duplicated column where one is ounceeq and one is ounceseq
# Make sure there is no overlap
# with(meat, table(is.na(Food_pattern_equivalents_Ounceeq), is.na(Food_pattern_equivalents_Ounceseq))) # No overlap

meat <- meat %>%
  mutate(Food_pattern_equivalents_Ounceeq = coalesce(Food_pattern_equivalents_Ounceeq, Food_pattern_equivalents_Ounceseq)) %>%
  select(-Food_pattern_equivalents_Ounceseq)

# Names in fruit have a duplicated column where one has two underscores
# Make sure there is no overlap
# with(fruit, table(is.na(Primary_weight_Lbs.year), is.na(Primary_weight__Lbs.year))) # No overlap of non-missing items.

fruit <- fruit %>%
  mutate(Primary_weight_Lbs.year = coalesce(Primary_weight_Lbs.year, Primary_weight__Lbs.year)) %>%
  select(-Primary_weight__Lbs.year)

# For each LAFA list element, correct the names so that they will join properly.
# Separate the units as well.
# Units: 
# dairy: lbs.yr, gals.yr, oz.day, g.day, gals.day, food pattern in cups
# Fat: lbs.yr, oz.day, g.day, daily fat grams, in grams and in number (the same?), no food pattern provided
# fruit: lbs. year, oz.day, g.day, there is also a column for gain from primary to retail weight!, edible weight in lbs, and per capita avail in lbs and gals for some items like juice
# grain: lbs.year, oz.day, g.day, food pattern in oz, edible weight in lbs
# meat: FPE in ounces
# sugar: No FPE
# veg: FPE in cups

dairy <- dairy %>% mutate(fpe_units = 'cup-eq')
fruit <- fruit %>% mutate(fpe_units = 'cup-eq')
grain <- grain %>% mutate(fpe_units = 'oz-eq')
meat <- meat %>% mutate(fpe_units = 'oz-eq')
veg <- veg %>% mutate(fpe_units = 'cup-eq')


lafa <- list(dairy, fat, fruit, grain, meat, sugar, veg)
# Note we also have calories, servings, calories_total, servings_total, and calories_percent loaded.

# Weights are always in lb/year. 
# Losses are expressed as percents and weights. Loss primary to retail, retail to consumer, consumer nonedible, consumer other: percent.
# Loss consumer edible weight as weight in lbs.
# We want per capita availability in lb/year as well, for consistency across weights. Can be converted to other units later.
# Per capita availability may be in different units, as well as food pattern equivalents.
clean_lafa <- function(dat) {
  
  # Remove duplicated per capita avail columns
  dat <- select(dat, !(starts_with('Per_capita_availability') & !contains('Lbs.year')))
  
  ns <- names(dat)
  ns[grepl('^Primary_weight', ns)] <- 'primary_weight_lb_y'
  ns[grepl('^Retail_weight', ns)] <- 'retail_weight_lb_y'
  ns[grepl('^Consumer_weight', ns)] <- 'consumer_weight_lb_y'
  ns[grepl('^Loss_from_primary', ns)] <- 'loss_primary_to_retail_percent'
  ns[grepl('^Loss_from_retail', ns)] <- 'loss_retail_to_consumer_percent'
  ns[grepl('^Loss_at_consumer', ns) & grepl('Nonedible', ns)] <- 'loss_consumer_nonedible_percent'
  ns[grepl('^Loss_at_consumer', ns) & grepl('Edible', ns)] <- 'loss_consumer_edible_lb_y'
  ns[grepl('^Loss_at_consumer', ns) & grepl('Other', ns)] <- 'loss_consumer_other_percent'
  ns[grepl('^Total_loss', ns)] <- 'loss_total_percent'
  ns[grepl('^Per_capita_availability', ns)] <- 'per_capita_availability_lb_y'
  ns[grepl('^Edible_weight', ns)] <- 'edible_weight_lb_y'
  ns[grepl('^Food_pattern', ns)] <- 'food_pattern_equivalents'
  ns[grepl('^Calories_available', ns)] <- 'calories_available_cal_day'
    setNames(dat, ns)
}
lafa_clean_names <- map(lafa, clean_lafa)

# Process LAFA to single year and no aggregates ---------------------------

# We only want the most recent year with complete data, and only the primary (not aggregated) categories
# Note that the aggregated groups do not have any individual loss rates for stages, only "total"

lafa_agg_groups <- lafa_cat_lookup %>% select(starts_with('subgroup')) %>% unlist %>% unique

lafa_df <- bind_rows(lafa_clean_names) %>%
  filter(!Category %in% lafa_agg_groups) %>%
  filter(!is.na('primary_weight_lb_y')) %>%
  group_by(Category) %>%
  filter(Year == max(Year))

# Legumes should be removed (misclassified as a primary food when it is in fact a category). So in total there are 214 categories.
lafa_df <- lafa_df %>% filter(!Category %in% 'Legumes')


# Join lafa data with lookups ---------------------------------------------

lafa_cat_lookup_tojoin <- lafa_cat_lookup %>% 
  filter(!(Group %in% 'fat' & Food %in% c('Half and half', 'Eggnog'))) %>%
  select(Group, Food, category_lancet, category_dietary_guidelines) %>%
  rename(Category = Food)

lafa_df <- lafa_df %>%
  left_join(lafa_cat_lookup_tojoin)

# Harmonize with USA guidelines -------------------------------------------

# # We will use the 2010 servings. (last year with full data)
# servings2010 <- servings %>% filter(year == 2010) %>% rename(servings = numeric)
# calories2010 <- calories %>% filter(year == 2010) %>% rename(calories = numeric)
# 
# # Do manual harmonization on the dataset (not that many rows to do)
# # It's basically the same names as the LAFA dataset but with annoyingly different names so hard to automatically join
# # This might not be necessary because of the fact that the calories and servings are found in the individual LAFA datasets.
# write_csv(servings2010, '/nfs/qread-data/cfs_io_analysis/lafa_output/servings2010.csv')
# write_csv(calories2010, '/nfs/qread-data/cfs_io_analysis/lafa_output/calories2010.csv')

# Put the three diets into wide form so they can all be joined.
diet_usa_wide <- diet_usa %>%
  filter(calorie_level == 2000) %>%
  select(name, food_group, unit, diet, value) %>%
  replace_na(list(value = 0)) %>%
  pivot_wider(id_cols = c(name, food_group, unit), names_from = diet, values_from = value) %>%
  filter(!name %in% 'proportion_other')

