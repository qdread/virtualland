# Diet shift simulation version 2
# QDR / Virtualland / 07 Dec 2020

# The original way of simulating this was naive, just getting rid of 50% of meat and replacing with evenly divided veg food
# Instead of doing this, and instead of manually doing the linear programming, 
# let's just get the lancet planetary health diet and USA healthy diet guidelines diets.
# See Lancet report (summary), Dietary Guidelines report, and Blackstone et al. which gave me the idea of the approach.
# Modified 20 April 2021 to use 2020-2025 dietary guidelines

# Read dietary guidelines -----------------------------------

library(tidyverse)

fp_diet <- 'data/raw_data/food_consumption/diet_guidelines'
fp_crosswalk <- 'data/crossreference_tables'
fp_out <- 'data/cfs_io_analysis'

# Read Lancet dietary guidelines
diet_lancet <- read_csv(file.path(fp_diet, 'lancet_planetary_health_diet.csv'))
# Read USA dietary guidelines
diet_usa <- read_csv(file.path(fp_diet, 'us_dietary_guidelines2020-2025_long.csv'))

# Harmonization of LAFA category names with dietary guidelines food pattern equivalent, and Lancet food categories
# Created from lafa_category_structure.csv
# Note that LAFA does not distinguish between whole and refined grains. So it is listed as just grains for both lancet and usa.
# Also note that we are not going to distinguish between saturated and unsaturated oil/fat because LAFA is by food category not fat type.
lafa_cat_lookup <- read_csv(file.path(fp_crosswalk, 'lafa_dietary_guidelines_crosswalk.csv'))

# Read cleaned LAFA data
lafa_df <- read_csv(file.path(fp_out, 'lafa_cleaned.csv'))

# Join lafa data with lookups ---------------------------------------------

lafa_cat_lookup_tojoin <- lafa_cat_lookup %>% 
  filter(!(Group %in% 'fat' & Food %in% c('Half and half', 'Eggnog'))) %>%
  select(Group, Food, category_lancet, category_dietary_guidelines) %>%
  rename(Category = Food)

lafa_df <- lafa_df %>%
  left_join(lafa_cat_lookup_tojoin)

# Manually add the four categories that aren't in the lookup table.
#lafa_df %>% filter(is.na(category_lancet) | is.na(category_dietary_guidelines)) %>% pull(Category) %>% dput
cats_add <- data.frame(Category = c("White and whole wheat flour", "Durum flour", "Fresh brussels sprouts", "Other processed vegetables"),
                       category_lancet = c('whole grains', 'whole grains', 'vegetables', 'vegetables'),
                       category_dietary_guidelines = c('grains', 'grains', 'dark_green_vegetables', 'other_vegetables'))
                       
lafa_df[is.na(lafa_df$category_lancet),c('category_lancet','category_dietary_guidelines')] <- cats_add[,c('category_lancet','category_dietary_guidelines')]                    

# Harmonize with USA guidelines -------------------------------------------

# Put the three diets into wide form so they can all be joined.
# Use 2600 cal which is the closest to the LAFA sum of calories available.
diet_usa_wide <- diet_usa %>%
  filter(calorie_level == 2600) %>%
  select(name, food_group, unit, diet, value) %>%
  replace_na(list(value = 0)) %>%
  pivot_wider(id_cols = c(name, food_group, unit), names_from = diet, values_from = value) %>%
  filter(!name %in% 'proportion_other')

# "Coarsen" and "refine" the diet usa wide data where appropriate:
# whole and refined grains are not distinguished.
# The "other" category is equivalent to calories_other and is all sugar/sweeteners.
# Add legumes as protein to legumes for vegetarians.
# In 2020-2025 dietary guidelines, soy is 0 for non-vegetarian diets, but separate for veg. Add it to legumes for the vegetarian diet.
# Finally, eggs are separated for vegetarians. Use LAFA proportion of eggs versus other foods to split out eggs for non-vegetarian diets.

diet_usa_wide_fixed <- diet_usa_wide

# Aggregate the two groups (note that legumes as protein is in oz-eq.) 
# The document on page 107 states divide the protein entry by 4 before adding to the vegetable entry.

diet_usa_wide_fixed$vegetarian[diet_usa_wide_fixed$name == 'legumes_as_protein'] <- 
  diet_usa_wide_fixed$vegetarian[diet_usa_wide_fixed$name == 'legumes_as_protein'] / 4

diet_usa_wide_fixed <- diet_usa_wide_fixed %>%
  mutate(name = case_when(name == 'legumes_as_protein' ~ 'legumes',
                          name %in% c('whole_grains','refined_grains') ~ 'grains',
                          TRUE ~ name),
         food_group = if_else(name == 'legumes', 'vegetables', food_group),
         unit = if_else(name == 'legumes', 'c-eq', unit)) %>%
  group_by_if(is.character) %>%
  summarize_if(is.numeric, sum) %>%
  ungroup

# Add soy to legumes for vegetarian diet
diet_usa_wide_fixed[diet_usa_wide_fixed$name %in% 'legumes', c('us_style', 'med_style', 'vegetarian')] <- diet_usa_wide_fixed[diet_usa_wide_fixed$name %in% 'legumes', c('us_style', 'med_style', 'vegetarian')] + diet_usa_wide_fixed[diet_usa_wide_fixed$name %in% 'soy', c('us_style', 'med_style', 'vegetarian')]

# Remove soy
diet_usa_wide_fixed <- diet_usa_wide_fixed %>%
  filter(!name %in% c('soy'))

# Split eggs from meat, poultry, and eggs for the us style and Mediterranean diets.
# Use the same relative proportion of meat vs. eggs from the baseline diet. (by weight)
proportion_eggs <- lafa_df %>% 
  filter(category_dietary_guidelines %in% c('meat_poultry_eggs', 'eggs')) %>% 
  group_by(category_dietary_guidelines) %>%
  summarize(weight = sum(per_capita_availability_lb_y)) %>%
  mutate(weight = weight/sum(weight))
# About 14% by weight is eggs.

diet_usa_wide_fixed[diet_usa_wide_fixed$name %in% c('eggs', 'meat_poultry_eggs'), c('us_style', 'med_style')] <- 
  proportion_eggs$weight %*% t(as.numeric(diet_usa_wide_fixed[diet_usa_wide_fixed$name %in% c('meat_poultry_eggs'), c('us_style', 'med_style')]))

# Sum lafa data food patterns by USA diet categories. 
# For those that aren't food pattern equivalents (sugars and fats), sum by calories (sugar) and grams (oils/fats)
# For oils and fats, we need to reverse engineer the lb/y to g/day.
lafa_sum_usa_diet <- lafa_df %>%
  group_by(category_dietary_guidelines, fpe_units) %>%
  summarize(food_pattern_equivalents = sum(food_pattern_equivalents), calories_available_cal_day = sum(calories_available_cal_day), per_capita_availability_lb_y = sum(per_capita_availability_lb_y)) %>%
  mutate(per_capita_availability_g_day = per_capita_availability_lb_y %>% units::set_units(lb/yr) %>% units::set_units(g/day) %>% as.numeric)

# Join summed lafa usa diet with the three healthy diets.
lafa_sum_usa_diet_tojoin <- lafa_sum_usa_diet %>%
  filter(!(category_dietary_guidelines %in% 'oils' & fpe_units %in% 'oz-eq')) %>% # Double counted
  mutate(baseline = case_when(category_dietary_guidelines == 'oils' ~ per_capita_availability_g_day,
                              category_dietary_guidelines == 'other' ~ calories_available_cal_day,
                              TRUE ~ food_pattern_equivalents)) %>%
  rename(name = category_dietary_guidelines) %>%
  select(name, baseline) %>%
  mutate(name = case_when(name == 'other' ~ 'calories_other',
                          TRUE ~ name))

diet_usa_joined <- left_join(diet_usa_wide_fixed, lafa_sum_usa_diet_tojoin) %>%
  mutate(name = if_else(name == 'meat_poultry_eggs', 'meat_poultry', name))

# We can't scale the guideline diets vs baseline because they're in all different units. But it is almost exactly the same.

# Make a plot.
diet_usa_joined_long <- diet_usa_joined %>% pivot_longer(c(us_style,med_style,vegetarian,baseline), names_to = 'diet', values_to = 'value')

ggplot(diet_usa_joined_long, aes(y = value, x = name, group = diet, fill = diet)) +
  facet_wrap(~ unit, scales = 'free') +
  geom_col(position = 'dodge') +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
  scale_fill_brewer(palette = 'Dark2') +
  theme_bw() +
  ggtitle('Comparison of baseline USA diet with the three guideline diets')

# Harmonize with Lancet diet ----------------------------------------------

# Everything needs to be linked by calories in this case.
lafa_sum_lancet_diet <- lafa_df %>%
  mutate(category_lancet = if_else(category_lancet == 'whole grains', 'grains', category_lancet)) %>%
  group_by(category_lancet) %>%
  summarize(calories_available_cal_day = sum(calories_available_cal_day)) %>%
  rename(name = category_lancet)

# Must sum unsaturated and saturated oils, and change the name whole grains to grains
diet_lancet_tojoin <- diet_lancet %>%
  select(food_group, calories_g_per_day) %>%
  rename(name = food_group, planetary_health = calories_g_per_day)

diet_lancet_tojoin$planetary_health[diet_lancet_tojoin$name == 'unsaturated oils'] <- sum(diet_lancet_tojoin$planetary_health[diet_lancet_tojoin$name %in% c('unsaturated oils', 'saturated oils')])

diet_lancet_tojoin <- diet_lancet_tojoin %>%
  mutate(name = case_when(name == 'whole grains' ~ 'grains',
                          name == 'unsaturated oils' ~ 'oils',
                          TRUE ~ name)) %>%
  filter(!name %in% 'saturated oils')

diet_lancet_joined <- left_join(diet_lancet_tojoin, lafa_sum_lancet_diet) %>%
  rename(baseline = calories_available_cal_day)

# Make a plot
diet_lancet_joined_long <- diet_lancet_joined %>%
  pivot_longer(cols = c(planetary_health, baseline), names_to = 'diet', values_to = 'calories')
  
ggplot(diet_lancet_joined_long, aes(y = calories, x = name, group = diet, fill = diet)) +
  geom_col(position = 'dodge') +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
  scale_fill_brewer(palette = 'Dark2') +
  theme_bw() +
  ggtitle('Comparison of baseline USA diet with the Lancet planetary health diet')



# Join all with lafa ------------------------------------------------------
# We need the calories per day for each of the five diets: the lafa baseline, the lancet diet, and the 3 dietary guidelines diets

# For the Lancet diet and the planetary health diets, get the ratio

diet_lancet_proportion <- diet_lancet_joined %>%
  mutate(planetary_health = planetary_health / baseline) %>% 
  select(-baseline)

diet_usa_proportion <- diet_usa_joined %>%
  mutate_at(vars(us_style, med_style, vegetarian), ~ ./baseline) %>%
  select(-baseline, -food_group) %>%
  rename(unit_convert_usa_diet = unit)

# Write these proportions out to CSV
write_csv(diet_lancet_proportion, 'data/cfs_io_analysis/proportion_diet_lancet.csv')
write_csv(diet_usa_proportion, 'data/cfs_io_analysis/proportion_diet_usaguidelines.csv')


# Correct the category names again so that they match between the lafa data frame and the diet to join data frames.
setdiff(y=lafa_df$category_dietary_guidelines, x=diet_usa_proportion$name)
setdiff(y=lafa_df$category_lancet, x=diet_lancet_proportion$name)
lafa_df_fixnames <- lafa_df %>%
  mutate(
    category_dietary_guidelines = case_when(
      category_dietary_guidelines == 'other' ~ 'calories_other',
      category_dietary_guidelines == 'meat_poultry_eggs' ~ 'meat_poultry',
      TRUE ~ category_dietary_guidelines
    ),
    category_lancet = if_else(category_lancet == 'whole grains', 'grains', category_lancet)
  )

# Join up the LAFA DF with the category ratios for each diet type.
lafa_df_joindiets <- lafa_df_fixnames %>%
  left_join(diet_lancet_proportion, by = c('category_lancet' = 'name')) %>%
  left_join(diet_usa_proportion %>% select(-unit_convert_usa_diet), by = c('category_dietary_guidelines' = 'name'))

write_csv(lafa_df_joindiets, file.path(fp_out, 'lafa_joined_with_diet_proportions.csv'))
