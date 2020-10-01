# Reverse-engineer the calories per weight for all LAFA items (originally derived from NDB)
# QDR / 


fp_rawdata <- '/nfs/qread-data/temp/data_for_fwi'
source('~/foodwasteinterventions/0_preprocessing/read_lafa.R')

lafa <- list(dairy, fat, fruit, grain, meat, sugar, veg)

map(lafa, names) # number 2, fat, has calories per gram already but everything else has calories per x and grams per x
# Also number 5, meat, has calories per ounce equivalent.

lafa_calories_weight <- map_dfr(list(dairy, fruit, grain, sugar, veg), function(x) {
  x %>%
    select(Category, Year, Calories_available_daily_Number, matches('(Calories_per).*(Number)'), matches('(Grams_per).*(Grams)')) %>%
    setNames(c('category', 'year', 'calories_available', 'calories_per_unit', 'grams_per_unit')) %>%
    filter(complete.cases(.)) %>%
    group_by(category) %>%
    filter(year == max(year)) %>%
    mutate(calories_per_gram = calories_per_unit/grams_per_unit)
})

fat_cal_weight <- fat %>%
  select(Category, Year, Calories_available_daily_Number, Calories_per_fat_gram_Number) %>%
  set_names('category', 'year', 'calories_available', 'calories_per_gram') %>%
  filter(complete.cases(.)) %>%
  group_by(category) %>%
  filter(year == max(year))

meat_cal_weight <- meat %>%
  mutate(calories_per_gram = Calories_available_daily_Number/Per_capita_availability_adjusted_for_loss_G.day) %>%
  select(Category, Year, Calories_available_daily_Number, calories_per_gram) %>%
  rename(calories_available = Calories_available_daily_Number) %>%
  rename_with(tolower) %>%
  filter(complete.cases(.)) %>%
  group_by(category) %>%
  filter(year == max(year))

lafa_calories_weight <- bind_rows(lafa_calories_weight, fat_cal_weight, meat_cal_weight)

# Create a category for animal product
lafa_calories_weight <- lafa_calories_weight %>%
  mutate(animal_product = case_when(
    category %in% dairy$Category ~ 'dairy',
    category %in% c('Butter', 'Half and half', 'Light cream', 'Heavy cream', 'Sour cream', 'Cream cheese', 'Eggnog') ~ 'dairy',
    category %in% c("Beef", "Veal", "Pork", "Lamb", "Red meat", "Chicken", "Turkey", 
                    "Poultry", 'Edible beef tallow') ~ 'meat', 
    category %in% c("Fresh and frozen fish", "Fresh and frozen shellfish", 
                    "Total Fresh and Frozen Fish", "Canned Salmon", "Canned Sardines", 
                    "Canned Tuna", "Canned shellfish", "Other canned fish", "Canned fish and shellfish", 
                    "Cured fish", "Total fish and shellfish") ~ 'fish',
    category %in% 'Eggs' ~ 'eggs',
    TRUE ~ 'no'
  ))

write_csv(lafa_calories_weight, '/nfs/qread-data/cfs_io_analysis/lafa_calories_weight.csv')
