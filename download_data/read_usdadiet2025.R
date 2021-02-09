# Read 2020-2025 US dietary guidelines to compare them to the 2015-2020 guidelines

# Read and clean USA dietary guidelines -----------------------------------

library(tidyverse)
library(readxl)

fp_diet <- 'data/raw_data/food_consumption/diet_guidelines'

# Dietary guidelines excel sheet, already somewhat cleaned by transposing the raw table and deleting empty columns in Excel
us_diet_file <- file.path(fp_diet, 'dietaryguidelines2020-2025cleaned.xlsx')
us_diet_raw <- map(excel_sheets(us_diet_file), ~ read_xlsx(us_diet_file, sheet = ., .name_repair = 'universal'))

# Function to convert the character columns to numeric by replacing 1/2 with 0.5 and removing non-numeric part of string
to_num <- function(x) {
  x <- gsub('½', '.5', x)
  x <- gsub(' ', '', x)
  x <- str_extract(x, '[0-9.]+')
  as.numeric(x)
}

# The formats of each sheet are the same for this 2020-2025 version I believe
us_diet_cleaned <- map(us_diet_raw, function(sheet) {
  sheet %>%
    mutate_if(is.character, to_num)
})

# Clean names so they are consistent across the three sheets
# Note: in this version, the legumes column is called beans and has two duplicated names for the vegetarian diet (all in cups).
# Also the starchy vegetables one has two similar names.
name_cleaner <- function(x) {
  if(grepl('^Calorie', x, ignore.case = TRUE)) return('calorie_level')
  if(grepl('Whole.grain', x, ignore.case = TRUE)) return('whole_grains')
  if(grepl('Refined.grain', x, ignore.case = TRUE)) return('refined_grains')
  if(grepl('Starchy', x)) return('starchy_vegetables')
  if(grepl('Dairy', x)) return('dairy')
  if(grepl('Protein', x)) return('protein_foods')
  if(grepl('Seafood', x)) return('seafood')
  if(grepl('^Meat', x)) return('meat_poultry_eggs')
  if(grepl('^Nuts', x) & grepl('soy', x)) return('nuts_seeds_soy')
  if(grepl('^Nuts', x) & !grepl('soy', x)) return('nuts_seeds')
  if(grepl('Oils', x)) return('oils')
  if(grepl('^Limit', x) & grepl('kcal', x)) return('calories_other')
  if(grepl('^Limit', x) & !grepl('kcal', x)) return('proportion_other')
  if(grepl('Beans', x)) return('legumes')
  if(grepl('^Eggs', x)) return('eggs')
  if(grepl('^Soy', x)) return('soy')
  gsub('\\.', '_', tolower(x))
}

for (i in 1:3) names(us_diet_cleaned[[i]]) <- sapply(names(us_diet_cleaned[[i]]), name_cleaner)

# for vegetarian diet, specify the 2nd legume is legumes as protein

names(us_diet_cleaned[[2]])[which('legumes' == names(us_diet_cleaned[[2]]))[2]] <- 'legumes_as_protein'

us_diet_final <- map2(us_diet_cleaned, c('us_style', 'vegetarian', 'med_style'), ~ tibble(diet = .y, .x)) %>% bind_rows

# Keep track of all the units that each column is in, plus which columns are aggregations of other columns
# All are per day except that subgroups of veg. and protein are per week.
# fruit, dairy, and oil have no subgroup
# grain subgroups are whole and refined
# protein food subgroups are seafood, meat+eggs, eggs (separated for veg diet), nut/seed/soy, nut/seed (sep. for veg), soy (sep. for veg), legumes, and legumes as protein (sep. for veg)
# vegetable food subgroups are anything with vegetable in the name

# Remove the trailing unit string from some of the names. It is anything with more than one underscore and anything after it
us_diet_final <- us_diet_final %>% rename_with(function(x) gsub('__.*', '', x))

# Remove aggregated groups
agg_groups <- c('vegetables', 'grains', 'protein_foods')
us_diet_final <- us_diet_final %>% select(-all_of(agg_groups))


# Divide the weekly requirements by 7 to get all into daily units.
weekly_groups <- c('dark_green_vegetables', 'red_and_orange_vegetables', 'legumes', 'starchy_vegetables', 'other_vegetables', 'seafood', 'meat_poultry_eggs', 'nuts_seeds', 'soy', 'eggs', 'legumes_as_protein')

us_diet_final <- us_diet_final %>% mutate_at(all_of(weekly_groups), ~ ./7)

# Keep everything in the pattern equivalent for now (cups and ounces)
# Also keep the categories separated for the veg diet.
# Units: veg cup eq, fruit cup eq, grains oz eq, dairy cup eq, protein oz eq, oils grams, other calories.

veg_groups <- c('dark_green_vegetables', 'red_and_orange_vegetables', 'legumes', 'starchy_vegetables', 'other_vegetables')
grain_groups <- c('refined_grains', 'whole_grains')
protein_groups <- c('seafood', 'meat_poultry_eggs', 'nuts_seeds', 'soy', 'eggs', 'legumes_as_protein')

us_diet_final_long <- us_diet_final %>%
  pivot_longer(-c(diet, calorie_level)) %>%
  mutate(food_group = case_when(name %in% veg_groups ~ 'vegetables',
                                name %in% grain_groups ~ 'grains',
                                name %in% protein_groups ~ 'protein',
                                name %in% c('dairy', 'fruits', 'oils') ~ name,
                                name %in% c('calories_other', 'proportion_other') ~ 'other'),
         unit = case_when(food_group %in% c('vegetables', 'fruits', 'dairy') ~ 'c-eq',
                          food_group %in% c('grains', 'protein') ~ 'oz-eq',
                          food_group %in% 'oils' ~ 'g',
                          name == 'calories_other' ~ 'cal'))

# Write output
write_csv(us_diet_final, file.path(fp_diet, 'us_dietary_guidelines2020-2025_wide.csv'))
write_csv(us_diet_final_long, file.path(fp_diet, 'us_dietary_guidelines2020-2025_long.csv'))
