# Diet shift simulation version 2
# QDR / Virtualland / 07 Dec 2020

# The original way of simulating this was naive, just getting rid of 50% of meat and replacing with evenly divided veg food
# Instead of doing this, and instead of manually doing the linear programming, 
# let's just get the lancet planetary health diet and USA healthy diet guidelines diets.
# See Lancet report (summary), Dietary Guidelines report, and Blackstone et al. which gave me the idea of the approach.


# Read and clean USA dietary guidelines -----------------------------------

library(tidyverse)
library(readxl)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_diet <- file.path(fp, 'raw_data/food_consumption/diet_guidelines')

# Dietary guidelines excel sheet, already somewhat cleaned by transposing the raw table and deleting empty columns in Excel
us_diet_file <- file.path(fp_diet, 'dietaryguidelinesfoodpatterns_cleaned.xlsx')
us_diet_raw <- map(excel_sheets(us_diet_file), ~ read_xlsx(us_diet_file, sheet = ., .name_repair = 'universal'))

# Function to convert the character columns to numeric by replacing 1/2 with 0.5 and removing non-numeric part of string
to_num <- function(x) {
  x <- gsub('½', '.5', x)
  x <- str_extract(x, '[0-9.]+')
  as.numeric(x)
}

# The formats of each sheet are the same except for the third (vegetarian) one.
# Correct it so it's the same as the others, then apply the same conversion to all.

us_diet_raw[[3]] <- us_diet_raw[[3]] %>%
  separate(Limit.on.Calories.for.Other.Uses..calories....of.calories.f.g, 
           into = c('calories_other', 'proportion_other'), sep = ' ') %>%
  mutate(calories_other = as.numeric(calories_other),
         proportion_other = as.numeric(str_extract(proportion_other, '[0-9.]+')) * -0.01)

us_diet_cleaned <- map(us_diet_raw, function(sheet) {
  sheet %>%
    mutate_if(is.character, to_num)
})

# Clean names so they are consistent across the three sheets
name_cleaner <- function(x) {
  if(grepl('^Calorie', x)) return('calorie_level')
  if(grepl('Whole.grain', x)) return('whole_grains')
  if(grepl('Refined.grain', x)) return('refined_grains')
  if(grepl('Dairy', x)) return('dairy')
  if(grepl('Protein', x)) return('protein_foods')
  if(grepl('Seafood', x)) return('seafood')
  if(grepl('^Meat', x)) return('meat_poultry_eggs')
  if(grepl('^Nuts', x) & grepl('soy', x)) return('nuts_seeds_soy')
  if(grepl('^Nuts', x) & !grepl('soy', x)) return('nuts_seeds')
  if(grepl('Oils', x)) return('oils')
  if(grepl('^Limit', x)) return('calories_other')
  if(grepl('proportion', x)) return('proportion_other')
  if(grepl('Legumes', x) & grepl('oz', x)) return('legumes_as_protein')
  if(grepl('Legumes', x)) return('legumes')
  if(grepl('^Eggs', x)) return('eggs')
  if(grepl('^Soy', x)) return('soy')
  gsub('\\.', '_', tolower(x))
}

for (i in 1:3) names(us_diet_cleaned[[i]]) <- sapply(names(us_diet_cleaned[[i]]), name_cleaner)

us_diet_final <- map2(us_diet_cleaned, c('us_style', 'med_style', 'vegetarian'), ~ tibble(diet = .y, .x)) %>% bind_rows
  
                     