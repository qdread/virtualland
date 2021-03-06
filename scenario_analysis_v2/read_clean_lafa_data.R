# New LAFA processing code
# For the LAFA data released in late 2019
# QDR / 09 Dec 2020 / Virtualland

# Define functions --------------------------------------------------------

# Function to read single sheet
read_lafa_sheet <- function(name, file) {
  sheet_raw <- suppressMessages(read_xls(file, sheet = name))
  # Find the index of the first and last row that contains a year number in the first column.
  numeric_col1 <- as.numeric(str_extract_all(pull(sheet_raw, 1), '^[0-9]{4}', simplify = TRUE))
  year_idx <- c(which.min(numeric_col1), which.max(numeric_col1))
  # Read the sheet again with only those rows.
  sheet_dat <- suppressMessages(
    read_xls(file, 
             sheet = name, 
             skip = year_idx[1],
             n_max = diff(year_idx) + 1,
             col_names = FALSE,
             col_types = 'numeric'))
  
  # Get the row with units in it.
  unit_row <- suppressMessages(read_xls(file, sheet = name, skip = year_idx[1] - 1, n_max = 1, col_names = FALSE))
  # Parse it
  unit_row_parsed <- trimws(gsub('-', '', as.character(unit_row)))
  unit_row_parsed <- gsub('/', '.', unit_row_parsed)
  # Get rid of any characters that are not alphanumeric at the end of the line.
  unit_row_parsed <- gsub('[^a-zA-Z0-9]*$', '', unit_row_parsed)
  
  # Read in header rows (row1 always has a title)
  header_rows <- suppressMessages(read_xls(file, sheet = name, skip = 1, n_max = 2, col_names = FALSE))
  # Fill first header row forward if there is a NA
  header_row1 <- na.locf(unlist(header_rows[1,]))
  # Paste second header row onto first, if it exists
  header_row2 <- unlist(header_rows[2,])
  header_row_parsed <- if_else(is.na(header_row2), header_row1, paste(header_row1, header_row2, sep = '_'))
  header_row_parsed <- gsub('[0-9]', '', header_row_parsed)
  header_row_parsed <- gsub('[^a-zA-Z\\s]', '_', header_row_parsed)
  
  
  # Get rid of excess header rows for cells present by mistake
  header_row_parsed <- header_row_parsed[2:(length(unit_row_parsed) + 1)]
  
  setNames(sheet_dat, c('Year', paste(header_row_parsed, unit_row_parsed, sep = '_')))
}

# Function to read entire workbook
read_lafa_workbook <- function(file) {
  require(tidyverse)
  require(readxl)
  require(zoo)
  
  # Get sheet names and remove table of contents
  sheet_names <- excel_sheets(file)
  sheet_names <- sheet_names[!sheet_names %in% c('TableOfContents')]
  
  all_sheets <- map_dfr(sheet_names, ~ data.frame(Category = ., read_lafa_sheet(., file = file), stringsAsFactors = FALSE))
  
  return(all_sheets)
}


# Read data ---------------------------------------------------------------

fp_lafa <- 'data/raw_data/USDA/LAFA'
fp_crosswalk <- 'data/crossreference_tables'

dairy <- read_lafa_workbook(file.path(fp_lafa, 'Dairy.xls'))
fat <- read_lafa_workbook(file.path(fp_lafa, 'fat.xls'))
fruit <- read_lafa_workbook(file.path(fp_lafa, 'Fruit.xls'))
grain <- read_lafa_workbook(file.path(fp_lafa, 'grain.xls'))
meat <- read_lafa_workbook(file.path(fp_lafa, 'meat.xls'))
sugar <- read_lafa_workbook(file.path(fp_lafa, 'sugar.xls'))
veg <- read_lafa_workbook(file.path(fp_lafa, 'veg.xls'))

lafa_cat_lookup <- read_csv(file.path(fp_crosswalk, 'lafa_dietary_guidelines_crosswalk.csv'))

# Additional processing ---------------------------------------------------

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

# We want the year closest to 2012 with complete data (this change made 20 Apr 2021), and only the primary (not aggregated) categories
# Note that the aggregated groups do not have any individual loss rates for stages, only "total"

lafa_agg_groups <- lafa_cat_lookup %>% select(starts_with('subgroup')) %>% unlist %>% unique

lafa_df <- bind_rows(lafa_clean_names) %>%
  filter(!Category %in% lafa_agg_groups) %>%
  filter(!is.na(primary_weight_lb_y)) %>%
  mutate(yeardiff = abs(Year - 2012)) %>%
  group_by(Category) %>%
  filter(yeardiff == min(yeardiff)) %>%
  ungroup %>%
  select(-yeardiff)

# Legumes should be removed (misclassified as a primary food when it is in fact a category). 
# Also, there is a double counted row, one is "White and whole wheat flour" and the other "Wheat flour" but they contain the same values
lafa_df <- lafa_df %>% filter(!Category %in% c('Legumes', 'Wheat flour'))

write_csv(lafa_df, 'data/cfs_io_analysis/lafa_cleaned.csv')
