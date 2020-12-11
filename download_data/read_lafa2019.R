# New LAFA processing code
# For the LAFA data released in late 2019
# QDR / 09 Dec 2020 / Virtualland

# We now want the calories and servings data too. Also make sure all aggregated categories are removed from the outset.

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

fp_lafa <- file.path(fp, 'raw_data/USDA/LAFA')

dairy <- read_lafa_workbook(file.path(fp_lafa, 'Dairy.xls'))
fat <- read_lafa_workbook(file.path(fp_lafa, 'fat.xls'))
fruit <- read_lafa_workbook(file.path(fp_lafa, 'Fruit.xls'))
grain <- read_lafa_workbook(file.path(fp_lafa, 'grain.xls'))
meat <- read_lafa_workbook(file.path(fp_lafa, 'meat.xls'))
sugar <- read_lafa_workbook(file.path(fp_lafa, 'sugar.xls'))
veg <- read_lafa_workbook(file.path(fp_lafa, 'veg.xls'))


# Read special datasets (calories and servings) ---------------------------

# calories.xls and servings.xls have the same structure but are idiosyncratically formatted.
library(tidyxl)
library(unpivotr)
library(zoo)

calories_raw <- xlsx_cells(file.path(fp_lafa, 'calories.xlsx')) %>% filter(!sheet %in% 'TableofContents')
servings_raw <- xlsx_cells(file.path(fp_lafa, 'servings.xlsx')) %>% filter(!sheet %in% 'TableofContents')

# In all sheets, top row is title of sheet (can be removed)
# Then there are some header rows, a unit row, then the data.

calories_raw <- filter(calories_raw, row != 1)
servings_raw <- filter(servings_raw, row != 1)

# Totals sheet has only one header row but it's merged from several rows, and one unit row.
# The most recent year with complete data is 2010.
calories_total <- calories_raw %>%
  filter(sheet == 'Totals') %>%
  behead(direction = 'W', name = 'year') %>%
  behead(direction = 'N', name = 'food_group') %>%
  select(year, food_group, numeric) %>%
  filter(!is.na(numeric))

calories_percent <- calories_raw %>%
  filter(sheet == 'Percents') %>%
  behead(direction = 'W', name = 'year') %>%
  behead(direction = 'N', name = 'food_group') %>%
  select(year, food_group, numeric) %>%
  filter(!is.na(numeric))

# Servings total is similar but has units
servings_total <- servings_raw %>%
  filter(sheet == 'Totals', !is_blank) %>%
  behead(direction = 'W', name = 'year') %>%
  behead(direction = 'N', name = 'food_group') %>%
  behead(direction = 'N', name = 'unit') %>%
  mutate(unit = gsub('-', '', unit) %>% trimws %>% na.locf) %>%
  select(year, food_group, unit, numeric) 
  
# The different food groups have multiple header rows with both horizontal and vertical merges.
# Servings sheet has a unit row, calories sheet doesn't.
clean_sheet <- function(x, has_unit_row = TRUE) {
  
  x <- x %>%
    filter(!is_blank) %>%
    mutate(character = na.locf(character))
  
  # Find number of header rows as maximum number of character entries in each column before unit row index.
  # Unit row index is one before the first non numeric entry in first column.
  first_data_row_idx <- x$row[x$col == 1 & !is.na(x$numeric)][1]
  
  n_header_rows <- x %>%
    filter(row < first_data_row_idx) %>%
    group_by(col) %>%
    summarize(nheader = length(unique(character))) %>%
    pull(nheader) %>%
    max
  if (has_unit_row) n_header_rows <- n_header_rows - 1
  
  x <- x %>%
    behead(direction = 'W', name = 'year')
  
  for (i in 1:n_header_rows) {
    x <- x %>%
      behead(direction = 'N', name = !!paste('group', i, sep = '_'))
  }
  
  if (has_unit_row) {
    x %>%
      behead(direction = 'N', name = 'unit') %>%
      mutate(unit = gsub('-', '', unit) %>% trimws %>% na.locf(na.rm = FALSE)) %>%
      select(year, starts_with('group'), unit, numeric) 
  } else {
    x %>%
      select(year, starts_with('group'), numeric) 
  }
  
}

calories_bygroup <- calories_raw %>%
  filter(!sheet %in% c('Totals','Percents')) %>%
  group_by(sheet) %>%
  group_modify(~ clean_sheet(., has_unit_row = FALSE))

servings_bygroup <- servings_raw %>%
  filter(!sheet %in% c('Totals','Percents')) %>%
  group_by(sheet) %>%
  group_modify(~ clean_sheet(., has_unit_row = TRUE))
