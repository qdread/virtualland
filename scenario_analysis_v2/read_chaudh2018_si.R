# Script to read in all data from supplemental information in Chaudhary and Brooks 2018, Env Sci Tech
# (global land use characterization factors for species loss, updated from 2015 paper)

library(tidyxl)
library(tidyverse)
library(zoo)
library(unpivotr)

sheet_data <- xlsx_cells('data/raw_data/biodiversity/chaudhary2015SI/chaudhary_brooks_2018_si.xlsx')

# Which sheets to use: S2, ecoregion summary variables and lookup table, S3 and S4, global occupation and transformation CFs by region
# S5 and S6, global occ and trans CF by country.
# Row 1 is title of each table. The rows below that are headers but with some merged rows/columns.
# Different ecoregion 
ecoregion_data <- sheet_data %>%
  filter(grepl('S2', sheet), row > 1, col <= 15)
cf_data <- sheet_data %>%
  filter(grepl('S3|S4|S5|S6', sheet), row > 1)
header_rows <- 2:3

# Fill merged cells in the header row group with cells either ahead or behind them
cf_data <- cf_data %>%
  group_by(sheet, row) %>%
  mutate(character = if_else(row %in% header_rows, na.locf(character, na.rm = FALSE), character)) %>%
  ungroup

ecoregion_data <- ecoregion_data %>%
  group_by(sheet, row) %>%
  mutate(character = if_else(row %in% header_rows, na.locf(character, na.rm = FALSE), character)) %>%
  ungroup

# Behead header rows
ecoregion_data_behead <- ecoregion_data %>%
  group_split(sheet) %>%
  map_dfr(behead, direction = 'W', name = 'ecoregion_code') %>%
  group_split(sheet) %>%
  map_dfr(behead, direction = 'W', name = 'biome') %>%
  group_split(sheet) %>%
  map_dfr(behead, direction = 'W', name = 'realm') %>%
  group_split(sheet) %>%
  map_dfr(behead, direction = 'W', name = 'ecoregion_name') %>%
  group_split(sheet) %>%
  map_dfr(behead, direction = 'W', name = 'habitat_type') %>%
  group_split(sheet) %>%
  map_dfr(behead, direction = 'N', name = 'variable') %>%
  group_split(sheet) %>%
  map_dfr(behead, direction = 'N', name = 'taxon') %>%
  select(ecoregion_code, biome, realm, ecoregion_name, habitat_type, variable, taxon, numeric) %>%
  mutate(variable = na.locf(variable))
  
cf_data_behead <- cf_data %>%
  group_split(sheet) %>%
  map_dfr(behead, direction = 'W', name = 'ecoregion_code') %>%
  group_split(sheet) %>%
  map_dfr(behead, direction = 'W', name = 'ecoregion_name') %>%
  group_split(sheet) %>%
  map_dfr(behead, direction = 'N', name = 'variable') %>%
  group_split(sheet) %>%
  map_dfr(behead, direction = 'N', name = 'land_use') %>%
  select(sheet, ecoregion_code, ecoregion_name, variable, land_use, numeric) %>%
  mutate(variable = na.locf(variable))
  
# Clean up the categorization columns: variable and taxon in ecoregion data, variable and landuse in CF data
ecoregion_data_final <- ecoregion_data_behead %>%
  mutate(variable = if_else(variable == 'Vulnerability scores of each taxa', 'vulnerability', 'S_orig'),
         taxon = if_else(variable == 'S_orig', map_chr(strsplit(taxon, '_'), 2), map_chr(strsplit(taxon, '_'), 1) %>% tolower)) %>%
  mutate(taxon = if_else(taxon == 'mammal', 'mammals', taxon)) %>%
  rename(value = numeric)

# For land use, most of the intensity groups are classed as min (minimal), Lt (light intensity), and int (intense)
# However for the managed forest land use type, the corresponding three groups are RIL, selective logging, and clear cut.
# See Table 2 in 2018 paper.
cf_data_final <- cf_data_behead %>%
  mutate(statistic = case_when(grepl('Mean', variable, fixed = TRUE) ~ 'mean',
                               grepl('2.5', variable, fixed = TRUE) ~ 'q025',
                               grepl('97.5', variable, fixed = TRUE) ~ 'q975'),
         taxon = case_when(grepl('Mammal', variable, fixed = TRUE) ~ 'mammals',
                           grepl('Bird', variable, fixed = TRUE) ~ 'birds',
                           grepl('Amphibian', variable, fixed = TRUE) ~ 'amphibians',
                           grepl('Plant', variable, fixed = TRUE) ~ 'plants',
                           grepl('Reptile', variable, fixed = TRUE) ~ 'reptiles',
                           grepl('Taxa', variable, fixed = TRUE) ~ 'taxa_aggregated'),
         unit = case_when(grepl('species loss', variable, fixed = TRUE) ~ 'potential species loss y m-2',
                          grepl('(PDF)/m2', variable, fixed = TRUE) ~ 'potential disappeared fraction m-2',
                          grepl('(PDF)*years/m2', variable, fixed = TRUE) ~ 'potential disappeared fraction y m-2'),
         land_use = gsub('CF_', '', land_use),
         intensity = case_when(land_use == 'RIL' | grepl('min', land_use) ~ 'low',
                               land_use == 'selective logging' | grepl('Lt', land_use) ~ 'med',
                               land_use == 'clear cut' | grepl('Int', land_use) ~ 'high'),
         land_use = if_else(land_use %in% c('clear cut', 'selective logging', 'RIL'), 'managed forest', map_chr(strsplit(land_use, ' '), function(x) x[length(x)])),
         CF_type = if_else(grepl('Occ', sheet), 'occupation', 'transformation'),
         region_type = if_else(grepl('Country', sheet), 'country', 'ecoregion')) %>%
  rename(value = numeric) %>%
  select(CF_type, region_type, ecoregion_code, ecoregion_name, taxon, unit, land_use, intensity, statistic, value)


write_csv(ecoregion_data_final, 'data/raw_data/biodiversity/chaudhary2015SI/chaud2018si_ecoregions.csv')
write_csv(cf_data_final, 'data/raw_data/biodiversity/chaudhary2015SI/chaud2018si_CFs.csv')
