# Script to read tables S2 and S3 from Chaudhary & Brooks 2019 (global and regional characterization factors from new model)
# They don't include the intermediate affinity values for recalculation unfortunately
# Also unfortunately, confidence intervals are not provided, though they were for the 2015 paper
# QDR / Virtualland / 04 Dec 2020

library(tidyxl)
library(tidyverse)
library(zoo)
library(unpivotr)
library(readxl)

fp <- ifelse(dir.exists('Q:/'), 'Q:/raw_data', '/nfs/qread-data/raw_data')
xlfile <- file.path(fp, 'biodiversity/chaudhary2015SI/chaud2019SI.xlsx')

# Hopefully this will not need complicated manipulation, because the formatting is reasonably simple in tables S2 and S3
xlsheets <- grep("Table S2|Table S3", excel_sheets(xlfile))

# In each table, after ecoregion column, first four cols are mammal, then bird, then amphibian
tables2 <- read_xlsx(xlfile, sheet = xlsheets[1], skip = 2)
tables3 <- read_xlsx(xlfile, sheet = xlsheets[2], skip = 2)

tables2_processed <- tables2 %>% 
  pivot_longer(cols = -Ecoregion_name, values_to = 'CF_global_endemic') %>%
  separate(name, into = c('land_use', 'taxon'), sep = '\\.\\.\\.') %>%
  mutate(taxon = case_when(taxon %in% 2:5 ~ 'mammal',
                           taxon %in% 6:9 ~ 'bird',
                           taxon %in% 10:13 ~ 'amphibian'))

# Table S3 is the same except that the name of the secondary forests is managed forests
tables3_processed <- tables3 %>% 
  pivot_longer(cols = -Ecoregion_name, values_to = 'CF_regional') %>%
  separate(name, into = c('land_use', 'taxon'), sep = '\\.\\.\\.') %>%
  mutate(taxon = case_when(taxon %in% 2:5 ~ 'mammal',
                           taxon %in% 6:9 ~ 'bird',
                           taxon %in% 10:13 ~ 'amphibian')) %>%
  mutate(land_use = if_else(land_use == 'Managed Forests', 'Secondary Forests', land_use))

# Join S2 and S3, and write to CSV
cfs <- full_join(tables2_processed, tables3_processed)

write_csv(cfs, file.path(fp, 'biodiversity/chaudhary2015SI/chaud2019CFs.csv'))
