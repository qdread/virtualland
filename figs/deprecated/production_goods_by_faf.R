# Production of processed agricultural goods by FAF region. Maps and figures to visualize.
# QDR / Virtualland / 21 Dec 2020

# Note: use symlink to access data (new approach)


# Load data ---------------------------------------------------------------

library(tidyverse)
library(sf)

fp_out <- 'data/cfs_io_analysis'
fp_faf <- 'data/raw_data/commodity_flows/FAF/Freight_Analysis_Framework_Regions'

# FAF-level production and consumption in BEA categories
faf_consumption <- read_csv(file.path(fp_out, 'faf_demand2012.csv'))
faf_production <- read_csv(file.path(fp_out, 'faf_production2012.csv'))

# Lookup table of region names
faf_region_lookup <- read_csv(file.path(fp_out, 'faf_region_lookup.csv'))

# FAF map
faf_map <- st_read(dsn = file.path(fp_faf, 'cfs_aea.gpkg'))

# Crosswalk table which has info for which codes are in the food system
bea_table <- read_csv('data/crossreference_tables/naics_crosswalk_final.csv')

# Ready data for joining --------------------------------------------------

faf_cons_prod <- full_join(faf_consumption, faf_production) %>%
  mutate(demand = demand * 1e6,
         CFS12_NAME = gsub('  ', ' ', CFS12_NAME))

# Fix region names
setdiff(faf_cons_prod$CFS12_NAME, faf_region_lookup$FAF_Region) # Laredo, Remainder of Alaska, Remainder of Idaho

faf_cons_prod <- faf_cons_prod %>%
  rename(FAF_Region = CFS12_NAME) %>%
  mutate(FAF_Region = case_when(FAF_Region == 'Remainder of Alaska' ~ 'Alaska',
                                FAF_Region == 'Remainder of Idaho'~ 'Idaho',
                                grepl('Laredo', FAF_Region) ~ 'Laredo, TX CFS Area',
                                TRUE ~ FAF_Region))

setdiff(faf_cons_prod$FAF_Region, faf_region_lookup$FAF_Region) # All are accounted for.

faf_cons_prod <- faf_cons_prod %>%
  left_join(faf_region_lookup[,c('Code','FAF_Region')])


# Join data with map ------------------------------------------------------

# Widen so it can be joined

food_codes <- bea_table$BEA_389_code[bea_table$food_system %in% c('y', 'partial') & substr(bea_table$BEA_389_code, 1, 1) %in% c('1','3')]

faf_cons_prod_wide <- faf_cons_prod %>%
  filter(BEA_code %in% food_codes) %>%
  pivot_wider(id_cols = c(FAF_Region, Code), names_from = BEA_code, values_from = c(demand, production))

faf_map <- faf_map %>% left_join(faf_cons_prod_wide)


# Quick plots -------------------------------------------------------------

ak_hi <- c('Alaska', 'Remainder of Hawaii', 'Urban Honolulu, HI CFS Area')

level3codes <- food_codes[substr(food_codes,1,1) == '3']

plot(faf_map[!faf_map$FAF_Region %in% ak_hi, paste0('production_', level3codes[1:4])])
plot(faf_map[!faf_map$FAF_Region %in% ak_hi, paste0('production_', level3codes[5:8])])
plot(faf_map[!faf_map$FAF_Region %in% ak_hi, paste0('production_', level3codes[9:12])])
plot(faf_map[!faf_map$FAF_Region %in% ak_hi, paste0('production_', level3codes[13:16])])
plot(faf_map[!faf_map$FAF_Region %in% ak_hi, paste0('production_', level3codes[17:20])])
plot(faf_map[!faf_map$FAF_Region %in% ak_hi, paste0('production_', level3codes[20:24])])
plot(faf_map[!faf_map$FAF_Region %in% ak_hi, paste0('production_', level3codes[25:26])])
             