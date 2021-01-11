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


# Allocate flows of agricultural goods ------------------------------------

# We assume that each producing region of the USA sends goods with equal probability around the country
# proportional only to the demand of the consuming region (i.e. ignores how far apart the two are)

ag_goods <- bea_table %>% filter(substr(BEA_389_code,1,3) %in% c('111','112'))

faf_cons_prod %>%
  filter(BEA_code %in% ag_goods$BEA_389_code) %>% summary

faf_cons_prod_norm <- faf_cons_prod %>%
  filter(BEA_code %in% ag_goods$BEA_389_code) %>%
  replace_na(list(production = 0)) %>%
  group_by(BEA_code) %>%
  mutate(demand_norm = demand/sum(demand), production_norm = production/sum(production))

# for each region and good, multiply the total demand times the relative proportion of production.
allocate_demand <- function(data) {
  mat <- data$production_norm %*% t(data$demand)
  dimnames(mat)[[2]] <- data$Code
  as_tibble(mat)
}

faf_demand_allocated_wide <- faf_cons_prod_norm %>%
  group_by(BEA_code) %>%
  nest %>%
  mutate(demand_allocated = map(data, allocate_demand))

# Format the allocated demand into long form.
faf_demand_allocated_long <- faf_demand_allocated_wide %>% 
  unnest(cols = c(data, demand_allocated)) %>%
  select(-c(demand, production, demand_norm, production_norm)) %>%
  pivot_longer(-c(BEA_code, FAF_Region, Code), names_to = 'from', values_to = 'demand') %>%
  rename(to = Code)

# Save to files
# .RData for the wideform tibble with list columns
save(faf_demand_allocated_wide, file = file.path(fp_out, 'faf_demand_allocated_wide.RData'))
write_csv(faf_demand_allocated_long, file.path(fp_out, 'faf_demand_allocated_long.csv'))
