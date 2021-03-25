# Script to load packages and data for making figures
# Split off into separate script, 15 Feb 2021

# Load data ---------------------------------------------------------------

library(tidyverse)
library(cowplot) # For labels of faceted plots
library(sf)
library(glue)
library(grid)
library(gridExtra)

fp_fig <- 'data/cfs_io_analysis/scenario_v2_figs'

### Diet
diet_lancet <- read_csv('data/cfs_io_analysis/proportion_diet_lancet.csv')
diet_usa <- read_csv('data/cfs_io_analysis/proportion_diet_usaguidelines.csv')
lafa_joined <- read_csv('data/cfs_io_analysis/lafa_joined_with_diet_proportions.csv')

### County-level consumption and production
# I've loaded only the summed data here. The raw data can be loaded separately later (some are big).
# county_production <- read_csv('data/cfs_io_analysis/county_production2012.csv')
# county_consumption <- read_csv('data/cfs_io_analysis/county_consumption2012_allscenarios.csv') # 320 MB
# county_totaldemand <- read_csv('data/cfs_io_analysis/county_totaldemand2012_allscenarios.csv') # 430 MB
totaldemand_sums <- read_csv('data/cfs_io_analysis/scenarios/totaldemand_sums_all_scenarios.csv')

### Scenario factors by BEA category
bea_scenario_factors <- read_csv('data/cfs_io_analysis/bea_consumption_factors_diet_waste_scenarios.csv')

### State land exchange tables
load('data/cfs_io_analysis/state_land_exchange_tables.RData')

# For flows of goods and land between counties, land between ecoregions, and species between ecoregions,
# I've loaded only the summed data here. The raw data can be loaded separately later (some are big).

# Flows of goods between counties
county_goods_flow_sums <- read_csv('data/cfs_io_analysis/scenarios/goodsflows_county_sums_all_scenarios.csv')

# Flows of land between counties
county_land_flow_sums <- read_csv('data/cfs_io_analysis/scenarios/landflows_county_sums_all_scenarios.csv')

# Flows of land between ecoregions
tnc_land_flow_sums <- read_csv('data/cfs_io_analysis/scenarios/landflows_tnc_sums_all_scenarios.csv')
#tnc_landflows <- read_csv('data/cfs_io_analysis/scenarios/landflows_tnc_x_tnc_all_scenarios.csv')

# Flows of species extinctions between counties
county_extinction_flow_sums <- read_csv('data/cfs_io_analysis/scenarios/species_lost_county_sums_all_scenarios_med.csv', col_types = 'ccccdd')

# Flows of species extinctions between states (raw pairwise)
state_extinction_flows <- read_csv('data/cfs_io_analysis/scenarios/species_lost_state_x_state_all_scenarios_med.csv', col_types = 'cccccd')

# Foreign virtual land transfers into USA, by country x tnc combination exporting them and by county importing them
foreign_vlt_export <- read_csv('data/cfs_io_analysis/foreign_VLT_by_country_x_TNC.csv')
foreign_vlt_import <- read_csv('data/cfs_io_analysis/foreign_VLT_to_counties.csv')

# Foreign flows of species extinctions into USA, by country x tnc combination exporting them and by county importing them
foreign_extinction_export <- read_csv('data/cfs_io_analysis/scenarios/foreign_species_lost_by_export_country_x_tnc.csv')
foreign_extinction_import <- read_csv('data/cfs_io_analysis/scenarios/foreign_species_lost_by_import_county.csv')

# Foreign goods transfers into USA, one for animals and one for crops
foreign_animal_export <- read_csv('data/cfs_io_analysis/fao_production_trade_animals.csv')
foreign_crop_export <- read_csv('data/cfs_io_analysis/fao_production_trade_crops.csv')

# Map of counties in AEA
county_map <- st_read('data/raw_data/landuse/USA/USA_county_2014_aea.gpkg')
# Map of TNC ecoregions in AEA
tnc_map <- st_read('data/raw_data/landuse/ecoregions/tnc_usa_aea.gpkg')

# Plotting functions/themes, and lookup tables/vectors of names for plot labels.
source('figs/figs_v2_lookups.R')
source('figs/us_map_fxns.R')

# Remove areas other than the 50 states plus DC from county_map. (anything beginning with 6 or 7)
county_map <- county_map %>% filter(!substr(STATEFP,1,1) %in% c('6','7'))
# Now the 3141 counties and county equivalents match up between the data and the map.

# Merge county map to state map.
state_map <- county_map %>%
  group_by(STATEFP) %>%
  summarize


# Index of Alaska and Hawaii in ecoregion map
tnc_ak_idx <- substr(tnc_map$ECO_CODE, 1, 4) %in% c('NA06', 'NA11') | tnc_map$ECO_CODE %in% c('NA0509', 'NA0518')
tnc_hi_idx <- substr(tnc_map$ECO_CODE, 1, 2) == 'OC'

# Get index of Alaska and Hawaii in county map. AK 02 HI 15
county_ak_idx <- substr(county_map$county, 1, 2) == '02'
county_hi_idx <- substr(county_map$county, 1, 2) == '15'

