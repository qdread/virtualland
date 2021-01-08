# check magnitude of land consumption

land_bystate <- land_consumption_allscenarios %>% 
  group_by(scenario, state_from, land_type) %>%
  summarize(land = sum(land_consumption)) %>%
  left_join(state_fips_table, by = c('state_from' = 'state_code')) %>%
  mutate(state = gsub('US_', '', state))

land_bystate %>%
  ungroup %>%
  arrange(scenario, land_type, -land) %>%
  group_by(scenario, land_type) %>%
  slice(5)

land_bystate %>% filter(scenario == 'D_baseline_WR_baseline', land_type == 'Annual crops') %>% arrange(-land)

# In the USA there are ~166M ha cropland
library(units)

set_units(166e6, 'ha') %>% set_units('km^2') # 1.66M square km of cropland.

land_consumption_allscenarios %>% group_by(scenario, land_type) %>% summarize(land = sum(land_consumption)) %>% mutate(land_km2 = set_units(land, 'm^2') %>% set_units('km^2'))

# Check magnitude using all personal consumption expenditures 2012, multiplied out by the DRC matrix.
# (Manually do I-O model)

fp_bea <- 'data/raw_data/BEA/formatted'

# Read the input-output BEA table to get the personal consumption expenditure for each good.
use2012 <- read_csv(file.path(fp_bea, 'use2012.csv'))
# Get the PCE for the 389 BEA goods from this table.
# F01000 is the code for personal consumption expenditures.
pce2012 <- setNames(use2012$F01000[1:389], use2012$X1[1:389])

fp_useeio <- file.path(ifelse(dir.exists('Q:/'), '~/Documents/GitHub/foodwaste', '~'), 'USEEIO/useeiopy/Model Builds/USEEIO2012')

drc2012 <- read.csv(file.path(fp_useeio, 'USEEIO2012_DRC.csv'), row.names = 1, check.names = FALSE) # Use this to get row names

# Convert DRC2012 row and column names to the six digit code with uppercase letters
dimnames(drc2012) <- map(dimnames(drc2012), ~ toupper(substr(., 1, 6)))

pce2012 <- pce2012[dimnames(drc2012)[[1]]]

# Direct + indirect demand
demand <- as.matrix(drc2012) %*% pce2012 * 1e6

# Demand only for ag goods
demand_ag <- demand[dimnames(demand)[[1]] %in% names(land_exch_tables), ]

# Multiply this by a typical land exchange table. (for California)
cal_land_exch <- land_exch_matrices$data[[6]]

cal_land_exch %*% demand_ag / 1e6
