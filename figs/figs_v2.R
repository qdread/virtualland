# Figures and summary statistics for "Scenario Analysis V2"
# QDR / Virtualland / 14 Jan 2021

# Proposed outline of figures

# 0. Consumption differences among scenarios, or basic figure showing how the scenarios differ among one another
# 1. Maps showing the trade in different agricultural products, in baseline and alternative scenarios.
# 2. Boxplot or similar summarizing the maps in fig1.
# 3. Maps showing the land flows associated with the trade in fig1.
# 4. Boxplot or similar summarizing the maps in fig3.
# 5. Maps showing the extinction threat exports/imports associated with the land flows in fig3.
# 6. Boxplot or similar summarizing the maps in fig5.


# Load data ---------------------------------------------------------------

library(tidyverse)

### Diet
diet_lancet <- read_csv('data/cfs_io_analysis/proportion_diet_lancet.csv')
diet_usa <- read_csv('data/cfs_io_analysis/proportion_diet_usaguidelines.csv')
lafa_joined <- read_csv('data/cfs_io_analysis/lafa_joined_with_diet_proportions.csv')

### County-level consumption and production
county_production <- read_csv('data/cfs_io_analysis/county_production2012.csv')
county_consumption <- read_csv('data/cfs_io_analysis/county_consumption2012_allscenarios.csv') # 300 MB

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

# Flows of species extinctions between ecoregions
tnc_extinction_flow_sums <- read_csv('data/cfs_io_analysis/scenarios/species_lost_tnc_sums_all_scenarios.csv')


# Plotting functions/themes -----------------------------------------------

# Order levels for the different scenarios
diet_levels_ordered <- c('baseline', 'planetaryhealth', 'usstyle', 'medstyle', 'vegetarian')
waste_levels_ordered <- c('baseline', 'preconsumer', 'consumer', 'allavoidable')
land_levels_ordered <- c('annual', 'permanent', 'pasture')

theme_set(theme_bw() + theme(strip.background = element_blank()))
fill_dark <- scale_fill_brewer(palette = 'Dark2')

# Diet differences among scenarios ----------------------------------------



# Food consumption differences among scenarios ----------------------------



# Flows of goods between counties -----------------------------------------



# Flows of land between counties ------------------------------------------


# Flows of land between ecoregions ----------------------------------------


# Flows of extinctions between ecoregions ---------------------------------

# Sum across land types and show by intensity. Make a different plot for each taxon.
plot_tax_intensity <- function(dat) {
  ggplot(dat, aes(x = intensity, y = flow_outbound, fill = intensity)) +
    geom_violin() +
    facet_grid(scenario_diet ~ scenario_waste) +
    fill_dark
}

taxon_intensity_plots <- tnc_extinction_flow_sums %>%
  mutate(intensity = factor(intensity, levels = c('low','med','high'))) %>%
  group_by(scenario_diet, scenario_waste, intensity, taxon, TNC) %>%
  summarize(flow_outbound = sum(flow_outbound), flow_inbound = sum(flow_inbound)) %>%
  group_by(taxon) %>% 
  nest %>%
  mutate(plot = map(data, plot_tax_intensity))

# The intensity does not appear to have any real difference so let's go with medium intensity.
# Total across all taxa, show land use based flows for each scenario.

# Ecoregion level
tnc_extinction_med_alltaxa <- tnc_extinction_flow_sums %>%
  filter(intensity == 'med') %>%
  mutate(scenario_diet = factor(scenario_diet, levels = diet_levels_ordered),
         scenario_waste = factor(scenario_waste, levels = waste_levels_ordered),
         land_use = factor(land_use, levels = land_levels_ordered)) %>%
  group_by(scenario_diet, scenario_waste, land_use, TNC) %>%
  summarize(flow_outbound = sum(flow_outbound), flow_inbound = sum(flow_inbound))
  
ggplot(tnc_extinction_med_alltaxa, aes(x = land_use, y = flow_outbound, fill = land_use)) +
  geom_violin() +
  facet_grid(scenario_diet ~ scenario_waste) +
  fill_dark

# High level sums
extinction_sums_byscenario <- tnc_extinction_med_alltaxa %>%
  group_by(scenario_diet, scenario_waste, land_use) %>%
  summarize(extinction = sum(flow_outbound, na.rm = TRUE))
  
# Plot
ggplot(extinction_sums_byscenario, aes(y = extinction, x = scenario_diet, group = scenario_waste, fill = scenario_waste)) +
  geom_col(position = 'dodge') +
  facet_wrap(~ land_use, nrow = 3) +
  fill_dark +
  labs(x = 'diet scenario', fill = 'waste scenario') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.03)), name = 'species committed to extinction')
