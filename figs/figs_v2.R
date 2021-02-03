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
library(cowplot) # For labels of faceted plots
library(sf)
library(glue)

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

# Flows of species extinctions between ecoregions
tnc_extinction_flow_sums <- read_csv('data/cfs_io_analysis/scenarios/species_lost_tnc_sums_all_scenarios.csv')

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


# Diet differences among scenarios ----------------------------------------

# Show by the "Group" column in lafa_joined
# Need to assign the missing ones to groups.
missing_groups <- data.frame(Category = c("White and whole wheat flour", "Durum flour", "Fresh brussels sprouts", "Other processed vegetables"),
                    Group = c('grain', 'grain', 'veg', 'veg'))
idx <- match(missing_groups$Category, lafa_joined$Category)
lafa_joined$Group[idx] <- missing_groups$Group

lafa_cal_by_diet <- lafa_joined %>%
  select(Category, Group, calories_available_cal_day, planetary_health, us_style, med_style, vegetarian) %>%
  mutate(baseline = rep(1, nrow(.))) %>%
  pivot_longer(planetary_health:baseline, names_to = 'diet', values_to = 'factor') %>%
  mutate(calories_available_cal_day = calories_available_cal_day * factor) %>%
  rename(food = Category, food_group = Group)

lafa_cal_summ <- lafa_cal_by_diet %>%
  group_by(diet, food_group) %>%
  summarize(calories_day = sum(calories_available_cal_day)) %>%
  ungroup %>%
  mutate(food_group = case_when(food_group == 'veg' ~ 'vegetables',
                                food_group == 'meat' ~ 'meat/eggs/nuts',
                                TRUE ~ food_group),
         diet = factor(gsub('_', '', diet), levels = diet_levels_ordered))

p_diet_foodgroups <- ggplot(lafa_cal_summ %>% mutate(), aes(y = calories_day, x = food_group, color = diet, fill = diet, group = diet)) +
  geom_col(position = 'dodge', color = NA) +
  scale_fill_manual(values = okabe_colors[c(1,7,3,4,5)] %>% setNames(NA), labels = diet_long_names$long_name) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.03)), name = 'calories per person per day') +
  scale_x_discrete(name = 'food group') +
  theme(legend.position = 'bottom',
        legend.text = element_text(size = rel(.6))) +
  guides(fill=guide_legend(nrow = 2, byrow = FALSE))

ggsave(file.path(fp_fig, 'foodgroup_consumption_by_diet.png'), p_diet_foodgroups, height = 5, width = 6, dpi = 400)

# Goods consumption differences among scenarios ---------------------------

# This is by BEA code across diet change and waste change scenarios.
# It also accounts for the "footprint" of consumption, so for instance meat consumption would reflect the consumption of feed (need to check this)

# Reorder factors in loaded CSV.
totaldemand_sums <- totaldemand_sums %>%
  mutate(short_name = factor(short_name, levels = unique(short_name)),
         scenario_diet = factor(scenario_diet, levels = unique(scenario_diet)),
         scenario_waste = factor(scenario_waste, levels = unique(scenario_waste)))

# Absolute values
p_totaldemand_sums <- ggplot(totaldemand_sums, aes(x = short_name, y = demand)) +
  geom_col(aes(fill = kingdom)) +
  facet_grid(scenario_waste ~ scenario_diet, labeller = scenario_labeller) +
  scale_x_discrete(name = 'food category') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.03)), name = 'consumption (million USD)') +
  scale_fill_manual(values = setNames(okabe_colors[c(8, 4)], c('animal', 'plant'))) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none')

p_totaldemand_sums <- ggdraw(p_totaldemand_sums + theme(plot.margin = unit(c(25, 25, 5.5, 5.5), 'points'))) +
  draw_label(label = 'diet scenario', x = 0.5, y = 0.97) +
  draw_label(label = 'waste scenario', x = 0.99, y = 0.5, angle = -90)

ggsave(file.path(fp_fig, 'total_consumption_all_scenarios.png'), p_totaldemand_sums, height = 9, width = 12, dpi = 400)

# Relative to baseline. Must reconstruct the darn scenarios again.
totaldemand_relative <- totaldemand_sums %>%
  pivot_wider(names_from = c(scenario_diet, scenario_waste), values_from = demand) %>%
  mutate(across(where(is.numeric), ~ . / baseline_baseline)) %>%
  pivot_longer(-c(BEA_code, short_name, kingdom), names_to = 'scenario', values_to = 'demand') %>%
  separate(scenario, into = c('scenario_diet', 'scenario_waste'), sep = '_') %>%
  mutate(scenario_diet = factor(scenario_diet, levels = diet_levels_ordered),
         scenario_waste = factor(scenario_waste, levels = waste_levels_ordered))

p_totaldemand_relative <- ggplot(totaldemand_relative, aes(x = short_name, y = demand)) +
  geom_col(aes(fill = kingdom)) +
  geom_hline(yintercept = 1, linetype = 'dotted', color = 'black', size = 0.5) +
  facet_grid(scenario_waste ~ scenario_diet, labeller = scenario_labeller) +
  scale_x_discrete(name = 'food category') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.03)), name = 'consumption relative to baseline') +
  scale_fill_manual(values = setNames(okabe_colors[c(8, 4)], c('animal', 'plant'))) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none')

p_totaldemand_relative <- ggdraw(p_totaldemand_relative + theme(plot.margin = unit(c(25, 25, 5.5, 5.5), 'points'))) +
  draw_label(label = 'diet scenario', x = 0.5, y = 0.97) +
  draw_label(label = 'waste scenario', x = 0.99, y = 0.5, angle = -90)

ggsave(file.path(fp_fig, 'total_consumption_relative_all_scenarios.png'), p_totaldemand_relative, height = 9, width = 12, dpi = 400)

### Deprecated: plot with bea_factors (does not account for the full footprint, it's just the final demand)
bea_factors_long <- bea_scenario_factors %>%
  pivot_longer(-c(BEA_389_code, BEA_389_def), names_to = 'scenario', values_to = 'factor') %>%
  separate(scenario, into = c('d', 'scenario_diet', 'w', 'scenario_waste'), sep = '_') %>% 
  select(-d, -w) %>%
  mutate(scenario_diet = factor(scenario_diet, levels = diet_levels_ordered),
         scenario_waste = factor(scenario_waste, levels = waste_levels_ordered)) %>% 
  left_join(ag_names_lookup)

# Plot 4x5 diet x waste scenarios
bea_factors_long %>%
  filter(substr(BEA_389_code, 1, 1) == '1') %>%
  ggplot(aes(x = short_name, y = factor)) +
  geom_col() +
  geom_hline(yintercept = 1, linetype = 'dotted', color = 'indianred', size = 0.5) +
  facet_grid(scenario_waste ~ scenario_diet) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.03)), name = 'consumption relative to baseline') +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Flows of goods between counties -----------------------------------------



# Flows of land between counties ------------------------------------------

# Sum up the flows across all land types
county_land_flow_sumalltypes <- county_land_flow_sums %>%
  group_by(scenario_diet, scenario_waste, county) %>%
  summarize(flow_inbound = sum(flow_inbound), flow_outbound = sum(flow_outbound)) %>%
  mutate(land_type = 'total_agricultural_land') %>%
  ungroup

# Group and nest the county landflow dataframe.
# Replace the zero flows with NA in the outbound ones.
county_land_maps <- county_land_flow_sums %>%
  bind_rows(county_land_flow_sumalltypes) %>%
  arrange(scenario_diet, scenario_waste, land_type, county) %>%
  mutate(flow_net = flow_outbound - flow_inbound,
         flow_outbound = if_else(flow_outbound == 0, as.numeric(NA), flow_outbound)) %>%
  group_by(scenario_diet, scenario_waste, land_type) %>%
  nest

# Match up the sf object for county boundaries with one of the county land data subsets.
setdiff(county_land_maps$data[[1]]$county, county_map$county) # All were already fixed in county_aea.R.

# Get index of alaska and hawaii. AK 02 HI 15
county_ak_idx <- substr(county_map$county, 1, 2) == '02'
county_hi_idx <- substr(county_map$county, 1, 2) == '15'

# Calculate global scale for log breaks
range(county_land_flow_sumalltypes$flow_inbound)
range(county_land_flow_sumalltypes$flow_outbound[county_land_flow_sumalltypes$flow_outbound > 0], na.rm = TRUE)

county_land_breaks <- c(2e1, 2e3, 2e5, 2e7)

# Annual cropland, permanent cropland, pastureland, and total.
# Do this for all 20 scenarios.
# Log10 transformation is best for viewing pattern. 
county_land_maps <- county_land_maps %>%
  ungroup %>%
  mutate(plot_title = pmap(county_land_maps[, c('land_type','scenario_diet','scenario_waste')], 
                           function(land_type, scenario_diet, scenario_waste) 
                             list(land_type = gsub('_', ' ', land_type), 
                                  diet_name = diet_long_names$long_name[match(scenario_diet, diet_long_names$scenario_diet)], 
                                  waste_name = waste_long_names$long_name[match(scenario_waste, waste_long_names$scenario_waste)],
                                  file_prefix = glue('D_{scenario_diet}_W_{scenario_waste}_{land_type}')))) %>%
  mutate(map_inbound = map2(data, plot_title, 
                            ~ draw_usmap_with_insets(map_data = left_join(county_map, .x), 
                                                     ak_idx = county_ak_idx,
                                                     hi_idx = county_hi_idx,
                                                     variable = flow_inbound,
                                                     title = glue('{.y$land_type} imported by county'),
                                                     subtitle = glue('diet scenario: {.y$diet_name}, waste scenario: {.y$waste_name}'),
                                                     scale_name = 'area (ha)',
                                                     scale_factor = 10000,
                                                     scale_trans = 'log10',
                                                     scale_range = county_land_breaks[c(1,4)],
                                                     scale_breaks = county_land_breaks,
                                                     add_theme = theme_void() + theme(legend.position = c(0.62, 0.1),
                                                                                      legend.key.width = unit(0.23, 'in')),
                                                     write_to_file = glue('{fp_fig}/county_landflow_maps/{.y$file_prefix}_inbound.png'),
                                                     img_size = c(7, 7))),
         map_outbound = map2(data, plot_title,
                             ~ draw_usmap_with_insets(map_data = left_join(county_map, .x), 
                                                      ak_idx = county_ak_idx,
                                                      hi_idx = county_hi_idx,
                                                      variable = flow_outbound,
                                                      title = glue('{.y$land_type} exported by county'),
                                                      subtitle = glue('diet scenario: {.y$diet_name}, waste scenario: {.y$waste_name}'),
                                                      scale_name = 'area (ha)',
                                                      scale_factor = 10000,
                                                      scale_trans = 'log10',
                                                      scale_range = county_land_breaks[c(1,4)],
                                                      scale_breaks = county_land_breaks,
                                                      add_theme = theme_void() + theme(legend.position = c(0.62, 0.1),
                                                                                       legend.key.width = unit(0.23, 'in')),
                                                      write_to_file = glue('{fp_fig}/county_landflow_maps/{.y$file_prefix}_outbound.png'),
                                                      img_size = c(7, 7))))

# Flows of land between ecoregions ----------------------------------------

# Sum up the flows across all land types
tnc_land_flow_sumalltypes <- tnc_land_flow_sums %>%
  group_by(scenario_diet, scenario_waste, TNC) %>%
  summarize(flow_inbound = sum(flow_inbound), flow_outbound = sum(flow_outbound)) %>%
  mutate(land_type = 'total agricultural land') %>%
  ungroup

# Group and nest the county landflow dataframe.
tnc_land_maps <- tnc_land_flow_sums %>%
  bind_rows(tnc_land_flow_sumalltypes) %>%
  arrange(scenario_diet, scenario_waste, land_type, TNC) %>%
  mutate(flow_net = flow_outbound - flow_inbound,
         flow_outbound = if_else(flow_outbound == 0, as.numeric(NA), flow_outbound)) %>%
  group_by(scenario_diet, scenario_waste, land_type) %>%
  nest

# Match up the sf object for TNC boundaries with one of the TNC land data subsets.
setdiff(tnc_land_maps$data[[1]]$TNC, tnc_map$ECO_CODE) 

# Get index of alaska and hawaii. 
tnc_ak_idx <- substr(tnc_map$ECO_CODE, 1, 4) %in% c('NA06', 'NA11') | tnc_map$ECO_CODE %in% c('NA0509', 'NA0518')
tnc_hi_idx <- substr(tnc_map$ECO_CODE, 1, 2) == 'OC'

# Annual cropland, permanent cropland, pastureland, and total.
# Do this for all 20 scenarios.
# Log10 transformation is best for viewing pattern. 
tnc_land_maps <- tnc_land_maps %>%
  ungroup %>%
  mutate(plot_title = pmap(tnc[, c('land_type','scenario_diet','scenario_waste')], 
                           function(land_type, scenario_diet, scenario_waste) 
                             list(land_type = gsub('_', ' ', land_type), 
                                  diet_name = diet_long_names$long_name[match(scenario_diet, diet_long_names$scenario_diet)], 
                                  waste_name = waste_long_names$long_name[match(scenario_waste, waste_long_names$scenario_waste)]))) %>%
  mutate(map_inbound = map2(data, plot_title,  ~ draw_usmap_with_insets(map_data = left_join(tnc_map, .x), 
                                                                        ak_idx = tnc_ak_idx,
                                                                        hi_idx = tnc_hi_idx,
                                                                        variable = flow_inbound,
                                                                        title = glue('{.y$land_type} imported by ecoregion'),
                                                                        subtitle = glue('diet scenario: {.y$diet_name}, waste scenario: {.y$waste_name}'),
                                                                        scale_name = 'land area (ha)',
                                                                        scale_factor = 10000,
                                                                        scale_trans = 'log10',
                                                                        add_theme = theme_void())),
         map_outbound = map2(data, plot_title,  ~ draw_usmap_with_insets(map_data = left_join(tnc_map, .x), 
                                                                         ak_idx = tnc_ak_idx,
                                                                         hi_idx = tnc_hi_idx,
                                                                         variable = flow_outbound,
                                                                         title = glue('{.y$land_type} exported by ecoregion'),
                                                                         subtitle = glue('diet scenario: {.y$diet_name}, waste scenario: {.y$waste_name}'),
                                                                         scale_name = 'land area (ha)',
                                                                         scale_factor = 10000,
                                                                         scale_trans = 'log10',
                                                                         add_theme = theme_void())))

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
diet_medium_names <- c('baseline', 'planetary\nhealth', 'healthy\nUS-style', 'healthy\nMediterranean', 'vegetarian')

# Fixed y axis
p_extinction_sums_fixed <- ggplot(extinction_sums_byscenario, aes(y = extinction, x = scenario_diet, group = scenario_waste, fill = scenario_waste)) +
  geom_col(position = 'dodge') +
  facet_wrap(~ land_use, nrow = 3) +
  scale_x_discrete(name = 'diet scenario', labels = diet_medium_names) +
  scale_fill_brewer(name = 'waste scenario', palette = 'Dark2', labels = waste_long_names$long_name) +
  scale_y_continuous(name = 'species committed to extinction', expand = expansion(mult = c(0, 0.03))) +
  theme(legend.position = 'bottom', 
        legend.text = element_text(size = rel(.7))) +
  guides(fill=guide_legend(nrow = 2, byrow = FALSE))

# Free y axis
p_extinction_sums_free <- ggplot(extinction_sums_byscenario, aes(y = extinction, x = scenario_diet, group = scenario_waste, fill = scenario_waste)) +
  geom_col(position = 'dodge') +
  facet_wrap(~ land_use, nrow = 3, scales = 'free_y') +
  scale_x_discrete(name = 'diet scenario', labels = diet_medium_names) +
  scale_fill_brewer(name = 'waste scenario', palette = 'Dark2', labels = waste_long_names$long_name) +
  scale_y_continuous(name = 'species committed to extinction', expand = expansion(mult = c(0, 0.03))) +
  theme(legend.position = 'bottom', 
        legend.text = element_text(size = rel(.7))) +
  guides(fill=guide_legend(nrow = 2, byrow = FALSE))

ggsave(file.path(fp_fig, 'extinction_sums_fixed_y.png'), p_extinction_sums_fixed, height = 7, width = 5, dpi = 400)
ggsave(file.path(fp_fig, 'extinction_sums_free_y.png'), p_extinction_sums_free, height = 7, width = 5, dpi = 400)
