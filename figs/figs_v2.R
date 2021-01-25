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

### Diet
diet_lancet <- read_csv('data/cfs_io_analysis/proportion_diet_lancet.csv')
diet_usa <- read_csv('data/cfs_io_analysis/proportion_diet_usaguidelines.csv')
lafa_joined <- read_csv('data/cfs_io_analysis/lafa_joined_with_diet_proportions.csv')

### County-level consumption and production
county_production <- read_csv('data/cfs_io_analysis/county_production2012.csv')
county_consumption <- read_csv('data/cfs_io_analysis/county_consumption2012_allscenarios.csv') # 320 MB
county_totaldemand <- read_csv('data/cfs_io_analysis/county_totaldemand2012_allscenarios.csv') # 430 MB

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

# Lookup tables for longer legend names
diet_long_names <- data.frame(scenario_diet = diet_levels_ordered,
                              long_name = c('baseline', 'planetary health (Lancet)', 'healthy US-style (USDA)', 'healthy Mediterranean-style (USDA)', 'healthy vegetarian (USDA)'))
waste_long_names <- data.frame(scenario_waste = waste_levels_ordered,
                               long_name = c('baseline', 'pre-consumer waste cut 50%', 'consumer waste cut 50%', 'all waste cut 50%'))

# Labeller function with character vector lookup tables for 2x2 scenarios
scenario_labeller <- labeller(scenario_diet = setNames(diet_long_names$long_name, diet_long_names$scenario_diet),
                              scenario_waste = setNames(waste_long_names$long_name, waste_long_names$scenario_waste))

# Short names of the ten agricultural goods in BEA, plus wild-caught fish
ag_names_lookup <- data.frame(
  BEA_389_code = c("1111A0", "1111B0", "111200", "111300", "111400", "111900", "112120", "1121A0", "112300", "112A00", "114000"
  ), 
  BEA_389_def = c("Fresh soybeans, canola, flaxseeds, and other oilseeds",
                  "Fresh wheat, corn, rice, and other grains", 
                  "Fresh vegetables, melons, and potatoes", 
                  "Fresh fruits and tree nuts", 
                  "Greenhouse crops, mushrooms, nurseries, and flowers", 
                  "Tobacco, cotton, sugarcane, peanuts, sugar beets, herbs and spices, and other crops", 
                  "Dairies", 
                  "Cattle ranches and feedlots", 
                  "Poultry farms", 
                  "Animal farms and aquaculture ponds (except cattle and poultry)", 
                  "Wild-caught fish and game"),
  short_name = c('oilseeds & soybeans', 'grains', 'vegetables & potatoes', 'fruits & nuts', 'greenhouse crops', 'peanuts, sugar, etc.', 'dairy', 'beef cattle', 'poultry & eggs', 'other meat', 'wild-caught fish'),
  kingdom = rep(c('plant', 'animal'), c(6, 5))) %>%
  mutate(short_name = factor(short_name, levels = unique(short_name)))

theme_set(theme_bw() + theme(strip.background = element_blank()))
fill_dark <- scale_fill_brewer(palette = 'Dark2')
okabe_colors <- palette.colors(n = 9, palette = 'Okabe-Ito')

# Function to make a "dummy axis" so I can label the secondary axis.
dummy_axis <- function(label) sec_axis(~ . , name = label, labels = NULL, breaks = NULL)

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

ggplot(lafa_cal_summ %>% mutate(), aes(y = calories_day, x = food_group, color = diet, fill = diet, group = diet)) +
  geom_col(position = 'dodge', color = NA) +
  scale_fill_manual(values = okabe_colors[c(1,7,3,4,5)] %>% setNames(NA), labels = diet_long_names$long_name) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.03)), name = 'calories per person per day') +
  scale_x_discrete(name = 'food group') +
  theme(legend.position = 'bottom') +
  guides(fill=guide_legend(nrow = 2, byrow=FALSE))

# Goods consumption differences among scenarios ---------------------------

# This is by BEA code across diet change and waste change scenarios.
# It also accounts for the "footprint" of consumption, so for instance meat consumption would reflect the consumption of feed.
# Filter down to the ten primary agricultural goods in the BEA table.


# Need to reshape county_totaldemand, sum across counties, and possibly calculate relative to baseline.
totaldemand_sums <- cbind(county_totaldemand[,c('BEA_code', 'scenario')], demand = rowSums(county_totaldemand[,-(1:2)])) %>%
  separate(scenario, into = c('d', 'scenario_diet', 'w', 'scenario_waste'), sep = '_') %>% 
  select(-d, -w) %>%
  mutate(scenario_diet = factor(scenario_diet, levels = diet_levels_ordered),
         scenario_waste = factor(scenario_waste, levels = waste_levels_ordered)) %>% 
  right_join(ag_names_lookup, by = c('BEA_code' = 'BEA_389_code'))

# Correct such that greenhouse crops are added to peanuts and sugar.
totaldemand_sums <- totaldemand_sums %>%
  mutate(BEA_code = if_else(BEA_code == '111400', '111900', BEA_code),
         short_name = fct_collapse(short_name, `peanuts, sugar, etc.` = c('greenhouse crops', 'peanuts, sugar, etc.'))) %>%
  group_by(BEA_code, short_name, kingdom, scenario_diet, scenario_waste) %>%
  summarize(demand = sum(demand))

# Absolute values

library(cowplot)

p <- ggplot(totaldemand_sums, aes(x = short_name, y = demand)) +
  geom_col(aes(fill = kingdom)) +
  facet_grid(scenario_waste ~ scenario_diet, labeller = scenario_labeller) +
  scale_x_discrete(name = 'food category') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.03)), name = 'consumption (million USD)') +
  scale_fill_manual(values = setNames(okabe_colors[c(8, 4)], c('animal', 'plant'))) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none')

ggdraw(p + theme(plot.margin = unit(c(25, 25, 5.5, 5.5), 'points'))) +
  draw_label(label = 'diet scenario', x = 0.5, y = 0.97) +
  draw_label(label = 'waste scenario', x = 0.99, y = 0.5, angle = -90)

# Relative to baseline. Must reconstruct the darn scenarios again.
totaldemand_relative <- totaldemand_sums %>%
  pivot_wider(names_from = c(scenario_diet, scenario_waste), values_from = demand) %>%
  mutate(across(where(is.numeric), ~ . / baseline_baseline)) %>%
  pivot_longer(-c(BEA_code, short_name, kingdom), names_to = 'scenario', values_to = 'demand') %>%
  separate(scenario, into = c('scenario_diet', 'scenario_waste'), sep = '_') %>%
  mutate(scenario_diet = factor(scenario_diet, levels = diet_levels_ordered),
         scenario_waste = factor(scenario_waste, levels = waste_levels_ordered))

p <- ggplot(totaldemand_relative, aes(x = short_name, y = demand)) +
  geom_col(aes(fill = kingdom)) +
  geom_hline(yintercept = 1, linetype = 'dotted', color = 'black', size = 0.5) +
  facet_grid(scenario_waste ~ scenario_diet, labeller = scenario_labeller) +
  scale_x_discrete(name = 'food category') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.03)), name = 'consumption relative to baseline') +
  scale_fill_manual(values = setNames(okabe_colors[c(8, 4)], c('animal', 'plant'))) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'none')

ggdraw(p + theme(plot.margin = unit(c(25, 25, 5.5, 5.5), 'points'))) +
  draw_label(label = 'diet scenario', x = 0.5, y = 0.97) +
  draw_label(label = 'waste scenario', x = 0.99, y = 0.5, angle = -90)


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
