library(ggplot2)
library(data.table)

load('~/GitHub/foodwaste/biodiversity-farm2fork/data/all_app_data.RData')

windowsFonts(`fgm` = windowsFont("Franklin Gothic Medium"))

# Calculate grand totals. For extinctions, separate into plants and animals (vertebrates) but ignore other taxa
sum_cols <- grep('flow_inbound', names(county_land_flow_sums), value = TRUE)
county_extinction_flow_sums[, kingdom := ifelse(taxon == 'plants', 'plants', 'vertebrates')]
extinction_grandtotals <- county_extinction_flow_sums[, lapply(.SD, sum), by = .(scenario_diet, scenario_waste, kingdom), .SDcols = sum_cols]

# Data for figure
fig_dat <- extinction_grandtotals[scenario_diet %in% c('baseline', 'planetaryhealth', 'usstyle'), .(total = sum(flow_inbound_total)), by = .(scenario_diet, scenario_waste)]

names_dat <- data.frame(scenario_diet = c('baseline', 'usstyle', 'planetaryhealth'), 
                        long_name = c('Baseline\nAmerican diet', 'USDA-recommended\nhealthy diet', 'Planetary\nHealth diet'))
names_waste <- expand.grid(scenario_diet = c('baseline', 'usstyle', 'planetaryhealth'), scenario_waste = c('baseline', 'allavoidable'))
names_waste$long_name = rep(c('Baseline waste', 'Waste -50%'), each=3)

ggplot(fig_dat, aes(y = total, x = scenario_diet)) +
  geom_col(position = 'dodge', aes(fill = scenario_waste, group = interaction(scenario_diet, scenario_waste))) +
  geom_text(data = names_dat, aes(y = 260, label = long_name), family = 'fgm', size = 5) +
  geom_text(data = names_waste, aes(y = 10, label = long_name), color = 'white', angle = 90, hjust = 0, vjust = rep(c(-2.2, 2.9), each = 3), family = 'fgm', size = 5) +
  theme_classic(base_family = 'fgm', base_size = 16) +
  scale_fill_brewer(palette = 'Set1') +
  scale_y_continuous(name = 'extinctions due to USA food consumption', expand = c(0, 0), limits = c(0, 275)) +
  theme(legend.position = 'none', axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(), axis.text.y = element_text(color = 'black'))
ggsave('C:/Users/qdread/onedrive_usda/virtualland_ms/pressreleasefigure.png', height = 4, width = 6, dpi = 400)

# Alternative capitalization
names_dat <- data.frame(scenario_diet = c('baseline', 'usstyle', 'planetaryhealth'), 
                        long_name = c('Baseline\nAmerican diet', 'USDA-recommended\nhealthy diet', 'Planetary\nHealth diet'))


ggplot(fig_dat, aes(y = total, x = scenario_diet)) +
  geom_col(position = 'dodge', aes(fill = scenario_waste, group = interaction(scenario_diet, scenario_waste))) +
  geom_text(data = names_dat, aes(y = 260, label = long_name), family = 'fgm', size = 5) +
  geom_text(data = names_waste, aes(y = 10, label = long_name), color = 'white', angle = 90, hjust = 0, vjust = rep(c(-2.2, 2.9), each = 3), family = 'fgm', size = 5) +
  theme_classic(base_family = 'fgm', base_size = 16) +
  scale_fill_brewer(palette = 'Set1') +
  scale_y_continuous(name = 'Extinctions Due To U.S. Food Consumption', expand = c(0, 0), limits = c(0, 275)) +
  theme(legend.position = 'none', axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(), axis.text.y = element_text(color = 'black'))
ggsave('C:/Users/qdread/onedrive_usda/virtualland_ms/pressreleasefigurev2.png', height = 4, width = 6, dpi = 400)

####### percentages for foreign vs dom
extgtotals <- county_extinction_flow_sums[, lapply(.SD, sum), by = .(scenario_diet, scenario_waste), .SDcols = sum_cols]
extgtotals[, foreign_rel := round(flow_inbound_foreign/flow_inbound_foreign[2]-1,2)]
extgtotals[, domestic_rel := round(flow_inbound_domestic / flow_inbound_domestic[2]-1,2)]
