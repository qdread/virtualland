# Black on white figures: stacked bar, both full and simplified, for PPTX 
# Can be run locally
library(ggplot2)
library(data.table)
library(Rutilitybelt)
library(ggpattern)
load('~/GitHub/foodwaste/biodiversity-farm2fork/data/all_app_data.RData')

# Calculate grand totals. For extinctions, separate into plants and animals (vertebrates) but ignore other taxa
sum_cols <- grep('flow_inbound', names(county_land_flow_sums), value = TRUE)
land_grandtotals <- county_land_flow_sums[, lapply(.SD, sum), by = .(scenario_diet, scenario_waste, land_type), .SDcols = sum_cols]
county_extinction_flow_sums[, kingdom := ifelse(taxon == 'plants', 'plants', 'vertebrates')]
extinction_grandtotals <- county_extinction_flow_sums[, lapply(.SD, sum), by = .(scenario_diet, scenario_waste, kingdom), .SDcols = sum_cols]

# Convert grand total dataframes to long form
land_grandtotals_long <- melt(land_grandtotals, id.vars = c('scenario_diet', 'scenario_waste', 'land_type'))
extinction_grandtotals_long <- melt(extinction_grandtotals, id.vars = c('scenario_diet', 'scenario_waste', 'kingdom'))

fill_dark <- scale_fill_brewer(palette = 'Dark2')
okabe_colors <- palette.colors(n = 9, palette = 'Okabe-Ito')

diet_levels_ordered <- c('baseline', 'usstyle', 'medstyle', 'vegetarian', 'planetaryhealth')
waste_levels_ordered <- c('baseline', 'preconsumer', 'consumer', 'allavoidable')
land_levels_ordered <- c('annual', 'permanent', 'pasture')

# Lookup tables for longer legend names
diet_long_names <- data.frame(scenario_diet = diet_levels_ordered,
                              long_name = c('baseline diet', 'healthy US-style (USDA)', 'healthy Mediterranean-style (USDA)', 'healthy vegetarian (USDA)', 'planetary health (Lancet)'),
                              medium_name = c('baseline diet', 'USDA US-style', 'USDA Mediterranean-style', 'USDA vegetarian', 'planetary health'))
waste_long_names <- data.frame(scenario_waste = waste_levels_ordered,
                               long_name = c('no waste reduction', 'pre-consumer waste cut 50%', 'consumer waste cut 50%', 'all waste cut 50%'),
                               medium_name = c('no reduction', 'pre-consumer -50%', 'consumer -50%', 'all -50%'))

# Land: waste scenarios only ----------------------------------------------

# Total
plwtotal <- ggplot(land_grandtotals_long[scenario_diet %in% 'baseline' & variable %in% 'flow_inbound_total'], aes(x = scenario_waste, y = value/1e4, fill = land_type)) +
  geom_bar(position = 'stack', stat = 'identity', color = 'black') +
  scale_x_discrete(name = 'waste scenario', labels = c('baseline', '50% waste reduction')) +
  scale_y_continuous(name = 'land footprint (million ha)', expand = expansion(mult = c(0, 0.01))) +
  scale_fill_manual(name = 'land use', values = as.character(okabe_colors[c('orange', 'blue', 'vermillion')]), labels = c('annual crops', 'permanent crops', 'pastureland')) +
  theme_black() +
  theme(strip.text = element_text(size = rel(0.7)),
        legend.title = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

# Split by domestic and foreign (Just split the bar, not pattern like a fool)
plwsplit <- ggplot(land_grandtotals_long[scenario_diet %in% 'baseline' & !variable %in% 'flow_inbound_total'], aes(x = scenario_waste, y = value/1e4, fill = land_type)) +
  geom_bar(position = 'stack', stat = 'identity', color = 'black') +
  facet_wrap(~ variable, labeller = labeller(variable = c(flow_inbound_domestic = 'domestic origin', flow_inbound_foreign = 'foreign origin'))) +
  scale_x_discrete(name = 'waste scenario', labels = c('baseline', '50% waste reduction')) +
  scale_y_continuous(name = 'land footprint (million ha)', expand = expansion(mult = c(0, 0.01))) +
  scale_fill_manual(name = 'land use', values = as.character(okabe_colors[c('orange', 'blue', 'vermillion')]), labels = c('annual crops', 'permanent crops', 'pastureland')) +
  theme_black() +
  theme(strip.text = element_text(size = rel(0.7)),
        legend.title = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


# Land: diet scenarios only -----------------------------------------------

dietlabs <- stringr::str_wrap(diet_long_names$medium_name, 10)

# Total
pldtotal <- ggplot(land_grandtotals_long[scenario_waste %in% 'baseline' & variable %in% 'flow_inbound_total'], aes(x = scenario_diet, y = value/1e4, fill = land_type)) +
  geom_bar(position = 'stack', stat = 'identity', color = 'black') +
  scale_x_discrete(name = 'diet scenario', labels = dietlabs) +
  scale_y_continuous(name = 'land footprint (million ha)', expand = expansion(mult = c(0, 0.01))) +
  scale_fill_manual(name = 'land use', values = as.character(okabe_colors[c('orange', 'blue', 'vermillion')]), labels = c('annual crops', 'permanent crops', 'pastureland')) +
  theme_black() +
  theme(strip.text = element_text(size = rel(0.7)),
        legend.title = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

# Split domestic and foreign
pldsplit <- ggplot(land_grandtotals_long[scenario_waste %in% 'baseline' & !variable %in% 'flow_inbound_total'], aes(x = scenario_diet, y = value/1e4, fill = land_type)) +
  geom_bar(position = 'stack', stat = 'identity', color = 'black') +
  facet_wrap(~ variable, labeller = labeller(variable = c(flow_inbound_domestic = 'domestic origin', flow_inbound_foreign = 'foreign origin'))) +
  scale_x_discrete(name = 'diet scenario', labels = dietlabs) +
  scale_y_continuous(name = 'land footprint (million ha)', expand = expansion(mult = c(0, 0.01))) +
  scale_fill_manual(name = 'land use', values = as.character(okabe_colors[c('orange', 'blue', 'vermillion')]), labels = c('annual crops', 'permanent crops', 'pastureland')) +
  theme_black() +
  theme(strip.text = element_text(size = rel(0.7)),
        legend.title = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


# Land: combine scenarios ------------------------------------------------------

# Total
plctotal <- ggplot(land_grandtotals_long[variable %in% 'flow_inbound_total'], aes(x = scenario_diet, y = value/1e4, fill = land_type)) +
  geom_bar(position = 'stack', stat = 'identity', color = 'black') +
  facet_wrap(~ scenario_waste, labeller = labeller(scenario_waste = c(baseline = 'baseline food waste', allavoidable = '50% reduction'))) +
  scale_x_discrete(name = 'diet scenario', labels = dietlabs) +
  scale_y_continuous(name = 'land footprint (million ha)', expand = expansion(mult = c(0, 0.01))) +
  scale_fill_manual(name = 'land use', values = as.character(okabe_colors[c('orange', 'blue', 'vermillion')]), labels = c('annual crops', 'permanent crops', 'pastureland')) +
  theme_black() +
  theme(strip.text = element_text(size = rel(0.7)),
        legend.title = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

# Split domestic and foreign
plcsplit <- ggplot(land_grandtotals_long[!variable %in% 'flow_inbound_total'], aes(x = scenario_diet, y = value/1e4, fill = land_type)) +
  geom_bar(position = 'stack', stat = 'identity', color = 'black') +
  facet_grid(variable ~ scenario_waste, labeller = labeller(variable = c(flow_inbound_domestic = 'domestic origin', flow_inbound_foreign = 'foreign origin'), scenario_waste = c(baseline = 'baseline food waste', allavoidable = '50% reduction'))) +
  scale_x_discrete(name = 'diet scenario', labels = dietlabs) +
  scale_y_continuous(name = 'land footprint (million ha)', expand = expansion(mult = c(0, 0.01))) +
  scale_fill_manual(name = 'land use', values = as.character(okabe_colors[c('orange', 'blue', 'vermillion')]), labels = c('annual crops', 'permanent crops', 'pastureland')) +
  theme_black() +
  theme(strip.text = element_text(size = rel(0.7)),
        legend.title = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


# Extinction: waste scenarios only ----------------------------------------------

# Total
pewtotal <- ggplot(extinction_grandtotals_long[scenario_diet %in% 'baseline' & variable %in% 'flow_inbound_total'], aes(x = scenario_waste, y = value, fill = kingdom)) +
  geom_bar(position = 'stack', stat = 'identity', color = 'black') +
  scale_x_discrete(name = 'waste scenario', labels = c('baseline', '50% waste reduction')) +
  scale_y_continuous(name = 'extinction footprint (species)', expand = expansion(mult = c(0, 0.01))) +
  scale_fill_manual(name = 'taxon', values = as.character(okabe_colors[c('bluishgreen', 'reddishpurple')])) +
  theme_black() +
  theme(strip.text = element_text(size = rel(0.7)),
        legend.title = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

# Split by domestic and foreign (Just split the bar, not pattern like a fool)
pewsplit <- ggplot(extinction_grandtotals_long[scenario_diet %in% 'baseline' & !variable %in% 'flow_inbound_total'], aes(x = scenario_waste, y = value, fill = kingdom)) +
  geom_bar(position = 'stack', stat = 'identity', color = 'black') +
  facet_wrap(~ variable, labeller = labeller(variable = c(flow_inbound_domestic = 'domestic origin', flow_inbound_foreign = 'foreign origin'))) +
  scale_x_discrete(name = 'waste scenario', labels = c('baseline', '50% waste reduction')) +
  scale_y_continuous(name = 'extinction footprint (species)', expand = expansion(mult = c(0, 0.01))) +
  scale_fill_manual(name = 'taxon', values = as.character(okabe_colors[c('bluishgreen', 'reddishpurple')])) +
  theme_black() +
  theme(strip.text = element_text(size = rel(0.7)),
        legend.title = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


# Extinctions: diet scenarios only -----------------------------------------------

# Total
pedtotal <- ggplot(extinction_grandtotals_long[scenario_waste %in% 'baseline' & variable %in% 'flow_inbound_total'], aes(x = scenario_diet, y = value, fill = kingdom)) +
  geom_bar(position = 'stack', stat = 'identity', color = 'black') +
  scale_x_discrete(name = 'diet scenario', labels = dietlabs) +
  scale_y_continuous(name = 'extinction footprint (species)', expand = expansion(mult = c(0, 0.01))) +
  scale_fill_manual(name = 'taxon', values = as.character(okabe_colors[c('bluishgreen', 'reddishpurple')])) +
  theme_black() +
  theme(strip.text = element_text(size = rel(0.7)),
        legend.title = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

# Split domestic and foreign
pedsplit <- ggplot(extinction_grandtotals_long[scenario_waste %in% 'baseline' & !variable %in% 'flow_inbound_total'], aes(x = scenario_diet, y = value, fill = kingdom)) +
  geom_bar(position = 'stack', stat = 'identity', color = 'black') +
  facet_wrap(~ variable, labeller = labeller(variable = c(flow_inbound_domestic = 'domestic origin', flow_inbound_foreign = 'foreign origin'))) +
  scale_x_discrete(name = 'diet scenario', labels = dietlabs) +
  scale_y_continuous(name = 'extinction footprint (species)', expand = expansion(mult = c(0, 0.01))) +
  scale_fill_manual(name = 'taxon', values = as.character(okabe_colors[c('bluishgreen', 'reddishpurple')])) +
  theme_black() +
  theme(strip.text = element_text(size = rel(0.7)),
        legend.title = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


# Extinctions: combine scenarios ------------------------------------------------------

# Total
pectotal <- ggplot(extinction_grandtotals_long[variable %in% 'flow_inbound_total'], aes(x = scenario_diet, y = value, fill = kingdom)) +
  geom_bar(position = 'stack', stat = 'identity', color = 'black') +
  facet_wrap(~ scenario_waste, labeller = labeller(scenario_waste = c(baseline = 'baseline food waste', allavoidable = '50% reduction'))) +
  scale_x_discrete(name = 'diet scenario', labels = dietlabs) +
  scale_y_continuous(name = 'extinction footprint (species)', expand = expansion(mult = c(0, 0.01))) +
  scale_fill_manual(name = 'taxon', values = as.character(okabe_colors[c('bluishgreen', 'reddishpurple')])) +
  theme_black() +
  theme(strip.text = element_text(size = rel(0.7)),
        legend.title = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

# Split domestic and foreign
pecsplit <- ggplot(extinction_grandtotals_long[!variable %in% 'flow_inbound_total'], aes(x = scenario_diet, y = value, fill = kingdom)) +
  geom_bar(position = 'stack', stat = 'identity', color = 'black') +
  facet_grid(variable ~ scenario_waste, labeller = labeller(variable = c(flow_inbound_domestic = 'domestic origin', flow_inbound_foreign = 'foreign origin'), scenario_waste = c(baseline = 'baseline food waste', allavoidable = '50% reduction'))) +
  scale_x_discrete(name = 'diet scenario', labels = dietlabs) +
  scale_y_continuous(name = 'extinction footprint (species)', expand = expansion(mult = c(0, 0.01))) +
  scale_fill_manual(name = 'taxon', values = as.character(okabe_colors[c('bluishgreen', 'reddishpurple')])) +
  theme_black() +
  theme(strip.text = element_text(size = rel(0.7)),
        legend.title = element_blank(),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


# Save results ------------------------------------------------------------

fp <- 'C:/Users/qdread/onedrive_usda/presentations/ncsupmb2022'
ggsave(file.path(fp, 'stackbar_land_waste_total.png'), plwtotal, height = 4, width = 5, dpi = 400)
ggsave(file.path(fp, 'stackbar_land_waste_dvsf.png'), plwsplit, height = 4, width = 6, dpi = 400)
ggsave(file.path(fp, 'stackbar_ext_waste_total.png'), pewtotal, height = 4, width = 5, dpi = 400)
ggsave(file.path(fp, 'stackbar_ext_waste_dvsf.png'), pewsplit, height = 4, width = 6, dpi = 400)

ggsave(file.path(fp, 'stackbar_land_diet_total.png'), pldtotal, height = 4, width = 6, dpi = 400)
ggsave(file.path(fp, 'stackbar_land_diet_dvsf.png'), pldsplit, height = 4, width = 9, dpi = 400)
ggsave(file.path(fp, 'stackbar_ext_diet_total.png'), pedtotal, height = 4, width = 6, dpi = 400)
ggsave(file.path(fp, 'stackbar_ext_diet_dvsf.png'), pedsplit, height = 4, width = 9, dpi = 400)

ggsave(file.path(fp, 'stackbar_land_comb_total.png'), plctotal, height = 4, width = 9, dpi = 400)
ggsave(file.path(fp, 'stackbar_land_comb_dvsf.png'), plcsplit, height = 7, width = 9, dpi = 400)
ggsave(file.path(fp, 'stackbar_ext_comb_total.png'), pectotal, height = 4, width = 9, dpi = 400)
ggsave(file.path(fp, 'stackbar_ext_comb_dvsf.png'), pecsplit, height = 7, width = 9, dpi = 400)

# Show overall domestic vs foreign land vs biodiv -------------------------

# Probably best to show as a table?
lgt <- land_grandtotals[scenario_diet == 'baseline' & scenario_waste == 'baseline', lapply(.SD, sum), .SDcols = patterns('flow')]
egt <- extinction_grandtotals[scenario_diet == 'baseline' & scenario_waste == 'baseline', lapply(.SD, sum), .SDcols = patterns('flow')]

allgt <- data.frame(flow = c('land', 'extinctions'), rbind(lgt,egt))
