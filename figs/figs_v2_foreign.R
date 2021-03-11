# Figures and/or maps to summarize foreign incoming land and biodiversity transfers, and compare them to the domestic ones
# QDR / Virtualland / 19 Feb 2021

source('figs/figs_v2_loaddata.R')

# Additionally, load foreign ecoregion map and country map
global_eco_map <- st_read('data/raw_data/landuse/ecoregions/tnc_global_equalarea.gpkg')
global_country_map <- st_read('data/raw_data/landuse/ecoregions/countries_global_equalarea.gpkg') %>%
  select(NAME_LONG, ISO_A3)

# Summary figs comparing land totals --------------------------------------

# Land imported to the United States virtually, according to FAOSTAT
# By country

fill_fvsd <- scale_fill_manual(values = setNames(okabe_colors[c(3, 2)], c('domestic', 'foreign')))

library(data.table)
setDT(foreign_vlt_export)
setDT(county_land_flow_sums)
setnames(foreign_vlt_export, 
         old = c('ECO_CODE', 'ECO_NAME', 'NAME_LONG', 'REGION_UN', 'SUBREGION'),
         new = c('TNC', 'TNC_name', 'country_name', 'region_UN', 'subregion_UN'))

foreign_vlt_countries <- foreign_vlt_export[, lapply(.SD, sum, na.rm = TRUE), 
                                            by = .(scenario_diet, scenario_waste, country_name, ISO_A3, region_UN, subregion_UN), 
                                            .SDcols = patterns('^V')]

foreign_vlt_countries[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline'][order(-VLT_pasture)]
foreign_vlt_countries[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline'][order(-VLT_annual)]
foreign_vlt_countries[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline'][order(-VLT_permanent)]

# Top exporters of virtual land to the United States, baseline case.
foreign_vlt_countries[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline']

### Compare domestic and foreign VLT into the USA.

# Sum the foreign vlt across all countries.
foreign_vlt_sum <- foreign_vlt_export[, lapply(.SD, sum, na.rm = TRUE), by = .(scenario_diet, scenario_waste), .SDcols = patterns('_region')]
setnames(foreign_vlt_sum, gsub('(VLT_)|(_region)', '', names(foreign_vlt_sum)))
foreign_vlt_sum[, annual := annual + mixed/2]
foreign_vlt_sum[, permanent := permanent + mixed/2]
foreign_vlt_sum[, mixed := NULL]
foreign_vlt_sum <- melt(foreign_vlt_sum, id.vars = c('scenario_diet', 'scenario_waste'), variable.name = 'land_type', value.name = 'foreign')

# sum the domestic vlt across all counties. Convert to ha
domestic_vlt_sum <- county_land_flow_sums[, .(domestic = sum(flow_inbound)/1e4), by = .(scenario_diet, scenario_waste, land_type)]
domestic_vlt_sum[, land_type := gsub('(_cropland)|(land)', '', land_type)]

all_vlt_sum <- foreign_vlt_sum[domestic_vlt_sum, on = .NATURAL]
all_vlt_sum <- melt(all_vlt_sum, id.vars = c('scenario_diet', 'scenario_waste', 'land_type'), variable.name = 'origin', value.name = 'VLT')

# Reorder factor levels
all_vlt_sum[, scenario_diet := factor(scenario_diet, levels = unique(scenario_diet))]
all_vlt_sum[, scenario_waste := factor(scenario_waste, levels = unique(scenario_waste))]

ggplot(all_vlt_sum[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline'], aes(x = land_type, y = VLT, fill = origin)) +
  geom_col(position = 'dodge') +
  fill_fvsd +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)))

# By scenario
p_vlt_fvsd <- ggplot(all_vlt_sum, aes(x = land_type, y = VLT, fill = origin)) +
  facet_grid(scenario_waste ~ scenario_diet, labeller = scenario_labeller) +
  geom_col(position = 'dodge') +
  fill_fvsd +
  scale_y_continuous(name = 'Land consumed in USA (ha/yr)', expand = expansion(mult = c(0, 0.01))) +
  scale_x_discrete(name = 'Land use type') +
  theme(legend.position = c(0.95, 0.92), legend.background = element_blank())

p_vlt_fvsd <- label_scenario_categories(p_vlt_fvsd)

# Summary figs comparing species lost -------------------------------------

setDT(foreign_extinction_import)
setDT(foreign_extinction_export)
setDT(county_extinction_flow_sums)

foreign_extinction_sum <- foreign_extinction_import[, .(foreign = sum(species_lost)), by = .(scenario_diet, scenario_waste, land_use, taxon)]

county_extinction_flow_sums <- tidyr::separate(county_extinction_flow_sums, scenario, into = c('D', 'scenario_diet', 'W', 'scenario_waste'), sep = '_')
county_extinction_flow_sums[, c('D','W') := NULL]

domestic_extinction_sum <- county_extinction_flow_sums[, .(domestic = sum(extinction_inbound)), by = .(scenario_diet, scenario_waste, land_use, taxon)]

all_extinction_sum <- foreign_extinction_sum[domestic_extinction_sum, on = .NATURAL]
all_extinction_sum <- melt(all_extinction_sum, id.vars = c('scenario_diet', 'scenario_waste', 'land_use', 'taxon'), variable.name = 'origin', value.name = 'extinctions')

all_extinction_sum[, scenario_diet := factor(scenario_diet, levels = unique(scenario_diet))]
all_extinction_sum[, scenario_waste := factor(scenario_waste, levels = unique(scenario_waste))]

ggplot(all_extinction_sum[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline'], aes(x = land_use, y = extinctions, fill = origin)) +
  facet_wrap(~ taxon, scales = 'free_y') +
  geom_col(position = 'dodge') +
  fill_fvsd +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
  labs(x = 'land use type')

# By scenario: plants
p_plantextinction_fvsd <- ggplot(all_extinction_sum[taxon %in% 'plants'], aes(x = land_use, y = extinctions, fill = origin)) +
  facet_grid(scenario_waste ~ scenario_diet, labeller = scenario_labeller) +
  geom_col(position = 'dodge') +
  fill_fvsd +
  scale_y_continuous(name = 'Plant extinctions imported to USA', expand = expansion(mult = c(0, 0.01))) +
  scale_x_discrete(name = 'Land use type') +
  theme(legend.position = c(0.95, 0.92), legend.background = element_blank())

p_plantextinction_fvsd <- label_scenario_categories(p_plantextinction_fvsd)

# By scenario, all taxa stacked together.
# Sum up across land types, and sum animals vs plants
all_extinction_sum[, kingdom := ifelse(taxon == 'plants', 'plants', 'animals')]
all_extinction_sum_kingdom <- all_extinction_sum[, .(extinctions = sum(extinctions)), by = .(scenario_diet, scenario_waste, kingdom, origin)]
p_taxa_fvsd <- ggplot(all_extinction_sum, aes(x = kingdom, y = extinctions, fill = origin)) +
  fill_fvsd +
  facet_grid(scenario_waste ~ scenario_diet, labeller = scenario_labeller) +
  geom_col(position = 'dodge') 

# By scenario: animals
p_animal_fvsd <- ggplot()
all_extinction_sum_kingxland <- all_extinction_sum[, .(extinctions = sum(extinctions)), by = .(scenario_diet, scenario_waste, land_use, kingdom, origin)]
p_animal_fvsd <- ggplot(all_extinction_sum[kingdom %in% 'animals'], aes(x = land_use, y = extinctions, fill = origin)) +
  fill_fvsd +
  facet_grid(scenario_waste ~ scenario_diet, labeller = scenario_labeller) +
  scale_y_continuous(name = 'Animal extinctions imported to USA', expand = expansion(mult = c(0, 0.01))) +
  geom_bar(stat = 'sum', position = 'dodge', show.legend = c(fill = TRUE, size = FALSE)) +
  scale_x_discrete(name = 'Land use type') +
  theme(legend.position = c(0.95, 0.92), legend.background = element_blank())

p_animal_fvsd <- label_scenario_categories(p_animal_fvsd)

ggsave(file.path(fp_fig, 'foreign_vs_domestic_vlt_by_scenario.png'), p_vlt_fvsd, height = 9, width = 12, dpi = 400)
ggsave(file.path(fp_fig, 'foreign_vs_domestic_plantextinctions_by_scenario.png'), p_plantextinction_fvsd, height = 9, width = 12, dpi = 400)
ggsave(file.path(fp_fig, 'foreign_vs_domestic_animalextinctions_by_scenario.png'), p_animal_fvsd, height = 9, width = 12, dpi = 400)


# Sum foreign and domestic extinctions ------------------------------------

# Create barplots similar to those made for domestic only for all scenarios
# Use all_extinction_sum
# Use ggpattern package to fill in bars with patterns.

# animals + plants x foreign + domestic, ignore land use.
all_extinction_sum[, kingdom := ifelse(taxon == 'plants', 'plants', 'animals')]
extinction_grandtotals <- all_extinction_sum[, .(extinctions = sum(extinctions)), by = .(scenario_diet, scenario_waste, origin, kingdom)]

library(ggpattern)

ggplot(extinction_grandtotals, aes(y = extinctions, x = scenario_waste, fill = kingdom, pattern = origin)) +
  facet_grid(. ~ scenario_diet) +
  geom_bar_pattern(position = 'stack', stat = 'identity') 
#FIXME this needs to be completed.


# Maps: foreign exports to counties ---------------------------------------

# Threats shown as exports from the originating ecoregion and originating country, depending on how they are totaled.
foreign_extinction_export_tnc <- foreign_extinction_export[, .(species_lost = sum(species_lost, na.rm = TRUE)), by = .(scenario_diet, scenario_waste, TNC, TNC_name, land_use, taxon)]
foreign_extinction_export_country <- foreign_extinction_export[, .(species_lost = sum(species_lost, na.rm = TRUE)), by = .(scenario_diet, scenario_waste, country_name, ISO_A3, land_use, taxon)]

# Test map, countries, baseline, animals only
extinction_country_baseline_animals <- foreign_extinction_export_country[
  scenario_diet %in% "baseline" & scenario_waste %in% "baseline" & !taxon %in% 'plants', 
  .(species_lost = sum(species_lost, na.rm = TRUE)),
  by = .(country_name, ISO_A3)]

country_map_toplot <- global_country_map %>%
  st_transform("+proj=robin") %>%
  rename(country_name = NAME_LONG) %>%
  left_join(extinction_country_baseline_animals)

ggplot(country_map_toplot) +
  geom_sf(aes(fill = species_lost)) +
  scale_fill_viridis_c()

# Test map, ecoregions, baseline, animals only
extinction_tnc_baseline_animals <- foreign_extinction_export_tnc[
  scenario_diet %in% "baseline" & scenario_waste %in% "baseline" & !taxon %in% 'plants', 
  .(species_lost = sum(species_lost, na.rm = TRUE)),
  by = .(TNC, TNC_name)]

tnc_map_toplot <- global_eco_map %>%
  st_transform("+proj=robin") %>%
  rename(TNC = ECO_CODE, TNC_name = ECO_NAME) %>%
  select(TNC, TNC_name) %>%
  left_join(extinction_tnc_baseline_animals)

ggplot(tnc_map_toplot) +
  geom_sf(aes(fill = species_lost)) +
  scale_fill_viridis_c()
