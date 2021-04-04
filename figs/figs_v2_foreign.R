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
  theme(legend.position = c(0.05, 0.92), legend.background = element_blank())

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
all_extinction_sum_kingxland <- all_extinction_sum[, .(extinctions = sum(extinctions)), by = .(scenario_diet, scenario_waste, land_use, kingdom, origin)]
p_animal_fvsd <- ggplot(all_extinction_sum[kingdom %in% 'animals'], aes(x = land_use, y = extinctions, fill = origin)) +
  fill_fvsd +
  facet_grid(scenario_waste ~ scenario_diet, labeller = scenario_labeller) +
  scale_y_continuous(name = 'Animal extinctions imported to USA', expand = expansion(mult = c(0, 0.01))) +
  geom_bar(stat = 'sum', position = 'dodge', show.legend = c(fill = TRUE, size = FALSE)) +
  scale_x_discrete(name = 'Land use type') +
  theme(legend.position = c(0.05, 0.92), legend.background = element_blank())

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

p_ext_grandtotals <- ggplot(extinction_grandtotals, aes(y = extinctions, x = scenario_waste, pattern = origin, fill = kingdom)) +
  facet_grid(. ~ scenario_diet, labeller = labeller(scenario_diet = setNames(diet_long_names$medium_name, diet_long_names$scenario_diet))) +
  geom_bar_pattern(position = 'stack', stat = 'identity', pattern_fill = 'black', pattern_spacing = 0.02, color = 'black') +
  scale_x_discrete(name = 'waste scenario', labels = c('baseline', 'pre-consumer -50%', 'consumer -50%', 'all -50%')) +
  scale_y_continuous(name = 'species committed to extinction', expand = expansion(mult = c(0, 0.01))) +
  scale_pattern_manual(values = c('circle', 'none')) +
  scale_fill_manual(values = as.character(okabe_colors[c('reddishpurple', 'bluishgreen')])) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        strip.text = element_text(size = rel(0.7)),
        legend.title = element_text(size = rel(0.8)),
        legend.position = c(0.93, 0.78),
        legend.background = element_blank(),
        legend.spacing.y = unit(0.1, 'cm')) +
  guides(fill = guide_legend(override.aes = list(pattern = 'none')))

ggsave(file.path(fp_fig, 'foreign_vs_domestic_extinction_grandtotals.png'), p_ext_grandtotals, height = 6*0.8, width = 10*0.8, dpi = 400)


# Sum foreign and domestic land -------------------------------------------

# Make the same pattern fill barplot as above but with virtual land.

p_land_grandtotals <- ggplot(all_vlt_sum[!land_type %in% 'total'], aes(y = VLT/1e6, x = scenario_waste, pattern = origin, fill = land_type)) +
  facet_grid(. ~ scenario_diet, labeller = labeller(scenario_diet = setNames(diet_long_names$medium_name, diet_long_names$scenario_diet))) +
  geom_bar_pattern(position = 'stack', stat = 'identity', pattern_fill = 'black', pattern_spacing = 0.02, color = 'black') +
  scale_pattern_manual(values = c('circle', 'none')) +
  scale_x_discrete(name = 'waste scenario', labels = c('baseline', 'pre-consumer -50%', 'consumer -50%', 'all -50%')) +
  scale_y_continuous(name = 'land footprint (million ha)', expand = expansion(mult = c(0, 0.01))) +
  scale_fill_manual(name = 'land use', values = as.character(okabe_colors[c('orange', 'blue', 'vermillion')])) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        strip.text = element_text(size = rel(0.7)),
        legend.title = element_text(size = rel(0.8)),
        legend.position = c(0.93, 0.71),
        legend.background = element_blank(),
        legend.spacing.y = unit(0.1, 'cm')) +
  guides(fill = guide_legend(override.aes = list(pattern = 'none')))

ggsave(file.path(fp_fig, 'foreign_vs_domestic_land_grandtotals.png'), p_land_grandtotals, height = 6*0.8, width = 10*0.8, dpi = 400)

# Process data: foreign extinction exports to counties ---------------------------------------

# Threats shown as exports from the originating ecoregion and originating country, depending on how they are totaled.
foreign_extinction_export_tnc <- foreign_extinction_export[, .(species_lost = sum(species_lost, na.rm = TRUE)), by = .(scenario_diet, scenario_waste, TNC, TNC_name, land_use, taxon)]
foreign_extinction_export_country <- foreign_extinction_export[, .(species_lost = sum(species_lost, na.rm = TRUE)), by = .(scenario_diet, scenario_waste, country_name, ISO_A3, land_use, taxon)]

# For both TNC and country maps, calculate sums by land use and by taxon (animals and total)
foreign_extinction_export_tnc_animals <- foreign_extinction_export_tnc[!taxon %in% 'plants', .(species_lost = sum(species_lost)), by = .(scenario_diet, scenario_waste, TNC, TNC_name, land_use)][,
  taxon := 'animals'
]
foreign_extinction_export_country_animals <- foreign_extinction_export_country[!taxon %in% 'plants', .(species_lost = sum(species_lost)), by = .(scenario_diet, scenario_waste, country_name, ISO_A3, land_use)][,
  taxon := 'animals'
]

foreign_extinction_export_tnc_total <- foreign_extinction_export_tnc[, .(species_lost = sum(species_lost)), by = .(scenario_diet, scenario_waste, TNC, TNC_name, land_use)][,
  taxon := 'total'
]
foreign_extinction_export_country_total <- foreign_extinction_export_country[, .(species_lost = sum(species_lost)), by = .(scenario_diet, scenario_waste, country_name, ISO_A3, land_use)][,
  taxon := 'total'
]

foreign_extinction_export_tnc <- rbindlist(list(foreign_extinction_export_tnc, foreign_extinction_export_tnc_animals, foreign_extinction_export_tnc_total), use.names = TRUE)
foreign_extinction_export_country <- rbindlist(list(foreign_extinction_export_country, foreign_extinction_export_country_animals, foreign_extinction_export_country_total), use.names = TRUE)

foreign_extinction_export_tnc_totalland <- foreign_extinction_export_tnc[, .(species_lost = sum(species_lost)), by = .(scenario_diet, scenario_waste, TNC, TNC_name, taxon)][,
  land_use := 'total'
]

foreign_extinction_export_country_totalland <- foreign_extinction_export_country[, .(species_lost = sum(species_lost)), by = .(scenario_diet, scenario_waste, country_name, ISO_A3, taxon)][,
  land_use := 'total'
]

foreign_extinction_export_tnc <- rbindlist(list(foreign_extinction_export_tnc, foreign_extinction_export_tnc_totalland), use.names = TRUE)
foreign_extinction_export_country <- rbindlist(list(foreign_extinction_export_country, foreign_extinction_export_country_totalland), use.names = TRUE)

# Calculate the difference between each scenario and the baseline.
foreign_ext_tnc_base <- foreign_extinction_export_tnc[scenario_waste %in% 'baseline' & scenario_diet %in% 'baseline']
setnames(foreign_ext_tnc_base, old = 'species_lost', new = 'species_lost_baseline')
foreign_ext_tnc_base[, c('scenario_diet', 'scenario_waste') := NULL]

foreign_ext_country_base <- foreign_extinction_export_country[scenario_waste %in% 'baseline' & scenario_diet %in% 'baseline']
setnames(foreign_ext_country_base, old = 'species_lost', new = 'species_lost_baseline')
foreign_ext_country_base[, c('scenario_diet', 'scenario_waste') := NULL]

foreign_extinction_export_tnc <- foreign_ext_tnc_base[foreign_extinction_export_tnc, on = .NATURAL]
foreign_extinction_export_tnc[, species_vs_baseline := species_lost/species_lost_baseline]

foreign_extinction_export_country <- foreign_ext_country_base[foreign_extinction_export_country, on = .NATURAL]
foreign_extinction_export_country[, species_vs_baseline := species_lost/species_lost_baseline]

# Nest into panels
library(Rutilitybelt)

foreign_extinction_tnc_panels <- group_nest_dt(foreign_extinction_export_tnc, scenario_diet, scenario_waste, land_use, taxon)
foreign_extinction_country_panels <- group_nest_dt(foreign_extinction_export_country, scenario_diet, scenario_waste, land_use, taxon)


# Process data: foreign land exports to counties --------------------------

# Get rid of the columns that were not divided out by region weights
cols_remove <- names(foreign_vlt_export)[grepl('VLT', names(foreign_vlt_export)) & !grepl('region', names(foreign_vlt_export))]
foreign_vlt_export[, (cols_remove) := NULL]

# Melt to long form
foreign_vlt_export_long <- melt(foreign_vlt_export, measure.vars = patterns('VLT'), variable.name = 'land_use', value.name = 'VLT')
foreign_vlt_export_long <- foreign_vlt_export_long[, .(scenario_diet, scenario_waste, TNC, TNC_name, country_name, ISO_A3, land_use, VLT)]
foreign_vlt_export_long[, land_use := gsub('(VLT_)|(_region)', '', land_use)]

# Total VLT across land types and then bind.
foreign_vlt_export_long_total <- foreign_vlt_export_long[, .(VLT = sum(VLT, na.rm = TRUE)), by = .(scenario_diet, scenario_waste, TNC, TNC_name, country_name, ISO_A3)]
foreign_vlt_export_long_total[, land_use := 'total']

foreign_vlt_export_long <- rbindlist(list(foreign_vlt_export_long, foreign_vlt_export_long_total), use.names = TRUE)

# Calculate sums by TNC ecoregion and by country separately.
foreign_vlt_tnc <- foreign_vlt_export_long[, .(VLT = sum(VLT)), by = .(scenario_diet, scenario_waste, TNC, TNC_name, land_use)]
foreign_vlt_country <- foreign_vlt_export_long[, .(VLT = sum(VLT)), by = .(scenario_diet, scenario_waste, country_name, ISO_A3, land_use)]

# Calculate the difference between each scenario and the baseline.
foreign_vlt_tnc_base <- foreign_vlt_tnc[scenario_waste %in% 'baseline' & scenario_diet %in% 'baseline']
setnames(foreign_vlt_tnc_base, old = 'VLT', new = 'VLT_baseline')
foreign_vlt_tnc_base[, c('scenario_diet', 'scenario_waste') := NULL]

foreign_vlt_tnc <- foreign_vlt_tnc_base[foreign_vlt_tnc, on = .NATURAL]
foreign_vlt_tnc[, VLT_vs_baseline := VLT/VLT_baseline]
foreign_vlt_tnc[is.nan(VLT_vs_baseline), VLT_vs_baseline := 0]

foreign_vlt_country_base <- foreign_vlt_country[scenario_waste %in% 'baseline' & scenario_diet %in% 'baseline']
setnames(foreign_vlt_country_base, old = 'VLT', new = 'VLT_baseline')
foreign_vlt_country_base[, c('scenario_diet', 'scenario_waste') := NULL]

foreign_vlt_country <- foreign_vlt_country_base[foreign_vlt_country, on = .NATURAL]
foreign_vlt_country[, VLT_vs_baseline := VLT/VLT_baseline]
foreign_vlt_country[is.nan(VLT_vs_baseline), VLT_vs_baseline := 0]

# Nest to panels
foreign_vlt_tnc_panels <- group_nest_dt(foreign_vlt_tnc, scenario_diet, scenario_waste, land_use)
foreign_vlt_country_panels <- group_nest_dt(foreign_vlt_country, scenario_diet, scenario_waste, land_use)

# Draw the maps -----------------------------------------------------------

# Loop through with pmap
# Do relative and absolute plots. PDF takes up too much space so do as images.

country_map_toplot <- global_country_map %>%
  st_transform("+proj=robin") %>%
  rename(country_name = NAME_LONG)

tnc_map_toplot <- global_eco_map %>%
  st_transform("+proj=robin") %>%
  rename(TNC = ECO_CODE, TNC_name = ECO_NAME) %>%
  select(TNC, TNC_name)

div_pal <- scico::scico(15, palette = 'berlin')

# Draw maps: PNG ---------------------------------------------------------------------

calc_range <- function(x) {
  w <- max(abs(range(x, na.rm = TRUE) - 1))
  c(1 - w, 1 + w)
}

pwalk(foreign_extinction_country_panels[taxon %in% c('animals','plants','total')], 
      function(scenario_diet, scenario_waste, land_use, taxon, data) {
        dat <- country_map_toplot %>%
          left_join(data, by = 'country_name')
        
        # Calculate range for divergent palette.
        div_range <- calc_range(data$species_vs_baseline)
        
        p1 <- ggplot(dat) +
          geom_sf(aes(fill = species_lost), size = 0.25) +
          scale_fill_viridis_c(name = 'Extinction\nexport', trans = 'log10',
                               breaks = scales::trans_breaks("log10", function(x) 10^x),
                               labels = scales::trans_format("log10", scales::math_format(10^.x))) +
          theme(legend.position = 'bottom') +
          ggtitle(glue('{scenario_diet} diet x {scenario_waste} waste'), glue('threats to {taxon} from {land_use} land use'))   
        p2 <- ggplot(dat) +
          geom_sf(aes(fill = species_vs_baseline), size = 0.25) +
          scale_fill_gradientn(name = 'Change vs.\nbaseline', colours = div_pal, na.value = 'gray75', limits = div_range) +
          theme(legend.position = 'bottom') +
          ggtitle(glue('{scenario_diet} diet x {scenario_waste} waste'), glue('change vs. baseline in threats to {taxon} from {land_use} land use'))
        ggsave(glue('{fp_fig}/foreign_bioflow_maps/country_{scenario_diet}_{scenario_waste}_{taxon}_{land_use}.png'), p1, height = 5, width = 7, dpi = 200)
        ggsave(glue('{fp_fig}/foreign_bioflow_maps/country_{scenario_diet}_{scenario_waste}_{taxon}_{land_use}_change.png'), p2, height = 5, width = 7, dpi = 200)
      })

pwalk(foreign_extinction_tnc_panels[taxon %in% c('animals','plants','total')], 
      function(scenario_diet, scenario_waste, land_use, taxon, data) {
        dat <- tnc_map_toplot %>%
          left_join(data, by = 'TNC')
        
        # Calculate range for divergent palette.
        div_range <- calc_range(data$species_vs_baseline)
        
        p1 <- ggplot(dat) +
          geom_sf(aes(fill = species_lost), size = 0.25) +
          scale_fill_viridis_c(name = 'Extinction\nexport', trans = 'log10',
                               breaks = scales::trans_breaks("log10", function(x) 10^x),
                               labels = scales::trans_format("log10", scales::math_format(10^.x))) +
          theme(legend.position = 'bottom') +
          ggtitle(glue('{scenario_diet} diet x {scenario_waste} waste'), glue('threats to {taxon} from {land_use} land use'))   
        p2 <- ggplot(dat) +
          geom_sf(aes(fill = species_vs_baseline), size = 0.25) +
          scale_fill_gradientn(name = 'Change vs.\nbaseline', colours = div_pal, na.value = 'gray75', limits = div_range) +
          theme(legend.position = 'bottom') +
          ggtitle(glue('{scenario_diet} diet x {scenario_waste} waste'), glue('change vs. baseline in threats to {taxon} from {land_use} land use'))
        ggsave(glue('{fp_fig}/foreign_bioflow_maps/ecoregion_{scenario_diet}_{scenario_waste}_{taxon}_{land_use}.png'), p1, height = 5, width = 7, dpi = 200)
        ggsave(glue('{fp_fig}/foreign_bioflow_maps/ecoregion_{scenario_diet}_{scenario_waste}_{taxon}_{land_use}_change.png'), p2, height = 5, width = 7, dpi = 200)
      })

pwalk(foreign_vlt_country_panels, 
      function(scenario_diet, scenario_waste, land_use, data) {
        dat <- country_map_toplot %>%
          left_join(data, by = 'country_name')
        
        # Calculate range for divergent palette.
        div_range <- calc_range(data$VLT_vs_baseline)
        
        p1 <- ggplot(dat) +
          geom_sf(aes(fill = VLT), size = 0.25) +
          scale_fill_viridis_c(name = 'Virtual land\nexport', trans = 'log10',
                               breaks = scales::trans_breaks("log10", function(x) 10^x),
                               labels = scales::trans_format("log10", scales::math_format(10^.x))) +
          theme(legend.position = 'bottom') +
          ggtitle(glue('{scenario_diet} diet x {scenario_waste} waste'), glue('virtual exports of {land_use} land to USA'))   
        p2 <- ggplot(dat) +
          geom_sf(aes(fill = VLT_vs_baseline), size = 0.25) +
          scale_fill_gradientn(name = 'Change vs.\nbaseline', colours = div_pal, na.value = 'gray75', limits = div_range) +
          theme(legend.position = 'bottom') +
          ggtitle(glue('{scenario_diet} diet x {scenario_waste} waste'), glue('change vs. baseline in virtual exports of {land_use} land to USA'))
        ggsave(glue('{fp_fig}/foreign_landflow_maps/country_{scenario_diet}_{scenario_waste}_{land_use}.png'), p1, height = 5, width = 7, dpi = 200)
        ggsave(glue('{fp_fig}/foreign_landflow_maps/country_{scenario_diet}_{scenario_waste}_{land_use}_change.png'), p2, height = 5, width = 7, dpi = 200)
      })

pwalk(foreign_vlt_tnc_panels, 
      function(scenario_diet, scenario_waste, land_use, data) {
        dat <- tnc_map_toplot %>%
          left_join(data, by = 'TNC')
        
        # Calculate range for divergent palette.
        div_range <- calc_range(data$VLT_vs_baseline)
        
        p1 <- ggplot(dat) +
          geom_sf(aes(fill = VLT), size = 0.25) +
          scale_fill_viridis_c(name = 'Virtual land\nexport', trans = 'log10',
                               breaks = scales::trans_breaks("log10", function(x) 10^x),
                               labels = scales::trans_format("log10", scales::math_format(10^.x))) +
          theme(legend.position = 'bottom') +
          ggtitle(glue('{scenario_diet} diet x {scenario_waste} waste'), glue('virtual exports of {land_use} land to USA'))   
        p2 <- ggplot(dat) +
          geom_sf(aes(fill = VLT_vs_baseline), size = 0.25) +
          scale_fill_gradientn(name = 'Change vs.\nbaseline', colours = div_pal, na.value = 'gray75', limits = div_range) +
          theme(legend.position = 'bottom') +
          ggtitle(glue('{scenario_diet} diet x {scenario_waste} waste'), glue('change vs. baseline in virtual exports of {land_use} land to USA'))
        ggsave(glue('{fp_fig}/foreign_landflow_maps/ecoregion_{scenario_diet}_{scenario_waste}_{land_use}.png'), p1, height = 5, width = 7, dpi = 200)
        ggsave(glue('{fp_fig}/foreign_landflow_maps/ecoregion_{scenario_diet}_{scenario_waste}_{land_use}_change.png'), p2, height = 5, width = 7, dpi = 200)
      })



# PDF (don't run) ---------------------------------------------------------


pdf('data/cfs_io_analysis/scenario_v2_figs/foreign_bioflow_maps_bycountry.pdf', height = 5, width = 7)
pwalk(foreign_extinction_country_panels[taxon %in% c('animals','plants','total')], 
     function(scenario_diet, scenario_waste, land_use, taxon, data) {
       dat <- country_map_toplot %>%
         left_join(data, by = 'country_name')
       print(
         ggplot(dat) +
           geom_sf(aes(fill = species_lost)) +
           scale_fill_viridis_c(name = 'Extinction\nexport', trans = 'log10',
                                breaks = scales::trans_breaks("log10", function(x) 10^x),
                                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
           theme(legend.position = 'bottom') +
           ggtitle(glue('{scenario_diet} diet x {scenario_waste} waste'), glue('threats to {taxon} from {land_use} land use'))   
       )
     })
dev.off()


pdf('data/cfs_io_analysis/scenario_v2_figs/foreign_landflow_maps_byecoregion.pdf', height = 5, width = 7)
pwalk(foreign_extinction_tnc_panels[taxon %in% c('animals','plants','total')], 
      function(scenario_diet, scenario_waste, land_use, taxon, data) {
        dat <- tnc_map_toplot %>%
          left_join(data, by = 'TNC')
        print(
          ggplot(dat) +
            geom_sf(aes(fill = species_lost)) +
            scale_fill_viridis_c(name = 'Extinction\nexport', trans = 'log10',
                                 breaks = scales::trans_breaks("log10", function(x) 10^x),
                                 labels = scales::trans_format("log10", scales::math_format(10^.x))) +
            theme(legend.position = 'bottom') +
            ggtitle(glue('{scenario_diet} diet x {scenario_waste} waste'), glue('threats to {taxon} from {land_use} land use'))   
        )
      })
dev.off()

