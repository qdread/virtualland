# Ecoregion land and extinction flow maps
# QDR / Virtualland / 15 Feb 2021

source('figs/figs_v2_loaddata.R')

# Flows of land between ecoregions ----------------------------------------

# Sum up the flows across all land types

tnc_land_flow_sumalltypes <- tnc_land_flow_sums %>%
  group_by(scenario_diet, scenario_waste, TNC) %>%
  summarize(flow_inbound = sum(flow_inbound), flow_outbound = sum(flow_outbound)) %>%
  mutate(land_type = 'total_agricultural_land') %>%
  ungroup

# Group and nest the ecoregion landflow dataframe.
# For mapping purposes, anything under 1 hectare rounded to zero.
tnc_land_maps <- tnc_land_flow_sums %>%
  bind_rows(tnc_land_flow_sumalltypes) %>%
  mutate(flow_inbound = if_else(flow_inbound < 10000, 0, flow_inbound),
         flow_outbound = if_else(flow_outbound < 10000, 0, flow_outbound)) %>%
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

# Calculate global scale for log breaks
all_tnc_land_flows <- c(tnc_land_flow_sums$flow_inbound, tnc_land_flow_sumalltypes$flow_inbound, tnc_land_flow_sums$flow_outbound, tnc_land_flow_sumalltypes$flow_outbound)
(range(all_tnc_land_flows[all_tnc_land_flows > 10000], na.rm = TRUE)/1e4) %>% formatC(format = 'e')
tnc_land_breaks <- c(1e0, 1e4, 1e8)

# Annual cropland, permanent cropland, pastureland, and total.
# Do this for all 20 scenarios.
# Log10 transformation is best for viewing pattern. 
tnc_land_maps <- tnc_land_maps %>%
  ungroup %>%
  mutate(plot_title = pmap(tnc_land_maps[, c('land_type','scenario_diet','scenario_waste')], 
                           function(land_type, scenario_diet, scenario_waste) 
                             list(land_type = gsub('_', ' ', land_type), 
                                  diet_name = diet_long_names$long_name[match(scenario_diet, diet_long_names$scenario_diet)], 
                                  waste_name = waste_long_names$long_name[match(scenario_waste, waste_long_names$scenario_waste)],
                                  file_prefix = glue('D_{scenario_diet}_W_{scenario_waste}_{land_type}')))) %>%
  mutate(map_inbound = map2(data, plot_title, 
                            ~ draw_usmap_with_insets(map_data = left_join(tnc_map, .x, by = c('ECO_CODE' = 'TNC')), 
                                                     ak_idx = tnc_ak_idx,
                                                     hi_idx = tnc_hi_idx,
                                                     variable = flow_inbound,
                                                     title = glue('{.y$land_type} imported by ecoregion'),
                                                     subtitle = glue('diet scenario: {.y$diet_name}, waste scenario: {.y$waste_name}'),
                                                     scale_name = 'area (ha)',
                                                     scale_factor = 10000,
                                                     scale_trans = 'log10',
                                                     scale_range = tnc_land_breaks[c(1,length(tnc_land_breaks))],
                                                     scale_breaks = tnc_land_breaks,
                                                     add_theme = theme_void() + theme(legend.position = c(0.62, 0.1),
                                                                                      legend.key.width = unit(0.23, 'in')),
                                                     write_to_file = glue('{fp_fig}/ecoregion_landflow_maps/{.y$file_prefix}_inbound.png'),
                                                     img_size = c(7, 7))),
         map_outbound = map2(data, plot_title,
                             ~ draw_usmap_with_insets(map_data = left_join(tnc_map, .x, by = c('ECO_CODE' = 'TNC')), 
                                                      ak_idx = tnc_ak_idx,
                                                      hi_idx = tnc_hi_idx,
                                                      variable = flow_outbound,
                                                      title = glue('{.y$land_type} exported by ecoregion'),
                                                      subtitle = glue('diet scenario: {.y$diet_name}, waste scenario: {.y$waste_name}'),
                                                      scale_name = 'area (ha)',
                                                      scale_factor = 10000,
                                                      scale_trans = 'log10',
                                                      scale_range = tnc_land_breaks[c(1,length(tnc_land_breaks))],
                                                      scale_breaks = tnc_land_breaks,
                                                      add_theme = theme_void() + theme(legend.position = c(0.62, 0.1),
                                                                                       legend.key.width = unit(0.23, 'in')),
                                                      write_to_file = glue('{fp_fig}/ecoregion_landflow_maps/{.y$file_prefix}_outbound.png'),
                                                      img_size = c(7, 7))))


# Flows of extinctions between ecoregions (plots) -------------------------

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

# Flows of extinctions between ecoregions (maps) --------------------------

# Use medium intensity only
# Sum up flows across taxa first
tnc_extinction_flow_sums_medacrosstaxa <- tnc_extinction_flow_sums %>%
  rename(land_type = land_use) %>%
  filter(intensity == 'med') %>%
  group_by(scenario_diet, scenario_waste, TNC, land_type) %>%
  summarize(flow_inbound = sum(flow_inbound, na.rm = TRUE), flow_outbound = sum(flow_outbound, na.rm = TRUE))

# Sum up the flows across all land types (ignoring taxa for now)
tnc_extinction_flow_sumalltypes <- tnc_extinction_flow_sums_medacrosstaxa %>%
  group_by(scenario_diet, scenario_waste, TNC) %>%
  summarize(flow_inbound = sum(flow_inbound, na.rm = TRUE), flow_outbound = sum(flow_outbound, na.rm = TRUE)) %>%
  mutate(land_type = 'total_agricultural_land') %>%
  ungroup

# Group and nest the ecoregion extinction flow dataframe.
tnc_extinction_maps <- tnc_extinction_flow_sums_medacrosstaxa %>%
  ungroup %>%
  bind_rows(tnc_extinction_flow_sumalltypes) %>%
  arrange(scenario_diet, scenario_waste, land_type, TNC) %>%
  mutate(flow_net = flow_outbound - flow_inbound,
         flow_outbound = if_else(flow_outbound == 0, as.numeric(NA), flow_outbound)) %>%
  group_by(scenario_diet, scenario_waste, land_type) %>%
  nest

# Get index of alaska and hawaii. 
tnc_ak_idx <- substr(tnc_map$ECO_CODE, 1, 4) %in% c('NA06', 'NA11') | tnc_map$ECO_CODE %in% c('NA0509', 'NA0518')
tnc_hi_idx <- substr(tnc_map$ECO_CODE, 1, 2) == 'OC'

# Calculate global scale for log breaks
all_tnc_extinction_flows <- c(tnc_extinction_flow_sums_medacrosstaxa$flow_inbound, tnc_extinction_flow_sumalltypes$flow_inbound, tnc_extinction_flow_sums_medacrosstaxa$flow_outbound, tnc_extinction_flow_sumalltypes$flow_outbound)
range(all_tnc_extinction_flows, na.rm = TRUE)
tnc_extinction_breaks <- seq(0, 40, by = 10)

# Because of the high extinctions in pastureland relative to other groups, the global scale breaks won't work well.
# Instead we need to set separate ones for each.
tnc_extinction_flow_sums_medacrosstaxa %>%
  group_by(land_type) %>%
  summarize(across(where(is.numeric), list(min, max)))
breaks_ann <- c(0, 2, 4, 6, 8)
breaks_past <- c(0, 10, 20, 30)
breaks_perm <- c(0, 3, 6, 9, 12)
max_vals <- c(annual_cropland = 8.1, permanent_cropland = 12, pastureland = 33.7, total_agricultural_land = 43)
range_vals <- list(annual_cropland = breaks_ann, permanent_cropland = breaks_perm, pastureland = breaks_past, total_agricultural_land = tnc_extinction_breaks)

tnc_extinction_maps <- tnc_extinction_maps %>%
  ungroup %>%
  mutate(land_type = case_when(land_type == 'annual' ~ 'annual_cropland',
                               land_type == 'pasture' ~ 'pastureland',
                               land_type == 'permanent' ~ 'permanent_cropland',
                               TRUE ~ land_type)) %>%
  mutate(plot_title = pmap(.[, c('land_type','scenario_diet','scenario_waste')], 
                           function(land_type, scenario_diet, scenario_waste) {
                             list(land_type = gsub('_', ' ', land_type), 
                                  diet_name = diet_long_names$long_name[match(scenario_diet, diet_long_names$scenario_diet)], 
                                  waste_name = waste_long_names$long_name[match(scenario_waste, waste_long_names$scenario_waste)],
                                  file_prefix = glue('D_{scenario_diet}_W_{scenario_waste}_{land_type}'),
                                  scale_range = c(0, max_vals[land_type]),
                                  scale_breaks = range_vals[[land_type]])
                           })) %>%
  mutate(map_inbound = map2(data, plot_title, 
                            ~ draw_usmap_with_insets(map_data = left_join(tnc_map, .x, by = c('ECO_CODE' = 'TNC')), 
                                                     ak_idx = tnc_ak_idx,
                                                     hi_idx = tnc_hi_idx,
                                                     variable = flow_inbound,
                                                     title = glue('extinctions due to {.y$land_type} imported by ecoregion'),
                                                     subtitle = glue('diet scenario: {.y$diet_name}, waste scenario: {.y$waste_name}'),
                                                     scale_name = 'extinctions',
                                                     scale_factor = 1,
                                                     scale_trans = 'identity',
                                                     scale_range = .y$scale_range,
                                                     scale_breaks = .y$scale_breaks,
                                                     add_theme = theme_void() + theme(legend.position = c(0.62, 0.1),
                                                                                      legend.key.width = unit(0.2, 'in')),
                                                     write_to_file = glue('{fp_fig}/ecoregion_extinctionflow_maps/{.y$file_prefix}_inbound.png'),
                                                     img_size = c(7, 7))),
         map_outbound = map2(data, plot_title,
                             ~ draw_usmap_with_insets(map_data = left_join(tnc_map, .x, by = c('ECO_CODE' = 'TNC')), 
                                                      ak_idx = tnc_ak_idx,
                                                      hi_idx = tnc_hi_idx,
                                                      variable = flow_outbound,
                                                      title = glue('extinctions due to {.y$land_type} exported by ecoregion'),
                                                      subtitle = glue('diet scenario: {.y$diet_name}, waste scenario: {.y$waste_name}'),
                                                      scale_name = 'extinctions',
                                                      scale_factor = 1,
                                                      scale_trans = 'identity',
                                                      scale_range = .y$scale_range,
                                                      scale_breaks = .y$scale_breaks,
                                                      add_theme = theme_void() + theme(legend.position = c(0.62, 0.1),
                                                                                       legend.key.width = unit(0.2, 'in')),
                                                      write_to_file = glue('{fp_fig}/ecoregion_extinctionflow_maps/{.y$file_prefix}_outbound.png'),
                                                      img_size = c(7, 7))))
