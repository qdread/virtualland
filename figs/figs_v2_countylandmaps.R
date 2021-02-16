# County land and goods flow maps
# QDR / Virtualland / 15 Feb 2021

source('figs/figs_v2_loaddata.R')

# Flows of goods between counties -----------------------------------------

# Maps (insert here)

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
all_county_land_flows <- c(county_land_flow_sums$flow_inbound, county_land_flow_sumalltypes$flow_inbound, county_land_flow_sums$flow_outbound, county_land_flow_sumalltypes$flow_outbound)
(range(all_county_land_flows[all_county_land_flows > 0], na.rm = TRUE)/1e4) %>% formatC(format = 'e')

county_land_breaks <- c(1e0, 1e2, 1e4, 1e6, 1e8)

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
