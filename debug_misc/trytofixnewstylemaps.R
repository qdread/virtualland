source('figs/us_map_fxns_v2.R')

make_20panel_map_v2(map_panel_data = county_land_map_panels[land_type %in% 'total' & scenario_waste %in% c("baseline","allavoidable")],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'outbound_vs_baseline',
                    file_name = 'testtotallandoutrelative',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    n_waste = 2,
                    add_theme = theme_void() + theme(legend.position = 'none')
)

make_20panel_map_v2(map_panel_data = county_land_map_panels[land_type %in% 'total' & scenario_waste %in% c("baseline","allavoidable")],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'flow_outbound',
                    file_name = 'testtotallandoutabs',
                    scale_name = 'Flow',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'sequential',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    n_waste = 2,
                    percent_scale = FALSE,
                    add_theme = theme_void() + theme(legend.position = 'none')
)

# DEBUG
map_panel_data = county_land_map_panels[land_type %in% 'total' & scenario_waste %in% c("baseline","allavoidable")]
base_map = county_map
region_type = 'county'
variable = 'flow_outbound'
file_name = 'testtotallandoutabs'
scale_name = 'Flow'
scale_factor = 1
scale_trans = 'log10'
scale_type = 'sequential'
ak_idx = county_ak_idx
hi_idx = county_hi_idx
add_theme = theme_void() + theme(legend.position = 'none')
n_waste = 2
percent_scale = FALSE

maps_list <- map(map_panel_data$data, function(dat) draw_usmap_with_insets_v2(map_data = left_join(base_map, dat[, c(region_type, variable), with = FALSE]),
                                                                              ak_idx = ak_idx,
                                                                              hi_idx = hi_idx,
                                                                              variable = "flow_outbound",
                                                                              linewidth = 0,
                                                                              scale_type = scale_type,
                                                                              scale_name = scale_name,
                                                                              scale_factor = scale_factor,
                                                                              scale_trans = scale_trans,
                                                                              ak_pos = c(-0.01, 0.15), hi_pos = c(0.23, 0.15),
                                                                              add_theme = add_theme))

draw_usmap_with_insets_v2(map_data = left_join(base_map, map_panel_data$data[[3]][, c(region_type, variable), with = FALSE]),
                          ak_idx = ak_idx,
                          hi_idx = hi_idx,
                          variable = "outbound_vs_baseline",
                          linewidth = 0,
                          scale_type = scale_type,
                          scale_name = scale_name,
                          scale_factor = scale_factor,
                          scale_trans = scale_trans,
                          ak_pos = c(-0.01, 0.15), hi_pos = c(0.23, 0.15),
                          add_theme = add_theme)


map_data = left_join(base_map, map_panel_data$data[[3]][, c(region_type, variable), with = FALSE])
                          ak_idx = ak_idx
                          hi_idx = hi_idx
                          variable = "outbound_vs_baseline"
                          linewidth = 0
                          scale_type = scale_type
                          scale_name = scale_name
                          scale_factor = scale_factor
                          scale_trans = scale_trans
                          ak_pos = c(-0.01, 0.15)
                          hi_pos = c(0.23, 0.15)
                          add_theme = add_theme
                          ak_ratio = 0.58; ak_size = 0.32; hi_ratio = 0.71; hi_size = 0.2
