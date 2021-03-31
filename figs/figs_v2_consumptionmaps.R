# Consumption-side maps, baseline only
# QDR / Virtualland / 30 Mar 2021

# Use the data objects from figs_v2_panelmaps.R (only baselinexbaseline)

fp_fig <- 'data/cfs_io_analysis/scenario_v2_figs/baseline_maps'

county_land_map_base <- county_land_map_panels[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline']
county_extinction_map_base <- county_extinction_map_panels[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline']

# Land inbound
div_pal <- scico::scico(15, palette = 'berlin')
seq_pal <- viridis::viridis_pal()(15)

land_range <- county_land_flow_sums[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline', .(min = min(flow_inbound/1e4, na.rm = TRUE), max = max(flow_inbound/1e4, na.rm = TRUE)), by = land_type]

leg_bottom_theme <- theme_void() + theme(legend.position = c(0.62, 0.1),
                                         legend.title = element_text(size = rel(0.8)),
                                         legend.key.width = unit(0.2, 'in'))

# Total land inbound
draw_usmap_with_insets(map_data = left_join(county_map, county_land_map_base$data[[4]]),
                       ak_idx = county_ak_idx,
                       hi_idx = county_hi_idx,
                       variable = flow_inbound,
                       scale_name = 'Virtual land\nimport (ha)',
                       scale_factor = 10000,
                       scale_trans = 'log10',
                       scale_palette = seq_pal,
                       scale_range = c(2e1, 6.2e6),
                       scale_breaks = scales::trans_breaks("log10", function(x) 10^x),
                       scale_labels = scales::trans_format("log10", scales::math_format(10^.x)),
                       add_theme = leg_bottom_theme,
                       write_to_file = glue('{fp_fig}/total_land_inbound_baseline.png'),
                       img_size = c(7, 7)
)

extinction_range <- county_extinction_flow_sums[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline', .(min = min(extinction_inbound, na.rm = TRUE), max = max(extinction_inbound, na.rm = TRUE)), by = .(land_use, taxon)]


# Total extinctions inbound         
draw_usmap_with_insets(map_data = left_join(county_map, county_extinction_map_base$data[[28]]),
                       ak_idx = county_ak_idx,
                       hi_idx = county_hi_idx,
                       variable = extinction_inbound,
                       scale_name = 'Extinctions',
                       scale_factor = 1,
                       scale_trans = 'log10',
                       scale_palette = seq_pal,
                       scale_range = c(1e-5, 5),
                       scale_breaks = scales::trans_breaks("log10", function(x) 10^x),
                       scale_labels = scales::trans_format("log10", scales::math_format(10^.x)),
                       add_theme = leg_bottom_theme,
                       write_to_file = glue('{fp_fig}/total_extinction_inbound_baseline.png'),
                       img_size = c(7, 7)
)


# Draw all maps programmatically ------------------------------------------

pmap(county_land_map_base[land_range, on = .NATURAL],
     function(data, min, max, land_type, ...)
       draw_usmap_with_insets(map_data = left_join(county_map, data),
                              ak_idx = county_ak_idx,
                              hi_idx = county_hi_idx,
                              variable = flow_inbound,
                              scale_name = 'Virtual land\nimport (ha)',
                              scale_factor = 10000,
                              scale_trans = 'log10',
                              scale_palette = seq_pal,
                              scale_range = c(min, max),
                              scale_breaks = scales::trans_breaks("log10", function(x) 10^x),
                              scale_labels = scales::trans_format("log10", scales::math_format(10^.x)),
                              add_theme = leg_bottom_theme,
                              write_to_file = glue('{fp_fig}/land_inbound_{land_type}_baseline.png'),
                              img_size = c(7, 7)
       ))
# These essentially all look the same.

# Inbound extinctions: do total only
pmap(county_extinction_map_base[extinction_range, on = .NATURAL][taxon %in% 'total'],
     function(data, min, max, land_use, ...)
       draw_usmap_with_insets(map_data = left_join(county_map, data),
                              ak_idx = county_ak_idx,
                              hi_idx = county_hi_idx,
                              variable = extinction_inbound,
                              scale_name = 'Extinctions',
                              scale_factor = 1,
                              scale_trans = 'log10',
                              scale_palette = seq_pal,
                              scale_range = c(min, max),
                              scale_breaks = scales::trans_breaks("log10", function(x) 10^x),
                              scale_labels = scales::trans_format("log10", scales::math_format(10^.x)),
                              add_theme = leg_bottom_theme,
                              write_to_file = glue('{fp_fig}/extinction_inbound_{land_use}_total_baseline.png'),
                              img_size = c(7, 7)
       ))

# Outbound land and extinctions
outbound_land_range <- county_land_flow_sums[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline' & flow_outbound > 0, 
                                             .(min = min(flow_outbound/1e4, na.rm = TRUE), max = max(flow_outbound/1e4, na.rm = TRUE)), 
                                             by = land_type]

pmap(county_land_map_base[outbound_land_range, on = .NATURAL],
     function(data, min, max, land_type, ...)
       draw_usmap_with_insets(map_data = left_join(county_map, data),
                              ak_idx = county_ak_idx,
                              hi_idx = county_hi_idx,
                              variable = flow_outbound,
                              scale_name = 'Virtual land\nexport (ha)',
                              scale_factor = 10000,
                              scale_trans = 'log10',
                              scale_palette = seq_pal,
                              scale_range = c(min, max),
                              scale_breaks = scales::trans_breaks("log10", function(x) 10^x),
                              scale_labels = scales::trans_format("log10", scales::math_format(10^.x)),
                              add_theme = leg_bottom_theme,
                              write_to_file = glue('{fp_fig}/land_outbound_{land_type}_baseline.png'),
                              img_size = c(7, 7)
       ))

outbound_extinction_range <- county_extinction_flow_sums[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline' & extinction_outbound > 0,
                                                         .(min = min(extinction_outbound, na.rm = TRUE), max = max(extinction_outbound, na.rm = TRUE)),
                                                         by = .(land_use, taxon)]

pmap(county_extinction_map_base[outbound_extinction_range, on = .NATURAL],
     function(data, min, max, land_use, taxon, ...)
       draw_usmap_with_insets(map_data = left_join(county_map, data),
                              ak_idx = county_ak_idx,
                              hi_idx = county_hi_idx,
                              variable = extinction_outbound,
                              scale_name = 'Extinctions',
                              scale_factor = 1,
                              scale_trans = 'log10',
                              scale_palette = seq_pal,
                              scale_range = c(min, max),
                              scale_breaks = scales::trans_breaks("log10", function(x) 10^x),
                              scale_labels = scales::trans_format("log10", scales::math_format(10^.x)),
                              add_theme = leg_bottom_theme,
                              write_to_file = glue('{fp_fig}/extinction_outbound_{land_use}_{taxon}_baseline.png'),
                              img_size = c(7, 7)
       ))
