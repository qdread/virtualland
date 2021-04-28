# Baseline paneled maps for main manuscript
# QDR / Virtualland / 31 Mar 2021

# Maps to create: 

# 1. inbound vs outbound x land vs extinctions (2x2) -- total land and total extinctions
# 2. facet wrap of outbound extinctions in baseline case, separated by taxon (may be supplemental) -- total land

# Follows figs_v2_consumptionmaps.R

fp_fig <- 'data/cfs_io_analysis/scenario_v2_figs'

leg_longbottom_theme <- theme_void() + theme(legend.position = 'bottom',
                                         legend.title = element_text(size = rel(1)),
                                         legend.key.width = unit(0.3, 'in'))

seq_pal <- viridis::viridis_pal()(15)

# Calculate some ranges
land_range <- county_land_flow_sums[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline', .(min = min(flow_inbound_total[flow_inbound_total > 0]/1e4, na.rm = TRUE), max = max(flow_inbound_total/1e4, na.rm = TRUE)), by = land_type]

extinction_range <- county_extinction_flow_sums[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline', .(min = min(extinction_inbound_total[extinction_inbound_total > 0], na.rm = TRUE), max = max(extinction_inbound_total, na.rm = TRUE)), by = .(land_use, taxon)]

outbound_land_range <- county_land_flow_sums[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline' & flow_outbound > 0, 
                                             .(min = min(flow_outbound/1e4, na.rm = TRUE), max = max(flow_outbound/1e4, na.rm = TRUE)), 
                                             by = land_type]

outbound_extinction_range <- county_extinction_flow_sums[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline' & extinction_outbound > 0,
                                                         .(min = min(extinction_outbound, na.rm = TRUE), max = max(extinction_outbound, na.rm = TRUE)),
                                                         by = .(land_use, taxon)]

# Map 1 -------------------------------------------------------------------

# Inbound land
land_inbound_dat <- county_land_map_base[land_range, on = .NATURAL][land_type == 'total'] 
map_land_inbound <- draw_usmap_with_insets(map_data = left_join(county_map, land_inbound_dat$data[[1]]),
                                           ak_idx = county_ak_idx,
                                           hi_idx = county_hi_idx,
                                           variable = flow_inbound,
                                           scale_name = 'Virtual land\nimport (ha)',
                                           scale_factor = 10000,
                                           scale_trans = 'log10',
                                           scale_palette = seq_pal,
                                           scale_range = with(land_inbound_dat, c(min, max)),
                                           scale_breaks = scales::trans_breaks("log10", function(x) 10^x),
                                           scale_labels = scales::trans_format("log10", scales::math_format(10^.x)),
                                           add_theme = leg_longbottom_theme)

# Inbound extinctions
ext_inbound_dat <- county_extinction_map_base[extinction_range, on = .NATURAL][taxon %in% 'total' & land_use %in% 'total']
map_ext_inbound <- draw_usmap_with_insets(map_data = left_join(county_map, ext_inbound_dat$data[[1]]),
                                          ak_idx = county_ak_idx,
                                          hi_idx = county_hi_idx,
                                          variable = extinction_inbound,
                                          scale_name = 'Extinction\nimport',
                                          scale_factor = 1,
                                          scale_trans = 'log10',
                                          scale_palette = seq_pal,
                                          scale_range = with(ext_inbound_dat, c(min, max)),
                                          scale_breaks = scales::trans_breaks("log10", function(x) 10^x),
                                          scale_labels = scales::trans_format("log10", scales::math_format(10^.x)),
                                          add_theme = leg_longbottom_theme)

# Outbound land
land_outbound_dat <- county_land_map_base[outbound_land_range, on = .NATURAL][land_type %in% 'total']
map_land_outbound <-  draw_usmap_with_insets(map_data = left_join(county_map, land_outbound_dat$data[[1]]),
                                             ak_idx = county_ak_idx,
                                             hi_idx = county_hi_idx,
                                             variable = flow_outbound,
                                             scale_name = 'Virtual land\nexport (ha)',
                                             scale_factor = 10000,
                                             scale_trans = 'log10',
                                             scale_palette = seq_pal,
                                             scale_range = with(land_outbound_dat, c(min, max)),
                                             scale_breaks = scales::trans_breaks("log10", function(x) 10^x),
                                             scale_labels = scales::trans_format("log10", scales::math_format(10^.x)),
                                             add_theme = leg_longbottom_theme)

# Outbound extinctions
ext_outbound_dat <- county_extinction_map_base[outbound_extinction_range, on = .NATURAL][land_use %in% 'total' & taxon %in% 'total']
map_ext_outbound <- draw_usmap_with_insets(map_data = left_join(county_map, ext_outbound_dat$data[[1]]),
                                           ak_idx = county_ak_idx,
                                           hi_idx = county_hi_idx,
                                           variable = extinction_outbound,
                                           scale_name = 'Extinction\nexport',
                                           scale_factor = 1,
                                           scale_trans = 'log10',
                                           scale_palette = seq_pal,
                                           scale_range = with(ext_outbound_dat, c(min, max)),
                                           scale_breaks = scales::trans_breaks("log10", function(x) 10^x),
                                           scale_labels = scales::trans_format("log10", scales::math_format(10^.x)),
                                           add_theme = leg_longbottom_theme)

# Combine the four into a single figure with labeled columns and rows

x_label_grobs <- map(c('imports (consumption)', 'exports (production)'), ~ textGrob(., gp = gpar(fontsize = 15)))
y_label_grobs <- map(c('land', 'biodiversity'), ~ textGrob(., gp = gpar(fontsize = 15), rot = 270))

mat <- rbind(c(1,2,NA), c(5,6,3), c(7,8,4))

panel_width <- 120
panel_height <- 90
label_width <- 8

fourmaps <- arrangeGrob(grobs = c(x_label_grobs, y_label_grobs, list(map_land_inbound, map_land_outbound, map_ext_inbound, map_ext_outbound)),
                        layout_matrix = mat,
                        widths = unit(c(rep(panel_width, 2), label_width), 'mm'),
                        heights = unit(c(label_width, rep(panel_height, 2)), 'mm'))

png(glue('{fp_fig}/bio_land_in_out_map.png'), height=8+90+90,width=8+120+120,res=300,units='mm')
grid.draw(fourmaps)
dev.off()


# Map 2 -------------------------------------------------------------------

# Outbound extinction, by taxon, total land. Combine 6 or 7 panels into one figure.

ext_outbound_dat_bytaxon <- county_extinction_map_base[outbound_extinction_range, on = .NATURAL][land_use %in% 'total' & !taxon %in% 'total']
ext_outbound_dat_bytaxon[taxon == 'animals', taxon := 'total animals']

outbound_ext_maps_bytaxon <- pmap(ext_outbound_dat_bytaxon,
     function(data, min, max, taxon, ...)
       draw_usmap_with_insets(map_data = left_join(county_map, data),
                              ak_idx = county_ak_idx,
                              hi_idx = county_hi_idx,
                              variable = extinction_outbound,
                              title = taxon,
                              scale_name = 'Extinction\nexport',
                              scale_factor = 1,
                              scale_trans = 'log10',
                              scale_palette = seq_pal,
                              scale_range = c(min, max),
                              scale_breaks = scales::trans_breaks("log10", function(x) 10^x),
                              scale_labels = scales::trans_format("log10", scales::math_format(10^.x)),
                              add_theme = leg_longbottom_theme
       ))

# Lay out six maps. Reorder so that plants comes after total animals.
panel_width <- 120
panel_height <- 90

sixmaps <- arrangeGrob(grobs = outbound_ext_maps_bytaxon,
                        layout_matrix = rbind(c(1,2), c(3,4), c(6,5)),
                        widths = unit(rep(panel_width, 2), 'mm'),
                        heights = unit(rep(panel_height, 3), 'mm'))

png(glue('{fp_fig}/bio_outbound_maps_bytaxon.png'), height=90*3,width=120*2,res=300,units='mm')
grid.draw(sixmaps)
dev.off()
