# Baseline consumption-side and production-side maps, paneled
# QDR / Virtualland / 23 May 2021
# Same maps as figs_v2_consumptionmaps but with multiple map panels in the same fig to save some figures.

# First generate plotting data in figs_v2_panelmaps.R
source('figs/us_map_fxns_baselinemaps.R')

fp_fig <- 'data/cfs_io_analysis/scenario_v2_figs/paneled_baseline_maps'

county_land_map_base <- county_land_map_panels[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline']
county_extinction_map_base <- county_extinction_map_panels[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline']
county_goods_map_base <- county_goods_map_panels[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline']

seq_pal <- viridis::viridis_pal()(15)


# Domestic land maps ------------------------------------------------------

# Plot each land type on a panel
# Repeat for: Inbound flow domestic, outbound flow, foreign inbound flow, total inbound flow


vars_to_plot <- c('flow_inbound', 'flow_outbound', 'flow_inbound_foreign', 'flow_inbound_total')

walk(vars_to_plot, ~ make_panel_map_wrap(map_panel_data = county_land_map_base,
                                         base_map = county_map,
                                         map_title = NULL,
                                         panel_titles = c('Annual cropland', 'Pastureland', 'Permanent cropland', 'Total'),
                                         region_type = 'county',
                                         variable = .x,
                                         nrows = 2,
                                         file_name = glue('county_land_{.x}'),
                                         percent_scale = FALSE,
                                         scale_name = 'Virtual land\nflow (ha)',
                                         scale_factor = 1e4,
                                         scale_trans = 'log10',
                                         scale_type = 'sequential',
                                         ak_idx = county_ak_idx,
                                         hi_idx = county_hi_idx,
                                         add_theme = theme_void() + theme(legend.position = 'none')))


# Domestic extinction maps ------------------------------------------------

# Reorder the panels by taxon, beginning with plants
taxa_order <- c('plants', 'amphibians', 'birds', 'mammals', 'reptiles', 'animals', 'total')
taxa_panel_titles <- c('Plants', 'Amphibians', 'Birds', 'Mammals', 'Reptiles', 'Animals total', 'All taxa total')

county_extinction_map_base[, taxon := factor(taxon, levels = taxa_order)]
county_extinction_map_base <- county_extinction_map_base[order(land_use, taxon), ]

ext_vars_to_plot <- c('extinction_outbound', 'extinction_inbound', 'extinction_inbound_foreign', 'extinction_inbound_total')

walk(ext_vars_to_plot, ~ make_panel_map_wrap(map_panel_data = county_extinction_map_base[land_use %in% 'total'],
                                             base_map = county_map,
                                             map_title = NULL,
                                             panel_titles = taxa_panel_titles,
                                             region_type = 'county',
                                             variable = .x,
                                             nrows = 2,
                                             file_name = glue('county_totalland_{.x}'),
                                             percent_scale = FALSE,
                                             scale_name = 'Virtual extinction\nflow',
                                             scale_factor = 1,
                                             scale_trans = 'log10',
                                             scale_type = 'sequential',
                                             ak_idx = county_ak_idx,
                                             hi_idx = county_hi_idx,
                                             add_theme = theme_void() + theme(legend.position = 'none')))

# Domestic goods maps -----------------------------------------------------

bea_panel_titles <- c('Oilseeds & soybeans', 'Grains', 'Vegetables', 'Fruits & nuts', 'Greenhouse crops', 'Other crops', 'Dairy products', 'Beef cattle', 'Poultry & eggs', 'Other meat', 'All goods total')

# Plot each type of goods on a panel.
# Repeat for the two domestic flow types.
walk(vars_to_plot[1:2], ~ make_panel_map_wrap(map_panel_data = county_goods_map_base,
                                              base_map = county_map,
                                              map_title = NULL,
                                              panel_titles = bea_panel_titles,
                                              region_type = 'county',
                                              variable = .x,
                                              nrows = 3,
                                              file_name = glue('county_goods_{.x}'),
                                              percent_scale = FALSE,
                                              scale_name = 'Goods flow\n(million USD)',
                                              scale_factor = 1e6,
                                              scale_trans = 'log10',
                                              scale_type = 'sequential',
                                              ak_idx = county_ak_idx,
                                              hi_idx = county_hi_idx,
                                              add_theme = theme_void() + theme(legend.position = 'none')))

