# Foreign maps drawn as multipanel figures (to reduce number of figures in supplement)
# QDR / Virtualland / 24 May 2021

# New version created 15 Oct 2021: do with opposite orientation (5x2)

# First generate all data in figs_v2_foreign.R
# Only make figures for animals, plants, and total
# In total 16 figures will be generated. 
# For extinction, 3 maps (animals, plants, total) x 2 (absolute and relative to baseline) = 6
# For land, 5 maps (annual, mixed, permanent, pasture, total) x 2 (absolute and relative to baseline) = 10


# Function to draw paneled world map --------------------------------------

make_world_panel_map <- function(map_panel_data, base_map, region_type, variable, file_name, n_waste = 4, percent_scale = TRUE, ...) {
  
  args <- list(...)
  
  # Calculate global range for fill scale for all plots and insets.
  vals <- unlist(map(map_panel_data$data, function(x) x[[variable]]/args$scale_factor))
  if (args$scale_trans == 'log10') vals <- vals[vals > 0]
  global_scale_range <- range(vals, na.rm = TRUE)
  
  # Create dummy plot with a legend so it can be extracted
  scale_labs <- if (percent_scale) scales::percent else waiver()
  
  if (args$scale_type == 'divergent') {
    
    # Remap scale range to 0-1
    global_scale_range_remap <- scale_begin_end(vals)
    
    scale_main <- scale_fill_scico(palette = 'vik', begin = global_scale_range_remap[1], end = global_scale_range_remap[2], na.value = 'gray75', name = args$scale_name, trans = args$scale_trans, labels = scale_labs, limits = global_scale_range, guide = guide_colorbar(direction = 'horizontal'))
  } else {
    global_scale_range_remap = NULL
    scale_main <- scale_fill_viridis_c(na.value = 'gray75', name = args$scale_name, trans = args$scale_trans, labels = scale_labs, limits = global_scale_range, guide = guide_colorbar(direction = 'horizontal'))
  }
  
  plot_leg <- get_legend(ggplot(mtcars, aes(x=cyl,y=hp,fill=mpg)) + geom_point() + scale_main + theme(legend.key.width = unit(1.5, 'cm')))
  
  # Draw plots
  maps_list <- map(map_panel_data$data, function(x) ggplot(left_join(base_map, x)) +
                     geom_sf(aes_string(fill = variable), size = 0.1) +
                     geom_sf(data = st_geometry(poly_countries), fill = NA) +
                     scale_main +
                     args$add_theme)
  
  waste_labels <- waste_long_names$long_name
  if (n_waste == 2) waste_labels <- waste_labels[c(1, 4)]
  
  maps_laidout <- panel_plot(plots = maps_list, 
                             y_labels = diet_long_names$long_name, 
                             x_labels = waste_labels,
                             y_title = 'diet scenario',
                             x_title = 'waste scenario',
                             global_legend = plot_leg,
                             label_fontsize = 7,
                             title_fontsize = 14,
                             panel_width = 60,
                             panel_height = 35,
                             label_width = 5,
                             title_width = 10,
                             legend_height = 15)
  
  png(glue('{fp_fig}/{file_name}.png'), width=16,height=24,res=200,units='cm')
  grid.draw(maps_laidout)
  dev.off()
  
}


# Apply function to draw the maps -----------------------------------------

# Filter country map to get rid of very southern latitudes
extent_countries <- st_bbox(c(xmin = -180, ymin = -58, xmax = 180, ymax = 84), crs = "+proj=longlat +ellps=WGS84") %>%
  st_as_sfc
poly_countries <- geosphere::makePoly(st_coordinates(extent_countries)[, c('X', 'Y')])
poly_countries <- st_polygon(list(poly_countries)) %>% st_sfc
st_crs(poly_countries) <- st_crs(extent_countries)
poly_countries <- st_transform(poly_countries, crs = "+proj=robin")

country_map_toplot <- country_map_toplot %>% filter(!country_name %in% "Antarctica")

fp_fig <- 'data/cfs_io_analysis/scenario_v2_figs/paneled_foreign_maps'


# Extinction maps ---------------------------------------------------------

taxa_to_plot <- c('animals', 'plants', 'total')

for (tax in taxa_to_plot) {
  dat <- foreign_extinction_country_panels[taxon %in% tax & land_use %in% 'total']
  dat[, scenario_diet := factor(scenario_diet, levels = diet_levels_ordered)]
  dat[, scenario_waste := factor(scenario_waste, levels = waste_levels_ordered)]
  dat <- dat[order(scenario_waste, scenario_diet)]
  message(paste('Drawing maps for', tax))
  
  # Outbound, relative, 10 panels
  make_world_panel_map(map_panel_data = dat[scenario_waste %in% c('baseline', 'allavoidable')],
                      base_map = country_map_toplot,
                      region_type = 'country',
                      variable = 'species_vs_baseline',
                      file_name = glue('10scenariosvertical_country_extinction_{tax}_outbound_vs_baseline'),
                      scale_name = 'Change vs.\nbaseline',
                      scale_factor = 1,
                      scale_trans = 'identity',
                      scale_type = 'divergent',
                      add_theme = theme_void() + theme(legend.position = 'none'),
                      n_waste = 2
  )
  
  # Outbound, absolute, 10 panels
  make_world_panel_map(map_panel_data = dat[scenario_waste %in% c('baseline', 'allavoidable')],
                      base_map = country_map_toplot,
                      region_type = 'country',
                      variable = 'species_lost',
                      file_name = glue('10scenariosvertical_country_extinction_{tax}_outbound'),
                      scale_name = 'Virtual extinction\nexport',
                      scale_factor = 1,
                      scale_trans = 'log10',
                      scale_type = 'sequential',
                      percent_scale = FALSE,
                      add_theme = theme_void() + theme(legend.position = 'none'),
                      n_waste = 2
  )
  
}



# Land maps ---------------------------------------------------------------


landtypes_to_plot <- unique(foreign_vlt_country_panels$land_use)

for (landtype in landtypes_to_plot) {
  dat <- foreign_vlt_country_panels[land_use %in% landtype]
  dat[, scenario_diet := factor(scenario_diet, levels = diet_levels_ordered)]
  dat[, scenario_waste := factor(scenario_waste, levels = waste_levels_ordered)]
  dat <- dat[order(scenario_waste, scenario_diet)]
  message(paste('Drawing maps for', landtype))
  
  # Outbound, relative, 10 panels
  make_world_panel_map(map_panel_data = dat[scenario_waste %in% c('baseline', 'allavoidable')],
                       base_map = country_map_toplot,
                       region_type = 'country',
                       variable = 'VLT_vs_baseline',
                       file_name = glue('10scenariosvertical_country_{landtype}_outbound_vs_baseline'),
                       scale_name = 'Change vs.\nbaseline',
                       scale_factor = 1,
                       scale_trans = 'identity',
                       scale_type = 'divergent',
                       add_theme = theme_void() + theme(legend.position = 'none'),
                       n_waste = 2
  )
  
  # Outbound, absolute, 10 panels
  make_world_panel_map(map_panel_data = dat[scenario_waste %in% c('baseline', 'allavoidable')],
                       base_map = country_map_toplot,
                       region_type = 'country',
                       variable = 'VLT',
                       file_name = glue('10scenariosvertical_country_{landtype}_outbound'),
                       scale_name = 'Virtual land\nexport (ha)',
                       scale_factor = 1,
                       scale_trans = 'log10',
                       scale_type = 'sequential',
                       percent_scale = FALSE,
                       add_theme = theme_void() + theme(legend.position = 'none'),
                       n_waste = 2
  )
  
}
