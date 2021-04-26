make_20panel_map_v2 <- function(map_panel_data, base_map, region_type, variable, file_name, n_waste = 4, percent_scale = TRUE, ...) {
  
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
  maps_list <- map(map_panel_data$data, function(dat) draw_usmap_with_insets_v2(map_data = left_join(base_map, dat[, c(region_type, variable), with = FALSE]),
                                                                                ak_idx = args$ak_idx,
                                                                                hi_idx = args$hi_idx,
                                                                                variable = variable,
                                                                                linewidth = 0,
                                                                                scale_type = args$scale_type,
                                                                                scale_name = args$scale_name,
                                                                                scale_factor = args$scale_factor,
                                                                                scale_trans = args$scale_trans,
                                                                                scale_range = global_scale_range,
                                                                                scale_range_remap = global_scale_range_remap,
                                                                                ak_pos = c(-0.01, 0.15), hi_pos = c(0.23, 0.15),
                                                                                add_theme = args$add_theme))
  
  
  
  waste_labels <- waste_long_names$long_name
  if (n_waste == 2) waste_labels <- waste_labels[c(1, 4)]
  
  maps_laidout <- panel_plot(plots = maps_list, 
                             x_labels = diet_long_names$long_name, 
                             y_labels = waste_labels,
                             x_title = 'diet scenario',
                             y_title = 'waste scenario',
                             global_legend = plot_leg,
                             label_fontsize = 10,
                             title_fontsize = 14,
                             panel_width = 60,
                             panel_height = 45,
                             label_width = 5,
                             title_width = 10,
                             legend_height = 15)
  
  png(glue('{fp_fig}/{file_name}.png'), height=4.5*n_waste+1+1.5,width=6.0*5+1,res=100,units='cm')
  grid.draw(maps_laidout)
  dev.off()
  
}

draw_usmap_with_insets_v2 <- function(map_data, ak_idx, hi_idx, variable, 
                                   title = NULL, subtitle = NULL, 
                                   scale_name = 'Value\n(billion $)', scale_factor = 1000, scale_trans = 'identity', 
                                   scale_type = 'divergent',
                                   scale_range,
                                   scale_range_remap = NULL,
                                   ak_pos = c(0.01, 0.15), hi_pos = c(0.26, 0.15),
                                   ak_ratio = 0.58, ak_size = 0.32, hi_ratio = 0.71, hi_size = 0.2,
                                   linewidth = 0.25, add_theme = theme_void(), write_to_file = NULL, img_size = NULL) {
  
  map_data[[variable]] <- map_data[[variable]]/scale_factor
  
  if (scale_type == 'divergent') {
    # Remap scale range to 0-1
    scale_main <- scale_fill_scico(palette = 'vik', begin = scale_range_remap[1], end = scale_range_remap[2], na.value = 'gray75', name = scale_name, trans = scale_trans, limits = scale_range, guide = guide_colorbar(direction = 'horizontal'))
    scale_inset <- scale_fill_scico(palette = 'vik', begin = scale_range_remap[1], end = scale_range_remap[2], na.value = 'gray75', name = scale_name, trans = scale_trans, limits = scale_range)
  } else {
    scale_main <- scale_fill_viridis_c(na.value = 'gray75', name = scale_name, trans = scale_trans, limits = scale_range, guide = guide_colorbar(direction = 'horizontal'))
    scale_inset <- scale_fill_viridis_c(na.value = 'gray75', name = scale_name, trans = scale_trans, limits = scale_range)
  }

  # Draw the three maps
  us_map <- ggplot(map_data %>% filter(!ak_idx, !hi_idx)) +
    geom_sf(aes_string(fill = variable), size = linewidth) +
    scale_main +
    add_theme +
    theme(legend.position = 'bottom') +
    ggtitle(title, subtitle)
  
  # Include insets for Alaska and Hawaii
  # Projections and limits from https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-3.html
  hi_map <- ggplot(map_data %>% filter(hi_idx)) +
    geom_sf(aes_string(fill = variable), size = linewidth) +
    coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE, datum = NA) +
    scale_inset +
    add_theme + 
    theme(legend.position = 'none')
  
  ak_map <- ggplot(map_data %>% filter(ak_idx)) +
    geom_sf(aes_string(fill = variable), size = linewidth) +
    coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA) +
    scale_inset +
    add_theme +
    theme(legend.position = 'none')
  
  # Place main map and inset maps on the same plot, scaling appropriately. 
  three_maps <- ggdraw(us_map + add_theme) +
    draw_plot(ak_map, width = ak_size, height = ak_size * ak_ratio, x = ak_pos[1], y = ak_pos[2], vjust = 0) +
    draw_plot(hi_map, width = hi_size, height = hi_size * hi_ratio, x = hi_pos[1], y = hi_pos[2], vjust = 0)
  if (is.null(write_to_file)) {
    return(three_maps)
  } else {
    ggsave(write_to_file, three_maps, dpi = 300, width = img_size[1], height = img_size[2])
    return(write_to_file)
  }
}

# Arrange arbitrary number of maps into a labeled panel with grid and gridExtra
# Control the widths and heights of plots and labels directly with units (in mm)
panel_plot <- function(plots, x_labels, y_labels, x_title, y_title, global_legend, label_fontsize = 10, title_fontsize = 20, panel_width = 50, panel_height = 50, label_width = 10, title_width = 10, legend_height = 10) {
  x_label_grobs <- map(x_labels, ~ textGrob(., gp = gpar(fontsize = label_fontsize)))
  y_label_grobs <- map(y_labels, ~ textGrob(., gp = gpar(fontsize = label_fontsize), rot = 270))
  
  x_title_grob <- textGrob(x_title, gp = gpar(fontsize = title_fontsize))
  y_title_grob <- textGrob(y_title, gp = gpar(fontsize = title_fontsize), rot = 270)
  
  mat <- rbind(
    cbind(
      rbind(
        rep(1, length(x_labels)),
        (1:length(x_labels)) + 2,
        matrix((1:length(plots)) + length(x_labels) + length(y_labels) + 2, ncol = length(x_labels))
      ),
      c(NA, NA, (1:length(y_labels)) + length(x_labels) + 2),
      c(NA, NA, rep(2, length(y_labels)))
    ),
    rep(length(x_labels) + length(y_labels) + length(plots) + 3, length(x_labels) + 2)
  )
  
  arrangeGrob(grobs = c(list(x_title_grob, y_title_grob), x_label_grobs, y_label_grobs, plots, list(global_legend)),
              layout_matrix = mat,
              widths = unit(c(rep(panel_width, length(x_labels)), label_width, title_width), 'mm'),
              heights = unit(c(title_width, label_width, rep(panel_height, length(y_labels)), legend_height), 'mm'))
  
}

# function to determine where to begin and end sampling color/fill scale so that it is centered at 0.
# x is the vector of values
scale_begin_end <- function(x) {
  r <- range(x, na.rm = TRUE)
  out <- 0.5 + 0.5 * (r/max(abs(r)))
  if (any(!is.finite(out))) c(0, 1) else out
}
