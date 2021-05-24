# Function to draw paneled maps not necessarily as grid (for baseline consumption and production maps)

make_panel_map_wrap <- function(map_panel_data, base_map, map_title, panel_titles, region_type, variable, nrows, file_name, percent_scale = TRUE, ...) {
  
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
  maps_list <- map2(map_panel_data$data, panel_titles, function(dat, panel_title) draw_usmap_with_insets_v2(map_data = left_join(base_map, dat[, c(region_type, variable), with = FALSE]),
                                                                                                            ak_idx = args$ak_idx,
                                                                                                            hi_idx = args$hi_idx,
                                                                                                            title = panel_title,
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
  
  ncols <- ceiling(length(maps_list)/nrows)
  
  maps_laidout <- panel_plot_wrap(plots = maps_list, 
                                  global_title = map_title,
                                  global_legend = plot_leg,
                                  n_rows = nrows,
                                  n_cols = ncols,
                                  title_fontsize = 14,
                                  panel_width = 60,
                                  panel_height = 60,
                                  title_height = 10,
                                  legend_height = 20)
  
  png(glue('{fp_fig}/{file_name}.png'), height=6.0*nrows+2.0,width=6.0*ncols,res=100,units='cm')
  grid.draw(maps_laidout)
  dev.off()
  
}


# "Wrapped" version of panel_plot
panel_plot_wrap <- function(plots, global_title, global_legend, n_rows, n_cols, title_fontsize = 20, panel_width = 50, panel_height = 50, title_height = 10, legend_height = 10) {

  global_title_grob <- textGrob(global_title, gp = gpar(fontsize = title_fontsize))
  
  n_na_pad <- n_rows * n_cols - length(plots)

  mat <- rbind(
    rep(1, n_cols),
    matrix(c(2:(length(plots) + 1), rep(NA, n_na_pad)), nrow = n_rows, ncol = n_cols, byrow = TRUE),
    rep(length(plots) + 2, n_cols)
  )
  
  arrangeGrob(grobs = c(list(global_title_grob), plots, list(global_legend)),
              layout_matrix = mat,
              widths = unit(rep(panel_width, n_cols), 'mm'),
              heights = unit(c(title_height, rep(panel_height, n_rows), legend_height), 'mm'))
  
}

