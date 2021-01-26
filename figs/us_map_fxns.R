draw_usmap_with_insets <- function(map_data, ak_idx, hi_idx, variable, title, scale_name = 'Value\n(billion $)', scale_factor = 1000, add_theme = theme_void()) {
  linewidth <- 0.25 # hardcode to 1/2 the default
  # Calculate scale range
  variable <- enquo(variable)
  map_data <- map_data %>% mutate(!!variable := !!variable/scale_factor)
  scale_range <- map_data %>% pull(!!variable) %>% range(na.rm = TRUE)
  
  # Draw the three maps
  us_map <- ggplot(map_data %>% filter(!ak_idx, !hi_idx)) +
    geom_sf(aes(fill = !!variable), size = linewidth) +
    scale_fill_viridis_c(na.value = 'gray75', name = scale_name, limits = scale_range, guide = guide_colorbar(direction = 'horizontal')) +
    add_theme +
    theme(legend.position = 'bottom') +
    ggtitle(title)
  
  # Include insets for Alaska and Hawaii
  # Projections and limits from https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-3.html
  hi_map <- ggplot(map_data %>% filter(hi_idx)) +
    geom_sf(aes(fill = !!variable), size = linewidth) +
    coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE, datum = NA) +
    scale_fill_viridis_c(na.value = 'gray75', limits = scale_range) +
    add_theme + 
    theme(legend.position = 'none')
  
  ak_map <- ggplot(map_data %>% filter(ak_idx)) +
    geom_sf(aes(fill = !!variable), size = linewidth) +
    coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA) +
    scale_fill_viridis_c(na.value = 'gray75', limits = scale_range) +
    add_theme +
    theme(legend.position = 'none')
  
  # Place main map and inset maps on the same plot, scaling appropriately. Hardcode ratios
  ratio_ak <- 0.58
  ratio_hi <- 0.71
  size_ak <- 0.32
  size_hi <- 0.2
  three_maps <- ggdraw(us_map + add_theme) +
    draw_plot(ak_map, width = size_ak, height = size_ak * ratio_ak, x = 0.01, y = 0.15, vjust = 0) +
    draw_plot(hi_map, width = size_hi, height = size_hi * ratio_hi, x = 0.26, y = 0.15, vjust = 0)
  return(three_maps)
}
