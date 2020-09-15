# Maps from FAF-only land transfers
# Split off from faf_land_transfers.R

# Modified 15 Sep 2020: Add foreign imports and exports to this map

source('FAF/faf_land_transfers.R')

# Draw maps ---------------------------------------------------------------

# New function with divergent color palette
draw_cfsmap_divergent <- function(map_data, variable, title, scale_name, scale_breaks, add_theme = theme_void()) {
  linewidth <- 0.25 # hardcode to 1/2 the default
  # three class RdYlBu
  threecols <- c("#FC8D59", "#FFFFBF", "#91BFDB")
  # Calculate scale range to set zero in the middle
  variable <- enquo(variable)
  #scale_limit <- map_data %>% pull(!!variable) %>% range(na.rm = TRUE)
  scale_limit <- (map_data %>% pull(!!variable) %>% abs %>% max(na.rm = TRUE)) * c(-1, 1)
  fill_scale <- scale_fill_gradient2(low = threecols[1], mid = threecols[2], high = threecols[3], name = scale_name, limits = scale_limit, guide = guide_colorbar(direction = 'horizontal'), na.value = 'gray75', trans = 'pseudo_log', breaks = scale_breaks)
  fill_scale_noleg <- scale_fill_gradient2(low = threecols[1], mid = threecols[2], high = threecols[3], limits = scale_limit, na.value = 'gray75', trans = 'pseudo_log')
  
  
  # Draw the three maps
  us_map <- ggplot(map_data %>% filter(!grepl('Alaska|Hawaii|Honolulu', FAF_Region))) +
    geom_sf(aes(fill = !!variable), size = linewidth) +
    fill_scale +
    add_theme +
    theme(legend.position = 'bottom') +
    ggtitle(title)
  
  # Include insets for Alaska and Hawaii
  # Projections and limits from https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-3.html
  hi_map <- ggplot(map_data %>% filter(grepl('Hawaii|Honolulu', FAF_Region))) +
    geom_sf(aes(fill = !!variable), size = linewidth) +
    coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE, datum = NA) +
    fill_scale_noleg +
    add_theme + 
    theme(legend.position = 'none')
  
  ak_map <- ggplot(map_data %>% filter(grepl('Alaska', FAF_Region))) +
    geom_sf(aes(fill = !!variable), size = linewidth) +
    coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA) +
    fill_scale_noleg +
    add_theme +
    theme(legend.position = 'none')
  
  # Place main map and inset maps on the same plot, scaling appropriately. Hardcode ratios
  ratio_ak <- 0.58
  ratio_hi <- 0.71
  size_ak <- 0.25
  size_hi <- 0.15
  three_maps <- ggdraw(us_map + add_theme) +
    draw_plot(ak_map, width = size_ak, height = size_ak * ratio_ak, x = -0.05, y = 0.15, vjust = 0) +
    draw_plot(hi_map, width = size_hi, height = size_hi * ratio_hi, x = 0.2, y = 0.15, vjust = 0)
  return(three_maps)
}

# Increasing color scale (not divergent)
draw_cfsmap_increasing <- function(map_data, variable, title, scale_name, scale_breaks, add_theme = theme_void()) {
  linewidth <- 0.25 # hardcode to 1/2 the default
  # Calculate scale range to set zero in the middle
  variable <- enquo(variable)
  scale_limit <- map_data %>% pull(!!variable) %>% range(na.rm = TRUE)
  map_data <- mutate(map_data, !!variable := if_else(!!variable == 0, as.numeric(NA), !!variable))
  fill_scale <- scale_fill_viridis_c(name = scale_name, limits = scale_limit, guide = guide_colorbar(direction = 'horizontal'), na.value = 'gray75', trans = 'pseudo_log', breaks = scale_breaks)
  fill_scale_noleg <- scale_fill_viridis_c(limits = scale_limit, na.value = 'gray75', trans = 'pseudo_log')
  
  
  # Draw the three maps
  us_map <- ggplot(map_data %>% filter(!grepl('Alaska|Hawaii|Honolulu', FAF_Region))) +
    geom_sf(aes(fill = !!variable), size = linewidth) +
    fill_scale +
    add_theme +
    theme(legend.position = 'bottom') +
    ggtitle(title)
  
  # Include insets for Alaska and Hawaii
  # Projections and limits from https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-3.html
  hi_map <- ggplot(map_data %>% filter(grepl('Hawaii|Honolulu', FAF_Region))) +
    geom_sf(aes(fill = !!variable), size = linewidth) +
    coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE, datum = NA) +
    fill_scale_noleg +
    add_theme + 
    theme(legend.position = 'none')
  
  ak_map <- ggplot(map_data %>% filter(grepl('Alaska', FAF_Region))) +
    geom_sf(aes(fill = !!variable), size = linewidth) +
    coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA) +
    fill_scale_noleg +
    add_theme +
    theme(legend.position = 'none')
  
  # Place main map and inset maps on the same plot, scaling appropriately. Hardcode ratios
  ratio_ak <- 0.58
  ratio_hi <- 0.71
  size_ak <- 0.25
  size_hi <- 0.15
  three_maps <- ggdraw(us_map + add_theme) +
    draw_plot(ak_map, width = size_ak, height = size_ak * ratio_ak, x = -0.05, y = 0.15, vjust = 0) +
    draw_plot(hi_map, width = size_hi, height = size_hi * ratio_hi, x = 0.2, y = 0.15, vjust = 0)
  return(three_maps)
}



library(sf)
library(cowplot)
cfsmap <- st_read(dsn = file.path(fp_faf, 'Freight_Analysis_Framework_Regions/cfs_aea.gpkg'))

map_land_outbound <- cfsmap %>% left_join(land_outbound, by = c('Code' = 'dms_orig'))
map_land_inbound <- cfsmap %>% left_join(land_inbound, by = c('Code' = 'dms_dest'))
map_land_net <- cfsmap %>% left_join(land_netdomestic, by = c('Code' = 'region'))
map_land_import <- cfsmap %>% left_join(land_import, by = c('Code' = 'dms_dest'))
map_land_export <- cfsmap %>% left_join(land_export, by = c('Code' = 'dms_orig'))

dark_theme <-  theme_void() +
  theme(panel.background = element_rect(fill = 'black', color = NA),
        plot.background = element_rect(color = "black", fill = "black"),
        legend.text = element_text(color = 'white'),
        legend.title = element_text(color = 'white'),
        plot.title = element_text(color = 'white'),
        legend.position = c(0.7, 0.9))

# Total
p_landmap <- draw_cfsmap_divergent(map_land_net %>% mutate(land_flow = (cropland_flow + pastureland_flow)/1000), 
                                   variable = land_flow, 
                                   title = 'Net domestic virtual land transfers',
                                   scale_name = parse(text='1000~km^2'),
                                   scale_breaks = c(-30,-10,0,10,30),
                                   add_theme = dark_theme)

p_inboundmap <- draw_cfsmap_increasing(map_land_inbound %>% mutate(land_flow = (cropland_flow + pastureland_flow)/1000), 
                                       variable = land_flow, 
                                       title = 'Domestic virtual land inflows',
                                       scale_name = parse(text='1000~km^2'),
                                       scale_breaks = c(1, 3, 10),
                                       add_theme = dark_theme)

p_outboundmap <- draw_cfsmap_increasing(map_land_outbound %>% mutate(land_flow = (cropland_flow + pastureland_flow)/1000), 
                                        variable = land_flow, 
                                        title = 'Domestic virtual land outflows',
                                        scale_name = parse(text='1000~km^2'),
                                        scale_breaks = c(1,3,10,30),
                                        add_theme = dark_theme)

p_exportmap <- draw_cfsmap_increasing(map_land_export %>% mutate(land_flow = (cropland_flow + pastureland_flow)/1000), 
                                        variable = land_flow, 
                                        title = 'Foreign virtual land exports',
                                        scale_name = parse(text='1000~km^2'),
                                        scale_breaks = c(1,3,10,30),
                                        add_theme = dark_theme)

p_importmap <- draw_cfsmap_increasing(map_land_import %>% mutate(land_flow = (cropland_flow + pastureland_flow)/1000), 
                                        variable = land_flow, 
                                        title = 'Foreign virtual land imports',
                                        scale_name = parse(text='1000~km^2'),
                                        scale_breaks = c(1,3,10,30),
                                        add_theme = dark_theme)


# Cropland only
p_landmap_crop <- draw_cfsmap_divergent(map_land_net %>% mutate(land_flow = (cropland_flow)/1000), 
                                        variable = land_flow, 
                                        title = 'Net domestic virtual cropland transfers',
                                        scale_name = parse(text='1000~km^2'),
                                        scale_breaks = c(-30,-10,0,10,30),
                                        add_theme = dark_theme)

p_inboundmap_crop <- draw_cfsmap_increasing(map_land_inbound %>% mutate(land_flow = (cropland_flow)/1000), 
                                            variable = land_flow, 
                                            title = 'Domestic virtual cropland inflows',
                                            scale_name = parse(text='1000~km^2'),
                                            scale_breaks = c(1, 3, 10),
                                            add_theme = dark_theme)

p_outboundmap_crop <- draw_cfsmap_increasing(map_land_outbound %>% mutate(land_flow = (cropland_flow)/1000), 
                                             variable = land_flow, 
                                             title = 'Domestic virtual cropland outflows',
                                             scale_name = parse(text='1000~km^2'),
                                             scale_breaks = c(1,3,10,30),
                                             add_theme = dark_theme)

p_exportmap_crop <- draw_cfsmap_increasing(map_land_export %>% mutate(land_flow = (cropland_flow)/1000), 
                                             variable = land_flow, 
                                             title = 'Foreign virtual cropland exports',
                                             scale_name = parse(text='1000~km^2'),
                                             scale_breaks = c(1,3,10,30),
                                             add_theme = dark_theme)

p_importmap_crop <- draw_cfsmap_increasing(map_land_import %>% mutate(land_flow = (cropland_flow)/1000), 
                                             variable = land_flow, 
                                             title = 'Foreign virtual cropland imports',
                                             scale_name = parse(text='1000~km^2'),
                                             scale_breaks = c(1,3,10,30),
                                             add_theme = dark_theme)

# Pastureland only
p_landmap_past <- draw_cfsmap_divergent(map_land_net %>% mutate(land_flow = (pastureland_flow)/1000), 
                                        variable = land_flow, 
                                        title = 'Net domestic virtual pastureland transfers',
                                        scale_name = parse(text='1000~km^2'),
                                        scale_breaks = c(-10,0,10),
                                        add_theme = dark_theme)

p_inboundmap_past <- draw_cfsmap_increasing(map_land_inbound %>% mutate(land_flow = (pastureland_flow)/1000), 
                                            variable = land_flow, 
                                            title = 'Domestic virtual pastureland inflows',
                                            scale_name = parse(text='1000~km^2'),
                                            scale_breaks = c(1, 2,5,10,20),
                                            add_theme = dark_theme)

p_outboundmap_past <- draw_cfsmap_increasing(map_land_outbound %>% mutate(land_flow = (pastureland_flow)/1000), 
                                             variable = land_flow, 
                                             title = 'Domestic virtual pastureland outflows',
                                             scale_name = parse(text='1000~km^2'),
                                             scale_breaks = c(1,3,10),
                                             add_theme = dark_theme)

p_exportmap_past <- draw_cfsmap_increasing(map_land_export %>% mutate(land_flow = (pastureland_flow)/1000), 
                                           variable = land_flow, 
                                           title = 'Foreign virtual pastureland exports',
                                           scale_name = parse(text='1000~km^2'),
                                           scale_breaks = c(1,3,10,30),
                                           add_theme = dark_theme)

p_importmap_past <- draw_cfsmap_increasing(map_land_import %>% mutate(land_flow = (pastureland_flow)/1000), 
                                           variable = land_flow, 
                                           title = 'Foreign virtual pastureland imports',
                                           scale_name = parse(text='1000~km^2'),
                                           scale_breaks = c(1,3,10,30),
                                           add_theme = dark_theme)

# Write maps to PNG output

walk2(list(p_landmap, p_inboundmap, p_outboundmap, p_landmap_crop, p_inboundmap_crop, p_outboundmap_crop, p_landmap_past, p_inboundmap_past, p_outboundmap_past),
      c('netflows_total', 'inflows_total', 'outflows_total', 'netflows_cropland', 'inflows_cropland', 'outflows_cropland', 'netflows_pastureland', 'inflows_pastureland', 'outflows_pastureland'),
      ~ ggsave(file.path(fp, 'figures/fafmaps', paste0(.y, '.png')), .x, dpi = 300))

walk2(list(p_exportmap, p_importmap, p_exportmap_crop, p_importmap_crop, p_exportmap_past, p_importmap_past),
      c('exports_total', 'imports_total', 'exports_cropland', 'imports_cropland', 'exports_pastureland', 'imports_pastureland'),
      ~ ggsave(file.path(fp, 'figures/fafmaps', paste0(.y, '.png')), .x, dpi = 300))
