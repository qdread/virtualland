# Map showing ecoregions in US states, plus the political boundaries.
# Same for foreign countries.
# QDR / Virtualland / 05 Apr 2021

source('figs/figs_v2_loaddata.R')

# Additionally, load foreign ecoregion map and country map
global_eco_map <- st_read('data/raw_data/landuse/ecoregions/tnc_global_equalarea.gpkg')
global_country_map <- st_read('data/raw_data/landuse/ecoregions/countries_global_equalarea.gpkg') %>%
  select(NAME_LONG, ISO_A3)

library(cowplot)

# US ecoregion x state map ------------------------------------------------

# Get fill scale for all the realms
realm_names <- unique(tnc_map$WWF_MHTNAM)
realm_colors <- RColorBrewer::brewer.pal(n = 11, name = 'Set3')
realm_fill_scale <- scale_fill_manual(name = 'Biome', values = setNames(realm_colors, realm_names), guide = guide_legend(ncol = 2))


p48 <- ggplot() +
  geom_sf(aes(fill = WWF_MHTNAM), data = tnc_map %>% filter(!tnc_ak_idx, !tnc_hi_idx), size = 0.5, color = 'gray70') +
  geom_sf(data = county_map %>% filter(!county_ak_idx, !county_hi_idx), size = 0.25, color = 'black', fill = 'transparent') +
  geom_sf(data = state_map %>% filter(!fips_state %in% c('02', '15')), size = 0.75, color = 'black', fill = 'transparent') +
  realm_fill_scale +
  theme(legend.position = 'none')

ak_crs <- '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs'

pak <- ggplot() +
  geom_sf(aes(fill = WWF_MHTNAM), data = tnc_map %>% filter(tnc_ak_idx) %>% st_transform(ak_crs), size = 0.5, color = 'gray70') +
  geom_sf(data = county_map %>% filter(county_ak_idx) %>% st_transform(ak_crs), size = 0.25, color = 'black', fill = 'transparent') +
  geom_sf(data = state_map %>% filter(fips_state %in% c('02')) %>% st_transform(ak_crs), size = 0.75, color = 'black', fill = 'transparent') +
  realm_fill_scale + 
  theme(legend.position = 'none')

hi_crs <- '+proj=aea +lat_1=8.000000000000002 +lat_2=18 +lat_0=3 +lon_0=-157 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs'
# Bounding box set manually to get rid of minor outlying islands
hi_box <- c(xmin = -400000, ymin = 1761000, xmax = 230000, ymax = 2130000)

phi <- ggplot() +
  geom_sf(aes(fill = WWF_MHTNAM), data = tnc_map %>% filter(tnc_hi_idx) %>% st_transform(hi_crs), size = 0.5, color = 'gray70') +
  geom_sf(data = county_map %>% filter(county_hi_idx) %>% st_transform(hi_crs), size = 0.25, color = 'black', fill = 'transparent') +
  geom_sf(data = state_map %>% filter(fips_state %in% c('15')) %>% st_transform(hi_crs), size = 0.75, color = 'black', fill = 'transparent') +
  coord_sf(xlim = hi_box[c('xmin','xmax')], ylim = hi_box[c('ymin','ymax')]) +
  realm_fill_scale +
  theme(legend.position = 'none')

# Draw legend with all biomes
p_dummy <- ggplot(data.frame(Biome = realm_names, x = 1:11, y = 1:11), aes(x=x, y=y, fill=Biome)) + 
  geom_col() +
  realm_fill_scale +
  theme(legend.text = element_text(size = rel(0.75)))

leg <- get_legend(p_dummy)

# Lay out the plot.
#plot_grid(p48, plot_grid(pak, phi, leg, nrow = 3), nrow = 1)
p_all <- plot_grid(plot_grid(p48, leg, nrow = 2, align = 'none', rel_heights = c(2, 1)), 
                   plot_grid(pak, phi, nrow = 2, align = 'h'), 
                   align = 'none', nrow = 1, rel_widths = c(2, 1))

ggsave('data/cfs_io_analysis/scenario_v2_figs/usa_ecoregions.png', p_all, height = 7, width = 10, dpi = 300)

# Global ecoregion map ----------------------------------------------------

# Get fill scale for all the realms
realm_names_extra <- setdiff(global_eco_map$WWF_MHTNAM, tnc_map$WWF_MHTNAM)
realm_colors_extra <- RColorBrewer::brewer.pal(n = 5, name = 'Dark2')
realm_fill_scale_world <- scale_fill_manual(name = 'Biome', values = setNames(c(realm_colors, realm_colors_extra), c(realm_names, realm_names_extra)), guide = guide_legend(ncol = 3))

pworld <- ggplot() +
  geom_sf(aes(fill = WWF_MHTNAM), data = global_eco_map, size = 0.5, color = 'gray70') +
  geom_sf(data = global_country_map, size = 0.5, color = 'black', fill = 'transparent') +
  realm_fill_scale_world +
  theme(legend.position = 'bottom', legend.text = element_text(size = rel(0.6)), legend.title = element_blank())

ggsave('data/cfs_io_analysis/scenario_v2_figs/world_ecoregions.png', pworld, height = 7, width = 8, dpi = 300)
