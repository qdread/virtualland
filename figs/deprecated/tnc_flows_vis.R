# Visualize the top TNC regions' land flows.

library(tidyverse)
library(units)
library(sf)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_cfs <- file.path(fp, 'raw_data/commodity_flows/CFS')
fp_faf <- file.path(fp, 'raw_data/commodity_flows/FAF')
fp_crosswalk <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'crossreference_tables')
fp_out <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'cfs_io_analysis')
fp_github <- ifelse(is_local, '~/Documents/GitHub/foodwaste/virtualland', '~/virtualland')
fp_eco <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'raw_data/landuse/ecoregions')

# Load TNC x TNC flows
tnc_flows <- read_csv(file.path(fp_out, 'TNC_x_TNC_all_flows.csv'))

# Load TNC map
tnc_map <- st_read(fp_eco, 'tnc_usa_aea')
tnc_lookup <- tnc_map %>% st_set_geometry(NULL)
tnc_lookup_2col <- tnc_lookup %>% select(ECO_CODE, ECO_NAME)

# Join the lookup table with full names
tnc_flows <- tnc_flows %>% 
  left_join(tnc_lookup_2col, by = c('TNC_orig' = 'ECO_CODE')) %>%
  rename(TNC_orig_name = ECO_NAME) %>%
  left_join(tnc_lookup_2col, by = c('TNC_dest' = 'ECO_CODE')) %>%
  rename(TNC_dest_name = ECO_NAME)

# View top ten
tnc_flows %>% arrange(-cropland_flow) %>% filter(TNC_orig != TNC_dest)
tnc_flows %>% arrange(-pastureland_flow) %>% filter(TNC_orig != TNC_dest)

# Top 10 largest recipients
# Reorganize foreign exports to separate columns

tnc_flows_wide <- tnc_flows %>% 
  filter(!is.na(trade_type)) %>%
  pivot_longer(cols = c(cropland_flow, pastureland_flow)) %>%
  mutate(trade_type = if_else(trade_type == 1, 'domestic', 'export')) %>%
  pivot_wider(names_from = c(name, trade_type), values_from = value, values_fill = 0)


dest_flows <- tnc_flows_wide %>%
  group_by(TNC_dest, TNC_dest_name) %>%
  summarize_if(is.numeric, sum)

dest_flows %>% arrange(-cropland_flow_domestic)
dest_flows %>% arrange(-pastureland_flow_domestic)
dest_flows %>% arrange(-cropland_flow_export)
dest_flows %>% arrange(-pastureland_flow_export)

# Top 10 largest donors
orig_flows <- tnc_flows_wide %>% 
  group_by(TNC_orig, TNC_orig_name) %>%
  summarize_if(is.numeric, sum)

orig_flows %>% arrange(-cropland_flow_domestic)
orig_flows %>% arrange(-pastureland_flow_domestic)
orig_flows %>% arrange(-cropland_flow_export)
orig_flows %>% arrange(-pastureland_flow_export)

# Largest net importers and exporters

names(dest_flows) <- c('TNC', 'TNC_name', 'cropland_in_domestic', 'pastureland_in_domestic', 'cropland_in_export', 'pastureland_in_export')
names(orig_flows) <- c('TNC', 'TNC_name',  'cropland_out_domestic', 'pastureland_out_domestic', 'cropland_out_export', 'pastureland_out_export')

all_flows <- full_join(dest_flows, orig_flows)

# Sum up net flows, accounting for domestic and foreign exports.
net_flows <- all_flows %>% 
  mutate(cropland_net = cropland_in_domestic + cropland_in_export - cropland_out_domestic - cropland_out_export,
         pastureland_net = pastureland_in_domestic + pastureland_in_export - pastureland_out_domestic - pastureland_out_export)

net_flows %>% select(TNC_name, cropland_net) %>% arrange(-cropland_net) %>% print(n=100)
net_flows %>% arrange(-pastureland_net) %>% print(n=100)


# Maps of net flows
tnc_map <- tnc_map %>%
  left_join(net_flows, by = c('ECO_CODE' = 'TNC', 'ECO_NAME' = 'TNC_name'))

p_tnc_crop <- ggplot(tnc_map %>% filter(!grepl("^OC|Alaska|Tundra|Yukon|Aleut|Bristol Bay|Cook Inlet|Boreal Cordillera", region))) +
  geom_sf(aes(fill = cropland_net)) +
  scale_fill_viridis_c(name = parse(text = 'Flow~(km^2)')) +
  ggtitle('Net cropland flow for each TNC ecoregion in continental USA')

p_tnc_pasture <- ggplot(tnc_map %>% filter(!grepl("^OC|Alaska|Tundra|Yukon|Aleut|Bristol Bay|Cook Inlet|Boreal Cordillera", region))) +
  geom_sf(aes(fill = pastureland_net)) +
  scale_fill_viridis_c(name = parse(text = 'Flow~(km^2)')) +
  ggtitle('Net pastureland flow for each TNC ecoregion in continental USA')

p_tnc_crop_in_domestic <- ggplot(tnc_map %>% filter(!grepl("^OC|Alaska|Tundra|Yukon|Aleut|Bristol Bay|Cook Inlet|Boreal Cordillera", region))) +
  geom_sf(aes(fill = cropland_in_domestic)) +
  scale_fill_viridis_c(trans = 'log10', name = parse(text = 'Flow~(km^2)')) +
  ggtitle('Domestic cropland inflow for each TNC ecoregion in continental USA')

p_tnc_pasture_in_domestic <- ggplot(tnc_map %>% filter(!grepl("^OC|Alaska|Tundra|Yukon|Aleut|Bristol Bay|Cook Inlet|Boreal Cordillera", region))) +
  geom_sf(aes(fill = pastureland_in_domestic)) +
  scale_fill_viridis_c(trans = 'log10', name = parse(text = 'Flow~(km^2)')) +
  ggtitle('Domestic pastureland inflow for each TNC ecoregion in continental USA')

p_tnc_crop_out_domestic <- ggplot(tnc_map %>% filter(!grepl("^OC|Alaska|Tundra|Yukon|Aleut|Bristol Bay|Cook Inlet|Boreal Cordillera", region))) +
  geom_sf(aes(fill = cropland_out_domestic)) +
  scale_fill_viridis_c(trans = 'log10', name = parse(text = 'Flow~(km^2)')) +
  ggtitle('Domestic cropland outflow for each TNC ecoregion in continental USA')

p_tnc_pasture_out_domestic <- ggplot(tnc_map %>% filter(!grepl("^OC|Alaska|Tundra|Yukon|Aleut|Bristol Bay|Cook Inlet|Boreal Cordillera", region))) +
  geom_sf(aes(fill = pastureland_out_domestic)) +
  scale_fill_viridis_c(trans = 'log10', name = parse(text = 'Flow~(km^2)')) +
  ggtitle('Domestic pastureland outflow for each TNC ecoregion in continental USA')

p_tnc_crop_in_export <- ggplot(tnc_map %>% filter(!grepl("^OC|Alaska|Tundra|Yukon|Aleut|Bristol Bay|Cook Inlet|Boreal Cordillera", region))) +
  geom_sf(aes(fill = cropland_in_export)) +
  scale_fill_viridis_c(trans = 'log10', name = parse(text = 'Flow~(km^2)')) +
  ggtitle('Foreign cropland inflow for each TNC ecoregion in continental USA')

p_tnc_pasture_in_export <- ggplot(tnc_map %>% filter(!grepl("^OC|Alaska|Tundra|Yukon|Aleut|Bristol Bay|Cook Inlet|Boreal Cordillera", region))) +
  geom_sf(aes(fill = pastureland_in_export)) +
  scale_fill_viridis_c(trans = 'log10', name = parse(text = 'Flow~(km^2)')) +
  ggtitle('Foreign pastureland inflow for each TNC ecoregion in continental USA')

p_tnc_crop_out_export <- ggplot(tnc_map %>% filter(!grepl("^OC|Alaska|Tundra|Yukon|Aleut|Bristol Bay|Cook Inlet|Boreal Cordillera", region))) +
  geom_sf(aes(fill = cropland_out_export)) +
  scale_fill_viridis_c(trans = 'log10', name = parse(text = 'Flow~(km^2)')) +
  ggtitle('Foreign cropland outflow for each TNC ecoregion in continental USA')

p_tnc_pasture_out_export <- ggplot(tnc_map %>% filter(!grepl("^OC|Alaska|Tundra|Yukon|Aleut|Bristol Bay|Cook Inlet|Boreal Cordillera", region))) +
  geom_sf(aes(fill = pastureland_out_export)) +
  scale_fill_viridis_c(trans = 'log10', name = parse(text = 'Flow~(km^2)')) +
  ggtitle('Foreign pastureland outflow for each TNC ecoregion in continental USA')

p_tnc_crop_in_total <- ggplot(tnc_map %>% filter(!grepl("^OC|Alaska|Tundra|Yukon|Aleut|Bristol Bay|Cook Inlet|Boreal Cordillera", region))) +
  geom_sf(aes(fill = cropland_in_export + cropland_in_domestic)) +
  scale_fill_viridis_c(trans = 'log10', name = parse(text = 'Flow~(km^2)')) +
  ggtitle('Total cropland inflow for each TNC ecoregion in continental USA')

p_tnc_pasture_in_total <- ggplot(tnc_map %>% filter(!grepl("^OC|Alaska|Tundra|Yukon|Aleut|Bristol Bay|Cook Inlet|Boreal Cordillera", region))) +
  geom_sf(aes(fill = pastureland_in_export + pastureland_in_domestic)) +
  scale_fill_viridis_c(trans = 'log10', name = parse(text = 'Flow~(km^2)')) +
  ggtitle('Total pastureland inflow for each TNC ecoregion in continental USA')

p_tnc_crop_out_total <- ggplot(tnc_map %>% filter(!grepl("^OC|Alaska|Tundra|Yukon|Aleut|Bristol Bay|Cook Inlet|Boreal Cordillera", region))) +
  geom_sf(aes(fill = cropland_out_export + cropland_out_domestic)) +
  scale_fill_viridis_c(trans = 'log10', name = parse(text = 'Flow~(km^2)')) +
  ggtitle('Total cropland outflow for each TNC ecoregion in continental USA')

p_tnc_pasture_out_total <- ggplot(tnc_map %>% filter(!grepl("^OC|Alaska|Tundra|Yukon|Aleut|Bristol Bay|Cook Inlet|Boreal Cordillera", region))) +
  geom_sf(aes(fill = pastureland_out_export + pastureland_out_domestic)) +
  scale_fill_viridis_c(trans = 'log10', name = parse(text = 'Flow~(km^2)')) +
  ggtitle('Total pastureland outflow for each TNC ecoregion in continental USA')

p_crop_netzero <- ggplot(tnc_map %>% filter(!grepl("^OC|Alaska|Tundra|Yukon|Aleut|Bristol Bay|Cook Inlet|Boreal Cordillera", region))) +
  geom_sf(aes(fill = cropland_net > 0)) +
  ggtitle('Is the area a net cropland importer?')

p_pasture_netzero <- ggplot(tnc_map %>% filter(!grepl("^OC|Alaska|Tundra|Yukon|Aleut|Bristol Bay|Cook Inlet|Boreal Cordillera", region))) +
  geom_sf(aes(fill = pastureland_net > 0)) +
  ggtitle('Is the area a net pastureland importer?')

ggsave('/nfs/qread-data/cfs_io_analysis/maps/tncxtnc_net_crop.png', p_tnc_crop, height = 6, width = 8, dpi = 400)
ggsave('/nfs/qread-data/cfs_io_analysis/maps/tncxtnc_net_pasture.png', p_tnc_pasture, height = 6, width = 8, dpi = 400)
ggsave('/nfs/qread-data/cfs_io_analysis/maps/tncxtnc_indomestic_crop.png', p_tnc_crop_in_domestic, height = 6, width = 8, dpi = 400)
ggsave('/nfs/qread-data/cfs_io_analysis/maps/tncxtnc_indomestic_pasture.png', p_tnc_pasture_in_domestic, height = 6, width = 8, dpi = 400)
ggsave('/nfs/qread-data/cfs_io_analysis/maps/tncxtnc_inforeign_crop.png', p_tnc_crop_in_export, height = 6, width = 8, dpi = 400)
ggsave('/nfs/qread-data/cfs_io_analysis/maps/tncxtnc_inforeign_pasture.png', p_tnc_pasture_in_export, height = 6, width = 8, dpi = 400)
ggsave('/nfs/qread-data/cfs_io_analysis/maps/tncxtnc_outdomestic_crop.png', p_tnc_crop_out_domestic, height = 6, width = 8, dpi = 400)
ggsave('/nfs/qread-data/cfs_io_analysis/maps/tncxtnc_outdomestic_pasture.png', p_tnc_pasture_out_domestic, height = 6, width = 8, dpi = 400)
ggsave('/nfs/qread-data/cfs_io_analysis/maps/tncxtnc_outforeign_crop.png', p_tnc_crop_out_export, height = 6, width = 8, dpi = 400)
ggsave('/nfs/qread-data/cfs_io_analysis/maps/tncxtnc_outforeign_pasture.png', p_tnc_pasture_out_export, height = 6, width = 8, dpi = 400)
ggsave('/nfs/qread-data/cfs_io_analysis/maps/tncxtnc_intotal_crop.png', p_tnc_crop_in_total, height = 6, width = 8, dpi = 400)
ggsave('/nfs/qread-data/cfs_io_analysis/maps/tncxtnc_intotal_pasture.png', p_tnc_pasture_in_total, height = 6, width = 8, dpi = 400)
ggsave('/nfs/qread-data/cfs_io_analysis/maps/tncxtnc_outtotal_crop.png', p_tnc_crop_out_total, height = 6, width = 8, dpi = 400)
ggsave('/nfs/qread-data/cfs_io_analysis/maps/tncxtnc_outtotal_pasture.png', p_tnc_pasture_out_total, height = 6, width = 8, dpi = 400)
ggsave('/nfs/qread-data/cfs_io_analysis/maps/tncxtnc_netimporters_crop.png', p_crop_netzero, height = 6, width = 8, dpi = 400)
ggsave('/nfs/qread-data/cfs_io_analysis/maps/tncxtnc_netimporters_pasture.png', p_pasture_netzero, height = 6, width = 8, dpi = 400)