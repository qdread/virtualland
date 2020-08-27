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
# Exclude within-region flows.
dest_flows <- tnc_flows %>%
  group_by(TNC_dest, TNC_dest_name, trade_type) %>%
  summarize(cropland_flow = sum(cropland_flow),
            pastureland_flow = sum(pastureland_flow))

dest_flows %>% arrange(-cropland_flow)
dest_flows %>% arrange(-pastureland_flow)

# Top 10 largest donors
orig_flows <- tnc_flows %>% 
  group_by(TNC_orig, TNC_orig_name, trade_type) %>%
  summarize(cropland_flow = sum(cropland_flow),
            pastureland_flow = sum(pastureland_flow))

orig_flows %>% arrange(-cropland_flow)
orig_flows %>% arrange(-pastureland_flow)

# Largest net importers and exporters

names(dest_flows) <- c('TNC', 'TNC_name', 'trade_type', 'cropland_in', 'pastureland_in')
names(orig_flows) <- c('TNC', 'TNC_name', 'trade_type', 'cropland_out', 'pastureland_out')

all_flows <- full_join(dest_flows, orig_flows)

net_flows <- all_flows %>% 
  filter(trade_type == 1) %>%
  mutate(cropland_net = cropland_in - cropland_out,
         pastureland_net = pastureland_in - pastureland_out)

net_flows %>% arrange(-cropland_net) %>% print(n=100)
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

ggsave('/nfs/qread-data/cfs_io_analysis/maps/tncxtnc_net_crop.png', p_tnc_crop, height = 6, width = 8, dpi = 400)
ggsave('/nfs/qread-data/cfs_io_analysis/maps/tncxtnc_net_pasture.png', p_tnc_pasture, height = 6, width = 8, dpi = 400)

p_tnc_crop_in <- ggplot(tnc_map %>% filter(!grepl("^OC|Alaska|Tundra|Yukon|Aleut|Bristol Bay|Cook Inlet|Boreal Cordillera", region))) +
  geom_sf(aes(fill = cropland_in)) +
  scale_fill_viridis_c(trans = 'log10', name = parse(text = 'Flow~(km^2)')) +
  ggtitle('Cropland inflow for each TNC ecoregion in continental USA')

p_tnc_pasture_in <- ggplot(tnc_map %>% filter(!grepl("^OC|Alaska|Tundra|Yukon|Aleut|Bristol Bay|Cook Inlet|Boreal Cordillera", region))) +
  geom_sf(aes(fill = pastureland_in)) +
  scale_fill_viridis_c(trans = 'log10', name = parse(text = 'Flow~(km^2)')) +
  ggtitle('Pastureland inflow for each TNC ecoregion in continental USA')
