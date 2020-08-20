# Map to show the amount of cropland and pastureland transferred from each ecoregion.
# (Diagnostic to ensure the method worked)

# Load spatial data
library(tidyverse)
library(sf)

tnc <- st_read('/nfs/qread-data/raw_data/landuse/ecoregions', layer = 'tnc_usa_aea')
faf_flows_tnc <- read_csv('/nfs/qread-data/cfs_io_analysis/FAF_all_flows_x_TNC.csv')

# Sum up flows by outgoing region
tnc_outgoing_flows <- faf_flows_tnc %>%
  group_by(ECO_CODE) %>%
  summarize(cropland_flow = sum(cropland_flow, na.rm = TRUE),
            pastureland_flow = sum(pastureland_flow, na.rm = TRUE))

tnc <- left_join(tnc, tnc_outgoing_flows)

# Separate tnc spatial data into Alaska and Hawaii.
tnc_hi <- tnc %>% filter(grepl("^OC", region))
tnc_ak <- tnc %>% filter(grepl("Alaska", region))


# Alaska and Hawaii both have zeroes for all transfers. For now, exclude them.
ggplot(tnc_ak) +
  geom_sf(aes(fill = cropland_flow)) +
  scale_fill_viridis_c(trans = 'log10', name = parse(text = 'Flow~(km^2)')) 

p_tnc_crop <- ggplot(tnc %>% filter(!grepl("^OC|Alaska|Tundra|Yukon|Aleut|Bristol Bay|Cook Inlet|Boreal Cordillera", region))) +
  geom_sf(aes(fill = cropland_flow)) +
  scale_fill_viridis_c(trans = 'log10', name = parse(text = 'Flow~(km^2)')) +
  ggtitle('Cropland virtually transferred from each TNC ecoregion in continental USA')

p_tnc_pasture <- ggplot(tnc %>% filter(!grepl("^OC|Alaska|Tundra|Yukon|Aleut|Bristol Bay|Cook Inlet|Boreal Cordillera", region))) +
  geom_sf(aes(fill = pastureland_flow)) +
  scale_fill_viridis_c(trans = 'log10', name = parse(text = 'Flow~(km^2)')) +
  ggtitle('Pastureland virtually transferred from each TNC ecoregion in continental USA')

ggsave('/nfs/qread-data/cfs_io_analysis/maps/tnc_outgoing_crop.png', p_tnc_crop, height = 6, width = 8, dpi = 400)
ggsave('/nfs/qread-data/cfs_io_analysis/maps/tnc_outgoing_pasture.png', p_tnc_pasture, height = 6, width = 8, dpi = 400)
