# Visualization of results from 2x2x2 scenarios (preliminary)
# QDR / Virtualland / 24 Nov 2020

# For now only show the species lost.

fp <- '~/Dropbox/Q/projects/foodwaste/Data/virtualland'

library(tidyverse)
library(sf)

# Biodiversity threat data
# ========================
splost <- read_csv(file.path(fp, 'species_lost_2x2x2_factorial_provisional_regionalocc.csv'))

# Spatial data
# ============
# Read TNC map
tnc_map <- st_read(file.path(fp, 'tnc_usa_aea.gpkg')) %>% select(-ECO_ID_U)

# Remove zones in Alaska and Hawaii
zones_ak <- tnc_map$ECO_CODE[grep("Alaska|Tundra|Yukon|Aleut|Bristol Bay|Cook Inlet|Boreal Cordillera", tnc_map$ECODE_NAME)]
zones_hi <- tnc_map$ECO_CODE[grep("^OC", tnc_map$ECODE_NAME)]
zones_remove <- c(zones_ak, zones_hi)

tnc_map48 <- tnc_map %>% filter(!ECO_CODE %in% zones_remove)

# Read CFS region map
cfs_map <- st_read(file.path(fp, 'cfs_aea.gpkg'))

# Alaska and Hawaii indices
ak_idx_tnc <- tnc_map$ECO_CODE %in% zones_ak
hi_idx_tnc <- tnc_map$ECO_CODE %in% zones_hi
ak_idx_cfs <- grepl('Alaska', cfs_map$FAF_Region)
hi_idx_cfs <- grepl('Hawaii|Honolulu', cfs_map$FAF_Region)

# Species lost plots ------------------------------------------------------

# Summarize species loss data
# Sum by land use
splost_alltaxa <- splost %>%
  filter(taxon == 'Taxa Aggregated (Units - PDF/m2)', stat == 'median') %>%
  group_by(scenario, TNC_orig, TNC_dest, CF) %>%
  summarize(species_lost = sum(species_lost))

# Sum up the species lost from each region
splost_byorigin <- splost_alltaxa %>%
  group_by(scenario, TNC_orig, CF) %>%
  summarize(species_lost = sum(species_lost))

scenario_grid <- cbind(scenario = 1:8,
                       expand_grid(diet_shift = c(FALSE, TRUE),
                                   waste_reduction = c(FALSE, TRUE),
                                   optimal_transport = c(FALSE, TRUE)))

splost_byorigin <- full_join(scenario_grid, splost_byorigin) %>%
  mutate(diet_shift = if_else(diet_shift, 'D', ''),
         waste_reduction = if_else(waste_reduction, 'W', ''),
         optimal_transport = if_else(optimal_transport, 'T', ''),
         scenario_label = paste0(diet_shift, waste_reduction, optimal_transport)) %>%
  mutate(scenario_label = if_else(nchar(scenario_label) == 0, 'baseline', scenario_label)) %>%
  mutate(scenario_label = factor(scenario_label, levels = c('baseline','D','W','T','DW','DT','WT','DWT')))

ggplot(splost_byorigin %>% filter(CF == 'Occ_average_regional', !TNC_orig %in% zones_remove), aes(x = scenario_label, y = species_lost)) +
  geom_boxplot() +
  theme_minimal() +
  scale_y_log10()


# Species lost maps -------------------------------------------------------

# Widen the species lost data and calculate threat reduction relative to baseline.
# Then plot D, W, and DW
threat_reductions_wide <- splost_byorigin %>% 
  filter(CF == 'Occ_average_regional', !TNC_orig %in% zones_remove) %>%
  select(-scenario, -diet_shift, -waste_reduction, -optimal_transport, -CF) %>%
  pivot_wider(names_from = scenario_label, values_from = species_lost) %>%
  mutate_at(vars(T:DWT), ~ ./baseline)
  

tnc_map_threat_reduction <- tnc_map48 %>% left_join(threat_reductions_wide, by = c('ECO_CODE' = 'TNC_orig'))

tnc_map_threat_reduction_long <- tnc_map_threat_reduction %>%
  select(D,W,DW) %>%
  gather(-geom, key = 'scenario', value = 'threat_reduction')

fill_ramp <- c('goldenrod', 'white', 'darkgreen')

fill_scale_2color <- scale_fill_gradient2(name = 'threat reduction\nratio', 
                                          low = fill_ramp[3],
                                          mid = fill_ramp[2],
                                          high = fill_ramp[1],
                                          midpoint = 1,
                                          na.value = 'gray75', 
                                          guide = guide_colorbar(direction = 'horizontal'))

maps_tnc_threat_reduction <- ggplot(tnc_map_threat_reduction_long) +
  geom_sf(aes(fill = threat_reduction), size = 0.25) +
  facet_wrap(~ scenario, labeller = labeller(scenario = c(D = '50% Vegetarian Shift',
                                                          W = '50% Waste Reduction',
                                                          DW = 'Both Interventions'))) +
  fill_scale_2color +
  theme_bw() +
  theme(legend.position = 'bottom') 
