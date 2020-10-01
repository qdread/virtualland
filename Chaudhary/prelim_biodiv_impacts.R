# Initial attempt to get biodiversity footprint of food shipments in the United States using Chaudhary et al. data.
# QDR / Virtualland / 28 Sept 2020

# We have the biodiversity model results with characterization factors for amphibians, birds, mammals, and reptiles.
# Annual crops, permanent crops, pasture, managed forests, and urban landuse

# We want a 2x2x2 factorial design of scenarios ultimately:
# 1. Biodiversity footprint of food shipments normally (baseline)
# 2. Biodiversity footprint if food waste is decreased by 50% (across all food categories, very crudely)
# 3. Biodiversity footprint if all domestic transfers in the USA are switched to "optimal" (or 50% thereof if I can't figure that out)
# 4. Biodiversity footprint if 50% of meat calories are replaced with plant based calories, using the most reasonably available crops for each location
# 5-7. Each combination of 2 of the 3 factors.
# 8. All 3 of the factors. 

# Method: For a given scenario, we work out the relevant shipments of ag goods. 
# We have square meters of virtual land transfers BY ECOREGION, and the CFs are in species lost per meter squared.
# So we just need to join them, then multiply the virtual land transfers (area) by the CFs (species lost per area)

# For the optimal transport, we need to convert the optimal FAF transfers to optimal ecoregion transfers
# For the food waste decrease, we can just reduce the size of the transfers
# For the diet shifts, that's more complicated. We would have to make a basic assumption on what mix of grains/pulses/fruits/veggies would replace the meat.

# For now, let's start with the baseline and then the optimal transport.

# Load data ---------------------------------------------------------------

library(tidyverse)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_cfs <- file.path(fp, 'raw_data/commodity_flows/CFS')
fp_faf <- file.path(fp, 'raw_data/commodity_flows/FAF')
fp_crosswalk <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'crossreference_tables')
fp_out <- file.path(ifelse(is_local, 'Q:', '/nfs/qread-data'), 'cfs_io_analysis')
fp_chaud <- file.path(fp, 'raw_data/biodiversity/chaudhary2015SI')
fp_github <- ifelse(is_local, '~/Documents/GitHub/foodwaste/virtualland', '~/virtualland')

# Needed data to load: domestic and foreign virtual land transfers, characterization factors from Chaudhary

# VLT
domestic_flows <- read_csv(file.path(fp_out, 'TNC_x_TNC_all_flows.csv'))
foreign_flows <- read_csv(file.path(fp_out, 'TNC_x_TNC_all_foreign_flows.csv'))

# Characterization factors (from read_chaudh_si.R)
chaudsi <- read_csv(file.path(fp_chaud, 'chaud2015SI2.csv'))


# Join CF and VLT ---------------------------------------------------------

# Coarsen Chaudhary by combining annual and permanent crops into one average.
# This is a preliminary value that will be changed.
# Or we could just use annual cropland value?
chaudsi_coarse <- chaudsi %>%
  filter(landuse %in% c('Annual crops', 'Permanent crops', 'Pasture')) %>%
  mutate(landuse = tolower(gsub(' ', '_', landuse))) %>%
  pivot_wider(names_from = landuse, values_from = c(lower95ci, median, upper95ci)) %>%
  mutate(lower95ci_crops = (lower95ci_annual_crops + lower95ci_permanent_crops)/2,
         median_crops = (median_annual_crops + median_permanent_crops)/2,
         upper95ci_crops = (upper95ci_annual_crops + upper95ci_permanent_crops)/2) %>%
  select(-contains('annual'), -contains('permanent')) %>%
  pivot_longer(-c(CF, ecoregion, taxon)) %>%
  separate(name, into = c('stat', 'landuse'), sep = '_')


# Join
domestic_flows_CF <- domestic_flows %>%
  rename(crops = cropland_flow, pasture = pastureland_flow) %>%
  pivot_longer(c(crops, pasture), names_to = 'landuse', values_to = 'VLT') %>%
  full_join(chaudsi_coarse, by = c('TNC_orig' = 'ecoregion', 'landuse' = 'landuse')) %>%
  mutate(species_lost = VLT * value * 1e6)

foreign_flows_CF <- foreign_flows %>%
  rename(crops = cropland_flow, pasture = pastureland_flow) %>%
  pivot_longer(c(crops, pasture), names_to = 'landuse', values_to = 'VLT') %>%
  full_join(chaudsi_coarse, by = c('TNC_orig' = 'ecoregion', 'landuse' = 'landuse')) %>%
  mutate(species_lost = VLT * value * 1e6)


# Sum biodiversity footprints ---------------------------------------------

# Domestic, incoming
domestic_bio_import_sums <- domestic_flows_CF %>%
  filter(trade_type == 1) %>%
  group_by(TNC_dest, landuse, CF, taxon, stat) %>%
  summarize(species_lost = sum(species_lost, na.rm = TRUE)) %>%
  ungroup %>%
  filter(complete.cases(.))

# Sum across land use types and taxa as well
domestic_bio_import_sums_aggregated <- domestic_bio_import_sums %>%
  group_by(TNC_dest, CF, stat) %>%
  summarize(species_lost = sum(species_lost)) %>%
  pivot_wider(names_from = stat, values_from = species_lost) 

# Foreign, incoming
foreign_bio_import_sums <- foreign_flows_CF %>%
  group_by(TNC_dest, landuse, CF, taxon, stat) %>%
  summarize(species_lost = sum(species_lost, na.rm = TRUE)) %>%
  ungroup %>%
  filter(complete.cases(.))

foreign_bio_import_sums_aggregated <- foreign_bio_import_sums %>%
  group_by(TNC_dest, CF, stat) %>%
  summarize(species_lost = sum(species_lost)) %>%
  pivot_wider(names_from = stat, values_from = species_lost) 

# Put the aggregated sums into a single DF
bio_import_sums_agg <- bind_rows(
  tibble(flow = 'domestic', domestic_bio_import_sums_aggregated),
  tibble(flow = 'foreign', foreign_bio_import_sums_aggregated)
)

# Visualize summary totals ------------------------------------------------

ggplot(bio_import_sums_agg, aes(x = median, fill = flow)) +
  facet_wrap(~ CF, scales = 'free_x') +
  geom_histogram(bins = 20, alpha = 0.5, position = 'identity', color = 'black', lwd = 0.2) +
  scale_fill_brewer(palette = 'Dark2') +
  theme_minimal() +
  ggtitle('Virtual biodiversity transfers: USA')

