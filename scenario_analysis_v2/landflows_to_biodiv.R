# Biodiversity impacts, modeled by Chaudhary & Kastner, for diet x waste scenarios in USA
# QDR / Virtualland / 12 Jan 2021
# copied and modified from scenario_prelim_biodiv.R

# modified 21 Jan 2021: correct bug in case_when

# We have the biodiversity model results with characterization factors for amphibians, birds, mammals, and reptiles.
# Annual crops, permanent crops, pasture, managed forests, and urban landuse

# We have 20 scenarios: 5 diet scenarios x 4 waste reduction scenarios

# For each scenario we have square meters of virtual land transfers BY ECOREGION,
# and the CFs are in species lost per meter squared.
# So we just need to join them, then multiply the virtual land transfers (area) by the CFs (species lost per area)

# FIXME for now this is domestic only (not foreign)
# FIXME they have a land transformation part with some time to regeneration values per land type which I am ignoring right now

# Load data ---------------------------------------------------------------

library(tidyverse)

fp_out <- 'data/cfs_io_analysis'
fp_chaud <- 'data/raw_data/biodiversity/chaudhary2015SI'

# Needed data to load: domestic (later foreign) virtual land transfers, characterization factors from Chaudhary

# VLT for the four scenarios, TNC x TNC
vlt_scenarios <- read_csv(file.path(fp_out, 'scenarios/landflows_tnc_x_tnc_all_scenarios.csv'), col_types = 'cccddd')

# Updated characterization factors from Chaudhary and Brooks 2018
chaudsi2018 <- read_csv(file.path(fp_chaud, 'chaud2018si_CFs.csv'), col_types = 'cccccccccd')

# Join CF and VLT ---------------------------------------------------------

# Process Chaudhary 2018 CFs data in preparation for joining with VLT data
# Plantation = permanent cropland, Crop = annual cropland
# Keep different intensity levels for now, and include 
chaudsi_processed <- chaudsi2018 %>%
  filter(region_type %in% 'ecoregion', land_use %in% c('crop', 'pasture', 'plantation'), unit %in% 'potential species loss y m-2') %>%
  mutate(land_use = case_when(land_use == 'crop' ~ 'annual',
                              land_use == 'plantation' ~ 'permanent',
                              land_use == 'pasture' ~ 'pasture')) %>%
  select(-region_type, -unit)

# Join land transfers and characterization factors for each scenario
VLT_CF <- vlt_scenarios %>%
    rename(annual = annual_cropland_flow, permanent = permanent_cropland_flow, pasture = pastureland_flow) %>%
    pivot_longer(c(annual, permanent, pasture), names_to = 'land_use', values_to = 'VLT') %>%
    full_join(chaudsi_processed, by = c('TNC_from' = 'ecoregion_code', 'land_use' = 'land_use')) %>%
    rename(CF = value) %>%
    mutate(species_lost = VLT * CF)

# Write output
write_csv(VLT_CF, file.path(fp_out, 'scenarios/species_lost_all_scenarios.csv'))

# Write smaller file with subset of output for plotting
# Ignore the land transformation part.
# Now we only are using global characterization factors, not regional. Regional were not included in this table.
VLT_CF_filtered <- VLT_CF %>%
  filter(complete.cases(.), CF_type %in% 'occupation')

write_csv(VLT_CF_filtered, file.path(fp_out, 'scenarios/species_lost_all_scenarios_occ.csv'))

# The CSV is >1GB so may be slow to load. To help with this, split it up into intensity levels. Maybe just use medium for all.
VLT_CF_filtered %>%
  group_by(intensity) %>%
  group_walk(~ write_csv(.x, glue::glue('{fp_out}/scenarios/species_lost_all_scenarios_occ_{.y}.csv')), .keep = TRUE)
