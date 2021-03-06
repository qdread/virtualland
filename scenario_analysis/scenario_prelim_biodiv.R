# Biodiversity impacts, modeled by Chaudhary & Kastner, for the four food system scenarios in USA
# QDR / Virtualland / 01 Oct 2020

# We have the biodiversity model results with characterization factors for amphibians, birds, mammals, and reptiles.
# Annual crops, permanent crops, pasture, managed forests, and urban landuse

# For now we have baseline, 50% meat replaced, 50% waste reduction, and 100% optimal transport (as local ag as possible)

# For each scenario we have square meters of virtual land transfers BY ECOREGION,
# and the CFs are in species lost per meter squared.
# So we just need to join them, then multiply the virtual land transfers (area) by the CFs (species lost per area)

# Modified 04 Dec 2020: Also include the CFs from 2019 (no conf interval)
# Modified 24 Nov 2020: Run for all eight scenarios (2x2x2 factorial)
# Modified 19 Nov 2020: Separate annual and permanent crop transformation factors.
# FIXME for now this is domestic only (not foreign)
# FIXME they have a land transformation part with some time to regeneration values per land type which I am ignoring right now
# FIXME in later editions, better to calculate biodiversity damage from land use WITHIN each region, then proportionally allocate
# FIXME the flows, rather than calculating the flows first and then the biodiversity damage, I think.

# Load data ---------------------------------------------------------------

library(tidyverse)

is_local <- dir.exists('Q:/')

fp <- ifelse(is_local, 'Q:', '/nfs/qread-data')
fp_cfs <- file.path(fp, 'raw_data/commodity_flows/CFS')
fp_faf <- file.path(fp, 'raw_data/commodity_flows/FAF')
fp_crosswalk <- file.path(fp, 'crossreference_tables')
fp_out <- file.path(fp, 'cfs_io_analysis')
fp_chaud <- file.path(fp, 'raw_data/biodiversity/chaudhary2015SI')
fp_github <- ifelse(is_local, '~/Documents/GitHub/foodwaste/virtualland', '~/virtualland')
fp_scen <- file.path(fp_out, 'scenarios')

# Needed data to load: domestic (later foreign) virtual land transfers, characterization factors from Chaudhary

# VLT for the four scenarios, TNC x TNC
vlt_scenarios <- read_csv(file.path(fp_scen, 'landflows_tnc_x_tnc_2x2x2_factorial_provisional.csv'))

# Characterization factors (from read_chaudh_si.R)
chaudsi <- read_csv(file.path(fp_chaud, 'chaud2015SI2.csv'))

# Updated characterization factors from Chaudhary and Brooks 2018
chaudsi2018 <- read_csv(file.path(fp_chaud, 'chaud2018si_CFs.csv'))

# Join CF and VLT ---------------------------------------------------------

# Chaudhary 2015 CFs reshape (with annual nad permanent cropland separated)
chaudsi_coarse <- chaudsi %>%
  filter(landuse %in% c('Annual crops', 'Permanent crops', 'Pasture')) %>%
  mutate(landuse = tolower(gsub(' ', '_', landuse))) %>%
  pivot_wider(names_from = landuse, values_from = c(lower95ci, median, upper95ci)) %>%
  pivot_longer(-c(CF, ecoregion, taxon)) %>%
  separate(name, into = c('stat', 'landuse'), sep = '_')

# Process Chaudhary 2018 CFs data in preparation for joining with VLT data
# Plantation = permanent cropland, Crop = annual cropland
# Keep different intensity levels for now, and include 
chaudsi_processed <- chaudsi2018 %>%
  filter(region_type %in% 'ecoregion', land_use %in% c('crop', 'pasture', 'plantation'), unit %in% 'potential species loss y m-2') %>%
  mutate(land_use = case_when(land_use == 'crop' ~ 'annual',
                              land_use == 'plantation' ~ 'permanent')) %>%
  select(-region_type, -unit)

# Join land transfers and characterization factors for each scenario
VLT_CF <- vlt_scenarios %>%
    rename(annual = annual_cropland_flow, permanent = permanent_cropland_flow, pasture = pastureland_flow) %>%
    pivot_longer(c(annual, permanent, pasture), names_to = 'land_use', values_to = 'VLT') %>%
    full_join(chaudsi_processed, by = c('TNC_orig' = 'ecoregion_code', 'land_use' = 'land_use')) %>%
    mutate(species_lost = VLT * value * 1e6)

# Write output
write_csv(VLT_CF, file.path(fp_scen, 'species_lost_2x2x2_factorial_provisional.csv'))

# Write smaller file with subset of output for plotting
# Ignore the land transformation part.
# Now we only are using global characterization factors, not regional. Regional were not included in this table.
splost_filtered <- VLT_CF %>%
  filter(complete.cases(.), CF_type %in% 'occupation')

write_csv(splost_filtered, file.path(fp_scen, 'species_lost_2x2x2_factorial_provisional_occ.csv'))
