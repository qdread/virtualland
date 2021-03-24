# Sensitivity analysis of effect of considering land use intensity and CF type
# Only save summary results; domestic only
# QDR / Virtualland / 23 Mar 2021

# Process Chaudhary data --------------------------------------------------

library(data.table)
library(rslurm)

# Updated characterization factors from Chaudhary and Brooks 2018
chaudsi2018 <- fread('data/raw_data/biodiversity/chaudhary2015SI/chaud2018si_CFs.csv', colClasses = rep(c('character', 'double'), c(9, 1)))

# Process Chaudhary 2018 CFs data in preparation for joining with VLT data
# Plantation = permanent cropland, Crop = annual cropland
chaudsi_processed <- chaudsi2018[region_type %in% 'ecoregion' & land_use %in% c('crop', 'pasture', 'plantation') & unit %in% 'potential species loss y m-2']
chaudsi_processed[, land_use := fcase(land_use == 'crop', 'annual',
                                      land_use == 'plantation', 'permanent',
                                      land_use == 'pasture', 'pasture')]
chaudsi_processed[, c('region_type', 'unit', 'ecoregion_name') := NULL]   

# Filter out only the mean value. But keep all intensities and CF types
chaudsi_processed <- chaudsi_processed[statistic %in% 'mean']

# Function to read VLT, join with Chaud, and calc extinctions -------------

extinctions_by_scenario <- function(diet, waste, intensity, CF_type) {
  # Read VLT values for scenario
  VLT <- fread(glue::glue('/nfs/qread-data/cfs_io_analysis/ecoregion_landflow_csvs/D_{diet}_WR_{waste}_county_x_county_landtncweights.csv'), 
               colClasses = rep(c('character', 'double'), c(4, 3)))
  
  # Subset CF
  CF <- chaudsi_processed[eval(chaudsi_processed[, intensity %in% ..intensity & CF_type %in% ..CF_type])]
  
  # Join land transfers and characterization factors
  setnames(VLT, old = c('annual_cropland', 'permanent_cropland', 'pastureland'), new = c('annual', 'permanent', 'pasture'))
  VLT <- melt(VLT, measure.vars = c('annual', 'permanent', 'pasture'), variable.name = 'land_use', value.name = 'VLT')
  
  VLT_CF <- merge(VLT, CF, 
                  by.x = c('TNC_from', 'land_use'), by.y = c('ecoregion_code', 'land_use'), 
                  all.x = TRUE, all.y = FALSE, allow.cartesian = TRUE)
  
  setnames(VLT_CF, old = 'value', new = 'CF')
  VLT_CF[, species_lost := VLT * CF]
  
  # Sum the mean species lost across all the TNC regions so we only have pairwise county extinctions
  VLT_CF <- VLT_CF[, .(species_lost = sum(species_lost, na.rm = TRUE)), by = .(scenario, intensity, CF_type, county_from, county_to, land_use, taxon)]
  VLT_CF <- VLT_CF[!is.na(taxon)]

  # Sum all species lost, ignoring any geography, for the sensitivity analysis
  totals <- VLT_CF[, .(species_lost = sum(species_lost)), by = .(scenario, intensity, CF_type, land_use, taxon)]

  fwrite(totals, glue::glue('/nfs/qread-data/cfs_io_analysis/lu_sensitivity/D_{diet}_WR_{waste}_{intensity}_{CF_type}_extinction_sums.csv'))
  
}


# Apply function across scenarios -----------------------------------------

scenario_combos <- expand.grid(diet = c('baseline','planetaryhealth','usstyle','medstyle','vegetarian'),
                               waste = c('baseline','preconsumer','consumer','allavoidable'),
                               intensity = c('low', 'med', 'high'),
                               CF_type = c('occupation', 'transformation'), stringsAsFactors = FALSE)

sjob_extinctions <- slurm_apply(extinctions_by_scenario, scenario_combos, 
                                jobname = 'lui_sensitivity', nodes = 8, cpus_per_node = 1, 
                                global_objects = c('chaudsi_processed'),
                                slurm_options = list(partition = 'sesync'))

cleanup_files(sjob_extinctions)


# Load and concatenate and write ------------------------------------------

extinctions_all <- purrr::pmap_dfr(scenario_combos, function(diet, waste, intensity, CF_type) fread(glue::glue('/nfs/qread-data/cfs_io_analysis/lu_sensitivity/D_{diet}_WR_{waste}_{intensity}_{CF_type}_extinction_sums.csv')))

fwrite(extinctions_all, '/nfs/qread-data/cfs_io_analysis/scenarios/extinctions_for_sensitivity_analysis.csv')


# Visualize results -------------------------------------------------------

# Can run starting from here
library(data.table)
library(ggplot2)

extinctions_all <- fread('/nfs/qread-data/cfs_io_analysis/scenarios/extinctions_for_sensitivity_analysis.csv')

extinctions_all <- tidyr::separate(extinctions_all, scenario, into = c('d','diet','w','waste'), sep = '_')

ggplot(extinctions_all, aes(y = species_lost, x = taxon, fill = land_use)) +
  facet_grid(CF_type ~ intensity) +
  geom_boxplot(position = 'dodge') +
  theme_bw()
# Transformation has a very high one for a few taxa.

ggplot(extinctions_all[CF_type %in% 'occupation'], aes(y = species_lost, x = taxon, fill = land_use)) +
  facet_wrap(~ intensity) +
  geom_boxplot(position = 'dodge') +
  theme_bw()

# Calculate relative sensitivity ------------------------------------------

extinctions_wide <- dcast(extinctions_all, ... ~ intensity, value.var = 'species_lost')

extinctions_wide[, low_vs_med := 1 - low/med]
extinctions_wide[, high_vs_med := 1 - high/med]
extinctions_wide[, high_vs_low := 1 - low/high]
