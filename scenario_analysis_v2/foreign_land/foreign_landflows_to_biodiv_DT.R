# Foreign landflows to biodiversity extinction flows
# Production-side: by foreign ecoregion (and possibly also foreign country x ecoregion)
# Consumption-side: by USA importing county by foreign ecoregion
# QDR / Virtualland / 18 Feb 2021


# Read data ---------------------------------------------------------------

library(data.table)
library(Rutilitybelt)
library(purrr)

# Updated characterization factors from Chaudhary and Brooks 2018
chaudsi2018 <- fread('data/raw_data/biodiversity/chaudhary2015SI/chaud2018si_CFs.csv', colClasses = rep(c('character', 'double'), c(9, 1)))

# Process Chaudhary 2018 CFs data in preparation for joining with VLT data
# Plantation = permanent cropland, Crop = annual cropland
chaudsi_processed <- chaudsi2018[region_type %in% 'ecoregion' & land_use %in% c('crop', 'pasture', 'plantation') & unit %in% 'potential species loss y m-2']
chaudsi_processed[, land_use := fcase(land_use == 'crop', 'annual',
                                      land_use == 'plantation', 'permanent',
                                      land_use == 'pasture', 'pasture')]
chaudsi_processed[, c('region_type', 'unit', 'ecoregion_name') := NULL]   

# Pre-filter to include only the medium intensity and occupation CFs
chaudsi_processed <- chaudsi_processed[intensity %in% 'med' & CF_type %in% 'occupation' & statistic %in% 'mean']

# Production-side VLT (foreign country x ecoregion combination)
foreign_vlt_export <- fread('data/cfs_io_analysis/foreign_VLT_by_country_x_TNC.csv')

# Consumption-side VLT (USA county)
foreign_vlt_import <- fread('data/cfs_io_analysis/foreign_VLT_by_TNC_x_county.csv')


# Biodiversity extinction flows, production-side --------------------------

# Join foreign VLT export with processed CFs (Cartesian)

foreign_vlt_export <- foreign_vlt_export[, .(scenario_diet, scenario_waste, ECO_CODE, ECO_NAME, NAME_LONG, ISO_A3, VLT_annual_region, VLT_mixed_region, VLT_permanent_region, VLT_pasture_region)]
setnames(foreign_vlt_export, c('scenario_diet', 'scenario_waste', 'TNC', 'TNC_name', 'country_name', 'ISO_A3', 'annual', 'mixed', 'permanent', 'pasture'))
# Allocate 50% of the mixed land to annual, 50% to permanent (should be a relatively small component)
foreign_vlt_export[, annual := annual + mixed/2]
foreign_vlt_export[, permanent := permanent + mixed/2]
foreign_vlt_export[, mixed := NULL]

foreign_vlt_export <- melt(foreign_vlt_export, measure.vars = c('annual', 'permanent', 'pasture'), variable.name = 'land_use', value.name = 'VLT')

foreign_vlt_export_CF <- merge(foreign_vlt_export, chaudsi_processed, 
                by.x = c('TNC', 'land_use'), by.y = c('ecoregion_code', 'land_use'), 
                all.x = TRUE, all.y = FALSE, allow.cartesian = TRUE)

setnames(foreign_vlt_export_CF, old = 'value', new = 'CF')
foreign_vlt_export_CF[, species_lost := VLT * CF * 1e4] # Convert from hectares to square meters.
foreign_vlt_export_CF <- foreign_vlt_export_CF[!is.na(CF_type) & !is.na(taxon)][, c('intensity', 'statistic') := NULL]

fwrite(foreign_vlt_export_CF, 'data/cfs_io_analysis/scenarios/foreign_species_lost_by_export_country_x_tnc.csv')


# Biodiversity extinction flows, consumption-side -------------------------

setnames(foreign_vlt_import, gsub('(VLT_)|(_region)', '', names(foreign_vlt_import)))
# Allocate 50% of the mixed land to annual, 50% to permanent (should be a relatively small component)
foreign_vlt_import[, annual := annual + mixed/2]
foreign_vlt_import[, permanent := permanent + mixed/2]
foreign_vlt_import[, mixed := NULL]

foreign_vlt_import <- melt(foreign_vlt_import, measure.vars = c('annual', 'permanent', 'pasture'), variable.name = 'land_use', value.name = 'VLT')

# Cartesian join of VLT imports and characterization factors must be done in chunks because it is so big
# Do not try to write the entire by-county data, only use the summed version for making maps and summary stats

foreign_vlt_import <- group_nest_dt(foreign_vlt_import, scenario_diet, scenario_waste)

# Function to be applied to the data for each scenario
get_extinction_flows_to_counties <- function(dat) {
  foreign_vlt_import_CF_scenario <- merge(dat, chaudsi_processed, 
                                 by.x = c('ECO_CODE', 'land_use'), by.y = c('ecoregion_code', 'land_use'), 
                                 all.x = TRUE, all.y = FALSE, allow.cartesian = TRUE)
  
  setnames(foreign_vlt_import_CF_scenario, old = 'value', new = 'CF')
  foreign_vlt_import_CF_scenario[, species_lost := VLT * CF * 1e4] # Convert from hectares to square meters.
  foreign_vlt_import_CF_scenario <- foreign_vlt_import_CF_scenario[!is.na(CF_type) & !is.na(taxon)][, c('intensity', 'statistic') := NULL]
  
  foreign_vlt_import_CF_scenario[, .(species_lost = sum(species_lost, na.rm = TRUE)), by = .(county, land_use, taxon)]
  
}

# Apply to each scenario
foreign_vlt_import[, species_lost := map(data, get_extinction_flows_to_counties)]

# Unnest the data
foreign_vlt_import[, data := NULL]
foreign_vlt_import_CF <- unnest_dt(foreign_vlt_import, col = species_lost, id = .(scenario_diet, scenario_waste))

fwrite(foreign_vlt_import_CF, 'data/cfs_io_analysis/scenarios/foreign_species_lost_by_import_county.csv')
