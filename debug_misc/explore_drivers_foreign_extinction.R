# Which countries' vlt exports to USA, and resulting extinctions, change most from baseline?

vlt <- fread('/nfs/qread-data/cfs_io_analysis/fao_VLT_provisional.csv')

# changes relative to baseline
vltbase <- vlt[scenario=='D_baseline_WR_baseline'][, scenario := NULL]
vltnames <- grep("VLT", names(vltbase), value = TRUE)
setnames(vltbase, old = vltnames, new = paste(vltnames, 'base', sep = '_'))

vlt <- vltbase[vlt, on = .NATURAL]
vlt[, (vltnames) := list(VLT_annual - VLT_annual_base, VLT_mixed - VLT_mixed_base, VLT_permanent - VLT_permanent_base, VLT_pasture - VLT_pasture_base)]

# Find the top ten countries by absolute increase of VLT
vlt[order(-VLT_annual)][1:50]
vlt[order(-VLT_mixed)][1:50]
vlt[order(-VLT_permanent)][1:50]
vlt[order(-VLT_pasture)][1:50]

# Which countries' extinctions change most from baseline in each scenario?

vet <- fread('data/cfs_io_analysis/scenarios/foreign_species_lost_by_export_country_x_tnc.csv')

vetsums <- vet[, .(species_lost = sum(species_lost, na.rm = TRUE)), by = .(scenario_diet, scenario_waste, country_name)]
vetsumsbase <- vetsums[scenario_diet == 'baseline' & scenario_waste == 'baseline']
setnames(vetsumsbase, 'species_lost', 'species_lost_base')
vetsumsbase[, c('scenario_diet', 'scenario_waste') := NULL]

vetsums <- vetsumsbase[vetsums, on = .NATURAL]
vetsums[, species_lost_diff := species_lost - species_lost_base]

vetsums[order(scenario_diet, scenario_waste, -species_lost_diff), head(.SD, 5), by = .(scenario_diet, scenario_waste)]

# Look with more detail at the countries whose exported extinctions increase under the diet scenarios
countries <- c('South Africa', 'Colombia', 'Mexico', 'Ecuador', 'Indonesia', 'Italy')

vet_top <- vet[country_name %in% countries & scenario_waste %in% 'baseline' & scenario_diet %in% c('baseline','vegetarian')]

# Make a plot to visualize
library(ggplot2)

ggplot(vet_top, aes(y = species_lost, x = country_name, color = scenario_diet)) +
  facet_grid(land_use ~ taxon) +
  geom_boxplot() + theme_bw()

# This shows that pastureland in ZAF, MEX, and ITA is the biggest driver of increases in foreign extinction threat exports under the veg. diet
# This is driven mostly by plant threats.
# Also to some extent permanent cultivation in COL, ECU, and MEX

# Why pasture increases under veg diet ------------------------------------

# But why does pastureland export *increase* under the veg diet?

prodcrops <- fread('/nfs/qread-data/cfs_io_analysis/fao_production_trade_crops.csv')
prodanimals <- fread('/nfs/qread-data/cfs_io_analysis/fao_production_trade_animals.csv')

scenario_factors_bea <- fread('data/cfs_io_analysis/bea_consumption_factors_diet_waste_scenarios.csv')

scenario_factors_long <- melt(scenario_factors_bea, id.vars = c('BEA_389_code', 'BEA_389_def'), variable.name = 'scenario', value.name = 'consumption_factor')
scenario_factors_long[, BEA_389_def := NULL]

# Reduce down to veg, then join to production crops and animals to see which VLTs increase under veg.
# Only prod animals should matter. I think it would be milk?

scenario_factors_long <- tidyr::separate(scenario_factors_long, scenario, into = c('d', 'scenario_diet', 'w', 'scenario_waste'), sep = '_')
scenario_factors_long[, c('d', 'w') := NULL]

factors_veg <- scenario_factors_long[scenario_diet %in% 'vegetarian' & scenario_waste %in% 'baseline']

prodanimals <- factors_veg[prodanimals, on = c('BEA_389_code'='BEA_code'), allow.cartesian = TRUE]

# Find the absolute increase in exports.
prodanimals[, export_increase := export_qty * (consumption_factor - 1)]

prodanimals[order(-export_increase)][1:50]
