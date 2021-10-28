library(data.table)
library(ggplot2)
library(dplyr)
library(purrr)

final_output_path <- '~/Documents/GitHub/foodwaste/biodiversity-farm2fork/data'
load(file.path(final_output_path, 'all_app_data.RData'))

# Comparison with Marques et al. bird extinctions
# ===============================================

bird_foreign <- foreign_extinction_flow_sums[scenario_diet == 'baseline' & scenario_waste == 'baseline' & taxon == 'birds']
bird_domestic <- county_extinction_flow_sums[scenario_diet == 'baseline' & scenario_waste == 'baseline' & taxon == 'birds']

flow_cols <- grep('^flow', names(bird_domestic), value = TRUE)
bird_domestic[, lapply(.SD, sum), .SDcols = flow_cols]
# This is approximately 8, while the Marques et al appears to give an estimate of 0.66.
bird_domestic[, lapply(.SD, sum), by = land_type, .SDcols = flow_cols]


# Comparison of relative CFs
# ==========================

# Load Chaudhary et al. supplemental information (do this on server)
chaud_cfs <- fread('/nfs/qread-data/raw_data/biodiversity/chaudhary2015SI/chaud2018si_CFs.csv')

chaud_bird <- chaud_cfs[taxon == 'birds' & CF_type == 'occupation' & unit == 'potential species loss y m-2' & land_use %in% c('crop', 'pasture') & statistic == 'mean' & region_type == 'ecoregion']

chaud_bird_wide <- dcast(chaud_bird[intensity == 'med'], ... ~ land_use, value.var = 'value')
chaud_bird_wide[, relative := pasture/crop]


# Comparison with Laroche et al. land use
# =======================================

# Sum up the land footprint by origin x land type
landflow_cols <- c('flow_inbound_total', 'flow_inbound_foreign')
our_landuse <- county_land_flow_sums[, lapply(.SD, sum), by = .(scenario_diet, scenario_waste, land_type), .SDcols = landflow_cols]
setnames(our_landuse, old = landflow_cols, new = c('total', 'outsourced'))

# Reshape and convert to per capita in square meters (currently square km for total population)
# 2012 USA population from https://www.multpl.com/united-states-population/table/by-year
pop2012 <- 314e6
our_landuse_long <- melt(our_landuse, variable.name = 'origin', value.name = 'total_footprint')
our_landuse_long[, per_capita_footprint := total_footprint / pop2012 * 1e6 ]

# Sum up annual and permanent cropland. Rename pasture to grassland
our_landuse_long[, land_type := ifelse(land_type %in% c('annual','permanent'), 'cropland', 'grassland')]
our_landuse_sums <- our_landuse_long[scenario_waste == 'baseline', .(per_capita_footprint = sum(per_capita_footprint)), by = .(scenario_diet, land_type, origin)]

# Add additional grand totals
total_outsourced <- our_landuse_sums[, .(per_capita_footprint = sum(per_capita_footprint)), by = .(scenario_diet, origin)]
total_outsourced[, land_type := 'total']
our_landuse_sums <- rbindlist(list(our_landuse_sums, total_outsourced), use.names = TRUE)
our_landuse_sums[, source := 'this study']
setnames(our_landuse_sums, old = 'scenario_diet', new = 'diet')

laroche_landuse <- fread('~/Documents/temp/laroche2020_table3.csv')

# Convert Laroche to the same classification we use 
# sum cropland food and cropland feed
laroche_landuse[, origin := ifelse(`Land type` == 'total', 'total', 'outsourced')]
laroche_landuse[, land_type := map_chr(strsplit(`Land type`, ' '), 1)]
setnames(laroche_landuse, old = c('Diet','Per capita footprint'), new = c('diet', 'per_capita_footprint'))
laroche_landuse_sums <- laroche_landuse[, .(per_capita_footprint = sum(per_capita_footprint)), by = .(diet, land_type, origin)]
laroche_landuse_sums[, source := 'Laroche et al.']

comparison_dat <- rbind(our_landuse_sums, laroche_landuse_sums)

# Harmonize diet names: baseline is AAD, non-vegetarian is healthy US style, lacto-ovo vegetarian is healthy veg, and EAT is planetary health.
comparison_dat[diet == 'AAD', diet := 'baseline']
comparison_dat[diet == 'lacto-ovo vegetarian', diet := 'vegetarian']
comparison_dat[diet == 'non-vegetarian', diet := 'usstyle']
comparison_dat[diet == 'EAT', diet := 'planetaryhealth']

ggplot(comparison_dat[diet %in% c('baseline','vegetarian','planetaryhealth') & (origin == 'outsourced' | land_type == 'total')], 
       aes(x = diet, y = per_capita_footprint, group = source, fill = source)) +
  geom_col(position = 'dodge') +
  facet_wrap(land_type ~ origin, scales = 'free_y') +
  scale_y_continuous(expand = expansion(mult = c(0, 0.02)), name = 'Per capita land footprint (m2/cap/y)') +
  theme_bw() +
  theme(legend.position = 'bottom',
        strip.background = element_blank())

ggsave('G:/My Drive/SESYNC Food Waste/MS4_VirtualLand/revision/comparison_laroche.png', height = 5, width = 7, dpi = 200)

# Top line sums,with relative comparisons
comparison_wide <- dcast(comparison_dat[diet %in% c('baseline','vegetarian','planetaryhealth')], diet + land_type + origin ~ source, value.var = 'per_capita_footprint')
comparison_wide[, relative := round(100 * (`this study`/`Laroche et al.` - 1), 1)]
