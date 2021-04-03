# Relative changes to land and extinction grand totals by scenario
# For results section of manuscript
# Uses data objects loaded in figs_v2_foreign.R

# Extinctions: get baseline
extinction_grandtotals_baseline <- extinction_grandtotals[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline']
setnames(extinction_grandtotals_baseline, old = 'extinctions', new = 'extinctions_baseline')
extinction_grandtotals_baseline[, c('scenario_diet', 'scenario_waste') := NULL]

# Baseline proportions by animals vs. plants and domestic vs. foreign
extinction_grandtotals_baseline[, .(ext = sum(extinctions_baseline)), by = .(kingdom)][, ext/sum(ext)]
extinction_grandtotals_baseline[, .(ext = sum(extinctions_baseline)), by = .(origin)][, ext/sum(ext)]

# Calculate percent reduction or increase, relative to baseline
extinction_grandtotals_relative <- extinction_grandtotals_baseline[extinction_grandtotals, on = .NATURAL]

# All combined, relative
ext_overall <- extinction_grandtotals_relative[, lapply(.SD, sum), by = .(scenario_diet, scenario_waste), .SDcols = patterns('extinction')]
ext_overall[, pct_change := (extinctions - extinctions_baseline)/extinctions_baseline]

# Separate domestic and foreign, relative
ext_byorigin <- extinction_grandtotals_relative[, lapply(.SD, sum), by = .(scenario_diet, scenario_waste, origin), .SDcols = patterns('extinction')]
ext_byorigin[, pct_change := (extinctions - extinctions_baseline)/extinctions_baseline]

# Land: get baseline
land_grandtotals_baseline <- all_vlt_sum[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline']
setnames(land_grandtotals_baseline, old = 'VLT', new = 'VLT_baseline')
land_grandtotals_baseline[, c('scenario_diet', 'scenario_waste') := NULL]

# Baseline proportions by land type and domestic vs. foreign
land_grandtotals_baseline[, .(vlt = sum(VLT_baseline)), by = .(land_type)][, vlt/sum(vlt)]
land_grandtotals_baseline[, .(vlt = sum(VLT_baseline)), by = .(origin)][, vlt/sum(vlt)]

# Baseline proportions within scenario
all_vlt_props <- all_vlt_sum[, .(vlt = sum(VLT)), by = .(scenario_diet, scenario_waste, land_type)]
all_vlt_props[, vlt := vlt/sum(vlt), by = .(scenario_diet, scenario_waste)]

# Calculate percent reduction or increase, relative to baseline
land_grandtotals_relative <- land_grandtotals_baseline[all_vlt_sum, on = .NATURAL]

# All combined, relative
land_overall <- land_grandtotals_relative[, lapply(.SD, sum), by = .(scenario_diet, scenario_waste), .SDcols = patterns('VLT')]
land_overall[, pct_change := (VLT - VLT_baseline)/VLT_baseline]
