# Tables with Changes by scenario for land and biodiversity inbound and outbound, by county and by foreign country

# Domestic (by county)

source('figs/figs_v2_loaddata.R')

# Load data from figs_v2_foreign.R

# also convert county extinction flow sums 
# Create summary data for all taxa, and animals only
county_extinction_flow_sums_animals <- county_extinction_flow_sums[!taxon %in% 'plants',
                                                                   lapply(.SD, sum, na.rm = TRUE),
                                                                   by = .(scenario_diet, scenario_waste, county, land_use),
                                                                   .SDcols = patterns('extinction')]
county_extinction_flow_sums_animals[, taxon := 'animals']
county_extinction_flows_all <- county_extinction_flow_sums[, lapply(.SD, sum, na.rm = TRUE),
                                                           by = .(scenario_diet, scenario_waste, county, land_use),
                                                           .SDcols = patterns('extinction')]
county_extinction_flows_all[, taxon := 'total']

county_extinction_flow_sums <- rbindlist(list(county_extinction_flow_sums, county_extinction_flow_sums_animals, county_extinction_flows_all), use.names = TRUE)

# Create summary data for all land use combined
county_extinction_flows_allland <- county_extinction_flow_sums[, lapply(.SD, sum, na.rm = TRUE),
                                                               by = .(scenario_diet, scenario_waste, county, taxon),
                                                               .SDcols = patterns('extinction')]
county_extinction_flows_allland[, land_use := 'total']

county_extinction_flow_sums <- rbindlist(list(county_extinction_flow_sums, county_extinction_flows_allland), use.names = TRUE)

# Separate out baseline
county_extinction_baseline <- county_extinction_flow_sums[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline']
county_extinction_baseline[, c('scenario_diet', 'scenario_waste') := NULL]
ext_names <- grep('extinction', names(county_extinction_baseline), value = TRUE)
setnames(county_extinction_baseline, old = ext_names, new = paste(ext_names, 'baseline', sep = '_'))

# Join baseline data to full data. Express as percent change
county_extinction_flow_sums <- county_extinction_baseline[county_extinction_flow_sums, on = .NATURAL]
county_extinction_flow_sums[, extinction_outbound_vs_baseline := extinction_outbound/extinction_outbound_baseline - 1]
county_extinction_flow_sums[, extinction_inbound_vs_baseline := extinction_inbound/extinction_inbound_baseline - 1]

# Correct NaN to zero (0/0 values)
county_extinction_flow_sums[is.nan(extinction_outbound_vs_baseline), extinction_outbound_vs_baseline := 0]

county_extinction_flow_sums[, paste(ext_names, 'baseline', sep = '_') := NULL]

# also convert county land flow sums
# ==================================

setDT(county_land_flow_sums)
county_land_flow_sums_total <- county_land_flow_sums[, lapply(.SD, sum, na.rm = TRUE), by = .(scenario_diet, scenario_waste, county), .SDcols = patterns('flow')]
county_land_flow_sums_total[, land_type := 'total']
county_land_flow_sums <- rbind(county_land_flow_sums, county_land_flow_sums_total)

# Separate out baseline
county_land_flow_sums_baseline <- county_land_flow_sums[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline']
county_land_flow_sums_baseline[, c('scenario_diet', 'scenario_waste') := NULL]
flow_names <- grep('flow', names(county_land_flow_sums_baseline), value = TRUE)
setnames(county_land_flow_sums_baseline, old = flow_names, new = paste(flow_names, 'baseline', sep = '_'))

# Join baseline data to full data. Express as percent change.
county_land_flow_sums <- county_land_flow_sums_baseline[county_land_flow_sums, on = .NATURAL]
county_land_flow_sums[, inbound_vs_baseline := flow_inbound/flow_inbound_baseline - 1]
county_land_flow_sums[, outbound_vs_baseline := flow_outbound/flow_outbound_baseline - 1]

county_land_flow_sums[, paste(flow_names, 'baseline', sep = '_') := NULL]


# Additional aggregating before making tables -----------------------------

# foreign_vlt_country is outbound land by foreign country, relative to baseline.
# county_land_flow_sums is outbound land by domestic county, relative to baseline
# foreign_extinction_export_country is outbound biodiversity by foreign country, relative to baseline
# county_extinction_flow_sums is outbound biodiversity by domestic county, relative to baseline

# Table with the changes relative to baseline by county/country and scenario.
# For counties, sum it up by state, and aggregate across land types
# Also keep only the 10 scenarios

fips_table <- st_drop_geometry(county_map)
setDT(fips_table)

county_land_flow_sums <- fips_table[county_land_flow_sums, on = 'county']

county_land_agg <- county_land_flow_sums[land_type %in% 'total' & scenario_waste %in% c('baseline', 'allavoidable'), 
                                         .(flow_outbound = sum(flow_outbound),
                                           outbound_vs_baseline = weighted.mean(outbound_vs_baseline, flow_outbound)), 
                                         by = .(scenario_diet, scenario_waste, fips_state)]

county_extinction_flow_sums <- fips_table[county_extinction_flow_sums, on = 'county']

county_extinction_agg <- county_extinction_flow_sums[taxon %in% c('animals', 'plants', 'total') & land_use %in% 'total' & scenario_waste %in% c('baseline', 'allavoidable'),
                                                     .(flow_outbound = sum(extinction_outbound),
                                                       outbound_vs_baseline = weighted.mean(extinction_outbound_vs_baseline, extinction_outbound)),
                                                     by = .(scenario_diet, scenario_waste, fips_state, taxon)]

# for foreign ones, keep only the top 20 contributors to biodiversity footprint and aggregate the rest to "Other"
# Get top20biodiv object from foreign_imports_tables.R script
top20biodiv <- c("Australia", "Mexico", "Canada", "Colombia", "Ecuador", "Nicaragua", 
                 "New Zealand", "Costa Rica", "Guatemala", "Indonesia", "Peru", 
                 "Brazil", "Honduras", "Côte d'Ivoire", "El Salvador", "Chile", 
                 "Sri Lanka", "Uruguay", "India", "Malaysia")
# Include the ISO code in parentheses
top20biodiv_iso <- foreign_vlt_country$ISO_A3[match(top20biodiv, foreign_vlt_country$country_name)]
top20biodiv <- paste0(top20biodiv, ' (', top20biodiv_iso, ')')

foreign_vlt_agg <- foreign_vlt_country[land_use %in% 'total' & scenario_waste %in% c('baseline', 'allavoidable')]

foreign_extinction_agg <- foreign_extinction_export_country[land_use %in% 'total' & scenario_waste %in% c('baseline', 'allavoidable'),
                                                            .(flow_outbound = sum(species_lost),
                                                              outbound_vs_baseline = weighted.mean(species_vs_baseline, species_lost)),
                                                            by = .(scenario_diet, scenario_waste, country_name, ISO_A3)]                    



# Reshape -----------------------------------------------------------------

# State names
data(fips_codes, package = 'tidycensus')
state_table <- unique(fips_codes[, c('state_code', 'state_name')])

# function to format percent as desired
to_pct <- function(x) {
  plus <- ifelse(x >= 0, '+', '')
  paste0(plus, round(x * 100, 1), '%')
}

### County land

# Convert land flows to km^2 (currently m^2)
county_land_agg[, flow_outbound := signif(flow_outbound/1e6, 3)]
county_land_agg[, flow_display := paste0(flow_outbound, ' (', to_pct(outbound_vs_baseline), ')')]
county_land_agg[, scenario_diet := factor(scenario_diet, levels = diet_levels_ordered)]
county_land_agg[, scenario_waste := factor(scenario_waste, levels = c('baseline', 'allavoidable'))]

county_land_agg_wide <- dcast(county_land_agg[!fips_state %in% '11'], fips_state ~ scenario_waste + scenario_diet, value.var = 'flow_display')
county_land_agg_wide[, fips_state := state_table$state_name[match(fips_state, state_table$state_code)]]

### County extinctions

county_extinction_agg[, flow_display := paste0(signif(flow_outbound, 3), ' (', to_pct(outbound_vs_baseline), ')')]
county_extinction_agg[, scenario_diet := factor(scenario_diet, levels = diet_levels_ordered)]
county_extinction_agg[, scenario_waste := factor(scenario_waste, levels = c('baseline', 'allavoidable'))]

county_extinction_agg_wide <- dcast(county_extinction_agg[!fips_state %in% '11' & taxon %in% 'total'], fips_state ~ scenario_waste + scenario_diet, value.var = 'flow_display')
county_extinction_agg_wide[, fips_state := state_table$state_name[match(fips_state, state_table$state_code)]]

### Foreign land

foreign_vlt_agg[, country_name := paste0(country_name, ' (', ISO_A3, ')')]

# Aggregate to other 
foreign_vlt_agg[!country_name %in% top20biodiv, country_name := 'Other']
foreign_vlt_agg <- foreign_vlt_agg[, .(VLT = sum(VLT),
                                       VLT_vs_baseline = weighted.mean(VLT_vs_baseline, VLT)),
                                   by = .(scenario_diet, scenario_waste, country_name)]

# Convert land flows to km^2 (currently ha so divide by 100)
foreign_vlt_agg[, flow_outbound := signif(VLT/100, 3)]
foreign_vlt_agg[, flow_display := paste0(flow_outbound, ' (', to_pct(VLT_vs_baseline), ')')]
foreign_vlt_agg[, scenario_diet := factor(scenario_diet, levels = diet_levels_ordered)]
foreign_vlt_agg[, scenario_waste := factor(scenario_waste, levels = c('baseline', 'allavoidable'))]
foreign_vlt_agg[, country_name := factor(country_name, levels = c(top20biodiv, 'Other'))]

foreign_land_agg_wide <- dcast(foreign_vlt_agg, country_name ~ scenario_waste + scenario_diet, value.var = 'flow_display')

### Foreign extinctions

foreign_extinction_agg[, country_name := paste0(country_name, ' (', ISO_A3, ')')]

# Aggregate to other 
foreign_extinction_agg[!country_name %in% top20biodiv, country_name := 'Other']
foreign_extinction_agg <- foreign_extinction_agg[, .(flow_outbound = sum(flow_outbound),
                                                     outbound_vs_baseline = weighted.mean(outbound_vs_baseline, flow_outbound)),
                                                 by = .(scenario_diet, scenario_waste, country_name)]

foreign_extinction_agg[, flow_outbound := signif(flow_outbound, 3)]
foreign_extinction_agg[, flow_display := paste0(flow_outbound, ' (', to_pct(outbound_vs_baseline), ')')]
foreign_extinction_agg[, scenario_diet := factor(scenario_diet, levels = diet_levels_ordered)]
foreign_extinction_agg[, scenario_waste := factor(scenario_waste, levels = c('baseline', 'allavoidable'))]
foreign_extinction_agg[, country_name := factor(country_name, levels = c(top20biodiv, 'Other'))]

foreign_extinction_agg_wide <- dcast(foreign_extinction_agg, country_name ~ scenario_waste + scenario_diet, value.var = 'flow_display')


# Create gt tables --------------------------------------------------------

library(gt)

gt_county_land <- gt(county_land_agg_wide, rowname_col = 'fips_state') %>%
  tab_spanner(
    label = 'No waste reduction',
    columns = starts_with('baseline')
  ) %>%
  tab_spanner(
    label = '50% waste reduction',
    columns = starts_with('allavoidable')
  ) %>%
  cols_label(
    baseline_baseline = 'baseline diet',
    baseline_usstyle = 'USDA U.S. style',
    baseline_medstyle = 'USDA Mediterranean',
    baseline_vegetarian = 'USDA vegetarian',
    baseline_planetaryhealth = 'Planetary Health',
    allavoidable_baseline = 'baseline diet',
    allavoidable_usstyle = 'U.S. style',
    allavoidable_medstyle = 'Mediterranean',
    allavoidable_vegetarian = 'vegetarian',
    allavoidable_planetaryhealth = 'Planetary Health'
  )

gt_county_extinction <- gt(county_extinction_agg_wide, rowname_col = 'fips_state') %>%
  tab_spanner(
    label = 'No waste reduction',
    columns = starts_with('baseline')
  ) %>%
  tab_spanner(
    label = '50% waste reduction',
    columns = starts_with('allavoidable')
  ) %>%
  cols_label(
    baseline_baseline = 'baseline diet',
    baseline_usstyle = 'USDA U.S. style',
    baseline_medstyle = 'USDA Mediterranean',
    baseline_vegetarian = 'USDA vegetarian',
    baseline_planetaryhealth = 'Planetary Health',
    allavoidable_baseline = 'baseline diet',
    allavoidable_usstyle = 'U.S. style',
    allavoidable_medstyle = 'Mediterranean',
    allavoidable_vegetarian = 'vegetarian',
    allavoidable_planetaryhealth = 'Planetary Health'
  )

gt_foreign_land <- gt(foreign_land_agg_wide, rowname_col = 'country_name') %>%
  tab_spanner(
    label = 'No waste reduction',
    columns = starts_with('baseline')
  ) %>%
  tab_spanner(
    label = '50% waste reduction',
    columns = starts_with('allavoidable')
  ) %>%
  cols_label(
    baseline_baseline = 'baseline diet',
    baseline_usstyle = 'USDA U.S. style',
    baseline_medstyle = 'USDA Mediterranean',
    baseline_vegetarian = 'USDA vegetarian',
    baseline_planetaryhealth = 'Planetary Health',
    allavoidable_baseline = 'baseline diet',
    allavoidable_usstyle = 'U.S. style',
    allavoidable_medstyle = 'Mediterranean',
    allavoidable_vegetarian = 'vegetarian',
    allavoidable_planetaryhealth = 'Planetary Health'
  )

gt_foreign_extinction <- gt(foreign_extinction_agg_wide, rowname_col = 'country_name') %>%
  tab_spanner(
    label = 'No waste reduction',
    columns = starts_with('baseline')
  ) %>%
  tab_spanner(
    label = '50% waste reduction',
    columns = starts_with('allavoidable')
  ) %>%
  cols_label(
    baseline_baseline = 'baseline diet',
    baseline_usstyle = 'USDA U.S. style',
    baseline_medstyle = 'USDA Mediterranean',
    baseline_vegetarian = 'USDA vegetarian',
    baseline_planetaryhealth = 'Planetary Health',
    allavoidable_baseline = 'baseline diet',
    allavoidable_usstyle = 'U.S. style',
    allavoidable_medstyle = 'Mediterranean',
    allavoidable_vegetarian = 'vegetarian',
    allavoidable_planetaryhealth = 'Planetary Health'
  )

save(gt_county_land, gt_county_extinction, gt_foreign_land, gt_foreign_extinction, file = 'data/cfs_io_analysis/scenario_v2_figs/gt_tables/gt_flowchanges.RData')
