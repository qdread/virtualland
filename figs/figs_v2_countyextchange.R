# Relative changes in extinction exports from different foreign countries & domestic regions and different land types
# Uses data object created in the figs_v2_foreign.R script

# Foreign extinction exports by country, change in species lost relative to baseline (additive)
foreign_extinction_export_country_baseline <- foreign_extinction_export_country[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline']
setnames(foreign_extinction_export_country_baseline, old = 'species_lost', new = 'VET_baseline')
foreign_extinction_export_country_baseline[, c('scenario_diet', 'scenario_waste') := NULL]

ext_exp_change <- foreign_extinction_export_country_baseline[foreign_extinction_export_country, on = .NATURAL]
setnames(ext_exp_change, old = 'species_lost', new = 'VET')
ext_exp_change[, VET_change := VET - VET_baseline]

# Visualize where the highest decreases and increases are coming from.

biggest_increases <- ext_exp_change[order(-VET_change), .SD[1:10], by = .(scenario_diet, scenario_waste, land_use)]
biggest_basewaste <- biggest_increases[scenario_waste %in% 'baseline']

ext_exp_change[scenario_waste %in% 'baseline'][order(-VET_change)][1:20]

# Domestic
# Separate out baseline (use DF constructed by figs_v2_panelmaps.R)
county_extinction_baseline <- county_extinction_flow_sums[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline']
county_extinction_baseline[, c('scenario_diet', 'scenario_waste', 'extinction_inbound') := NULL]
setnames(county_extinction_baseline, old = 'extinction_outbound', new = 'VET_baseline')

ext_dom_change <- county_extinction_baseline[county_extinction_flow_sums, on = .NATURAL]
setnames(ext_dom_change, old = 'extinction_outbound', new = 'VET')
ext_dom_change[, VET_change := VET - VET_baseline]

# Join with the lookup table for county FIPS so we know where these locations are.
data(fips_codes, package = 'tidycensus')
setDT(fips_codes)
setnames(fips_codes, old = 'county', new = 'county_name')
fips_codes[, county := paste0(state_code, county_code)]
fips_codes <- fips_codes[, .(state, county, county_name)]
ext_dom_change <- fips_codes[ext_dom_change, on = 'county']

# Visualize where the highest decreases and increases are coming from.

biggest_increases_dom <- ext_dom_change[order(-VET_change), .SD[1:15], by = .(scenario_diet, scenario_waste, land_use)]
biggest_basewaste_dom <- biggest_increases_dom[scenario_waste %in% 'baseline']

ext_dom_change[scenario_waste %in% 'baseline'][order(-VET_change)][1:20]
