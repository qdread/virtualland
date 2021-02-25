# Paneled map figures
# QDR / Virtualland / 15 Feb 2021

# Figures with 20 maps showing how the flows change for different scenarios (relative to baseline)

# Separate out the baseline from all the other scenarios, then join the data to itself so that everything can be divided by baseline.

source('figs/figs_v2_loaddata.R')
library(Rutilitybelt)


# State extinction data processing ----------------------------------------

# Split out scenario column
setDT(state_extinction_flows)
state_extinction_flows <- tidyr::separate(state_extinction_flows, scenario, into = c('d', 'scenario_diet', 'w', 'scenario_waste'), sep = '_')
state_extinction_flows[, c('d', 'w') := NULL]

# Separate out baseline
state_extinction_baseline <- state_extinction_flows[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline']
state_extinction_baseline[, c('scenario_diet', 'scenario_waste') := NULL]
setnames(state_extinction_baseline, old = 'species_lost', new = 'species_lost_baseline')

# Join baseline data to full data
state_extinction_flows <- state_extinction_baseline[state_extinction_flows, on = .NATURAL]
state_extinction_flows[, change_vs_baseline := species_lost/species_lost_baseline]

# Create summary data for inbound and outbound
state_extinction_inbound <- state_extinction_flows[, 
                                                   .(species_lost = sum(species_lost, na.rm = TRUE),
                                                     species_lost_baseline = sum(species_lost_baseline, na.rm = TRUE)),
                                                   by = .(scenario_diet, scenario_waste, state_to, land_use, taxon)]
state_extinction_outbound <- state_extinction_flows[, 
                                                   .(species_lost = sum(species_lost, na.rm = TRUE),
                                                     species_lost_baseline = sum(species_lost_baseline, na.rm = TRUE)),
                                                   by = .(scenario_diet, scenario_waste, state_from, land_use, taxon)]

state_extinction_inbound[, inbound_change_vs_baseline := species_lost/species_lost_baseline]
state_extinction_outbound[, outbound_change_vs_baseline := species_lost/species_lost_baseline]

state_extinction_inbound[, c('species_lost', 'species_lost_baseline') := NULL]
state_extinction_outbound[, c('species_lost', 'species_lost_baseline') := NULL]

setnames(state_extinction_inbound, old = 'state_to', new = 'state')
setnames(state_extinction_outbound, old = 'state_from', new = 'state')

state_extinction_change <- state_extinction_inbound[state_extinction_outbound, on = .NATURAL]


# County land data processing ---------------------------------------------

# Sum up total and bind it to the rest
county_land_flow_sums_total <- county_land_flow_sums[, lapply(.SD, sum, na.rm = TRUE), by = .(scenario_diet, scenario_waste, county), .SDcols = c('flow_inbound', 'flow_outbound')]
county_land_flow_sums_total[, land_type := 'total']
county_land_flow_sums <- rbind(county_land_flow_sums, county_land_flow_sums_total)

# Separate out baseline
county_land_flow_sums_baseline <- county_land_flow_sums[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline']
county_land_flow_sums_baseline[, c('scenario_diet', 'scenario_waste') := NULL]
setnames(county_land_flow_sums_baseline, old = c('flow_inbound', 'flow_outbound'), new = c('flow_inbound_baseline', 'flow_outbound_baseline'))

# Join baseline data to full data
county_land_flow_sums <- county_land_flow_sums_baseline[county_land_flow_sums, on = .NATURAL]
county_land_flow_sums[, inbound_vs_baseline := flow_inbound/flow_inbound_baseline]
county_land_flow_sums[, outbound_vs_baseline := flow_outbound/flow_outbound_baseline]

# Nest county map to list column
county_land_map_panels <- group_nest_dt(county_land_flow_sums, scenario_diet, scenario_waste