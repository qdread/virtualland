# Paneled map figures
# QDR / Virtualland / 15 Feb 2021

# Figures with 20 maps showing how the flows change for different scenarios (relative to baseline)

# Paneled figures to make:

# Flows of land in and out of counties and states
# Flows of extinctions in and out of counties, states, and ecoregions

# Separate out the baseline from all the other scenarios, then join the data to itself so that everything can be divided by baseline.

source('figs/figs_v2_loaddata.R')
library(Rutilitybelt)
library(data.table)

fp_fig <- 'data/cfs_io_analysis/scenario_v2_figs/paneled_maps'

# State extinction data processing ----------------------------------------

# Split out scenario column
setDT(state_extinction_flows)
state_extinction_flows <- tidyr::separate(state_extinction_flows, scenario, into = c('d', 'scenario_diet', 'w', 'scenario_waste'), sep = '_')
state_extinction_flows[, c('d', 'w') := NULL]

# Create summary data for all taxa, and animals only
state_extinction_flows_animals <- state_extinction_flows[!taxon %in% 'plants',
                                                         .(species_lost = sum(species_lost, na.rm = TRUE)),
                                                         by = .(scenario_diet, scenario_waste, state_from, state_to, land_use)]
state_extinction_flows_animals[, taxon := 'animals']
state_extinction_flows_all <- state_extinction_flows[, .(species_lost = sum(species_lost, na.rm = TRUE)),
                                                     by = .(scenario_diet, scenario_waste, state_from, state_to, land_use)]
state_extinction_flows_all[, taxon := 'total']

state_extinction_flows <- rbindlist(list(state_extinction_flows, state_extinction_flows_animals, state_extinction_flows_all), use.names = TRUE)

# Create summary data for all land use combined
state_extinction_flows_allland <- state_extinction_flows[, .(species_lost = sum(species_lost, na.rm = TRUE)),
                                                         by = .(scenario_diet, scenario_waste, state_from, state_to, taxon)]
state_extinction_flows_allland[, land_use := 'total']

state_extinction_flows <- rbindlist(list(state_extinction_flows, state_extinction_flows_allland), use.names = TRUE)


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

# Express change as percent change relative to baseline.
state_extinction_inbound[, inbound_change_vs_baseline := species_lost/species_lost_baseline - 1]
state_extinction_outbound[, outbound_change_vs_baseline := species_lost/species_lost_baseline - 1]

state_extinction_inbound[, c('species_lost', 'species_lost_baseline') := NULL]
state_extinction_outbound[, c('species_lost', 'species_lost_baseline') := NULL]

setnames(state_extinction_inbound, old = 'state_to', new = 'state')
setnames(state_extinction_outbound, old = 'state_from', new = 'state')

state_extinction_change <- state_extinction_inbound[state_extinction_outbound, on = .NATURAL]

state_extinction_map_panels <- group_nest_dt(state_extinction_change, scenario_diet, scenario_waste, land_use, taxon)


# County extinction data processing ---------------------------------------

# Separate scenario column
setDT(county_extinction_flow_sums)
county_extinction_flow_sums <- tidyr::separate(county_extinction_flow_sums, scenario, into = c('d', 'scenario_diet', 'w', 'scenario_waste'), sep = '_')
county_extinction_flow_sums[, c('d', 'w') := NULL]

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
setnames(county_extinction_baseline, old = c('extinction_outbound', 'extinction_inbound'), new = c('extinction_outbound_baseline', 'extinction_inbound_baseline'))

# Join baseline data to full data. Express as percent change
county_extinction_flow_sums <- county_extinction_baseline[county_extinction_flow_sums, on = .NATURAL]
county_extinction_flow_sums[, extinction_outbound_vs_baseline := extinction_outbound/extinction_outbound_baseline - 1]
county_extinction_flow_sums[, extinction_inbound_vs_baseline := extinction_inbound/extinction_inbound_baseline - 1]

county_extinction_flow_sums[, c('extinction_outbound_baseline', 'extinction_inbound_baseline', 'extinction_outbound', 'extinction_inbound') := NULL]

county_extinction_map_panels <- group_nest_dt(county_extinction_flow_sums, scenario_diet, scenario_waste, land_use, taxon)

# County land data processing ---------------------------------------------

# Sum up total and bind it to the rest
setDT(county_land_flow_sums)
county_land_flow_sums_total <- county_land_flow_sums[, lapply(.SD, sum, na.rm = TRUE), by = .(scenario_diet, scenario_waste, county), .SDcols = c('flow_inbound', 'flow_outbound')]
county_land_flow_sums_total[, land_type := 'total']
county_land_flow_sums <- rbind(county_land_flow_sums, county_land_flow_sums_total)

# Separate out baseline
county_land_flow_sums_baseline <- county_land_flow_sums[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline']
county_land_flow_sums_baseline[, c('scenario_diet', 'scenario_waste') := NULL]
setnames(county_land_flow_sums_baseline, old = c('flow_inbound', 'flow_outbound'), new = c('flow_inbound_baseline', 'flow_outbound_baseline'))

# Join baseline data to full data. Express as percent chagne.
county_land_flow_sums <- county_land_flow_sums_baseline[county_land_flow_sums, on = .NATURAL]
county_land_flow_sums[, inbound_vs_baseline := flow_inbound/flow_inbound_baseline - 1]
county_land_flow_sums[, outbound_vs_baseline := flow_outbound/flow_outbound_baseline - 1]

# Nest county map to list column
county_land_map_panels <- group_nest_dt(county_land_flow_sums, scenario_diet, scenario_waste, land_type)



# County land maps --------------------------------------------------------

# County outbound land maps; change vs. baseline
# Find scale values
county_land_flow_sums[, .(scale_width = max(abs(range(outbound_vs_baseline, na.rm =TRUE)))), by = land_type]

# Total land, outbounds vs. baseline
make_20panel_map(map_panel_data = county_land_map_panels[land_type %in% 'total'],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'outbound_vs_baseline',
                 file_name = 'county_totalland_outbound_vs_baseline',
                 scale_name = 'Change vs.\nbaseline',
                 scale_type = 'divergent',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range_legend = c(-1, 1),
                 scale_breaks_legend = c(-1, -0.5, 0, 0.5, 1),
                 scale_palette = scico::scico(15, palette = 'berlin'),
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none')
)

# Annual crops, outbound vs. baseline
make_20panel_map(map_panel_data = county_land_map_panels[land_type %in% 'annual_cropland'],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'outbound_vs_baseline',
                 file_name = 'county_annualcrop_outbound_vs_baseline',
                 scale_name = 'Change vs.\nbaseline',
                 scale_type = 'divergent',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range_legend = c(-.9, .9),
                 scale_breaks_legend = c(-.9, -0.45, 0, 0.45, .9),
                 scale_palette = scico::scico(15, palette = 'berlin'),
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none')
)

# Permanent crops, outbound vs. baseline
make_20panel_map(map_panel_data = county_land_map_panels[land_type %in% 'permanent_cropland'],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'outbound_vs_baseline',
                 file_name = 'county_permanentcrop_outbound_vs_baseline',
                 scale_name = 'Change vs.\nbaseline',
                 scale_type = 'divergent',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range_legend = c(-1.15, 1.15),
                 scale_breaks_legend = c(-1, -0.5, 0, 0.5, 1),
                 scale_palette = scico::scico(15, palette = 'berlin'),
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none')
)

# Pastureland, outbound vs. baseline
make_20panel_map(map_panel_data = county_land_map_panels[land_type %in% 'pastureland'],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'outbound_vs_baseline',
                 file_name = 'county_pasture_outbound_vs_baseline',
                 scale_name = 'Change vs.\nbaseline',
                 scale_type = 'divergent',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(-0.9, 0.9),
                 scale_breaks = c(-0.9, -0.45, 0, 0.45, 0.9),
                 scale_palette = scico::scico(15, palette = 'berlin'),
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none')
)

# County extinction maps --------------------------------------------------



