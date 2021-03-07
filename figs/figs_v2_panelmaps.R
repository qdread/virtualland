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


# Create maps -------------------------------------------------------------

# Test: total land maps, outbound vs. baseline
# Find scale values
county_land_flow_sums[, .(scale_width = max(abs(range(outbound_vs_baseline, na.rm =TRUE))) - 1), by = land_type]

total_maps <- county_land_map_panels[
  land_type %in% 'total'][
    , panel := map(data, function(dat) draw_usmap_with_insets(map_data = left_join(county_map, dat[, .(county, outbound_vs_baseline)]),
                                                               ak_idx = county_ak_idx,
                                                               hi_idx = county_hi_idx,
                                                               variable = outbound_vs_baseline,
                                                               linewidth = 0,
                                                               scale_name = 'Change vs.\nbaseline',
                                                               scale_type = 'divergent',
                                                               scale_factor = 1,
                                                               scale_trans = 'identity',
                                                               scale_range = c(0, 2),
                                                               scale_breaks = c(0, 1, 2),
                                                               ak_pos = c(-0.01, 0.15), hi_pos = c(0.23, 0.15),
                                                               add_theme = theme_void() + theme(legend.position = 'none')))]

# Use panel plot to make a large panel
# Create dummy plot with a legend so it can be extracted
plot_leg <- get_legend(ggplot(mtcars, aes(x=cyl, y=hp, fill=mpg)) + geom_point() +
  scale_fill_gradientn(colours = scico::scico(15, palette = 'berlin'), name = 'Change vs.\nbaseline', na.value = 'gray75',  limits = c(-1, 1), trans = 'identity', guide = guide_colorbar(direction = 'horizontal'), breaks = c(-1, 0, 1), labels = scales::percent))

total_maps_laidout <- panel_plot(plots = total_maps$panel, 
                                 x_labels = diet_long_names$long_name, 
                                 y_labels = waste_long_names$long_name,
                                 x_title = 'diet scenario',
                                 y_title = 'waste scenario',
                                 global_legend = plot_leg,
                                 label_fontsize = 10,
                                 title_fontsize = 14,
                                 panel_width = 60,
                                 panel_height = 45,
                                 label_width = 5,
                                 title_width = 10,
                                 legend_height = 15)

png('data/cfs_io_analysis/scenario_v2_figs/test.png', height=4.5*4+1+1.5,width=6.0*5+1,res=100,units='cm')
grid.draw(total_maps_laidout)
dev.off()
