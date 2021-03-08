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

# Correct NaN to zero (0/0 values)
county_extinction_flow_sums[is.nan(extinction_outbound_vs_baseline), extinction_outbound_vs_baseline := 0]

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

# Find scale widths, outbound vs. baseline
scale_width <- function(x) max(abs(range(x, na.rm = TRUE)))
county_ext_ob_widths <- county_extinction_flow_sums[, lapply(.SD, scale_width), keyby = .(land_use, taxon), .SDcols = patterns('extinction')]

# Do each taxon, with total land use.

# Amphibians, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'amphibians'],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound_vs_baseline',
                 file_name = 'county_totalland_amphibianextinction_outbound_vs_baseline',
                 scale_name = 'Change vs.\nbaseline',
                 scale_type = 'divergent',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(-1, 1),
                 scale_breaks = c(-1, -.5, 0, 0.5, 1),
                 scale_palette = scico::scico(15, palette = 'berlin'),
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none')
)

# Animals, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'animals'],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound_vs_baseline',
                 file_name = 'county_totalland_animalextinction_outbound_vs_baseline',
                 scale_name = 'Change vs.\nbaseline',
                 scale_type = 'divergent',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(-1, 1),
                 scale_breaks = c(-1, -.5, 0, 0.5, 1),
                 scale_palette = scico::scico(15, palette = 'berlin'),
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none')
)

# Birds, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'birds'],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound_vs_baseline',
                 file_name = 'county_totalland_birdextinction_outbound_vs_baseline',
                 scale_name = 'Change vs.\nbaseline',
                 scale_type = 'divergent',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(-1, 1),
                 scale_breaks = c(-1, -.5, 0, 0.5, 1),
                 scale_palette = scico::scico(15, palette = 'berlin'),
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none')
)

# Mammals, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'mammals'],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound_vs_baseline',
                 file_name = 'county_totalland_mammalextinction_outbound_vs_baseline',
                 scale_name = 'Change vs.\nbaseline',
                 scale_type = 'divergent',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(-1.01, 1.01),
                 scale_breaks = c(-1, -.5, 0, 0.5, 1),
                 scale_palette = scico::scico(15, palette = 'berlin'),
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none')
)

# Plants, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'plants'],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound_vs_baseline',
                 file_name = 'county_totalland_plantextinction_outbound_vs_baseline',
                 scale_name = 'Change vs.\nbaseline',
                 scale_type = 'divergent',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(-1, 1),
                 scale_breaks = c(-1, -.5, 0, 0.5, 1),
                 scale_palette = scico::scico(15, palette = 'berlin'),
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none')
)

# Reptiles, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'reptiles'],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound_vs_baseline',
                 file_name = 'county_totalland_reptileextinction_outbound_vs_baseline',
                 scale_name = 'Change vs.\nbaseline',
                 scale_type = 'divergent',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(-1, 1),
                 scale_breaks = c(-1, -.5, 0, 0.5, 1),
                 scale_palette = scico::scico(15, palette = 'berlin'),
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none')
)

# All taxa, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'total'],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound_vs_baseline',
                 file_name = 'county_totalland_totalextinction_outbound_vs_baseline',
                 scale_name = 'Change vs.\nbaseline',
                 scale_type = 'divergent',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(-1, 1),
                 scale_breaks = c(-1, -.5, 0, 0.5, 1),
                 scale_palette = scico::scico(15, palette = 'berlin'),
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none')
)