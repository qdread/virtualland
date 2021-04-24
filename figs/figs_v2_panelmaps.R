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

# Add foreign imported extinctions.
foreign_extinction_import <- foreign_extinction_import %>%
  mutate(county = sprintf('%05d', county)) %>% 
  rename(extinction_inbound_foreign = species_lost)

county_extinction_flow_sums <- county_extinction_flow_sums %>%
  separate(scenario, into = c('d', 'scenario_diet', 'w', 'scenario_waste'), sep = '_') %>%
  select(-d, -w) %>%
  full_join(foreign_extinction_import) %>%
  mutate(extinction_inbound_total = extinction_inbound + extinction_inbound_foreign) %>%
  mutate(across(where(is.numeric), replace_na, replace = 0))

setDT(county_extinction_flow_sums)

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
county_extinction_flow_sums[, extinction_inbound_foreign_vs_baseline := extinction_inbound_foreign/extinction_inbound_foreign_baseline - 1]
county_extinction_flow_sums[, extinction_inbound_total_vs_baseline := extinction_inbound_total/extinction_inbound_total_baseline - 1]

# Correct NaN to zero (0/0 values)
county_extinction_flow_sums[is.nan(extinction_outbound_vs_baseline), extinction_outbound_vs_baseline := 0]

county_extinction_flow_sums[, paste(ext_names, 'baseline', sep = '_') := NULL]

county_extinction_map_panels <- group_nest_dt(county_extinction_flow_sums, scenario_diet, scenario_waste, land_use, taxon)

# Set order so that the panels appear correctly.
county_extinction_map_panels[, scenario_diet := factor(scenario_diet, levels = diet_levels_ordered)]
county_extinction_map_panels[, scenario_waste := factor(scenario_waste, levels = waste_levels_ordered)]
county_extinction_map_panels <- county_extinction_map_panels[order(scenario_diet, scenario_waste, land_use, taxon)]

# County land data processing ---------------------------------------------

# Add foreign imported land.
foreign_vlt_import_long <- foreign_vlt_import %>%
  mutate(VLT_annual_region = VLT_annual_region + VLT_mixed_region / 2,
         VLT_permanent_region = VLT_permanent_region + VLT_mixed_region / 2) %>%
  select(-VLT_mixed_region) %>%
  rename(annual_cropland = VLT_annual_region, permanent_cropland = VLT_permanent_region, pastureland = VLT_pasture_region) %>%
  pivot_longer(contains('land'), names_to = 'land_type', values_to = 'flow_inbound_foreign')

county_land_flow_sums <- county_land_flow_sums %>%
  full_join(foreign_vlt_import_long) %>%
  mutate(flow_inbound_total = flow_inbound + flow_inbound_foreign * 10000) %>%
  mutate(across(where(is.numeric), replace_na, replace = 0))

# Sum up total and bind it to the rest
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
county_land_flow_sums[, inbound_foreign_vs_baseline := flow_inbound_foreign/flow_inbound_foreign_baseline - 1]
county_land_flow_sums[, inbound_total_vs_baseline := flow_inbound_total/flow_inbound_total_baseline - 1]

county_land_flow_sums[, paste(flow_names, 'baseline', sep = '_') := NULL]

# Nest county map to list column
county_land_map_panels <- group_nest_dt(county_land_flow_sums, scenario_diet, scenario_waste, land_type)



# County land maps --------------------------------------------------------

# County outbound land maps; change vs. baseline
# Find scale values
county_land_flow_sums[, .(scale_width = max(abs(range(outbound_vs_baseline, na.rm =TRUE))),
                          min = min(flow_outbound[flow_outbound>0]/1e4, na.rm = TRUE),
                          max = max(flow_outbound/1e4, na.rm = TRUE)), by = land_type]

div_pal <- scico::scico(15, palette = 'vik')
seq_pal <- viridis::viridis_pal()(15)

# Total land, outbound vs. baseline
make_20panel_map(map_panel_data = county_land_map_panels[land_type %in% 'total'],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'outbound_vs_baseline',
                 file_name = 'county_totalland_outbound_vs_baseline',
                 scale_name = 'Change vs.\nbaseline',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_palette = div_pal,
                 scale_range = c(-1.21, 1.21),
                 scale_breaks = c(-1.2, -.6, 0, 0.6, 1.2),
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
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_palette = div_pal,
                 scale_range = c(-1.035, 1.035),
                 scale_breaks = c(-1, -0.5, 0, 0.5, 1),
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
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_palette = div_pal,
                 scale_range = c(-1.5, 1.5),
                 scale_breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5),
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
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_palette = div_pal,
                 scale_range = c(-1.1, 1.1),
                 scale_breaks = c(-1, -0.5, 0, 0.5, 1),
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none')
)

###

# County outbound land, raw values

# Total land, outbound
make_20panel_map(map_panel_data = county_land_map_panels[land_type %in% 'total'],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'flow_outbound',
                 file_name = 'county_totalland_outbound',
                 scale_name = 'Virtual land\nexport (ha)',
                 scale_factor = 10000,
                 scale_trans = 'log10',
                 scale_palette = seq_pal,
                 scale_range = c(0.7, 3e6),
                 scale_breaks = c(3e0, 3e2, 3e4, 3e6),
                 percent_scale = FALSE,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none')
)

# Annual crops, outbound
make_20panel_map(map_panel_data = county_land_map_panels[land_type %in% 'annual_cropland'],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'flow_outbound',
                 file_name = 'county_annualcrop_outbound',
                 scale_name = 'Virtual land\nexport (ha)',
                 scale_factor = 10000,
                 scale_trans = 'log10',
                 scale_palette = seq_pal,
                 scale_range = c(0.38, 3e5),
                 scale_breaks = c(3e1, 3e3, 3e5),
                 percent_scale = FALSE,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none')
)

# Permanent crops, outbound 
make_20panel_map(map_panel_data = county_land_map_panels[land_type %in% 'permanent_cropland'],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'flow_outbound',
                 file_name = 'county_permanentcrop_outbound',
                 scale_name = 'Virtual land\nexport (ha)',
                 scale_factor = 10000,
                 scale_trans = 'log10',
                 scale_palette = seq_pal,
                 scale_range = c(4, 5e5),
                 scale_breaks = c(5e1, 5e3, 5e5),
                 percent_scale = FALSE,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none')
)

# Pastureland, outbound
make_20panel_map(map_panel_data = county_land_map_panels[land_type %in% 'pastureland'],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'flow_outbound',
                 file_name = 'county_pasture_outbound',
                 scale_name = 'Virtual land\nexport (ha)',
                 scale_factor = 10000,
                 scale_trans = 'log10',
                 scale_palette = seq_pal,
                 scale_range = c(0.4, 3e6),
                 scale_breaks = c(3e0, 3e2, 3e4, 3e6),
                 percent_scale = FALSE,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none')
)


# County land maps, 10 scenarios ------------------------------------------

# only all or nothing waste

# Total land, outbound vs. baseline
make_20panel_map(map_panel_data = county_land_map_panels[land_type %in% 'total' & scenario_waste %in% c('baseline', 'allavoidable')],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'outbound_vs_baseline',
                 file_name = '10scenarios_county_totalland_outbound_vs_baseline',
                 scale_name = 'Change vs.\nbaseline',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_palette = div_pal,
                 scale_range = c(-1.21, 1.21),
                 scale_breaks = c(-1.2, -.6, 0, 0.6, 1.2),
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none'),
                 n_waste = 2
)

# Annual crops, outbound vs. baseline
make_20panel_map(map_panel_data = county_land_map_panels[land_type %in% 'annual_cropland' & scenario_waste %in% c('baseline', 'allavoidable')],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'outbound_vs_baseline',
                 file_name = '10scenarios_county_annualcrop_outbound_vs_baseline',
                 scale_name = 'Change vs.\nbaseline',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_palette = div_pal,
                 scale_range = c(-1.035, 1.035),
                 scale_breaks = c(-1, -0.5, 0, 0.5, 1),
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none'),
                 n_waste = 2
)

# Permanent crops, outbound vs. baseline
make_20panel_map(map_panel_data = county_land_map_panels[land_type %in% 'permanent_cropland' & scenario_waste %in% c('baseline', 'allavoidable')],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'outbound_vs_baseline',
                 file_name = '10scenarios_county_permanentcrop_outbound_vs_baseline',
                 scale_name = 'Change vs.\nbaseline',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_palette = div_pal,
                 scale_range = c(-1.5, 1.5),
                 scale_breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5),
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none'),
                 n_waste = 2
)

# Pastureland, outbound vs. baseline
make_20panel_map(map_panel_data = county_land_map_panels[land_type %in% 'pastureland' & scenario_waste %in% c('baseline', 'allavoidable')],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'outbound_vs_baseline',
                 file_name = '10scenarios_county_pasture_outbound_vs_baseline',
                 scale_name = 'Change vs.\nbaseline',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_palette = div_pal,
                 scale_range = c(-1.1, 1.1),
                 scale_breaks = c(-1, -0.5, 0, 0.5, 1),
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none'),
                 n_waste = 2
)

###

# County outbound land, raw values

# Total land, outbound
make_20panel_map(map_panel_data = county_land_map_panels[land_type %in% 'total' & scenario_waste %in% c('baseline', 'allavoidable')],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'flow_outbound',
                 file_name = '10scenarios_county_totalland_outbound',
                 scale_name = 'Virtual land\nexport (ha)',
                 scale_factor = 10000,
                 scale_trans = 'log10',
                 scale_palette = seq_pal,
                 scale_range = c(0.7, 3e6),
                 scale_breaks = c(3e0, 3e2, 3e4, 3e6),
                 percent_scale = FALSE,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none'),
                 n_waste = 2
)

# Annual crops, outbound
make_20panel_map(map_panel_data = county_land_map_panels[land_type %in% 'annual_cropland' & scenario_waste %in% c('baseline', 'allavoidable')],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'flow_outbound',
                 file_name = '10scenarios_county_annualcrop_outbound',
                 scale_name = 'Virtual land\nexport (ha)',
                 scale_factor = 10000,
                 scale_trans = 'log10',
                 scale_palette = seq_pal,
                 scale_range = c(0.38, 3e5),
                 scale_breaks = c(3e1, 3e3, 3e5),
                 percent_scale = FALSE,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none'),
                 n_waste = 2
)

# Permanent crops, outbound 
make_20panel_map(map_panel_data = county_land_map_panels[land_type %in% 'permanent_cropland' & scenario_waste %in% c('baseline', 'allavoidable')],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'flow_outbound',
                 file_name = '10scenarios_county_permanentcrop_outbound',
                 scale_name = 'Virtual land\nexport (ha)',
                 scale_factor = 10000,
                 scale_trans = 'log10',
                 scale_palette = seq_pal,
                 scale_range = c(4, 5e5),
                 scale_breaks = c(5e1, 5e3, 5e5),
                 percent_scale = FALSE,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none'),
                 n_waste = 2
)

# Pastureland, outbound
make_20panel_map(map_panel_data = county_land_map_panels[land_type %in% 'pastureland' & scenario_waste %in% c('baseline', 'allavoidable')],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'flow_outbound',
                 file_name = '10scenarios_county_pasture_outbound',
                 scale_name = 'Virtual land\nexport (ha)',
                 scale_factor = 10000,
                 scale_trans = 'log10',
                 scale_palette = seq_pal,
                 scale_range = c(0.4, 3e6),
                 scale_breaks = c(3e0, 3e2, 3e4, 3e6),
                 percent_scale = FALSE,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none'),
                 n_waste = 2
)


# County extinction maps --------------------------------------------------

# Find scale widths, outbound vs. baseline
scale_width <- function(x) max(abs(range(x, na.rm = TRUE)))
county_ext_ob_widths <- county_extinction_flow_sums[, lapply(.SD, scale_width), keyby = .(land_use, taxon), .SDcols = patterns('baseline')]
county_ext_ob_max <- county_extinction_flow_sums[, lapply(.SD, max), keyby = .(land_use, taxon), .SDcols = c('extinction_outbound', 'extinction_inbound')]

div_pal <- scico::scico(15, palette = 'vik')
seq_pal <- viridis::viridis_pal()(15)

# For each taxon, total land use, change vs. baseline.
# Amphibians, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'amphibians'],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound_vs_baseline',
                 file_name = 'county_totalland_amphibianextinction_outbound_vs_baseline',
                 scale_name = 'Change vs.\nbaseline',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(-1.2, 1.2),
                 scale_breaks = c(-1.2, -.6, 0, 0.6, 1.2),
                 scale_palette = div_pal,
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
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(-1.2, 1.2),
                 scale_breaks = c(-1.2, -.6, 0, 0.6, 1.2),
                 scale_palette = div_pal,
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
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(-1.2, 1.2),
                 scale_breaks = c(-1.2, -.6, 0, 0.6, 1.2),
                 scale_palette = div_pal,
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
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(-1.2, 1.2),
                 scale_breaks = c(-1.2, -.6, 0, 0.6, 1.2),
                 scale_palette = div_pal,
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
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(-1.2, 1.2),
                 scale_breaks = c(-1.2, -.6, 0, 0.6, 1.2),
                 scale_palette = div_pal,
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
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(-1.2, 1.2),
                 scale_breaks = c(-1.2, -.6, 0, 0.6, 1.2),
                 scale_palette = div_pal,
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
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(-1.2, 1.2),
                 scale_breaks = c(-1.2, -.6, 0, 0.6, 1.2),
                 scale_palette = div_pal,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none')
)

###
# For each taxon, total land use, raw extinction outbound.

# Amphibians, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'amphibians'],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound',
                 file_name = 'county_totalland_amphibianextinction_outbound',
                 scale_name = 'Extinctions',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(0, 0.32),
                 scale_breaks = c(0, 0.1, 0.2, 0.3),
                 scale_palette = seq_pal,
                 percent_scale = FALSE,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none')
)

# Animals, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'animals'],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound',
                 file_name = 'county_totalland_animalextinction_outbound',
                 scale_name = 'Extinctions',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(0, 1.75),
                 scale_breaks = c(0, 0.5, 1, 1.5),
                 scale_palette = viridis::viridis_pal()(15),
                 percent_scale = FALSE,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none')
)

# Birds, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'birds'],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound',
                 file_name = 'county_totalland_birdextinction_outbound',
                 scale_name = 'Extinctions',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(0, 1.75),
                 scale_breaks = c(0, 0.5, 1, 1.5),
                 scale_palette = viridis::viridis_pal()(15),
                 percent_scale = FALSE,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none')
)

# Mammals, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'mammals'],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound',
                 file_name = 'county_totalland_mammalextinction_outbound',
                 scale_name = 'Extinctions',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(0, 0.185),
                 scale_breaks = c(0, 0.05, 0.1, 0.15),
                 scale_palette = viridis::viridis_pal()(15),
                 percent_scale = FALSE,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none')
)

# Plants, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'plants'],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound',
                 file_name = 'county_totalland_plantextinction_outbound',
                 scale_name = 'Extinctions',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(0, 16.3),
                 scale_breaks = c(0, 5, 10, 15),
                 scale_palette = viridis::viridis_pal()(15),
                 percent_scale = FALSE,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none')
)

# Reptiles, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'reptiles'],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound',
                 file_name = 'county_totalland_reptileextinction_outbound',
                 scale_name = 'Extinctions',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(0, 0.14),
                 scale_breaks = c(0, .025, .05, .075, .1, .125),
                 scale_palette = viridis::viridis_pal()(15),
                 percent_scale = FALSE,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none')
)

# All taxa, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'total'],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound',
                 file_name = 'county_totalland_totalextinction_outbound',
                 scale_name = 'Extinctions',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(0, 18),
                 scale_breaks = c(0, 5, 10, 15),
                 scale_palette = viridis::viridis_pal()(15),
                 percent_scale = FALSE,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none')
)


# County extinction maps, 10 scenario version -----------------------------

# For each taxon, total land use, change vs. baseline.
# Amphibians, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'amphibians' & scenario_waste %in% c('baseline', 'allavoidable')],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound_vs_baseline',
                 file_name = '10scenarios_county_totalland_amphibianextinction_outbound_vs_baseline',
                 scale_name = 'Change vs.\nbaseline',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(-1.2, 1.2),
                 scale_breaks = c(-1.2, -.6, 0, 0.6, 1.2),
                 scale_palette = div_pal,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none'),
                 n_waste = 2
)

# Animals, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'animals' & scenario_waste %in% c('baseline', 'allavoidable')],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound_vs_baseline',
                 file_name = '10scenarios_county_totalland_animalextinction_outbound_vs_baseline',
                 scale_name = 'Change vs.\nbaseline',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(-1.2, 1.2),
                 scale_breaks = c(-1.2, -.6, 0, 0.6, 1.2),
                 scale_palette = div_pal,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none'),
                 n_waste = 2
)

# Birds, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'birds' & scenario_waste %in% c('baseline', 'allavoidable')],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound_vs_baseline',
                 file_name = '10scenarios_county_totalland_birdextinction_outbound_vs_baseline',
                 scale_name = 'Change vs.\nbaseline',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(-1.2, 1.2),
                 scale_breaks = c(-1.2, -.6, 0, 0.6, 1.2),
                 scale_palette = div_pal,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none'),
                 n_waste = 2
)

# Mammals, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'mammals' & scenario_waste %in% c('baseline', 'allavoidable')],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound_vs_baseline',
                 file_name = '10scenarios_county_totalland_mammalextinction_outbound_vs_baseline',
                 scale_name = 'Change vs.\nbaseline',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(-1.2, 1.2),
                 scale_breaks = c(-1.2, -.6, 0, 0.6, 1.2),
                 scale_palette = div_pal,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none'),
                 n_waste = 2
)

# Plants, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'plants' & scenario_waste %in% c('baseline', 'allavoidable')],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound_vs_baseline',
                 file_name = '10scenarios_county_totalland_plantextinction_outbound_vs_baseline',
                 scale_name = 'Change vs.\nbaseline',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(-1.2, 1.2),
                 scale_breaks = c(-1.2, -.6, 0, 0.6, 1.2),
                 scale_palette = div_pal,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none'),
                 n_waste = 2
)

# Reptiles, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'reptiles' & scenario_waste %in% c('baseline', 'allavoidable')],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound_vs_baseline',
                 file_name = '10scenarios_county_totalland_reptileextinction_outbound_vs_baseline',
                 scale_name = 'Change vs.\nbaseline',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(-1.2, 1.2),
                 scale_breaks = c(-1.2, -.6, 0, 0.6, 1.2),
                 scale_palette = div_pal,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none'),
                 n_waste = 2
)

# All taxa, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'total' & scenario_waste %in% c('baseline', 'allavoidable')],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound_vs_baseline',
                 file_name = '10scenarios_county_totalland_totalextinction_outbound_vs_baseline',
                 scale_name = 'Change vs.\nbaseline',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(-1.2, 1.2),
                 scale_breaks = c(-1.2, -.6, 0, 0.6, 1.2),
                 scale_palette = div_pal,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none'),
                 n_waste = 2
)

###
# For each taxon, total land use, raw extinction outbound.

# Amphibians, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'amphibians' & scenario_waste %in% c('baseline', 'allavoidable')],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound',
                 file_name = '10scenarios_county_totalland_amphibianextinction_outbound',
                 scale_name = 'Extinctions',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(0, 0.32),
                 scale_breaks = c(0, 0.1, 0.2, 0.3),
                 scale_palette = seq_pal,
                 percent_scale = FALSE,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none'),
                 n_waste = 2
)

# Animals, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'animals' & scenario_waste %in% c('baseline', 'allavoidable')],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound',
                 file_name = '10scenarios_county_totalland_animalextinction_outbound',
                 scale_name = 'Extinctions',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(0, 1.75),
                 scale_breaks = c(0, 0.5, 1, 1.5),
                 scale_palette = viridis::viridis_pal()(15),
                 percent_scale = FALSE,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none'),
                 n_waste = 2
)

# Birds, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'birds' & scenario_waste %in% c('baseline', 'allavoidable')],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound',
                 file_name = '10scenarios_county_totalland_birdextinction_outbound',
                 scale_name = 'Extinctions',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(0, 1.75),
                 scale_breaks = c(0, 0.5, 1, 1.5),
                 scale_palette = viridis::viridis_pal()(15),
                 percent_scale = FALSE,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none'),
                 n_waste = 2
)

# Mammals, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'mammals' & scenario_waste %in% c('baseline', 'allavoidable')],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound',
                 file_name = '10scenarios_county_totalland_mammalextinction_outbound',
                 scale_name = 'Extinctions',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(0, 0.185),
                 scale_breaks = c(0, 0.05, 0.1, 0.15),
                 scale_palette = viridis::viridis_pal()(15),
                 percent_scale = FALSE,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none'),
                 n_waste = 2
)

# Plants, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'plants' & scenario_waste %in% c('baseline', 'allavoidable')],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound',
                 file_name = '10scenarios_county_totalland_plantextinction_outbound',
                 scale_name = 'Extinctions',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(0, 16.3),
                 scale_breaks = c(0, 5, 10, 15),
                 scale_palette = viridis::viridis_pal()(15),
                 percent_scale = FALSE,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none'),
                 n_waste = 2
)

# Reptiles, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'reptiles' & scenario_waste %in% c('baseline', 'allavoidable')],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound',
                 file_name = '10scenarios_county_totalland_reptileextinction_outbound',
                 scale_name = 'Extinctions',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(0, 0.14),
                 scale_breaks = c(0, .025, .05, .075, .1, .125),
                 scale_palette = viridis::viridis_pal()(15),
                 percent_scale = FALSE,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none'),
                 n_waste = 2
)

# All taxa, total land use
make_20panel_map(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'total' & scenario_waste %in% c('baseline', 'allavoidable')],
                 base_map = county_map,
                 region_type = 'county',
                 variable = 'extinction_outbound',
                 file_name = '10scenarios_county_totalland_totalextinction_outbound',
                 scale_name = 'Extinctions',
                 scale_factor = 1,
                 scale_trans = 'identity',
                 scale_range = c(0, 18),
                 scale_breaks = c(0, 5, 10, 15),
                 scale_palette = viridis::viridis_pal()(15),
                 percent_scale = FALSE,
                 ak_idx = county_ak_idx,
                 hi_idx = county_hi_idx,
                 add_theme = theme_void() + theme(legend.position = 'none'),
                 n_waste = 2
)
