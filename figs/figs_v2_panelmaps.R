# Paneled map figures
# QDR / Virtualland / 15 Feb 2021

# Modified 24 May 2021: Plot extinction maps on log scale color gradient
# Modified 21 May 2021: Add goods flows; set order for all nested DFs so that maps draw in the right places.
# Modified 26 Apr 2021: Use alternative map functions with better behavior for fill scale.

# Figures with 20 maps showing how the flows change for different scenarios (relative to baseline)

# Paneled figures to make:

# Flows of land in and out of counties and states
# Flows of extinctions in and out of counties, states, and ecoregions

# Separate out the baseline from all the other scenarios, then join the data to itself so that everything can be divided by baseline.

source('figs/figs_v2_loaddata.R')
library(Rutilitybelt)
library(data.table)
library(scico)

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

# Set order so that the panels appear correctly.
county_land_map_panels[, scenario_diet := factor(scenario_diet, levels = diet_levels_ordered)]
county_land_map_panels[, scenario_waste := factor(scenario_waste, levels = waste_levels_ordered)]
county_land_map_panels <- county_land_map_panels[order(scenario_diet, scenario_waste, land_type)]

# County goods data processing --------------------------------------------

# Goods flows, inbound and outbound, by value
# This will show domestic only, as this accounting isn't possible for foreign due to different methodology.

# Sum up total and bind it to the rest
setDT(county_goods_flow_sums)
county_goods_flow_sums_total <- county_goods_flow_sums[, lapply(.SD, sum, na.rm = TRUE), by = .(scenario_diet, scenario_waste, county), .SDcols = patterns('flow')]
county_goods_flow_sums_total[, BEA_code := 'total']
county_goods_flow_sums <- rbind(county_goods_flow_sums, county_goods_flow_sums_total)

# Separate out baseline
county_goods_flow_sums_baseline <- county_goods_flow_sums[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline']
county_goods_flow_sums_baseline[, c('scenario_diet', 'scenario_waste') := NULL]
flow_names <- grep('flow', names(county_goods_flow_sums_baseline), value = TRUE)
setnames(county_goods_flow_sums_baseline, old = flow_names, new = paste(flow_names, 'baseline', sep = '_'))

# Join baseline data to full data. Express as percent change.
county_goods_flow_sums <- county_goods_flow_sums_baseline[county_goods_flow_sums, on = .NATURAL]
county_goods_flow_sums[, inbound_vs_baseline := flow_inbound/flow_inbound_baseline - 1]
county_goods_flow_sums[, outbound_vs_baseline := flow_outbound/flow_outbound_baseline - 1]

county_goods_flow_sums[, paste(flow_names, 'baseline', sep = '_') := NULL]

# Nest county map to list column
county_goods_map_panels <- group_nest_dt(county_goods_flow_sums, scenario_diet, scenario_waste, BEA_code)

# Give descriptive names for the BEA codes (short, for filenames)
bea_names <- data.frame(BEA_code = c("1111A0", "1111B0", "111200", "111300", "111400", "111900", 
                                      "112120", "1121A0", "112300", "112A00", "114000", "total"),
                        BEA_name = c('oilseeds', 'grains', 'vegetables', 'fruits', 'greenhouse_crops', 'other_crops', 'dairy' ,'beef', 'poultry', 'other_meat', 'wild_seafood', 'total'))

county_goods_map_panels <- county_goods_map_panels[bea_names, on = 'BEA_code', nomatch = 0] # Inner join

# Set order so that the panels appear correctly.
county_goods_map_panels[, scenario_diet := factor(scenario_diet, levels = diet_levels_ordered)]
county_goods_map_panels[, scenario_waste := factor(scenario_waste, levels = waste_levels_ordered)]
county_goods_map_panels <- county_goods_map_panels[order(scenario_diet, scenario_waste, BEA_code)]

# County land maps --------------------------------------------------------

# County outbound land maps; change vs. baseline

# Total land, outbound vs. baseline
make_20panel_map_v2(map_panel_data = county_land_map_panels[land_type %in% 'total'],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'outbound_vs_baseline',
                    file_name = 'county_totalland_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none')
)

# Annual crops, outbound vs. baseline
make_20panel_map_v2(map_panel_data = county_land_map_panels[land_type %in% 'annual_cropland'],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'outbound_vs_baseline',
                    file_name = 'county_annualcrop_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none')
)

# Permanent crops, outbound vs. baseline
make_20panel_map_v2(map_panel_data = county_land_map_panels[land_type %in% 'permanent_cropland'],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'outbound_vs_baseline',
                    file_name = 'county_permanentcrop_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none')
)

# Pastureland, outbound vs. baseline
make_20panel_map_v2(map_panel_data = county_land_map_panels[land_type %in% 'pastureland'],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'outbound_vs_baseline',
                    file_name = 'county_pasture_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none')
)

###

# County outbound land, raw values

# Total land, outbound
make_20panel_map_v2(map_panel_data = county_land_map_panels[land_type %in% 'total'],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'flow_outbound',
                    file_name = 'county_totalland_outbound',
                    scale_name = 'Virtual land\nexport (ha)',
                    scale_factor = 10000,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none')
)

# Annual crops, outbound
make_20panel_map_v2(map_panel_data = county_land_map_panels[land_type %in% 'annual_cropland'],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'flow_outbound',
                    file_name = 'county_annualcrop_outbound',
                    scale_name = 'Virtual land\nexport (ha)',
                    scale_factor = 10000,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none')
)

# Permanent crops, outbound 
make_20panel_map_v2(map_panel_data = county_land_map_panels[land_type %in% 'permanent_cropland'],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'flow_outbound',
                    file_name = 'county_permanentcrop_outbound',
                    scale_name = 'Virtual land\nexport (ha)',
                    scale_factor = 10000,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none')
)

# Pastureland, outbound
make_20panel_map_v2(map_panel_data = county_land_map_panels[land_type %in% 'pastureland'],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'flow_outbound',
                    file_name = 'county_pasture_outbound',
                    scale_name = 'Virtual land\nexport (ha)',
                    scale_factor = 10000,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none')
)


# County land maps, 10 scenarios ------------------------------------------

# only all or nothing waste

# Total land, outbound vs. baseline
make_20panel_map_v2(map_panel_data = county_land_map_panels[land_type %in% 'total' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'outbound_vs_baseline',
                    file_name = '10scenarios_county_totalland_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Annual crops, outbound vs. baseline
make_20panel_map_v2(map_panel_data = county_land_map_panels[land_type %in% 'annual_cropland' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'outbound_vs_baseline',
                    file_name = '10scenarios_county_annualcrop_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Permanent crops, outbound vs. baseline
make_20panel_map_v2(map_panel_data = county_land_map_panels[land_type %in% 'permanent_cropland' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'outbound_vs_baseline',
                    file_name = '10scenarios_county_permanentcrop_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Pastureland, outbound vs. baseline
make_20panel_map_v2(map_panel_data = county_land_map_panels[land_type %in% 'pastureland' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'outbound_vs_baseline',
                    file_name = '10scenarios_county_pasture_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

###

# County outbound land, raw values

# Total land, outbound
make_20panel_map_v2(map_panel_data = county_land_map_panels[land_type %in% 'total' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'flow_outbound',
                    file_name = '10scenarios_county_totalland_outbound',
                    scale_name = 'Virtual land\nexport (ha)',
                    scale_factor = 10000,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Annual crops, outbound
make_20panel_map_v2(map_panel_data = county_land_map_panels[land_type %in% 'annual_cropland' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'flow_outbound',
                    file_name = '10scenarios_county_annualcrop_outbound',
                    scale_name = 'Virtual land\nexport (ha)',
                    scale_factor = 10000,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Permanent crops, outbound 
make_20panel_map_v2(map_panel_data = county_land_map_panels[land_type %in% 'permanent_cropland' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'flow_outbound',
                    file_name = '10scenarios_county_permanentcrop_outbound',
                    scale_name = 'Virtual land\nexport (ha)',
                    scale_factor = 10000,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Pastureland, outbound
make_20panel_map_v2(map_panel_data = county_land_map_panels[land_type %in% 'pastureland' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'flow_outbound',
                    file_name = '10scenarios_county_pasture_outbound',
                    scale_name = 'Virtual land\nexport (ha)',
                    scale_factor = 10000,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)


# County extinction maps --------------------------------------------------

# For each taxon, total land use, change vs. baseline.
# Amphibians, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'amphibians'],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound_vs_baseline',
                    file_name = 'county_totalland_amphibianextinction_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none')
)

# Animals, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'animals'],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound_vs_baseline',
                    file_name = 'county_totalland_animalextinction_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none')
)

# Birds, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'birds'],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound_vs_baseline',
                    file_name = 'county_totalland_birdextinction_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none')
)

# Mammals, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'mammals'],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound_vs_baseline',
                    file_name = 'county_totalland_mammalextinction_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none')
)

# Plants, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'plants'],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound_vs_baseline',
                    file_name = 'county_totalland_plantextinction_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none')
)

# Reptiles, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'reptiles'],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound_vs_baseline',
                    file_name = 'county_totalland_reptileextinction_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none')
)

# All taxa, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'total'],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound_vs_baseline',
                    file_name = 'county_totalland_totalextinction_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none')
)

###
# For each taxon, total land use, raw extinction outbound.

# Amphibians, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'amphibians'],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound',
                    file_name = 'county_totalland_amphibianextinction_outbound',
                    scale_name = 'Extinctions',
                    scale_factor = 1,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none')
)

# Animals, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'animals'],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound',
                    file_name = 'county_totalland_animalextinction_outbound',
                    scale_name = 'Extinctions',
                    scale_factor = 1,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none')
)

# Birds, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'birds'],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound',
                    file_name = 'county_totalland_birdextinction_outbound',
                    scale_name = 'Extinctions',
                    scale_factor = 1,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none')
)

# Mammals, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'mammals'],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound',
                    file_name = 'county_totalland_mammalextinction_outbound',
                    scale_name = 'Extinctions',
                    scale_factor = 1,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none')
)

# Plants, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'plants'],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound',
                    file_name = 'county_totalland_plantextinction_outbound',
                    scale_name = 'Extinctions',
                    scale_factor = 1,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none')
)

# Reptiles, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'reptiles'],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound',
                    file_name = 'county_totalland_reptileextinction_outbound',
                    scale_name = 'Extinctions',
                    scale_factor = 1,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none')
)

# All taxa, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'total'],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound',
                    file_name = 'county_totalland_totalextinction_outbound',
                    scale_name = 'Extinctions',
                    scale_factor = 1,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none')
)


# County extinction maps, 10 scenario version -----------------------------

# For each taxon, total land use, change vs. baseline.
# Amphibians, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'amphibians' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound_vs_baseline',
                    file_name = '10scenarios_county_totalland_amphibianextinction_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Animals, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'animals' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound_vs_baseline',
                    file_name = '10scenarios_county_totalland_animalextinction_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Birds, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'birds' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound_vs_baseline',
                    file_name = '10scenarios_county_totalland_birdextinction_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Mammals, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'mammals' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound_vs_baseline',
                    file_name = '10scenarios_county_totalland_mammalextinction_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Plants, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'plants' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound_vs_baseline',
                    file_name = '10scenarios_county_totalland_plantextinction_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Reptiles, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'reptiles' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound_vs_baseline',
                    file_name = '10scenarios_county_totalland_reptileextinction_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# All taxa, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'total' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound_vs_baseline',
                    file_name = '10scenarios_county_totalland_totalextinction_outbound_vs_baseline',
                    scale_name = 'Change vs.\nbaseline',
                    scale_factor = 1,
                    scale_trans = 'identity',
                    scale_type = 'divergent',
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

###
# For each taxon, total land use, raw extinction outbound.

# Amphibians, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'amphibians' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound',
                    file_name = '10scenarios_county_totalland_amphibianextinction_outbound',
                    scale_name = 'Extinctions',
                    scale_factor = 1,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Animals, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'animals' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound',
                    file_name = '10scenarios_county_totalland_animalextinction_outbound',
                    scale_name = 'Extinctions',
                    scale_factor = 1,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Birds, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'birds' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound',
                    file_name = '10scenarios_county_totalland_birdextinction_outbound',
                    scale_name = 'Extinctions',
                    scale_factor = 1,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Mammals, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'mammals' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound',
                    file_name = '10scenarios_county_totalland_mammalextinction_outbound',
                    scale_name = 'Extinctions',
                    scale_factor = 1,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Plants, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'plants' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound',
                    file_name = '10scenarios_county_totalland_plantextinction_outbound',
                    scale_name = 'Extinctions',
                    scale_factor = 1,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# Reptiles, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'reptiles' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound',
                    file_name = '10scenarios_county_totalland_reptileextinction_outbound',
                    scale_name = 'Extinctions',
                    scale_factor = 1,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)

# All taxa, total land use
make_20panel_map_v2(map_panel_data = county_extinction_map_panels[land_use %in% 'total' & taxon %in% 'total' & scenario_waste %in% c('baseline', 'allavoidable')],
                    base_map = county_map,
                    region_type = 'county',
                    variable = 'extinction_outbound',
                    file_name = '10scenarios_county_totalland_totalextinction_outbound',
                    scale_name = 'Extinctions',
                    scale_factor = 1,
                    scale_trans = 'log10',
                    scale_type = 'sequential',
                    percent_scale = FALSE,
                    ak_idx = county_ak_idx,
                    hi_idx = county_hi_idx,
                    add_theme = theme_void() + theme(legend.position = 'none'),
                    n_waste = 2
)


# County goods flow maps --------------------------------------------------

# Do this one programmatically
# For each good produce four paneled map figures: flow outbound relative 20 panels, flow outbound absolute 20 panels, flow outbound relative 10 panels, flow outbound absolute 10 panels 

bea_codes <- unique(county_goods_map_panels$BEA_code)

for (code in bea_codes) {
  dat <- county_goods_map_panels[BEA_code %in% code]
  bea_name <- dat$BEA_name[1]
  message(paste('Drawing maps for', bea_name))
  
  # Outbound, relative, 20 panels
  make_20panel_map_v2(map_panel_data = dat,
                      base_map = county_map,
                      region_type = 'county',
                      variable = 'outbound_vs_baseline',
                      file_name = glue('county_{bea_name}_outbound_vs_baseline'),
                      scale_name = 'Change vs.\nbaseline',
                      scale_factor = 1,
                      scale_trans = 'identity',
                      scale_type = 'divergent',
                      ak_idx = county_ak_idx,
                      hi_idx = county_hi_idx,
                      add_theme = theme_void() + theme(legend.position = 'none')
  )
  
  # Outbound, absolute, 20 panels
  make_20panel_map_v2(map_panel_data = dat,
                      base_map = county_map,
                      region_type = 'county',
                      variable = 'flow_outbound',
                      file_name = glue('county_{bea_name}_outbound'),
                      scale_name = 'Production\n(million USD)',
                      scale_factor = 1e6,
                      scale_trans = 'log10',
                      scale_type = 'sequential',
                      percent_scale = FALSE,
                      ak_idx = county_ak_idx,
                      hi_idx = county_hi_idx,
                      add_theme = theme_void() + theme(legend.position = 'none')
  )
  
  # Outbound, relative, 10 panels
  make_20panel_map_v2(map_panel_data = dat[scenario_waste %in% c('baseline', 'allavoidable')],
                      base_map = county_map,
                      region_type = 'county',
                      variable = 'outbound_vs_baseline',
                      file_name = glue('10scenarios_county_{bea_name}_outbound_vs_baseline'),
                      scale_name = 'Change vs.\nbaseline',
                      scale_factor = 1,
                      scale_trans = 'identity',
                      scale_type = 'divergent',
                      ak_idx = county_ak_idx,
                      hi_idx = county_hi_idx,
                      add_theme = theme_void() + theme(legend.position = 'none'),
                      n_waste = 2
  )
  
  # Outbound, absolute, 10 panels
  make_20panel_map_v2(map_panel_data = dat[scenario_waste %in% c('baseline', 'allavoidable')],
                      base_map = county_map,
                      region_type = 'county',
                      variable = 'flow_outbound',
                      file_name = glue('10scenarios_county_{bea_name}_outbound'),
                      scale_name = 'Production\n(million USD)',
                      scale_factor = 1e6,
                      scale_trans = 'log10',
                      scale_type = 'sequential',
                      percent_scale = FALSE,
                      ak_idx = county_ak_idx,
                      hi_idx = county_hi_idx,
                      add_theme = theme_void() + theme(legend.position = 'none'),
                      n_waste = 2
  )

}
