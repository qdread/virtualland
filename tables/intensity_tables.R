# Tables for Intensity of impacts, and total impacts, cross tabulated by taxon and land use type
# QDR / Virtualland / 26 May 2021


# Generate summary table --------------------------------------------------

# We need total VLT and total VBT by land use type and by taxon, possibly disaggregated to domestic and foreign.
# Generate data in figs_v2_foreign.R to line 101; create all_vlt_sum and all_extinction_sum

all_extinction_base <- all_extinction_sum[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline']
all_extinction_base_total <- all_extinction_base[, .(extinctions = sum(extinctions)), by = .(land_use, origin)]
all_extinction_base_total[, taxon := 'total']
all_extinction_base_animals <- all_extinction_base[!taxon %in% 'plants', .(extinctions = sum(extinctions)), by = .(land_use, origin)]
all_extinction_base_animals[, taxon := 'animals']

all_extinction_base[, scenario_diet := NULL]
all_extinction_base[, scenario_waste := NULL]

all_extinction_base <- rbindlist(list(all_extinction_base, all_extinction_base_total, all_extinction_base_animals), use.names = TRUE)
all_extinction_base[, taxon := paste0('VBT_', taxon)]

all_extinction_tojoin <- dcast(all_extinction_base[taxon %in% c('VBT_animals', 'VBT_plants', 'VBT_total')], land_use + origin ~ taxon)

all_vlt_base <- all_vlt_sum[scenario_diet %in% 'baseline' & scenario_waste %in% 'baseline']
all_vlt_base[, scenario_diet := NULL]
all_vlt_base[, scenario_waste := NULL]
setnames(all_vlt_base, 'land_type', 'land_use')

all_flows <- all_extinction_tojoin[all_vlt_base]

# Now that we have this very high level grand total we can calculate the intensities of VBT per unit VLT
# Units of extinctions per 1000 square km
VBT_cols <- grep('VBT', names(all_flows), value = TRUE)
all_flows[, (paste0(VBT_cols, '_intensity')) := lapply(.SD, function(x) 1000 * 100 * x/all_flows$VLT), .SDcols = VBT_cols]


# Format summary table with gt --------------------------------------------

library(gt)

# Reorder rows and rename columns
all_flows[, land_use := factor(land_use, levels = c('annual', 'permanent', 'pasture'), labels = c('annual cropland', 'permanent cropland', 'pastureland'))]
all_flows[, origin := factor(origin, levels = c('domestic', 'foreign'))]
all_flows <- all_flows[order(origin, land_use)]

# Convert VLT to 1000 sq km also
all_flows[, VLT := VLT / 1000 / 100]

gt_intensity <- as_tibble(all_flows) %>%
  mutate(across(where(is.numeric), ~ signif(., 2))) %>%
  group_by(origin) %>%
  gt(rowname_col = 'land_use') %>%
  tab_spanner(
    label = "Total footprint",
    columns = c(VLT, VBT_animals, VBT_plants, VBT_total)
  ) %>%
  tab_spanner(
    label = "Footprint intensity",
    columns = contains('intensity')
  ) %>%
  cols_label(
    VBT_animals = html('animals<br><small><i>extinctions</i></small>'),
    VBT_plants = html('plants<br><small><i>extinctions</i></small>'),
    VBT_total = html('total<br><small><i>extinctions</i></small>'),
    VLT = html('land<br><small><i>1000 km<sup>2</sup></i></small>'),
    VBT_animals_intensity = html('animals<br><small><i>extinctions/1000 km<sup>2</sup></i></small>'),
    VBT_plants_intensity = html('plants<br><small><i>extinctions/1000 km<sup>2</sup></i></small>'),
    VBT_total_intensity = html('total<br><small><i>extinctions/1000 km<sup>2</sup></i></small>')
  ) %>%
  data_color(
    columns = starts_with('V'),
    colors = scales::col_numeric('Reds', domain = NULL),
    apply_to = c("fill"),
    alpha = 0.75,
    autocolor_text = TRUE
  )
  
saveRDS(gt_intensity, 'data/cfs_io_analysis/scenario_v2_figs/gt_tables/gt_intensity.RDS')
