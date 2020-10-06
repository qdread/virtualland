# Needed maps:

# 1 Cropland and pastureland stocks in each FAF region
# 2 Representative production of some agricultural goods in each FAF region (or outbound shipments)
# 3a Incoming and outgoing shipments of ag goods in baseline case and in scenarios (FAF)
# 3b Network diagram of ag goods shipments in baseline case and in scenarios (FAF)
# 4a Incoming and outgoing transfers of cropland and pastureland in baseline case and in scenarios: FAF
# 4b Incoming and outgoing transfers of cropland and pastureland in baseline case and in scenarios: TNC
# 4c Network diagram corresponding to 4a and 4b
# 5 Incoming and outgoing species threats in baseline case and in scenarios: TNC
# 6 Network diagram of species threats in baseline case and in scenarios: TNC

# Remove Alaska and Hawaii for most maps since they don't have any data (no NLCD)

# load data ---------------------------------------------------------------

fp <- '~/Dropbox/Q/projects/foodwaste/Data/virtualland'

library(tidyverse)
library(sf)
library(stplanr)
library(cowplot)

# Cropland and pastureland by regions
# ===================================

crop_region <- read_csv(file.path(fp, 'NLCDcrop_FAF_x_TNC.csv'))

# Convert to square kilometers 
crop_region <- crop_region %>%
  mutate_at(vars(crop, other, pasture, water), ~ .*900/1e6)


# Sum up by CFS region and TNC region and take proportions
crop_cfs <- crop_region %>% 
  group_by(FAF_Region, Code) %>%
  summarize_at(vars(crop, other, pasture, water), sum) %>%
  mutate(total = crop + other + pasture + water) %>%
  mutate_at(vars(crop, other, pasture, water), list(prop = ~ ./total))
crop_tnc <- crop_region %>%
  group_by(ECO_CODE, ECO_NAME) %>%
  summarize_at(vars(crop, other, pasture, water), sum) %>%
  mutate(total = crop + other + pasture + water) %>%
  mutate_at(vars(crop, other, pasture, water), list(prop = ~ ./total))

# Production (outbound shipments and inbound shipments) of different goods by region
# ==================================================================================

load(file.path(fp, 'fafmapplottingdata.RData'))
cfsmap_outbound <- cfsmap %>% left_join(faf_outbound, by = c('Code' = 'dms_orig'))
cfsmap_inbound <- cfsmap %>% left_join(faf_inbound, by = c('Code' = 'dms_dest'))
cfsmap_exports <- cfsmap %>% left_join(faf_exports, by = c('Code' = 'dms_orig'))
cfsmap_imports <- cfsmap %>% left_join(faf_imports, by = c('Code' = 'dms_dest'))


# Agricultural shipments in baseline case and in scenarios
# ========================================================

agflows <- read_csv(file.path(fp, 'agflows_scenarios_provisional.csv'))

# Land flows for all scenarios
# ============================
landflows <- read_csv(file.path(fp, 'landflows_allscenarios_provisional.csv'))
landflowstnc <- read_csv(file.path(fp, 'landflows_tnc_x_tnc_scenarios_provisional.csv'))

# Biodiversity threat data
# ========================
splost <- read_csv(file.path(fp, 'species_lost_scenarios_provisional_regionalocc.csv'))

# Spatial data
# ============
# Read TNC map
tnc_map <- st_read(file.path(fp, 'tnc_usa_aea.gpkg')) %>% select(-ECO_ID_U)

# Remove zones in Alaska and Hawaii
zones_ak <- tnc_map$ECO_CODE[grep("Alaska|Tundra|Yukon|Aleut|Bristol Bay|Cook Inlet|Boreal Cordillera", tnc_map$ECODE_NAME)]
zones_hi <- tnc_map$ECO_CODE[grep("^OC", tnc_map$ECODE_NAME)]
zones_remove <- c(zones_ak, zones_hi)

tnc_map48 <- tnc_map %>% filter(!ECO_CODE %in% zones_remove)

# Read CFS region map
cfs_map <- st_read(file.path(fp, 'cfs_aea.gpkg'))

# Alaska and Hawaii indices
ak_idx_tnc <- tnc_map$ECO_CODE %in% zones_ak
hi_idx_tnc <- tnc_map$ECO_CODE %in% zones_hi
ak_idx_cfs <- grepl('Alaska', cfs_map$FAF_Region)
hi_idx_cfs <- grepl('Hawaii|Honolulu', cfs_map$FAF_Region)

# general map function ----------------------------------------------------

# US map with insets
draw_usmap_with_insets <- function(map_data, ak_idx, hi_idx, variable, title, scale_name = 'Value\n(billion $)', scale_factor = 1000, add_theme = theme_void()) {
  linewidth <- 0.25 # hardcode to 1/2 the default
  # Calculate scale range
  variable <- enquo(variable)
  map_data <- map_data %>% mutate(!!variable := !!variable/scale_factor)
  scale_range <- map_data %>% pull(!!variable) %>% range(na.rm = TRUE)
  
  # Draw the three maps
  us_map <- ggplot(map_data %>% filter(!ak_idx, !hi_idx)) +
    geom_sf(aes(fill = !!variable), size = linewidth) +
    scale_fill_viridis_c(na.value = 'gray75', name = scale_name, limits = scale_range, guide = guide_colorbar(direction = 'horizontal')) +
    add_theme +
    theme(legend.position = 'bottom') +
    ggtitle(title)
  
  # Include insets for Alaska and Hawaii
  # Projections and limits from https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-3.html
  hi_map <- ggplot(map_data %>% filter(hi_idx)) +
    geom_sf(aes(fill = !!variable), size = linewidth) +
    coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE, datum = NA) +
    scale_fill_viridis_c(na.value = 'gray75', limits = scale_range) +
    add_theme + 
    theme(legend.position = 'none')
  
  ak_map <- ggplot(map_data %>% filter(ak_idx)) +
    geom_sf(aes(fill = !!variable), size = linewidth) +
    coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000), expand = FALSE, datum = NA) +
    scale_fill_viridis_c(na.value = 'gray75', limits = scale_range) +
    add_theme +
    theme(legend.position = 'none')
  
  # Place main map and inset maps on the same plot, scaling appropriately. Hardcode ratios
  ratio_ak <- 0.58
  ratio_hi <- 0.71
  size_ak <- 0.32
  size_hi <- 0.2
  three_maps <- ggdraw(us_map + add_theme) +
    draw_plot(ak_map, width = size_ak, height = size_ak * ratio_ak, x = 0.01, y = 0.15, vjust = 0) +
    draw_plot(hi_map, width = size_hi, height = size_hi * ratio_hi, x = 0.26, y = 0.15, vjust = 0)
  return(three_maps)
}

# simpler function for lower 48
draw_48map <- function(map_data, variable, title, scale_name = 'Value\n(billion $)', scale_factor = 1000, log_scale = FALSE, scale_breaks = c(), add_theme = theme_void()) {
  linewidth <- 0.25 # hardcode to 1/2 the default
  # Calculate scale range
  variable <- enquo(variable)
  map_data <- map_data %>% mutate(!!variable := !!variable/scale_factor)
  scale_range <- map_data %>% pull(!!variable) %>% range(na.rm = TRUE)
  
  scale_trans <- ifelse(log_scale, 'pseudo_log', 'identity')
  if (length(scale_breaks) == 0) scale_breaks <- seq(floor(scale_range[1]), ceiling(scale_range[2]), length.out = 3)
  
  # Draw the three maps
  us_map <- ggplot(map_data) +
    geom_sf(aes(fill = !!variable), size = linewidth) +
    scale_fill_viridis_c(na.value = 'gray75', name = scale_name, limits = scale_range, guide = guide_colorbar(direction = 'horizontal'), trans = scale_trans, breaks = scale_breaks) +
    add_theme +
    theme(legend.position = 'bottom') +
    ggtitle(title)
  
 return(us_map + add_theme)

}


dark_theme <-  theme_void() +
  theme(panel.background = element_rect(fill = 'black', color = NA),
        plot.background = element_rect(color = "black", fill = "black"),
        legend.text = element_text(color = 'white'),
        legend.title = element_text(color = 'white'),
        plot.title = element_text(color = 'white'),
        legend.position = 'bottom')

dark_theme_notitle <- dark_theme + theme(plot.title = element_blank())
dark_theme_facet <- dark_theme + theme(plot.title = element_blank(),
                                       strip.background = element_blank(), 
                                       strip.text = element_text(color = 'white', margin=margin(b=5)))

# land stocks -------------------------------------------------------------

cfs_map_crop <- left_join(cfs_map, crop_cfs)

# map_crop_total <- draw_usmap_with_insets(map_data = cfs_map_crop, 
#                                          ak_idx = ak_idx_cfs,
#                                          hi_idx = hi_idx_cfs,
#                                          variable = crop,
#                                          title = 'cropland',
#                                          scale_name = parse(text = 'Cropland~(km^2)'),
#                                          scale_factor = 1,
#                                          add_theme = dark_theme)
map_crop_total <- draw_48map(map_data = cfs_map_crop[!(ak_idx_cfs | hi_idx_cfs),], 
                             variable = crop,
                             title = 'cropland',
                             scale_name = parse(text = 'Cropland~(1000~km^2)'),
                             scale_factor = 1000,
                             scale_breaks = c(0, 50, 100),
                             add_theme = dark_theme)
map_crop_proportion <- draw_48map(map_data = cfs_map_crop[!(ak_idx_cfs | hi_idx_cfs),], 
                                  variable = crop_prop,
                                  title = 'cropland proportion',
                                  scale_name = 'Cropland\nproportion',
                                  scale_factor = 1,
                                  scale_breaks = c(0, 0.25, 0.5, 0.75),
                                  add_theme = dark_theme)

map_pasture_total <- draw_48map(map_data = cfs_map_crop[!(ak_idx_cfs | hi_idx_cfs),], 
                             variable = pasture,
                             title = 'pastureland',
                             scale_name = parse(text = 'Pastureland~(1000~km^2)'),
                             scale_factor = 1000,
                             scale_breaks = c(0, 25, 50),
                             add_theme = dark_theme)
map_pastureland_proportion <- draw_48map(map_data = cfs_map_crop[!(ak_idx_cfs | hi_idx_cfs),], 
                                  variable = pasture_prop,
                                  title = 'pastureland proportion',
                                  scale_name = 'Pastureland\nproportion',
                                  scale_factor = 1,
                                  scale_breaks = c(0, 0.1, 0.2, 0.3),
                                  add_theme = dark_theme)


# selected goods production -----------------------------------------------

# Outbound, of some different goods
map_grain_outbound <- draw_usmap_with_insets(map_data = cfsmap_outbound, ak_idx = ak_idx_cfs, hi_idx = hi_idx_cfs,
                                             variable = `1111B0`,
                                             title = 'grain farming domestic exports',
                                             add_theme = dark_theme)
map_poultry_outbound <- draw_usmap_with_insets(map_data = cfsmap_outbound, ak_idx = ak_idx_cfs, hi_idx = hi_idx_cfs,
                                               variable = `112300`,
                                               title = 'poultry farming domestic exports',
                                               add_theme = dark_theme)
map_bread_outbound <- draw_usmap_with_insets(map_data = cfsmap_outbound, ak_idx = ak_idx_cfs, hi_idx = hi_idx_cfs,
                                               variable = `311810`,
                                               title = 'bread and bakery manufacturing domestic exports',
                                               add_theme = dark_theme)
map_beer_outbound <- draw_usmap_with_insets(map_data = cfsmap_outbound, ak_idx = ak_idx_cfs, hi_idx = hi_idx_cfs,
                                               variable = `312120`,
                                               title = 'beer manufacturing domestic exports',
                                               add_theme = dark_theme)
map_cheese_outbound <- draw_usmap_with_insets(map_data = cfsmap_outbound, ak_idx = ak_idx_cfs, hi_idx = hi_idx_cfs,
                                               variable = `311513`,
                                               title = 'cheese manufacturing domestic exports',
                                               add_theme = dark_theme)


# agricultural shipments --------------------------------------------------

# Summarize by origin and destination
agflows_outbound <- agflows %>%
  group_by(scenario, dms_orig, BEA_Code) %>%
  summarize(tons = sum(tons, na.rm = TRUE))
agflows_inbound <- agflows %>%
  group_by(scenario, dms_dest, BEA_Code) %>%
  summarize(tons = sum(tons, na.rm = TRUE))
# Reshape to wider format
agflows_outbound_wide <- agflows_outbound %>%
  pivot_wider(id_cols = c(scenario, dms_orig), names_from = BEA_Code, values_from = tons, values_fill = list(tons = 0)) %>%
  filter(!is.na(scenario))
agflows_inbound_wide <- agflows_inbound %>%
  pivot_wider(id_cols = c(scenario, dms_dest), names_from = BEA_Code, values_from = tons, values_fill = list(tons = 0)) %>%
  filter(!is.na(scenario))

# Join the map with each of the flow scenarios
cfs_map_ag_outbound <- left_join(cfs_map, agflows_outbound_wide, by = c('Code' = 'dms_orig')) %>% filter(!is.na(scenario))
cfs_map_ag_inbound <- left_join(cfs_map, agflows_inbound_wide, by = c('Code' = 'dms_dest')) %>% filter(!is.na(scenario))


# # Single product, single scenario
# draw_48map(map_data = cfs_map_ag_outbound %>% filter(scenario == 'baseline', !grepl('Alaska|Hawaii|Honolulu', FAF_Region)), 
#            variable = `1111A0`,
#            title = 'grain farming domestic outbound by weight',
#            scale_name = 'Outbound flows (tons)',
#            scale_factor = 1,
#            log_scale = TRUE,
#            scale_breaks = c(0, 30, 300, 3000, 30000),
#            add_theme = dark_theme_notitle)
# 
# # Single product, four scenarios
# # This m'n't work because the scales are different for each one.
# maps_outbound_cattle_4scenarios <- cfs_map_ag_outbound %>%
#   filter(!grepl('Alaska|Hawaii|Honolulu', FAF_Region)) %>%
#   group_by(scenario) %>%
#   group_map(~ draw_48map(map_data = ., 
#       variable = `1121A0`,
#       title = 'cattle farming and ranching domestic outbound by weight',
#       scale_name = 'Outbound flows (tons)',
#       scale_factor = 1,
#       log_scale = TRUE,
#       scale_breaks = c(0, 50, 500, 5000),
#       add_theme = dark_theme_notitle))

# Function to make maps for the four scenarios paneled/faceted.
draw_48map_scenarios <- function(map_data, variable, title, scale_name = 'Value\n(billion $)', scale_factor = 1000, log_scale = FALSE, scale_breaks = c(), add_theme = theme_void()) {
  linewidth <- 0.25 # hardcode to 1/2 the default
  # Calculate scale range
  variable <- enquo(variable)
  map_data <- map_data %>% mutate(!!variable := !!variable/scale_factor)
  scale_range <- map_data %>% pull(!!variable) %>% range(na.rm = TRUE)
  
  scale_trans <- ifelse(log_scale, 'pseudo_log', 'identity')
  if (length(scale_breaks) == 0) scale_breaks <- seq(scale_range[1], scale_range[2], length.out = 3)
  
  # Draw the three maps
  us_map <- ggplot(map_data) +
    geom_sf(aes(fill = !!variable), size = linewidth) +
    facet_wrap(~ scenario, labeller = labeller(scenario = c(baseline = 'Baseline',
                                                            diet = '50% Vegetarian Shift',
                                                            waste = '50% Waste Reduction',
                                                            transport = 'Minimize Transport'))) +
    scale_fill_viridis_c(na.value = 'gray75', name = scale_name, limits = scale_range, guide = guide_colorbar(direction = 'horizontal'), trans = scale_trans, breaks = scale_breaks) +
    add_theme +
    theme(legend.position = 'bottom') +
    ggtitle(title)
  
  return(us_map + add_theme)
  
}

maps_outbound_cattle_4scenarios <- draw_48map_scenarios(
  map_data = cfs_map_ag_outbound %>% filter(!grepl('Alaska|Hawaii|Honolulu', FAF_Region)), 
  variable = `1121A0`,
  title = 'cattle farming and ranching domestic outbound by weight',
  scale_name = 'Outbound flows (tons)',
  scale_factor = 1,
  log_scale = FALSE,
  scale_breaks = c(0, 2000, 4000),
  add_theme = dark_theme_facet)
maps_inbound_cattle_4scenarios <- draw_48map_scenarios(
  map_data = cfs_map_ag_inbound %>% filter(!grepl('Alaska|Hawaii|Honolulu', FAF_Region)), 
  variable = `1121A0`,
  title = 'cattle farming and ranching domestic inbound by weight',
  scale_name = 'Inbound flows (tons)',
  scale_factor = 1,
  log_scale = FALSE,
  scale_breaks = c(0, 2000, 4000),
  add_theme = dark_theme_facet)


# land transfers ----------------------------------------------------------

# Land transfers by CFS(FAF) region
# Summarize by origin and destination
landflows_outbound <- landflows %>%
  group_by(scenario, dms_orig) %>%
  summarize(cropland_flow = sum(cropland_flow, na.rm = TRUE), pastureland_flow = sum(pastureland_flow, na.rm = TRUE))
landflows_inbound <- landflows %>%
  group_by(scenario, dms_dest) %>%
  summarize(cropland_flow = sum(cropland_flow, na.rm = TRUE), pastureland_flow = sum(pastureland_flow, na.rm = TRUE))

# Join the map with each of the flow scenarios
cfs_map_land_outbound <- left_join(cfs_map, landflows_outbound, by = c('Code' = 'dms_orig')) %>% filter(!is.na(scenario))
cfs_map_land_inbound <- left_join(cfs_map, landflows_inbound, by = c('Code' = 'dms_dest')) %>% filter(!is.na(scenario))

# Land transfers by TNC ecoregion
# Summarize by origin and destination
landflowstnc_outbound <- landflowstnc %>%
  group_by(scenario, TNC_orig) %>%
  summarize(cropland_flow = sum(cropland_flow, na.rm = TRUE), pastureland_flow = sum(pastureland_flow, na.rm = TRUE))
landflowstnc_inbound <- landflowstnc %>%
  group_by(scenario, TNC_dest) %>%
  summarize(cropland_flow = sum(cropland_flow, na.rm = TRUE), pastureland_flow = sum(pastureland_flow, na.rm = TRUE))

# Join the map with each of the flow scenarios
tnc_map_land_outbound <- left_join(tnc_map48, landflowstnc_outbound, by = c('ECO_CODE' = 'TNC_orig')) %>% filter(!is.na(scenario))
tnc_map_land_inbound <- left_join(tnc_map48, landflowstnc_inbound, by = c('ECO_CODE' = 'TNC_dest')) %>% filter(!is.na(scenario))

maps_cfs_outbound_land_4scenarios <- draw_48map_scenarios(
  map_data = cfs_map_land_outbound %>% filter(!grepl('Alaska|Hawaii|Honolulu', FAF_Region), !is.na(scenario)) %>% mutate(land_flow = cropland_flow + pastureland_flow), 
  variable = land_flow,
  title = 'domestic virtual land exports',
  scale_name = parse(text = 'Land~flow~(1000~km^2)'),
  scale_factor = 1000,
  log_scale = FALSE,
  scale_breaks = c(0, 25, 50, 75),
  add_theme = dark_theme_facet)
maps_cfs_inbound_land_4scenarios <- draw_48map_scenarios(
  map_data = cfs_map_land_inbound %>% filter(!grepl('Alaska|Hawaii|Honolulu', FAF_Region), !is.na(scenario)) %>% mutate(land_flow = cropland_flow + pastureland_flow), 
  variable = land_flow,
  title = 'domestic virtual land exports',
  scale_name = parse(text = 'Land~flow~(1000~km^2)'),
  scale_factor = 1000,
  log_scale = FALSE,
  scale_breaks = c(0, 25, 50, 75),
  add_theme = dark_theme_facet)

maps_tnc_outbound_land_4scenarios <- draw_48map_scenarios(
  map_data = tnc_map_land_outbound %>% filter(!is.na(scenario)) %>% mutate(land_flow = cropland_flow + pastureland_flow), 
  variable = land_flow,
  title = 'domestic virtual land exports',
  scale_name = parse(text = 'Land~flow~(1000~km^2)'),
  scale_factor = 1000,
  log_scale = FALSE,
  scale_breaks = c(0, 25, 50, 75),
  add_theme = dark_theme_facet)

maps_tnc_inbound_land_4scenarios <- draw_48map_scenarios(
  map_data = tnc_map_land_inbound %>% filter(!is.na(scenario)) %>% mutate(land_flow = cropland_flow + pastureland_flow), 
  variable = land_flow,
  title = 'domestic virtual land exports',
  scale_name = parse(text = 'Land~flow~(1000~km^2)'),
  scale_factor = 1000,
  log_scale = FALSE,
  scale_breaks = c(0, 25, 50, 75),
  add_theme = dark_theme_facet)


# Species lost (maps and/or summary data) ---------------------------------

# Summarize species loss data
# Sum by land use
splost_alltaxa <- splost %>%
  filter(taxon == 'Taxa Aggregated (Units - PDF/m2)', stat == 'median') %>%
  group_by(scenario, TNC_orig, TNC_dest, CF) %>%
  summarize(species_lost = sum(species_lost))
  
# Sum up the species lost from each region
splost_byorigin <- splost_alltaxa %>%
  group_by(scenario, TNC_orig, CF) %>%
  summarize(species_lost = sum(species_lost))

# Plot density or histogram
ggplot(splost_byorigin %>% filter(CF == 'Occ_marginal_regional'), aes(x = species_lost, fill = scenario)) +
  geom_histogram(alpha = 0.4, position = 'identity', color = 'black', bins = 20) +
  theme_minimal()
ggplot(splost_byorigin %>% filter(CF == 'Occ_marginal_regional'), aes(x = species_lost, fill = scenario)) +
  geom_density(alpha = 0.4, color = 'black') +
  theme_minimal()

# Boxplot
ggplot(splost_byorigin %>% filter(CF == 'Occ_average_regional'), aes(x = scenario, fill = scenario, y = species_lost)) +
  geom_boxplot() +
  theme_minimal()

# Median and quantiles by regions
splost_byorigin_median <- splost_byorigin %>%
  group_by(scenario, CF) %>%
  group_modify(~ quantile(.$species_lost, probs = c(0.025, 0.05, 0.5, 0.95, 0.975)) %>% t %>% as.data.frame)

ggplot(splost_byorigin_median %>% filter(CF == 'Occ_average_regional'), aes(x = scenario,  y = `50%`, ymin = `5%`, ymax = `95%`)) +
  geom_errorbar()

# Calculate species loss relative to baseline.
splost_byorigin_wide <- splost_byorigin %>%
  pivot_wider(names_from = scenario, values_from = species_lost) %>%
  mutate(diet = diet/baseline, transport = transport/baseline, waste = waste/baseline)

# Make a plot.
splost_relative <- splost_byorigin_wide %>%
  filter(CF == 'Occ_average_regional') %>%
  select(-baseline, -CF) %>%
  pivot_longer(-TNC_orig, names_to = 'scenario', values_to = 'biodiversity_threat_decrease')

source('~/Documents/R/theme_black.R')
biothreat_boxplot <- ggplot(splost_relative, aes(x = scenario, y = biodiversity_threat_decrease)) +
  geom_boxplot(aes(fill = scenario), color = 'white') +
  scale_y_log10(name = 'biodiversity threat ratio') +
  scale_x_discrete(labels = c('50% Vegetarian\nShift', 'Minimize\nTransport', '50% Waste\nReduction')) +
  scale_fill_manual(values =  colorspace::sequential_hcl(5, 'Lajolla')[c(2,1,4)]) +
  theme_black() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none')



# Make maps of this.
# Note: some of the regions don't have a threat factor since they were not originally present in Chaudhary SI.
tnc_map_threat_reduction <- tnc_map48 %>% left_join(splost_byorigin_wide %>% filter(CF == 'Occ_average_regional'), by = c('ECO_CODE' = 'TNC_orig'))

tnc_map_threat_reduction_long <- tnc_map_threat_reduction %>%
  select(diet, transport, waste) %>%
  gather(-geom, key = 'scenario', value = 'threat_reduction')

# Create map outside of predefined function to use different color scales and layout.

scale_range <- range(tnc_map_threat_reduction_long$threat_reduction, na.rm = TRUE)

fill_scale_2color <- scale_fill_brewer(name = 'proportional threat\nreduction', type = 'div', palette = 'PuOr', limits = c(0.2, 1.5), na.value = 'gray75', guide = guide_colorbar(direction = 'horizontal'))

fill_ramp <- RColorBrewer::brewer.pal(3, 'RdYlBu')
fill_ramp <- c('goldenrod', 'white', 'darkgreen')

fill_scale_2color <- scale_fill_gradient2(name = 'threat reduction\nratio', 
                                          low = fill_ramp[3],
                                          mid = fill_ramp[2],
                                          high = fill_ramp[1],
                                          midpoint = 1,
                                          na.value = 'gray75', 
                                          guide = guide_colorbar(direction = 'horizontal'))


# Draw the three maps
maps_tnc_threat_reduction <- ggplot(tnc_map_threat_reduction_long) +
  geom_sf(aes(fill = threat_reduction), size = 0.25) +
  facet_wrap(~ scenario, labeller = labeller(scenario = c(baseline = 'Baseline',
                                                          diet = '50% Vegetarian Shift',
                                                          waste = '50% Waste Reduction',
                                                          transport = 'Minimize Transport'))) +
  fill_scale_2color +
  dark_theme_facet +
  theme(legend.position = 'bottom') 

# Write maps to files -----------------------------------------------------

fp_fig <- '~/Dropbox/Q/presentations/dook2020/plots'

ggsave(file.path(fp_fig, 'biothreat_boxplot.png'), biothreat_boxplot, height = 4, width = 5, dpi = 400)

# Plots with a single map on them
ggsave(file.path(fp_fig, 'map_crop_total.png'), map_crop_total, height = 3.5, width = 4.5, dpi = 400)
ggsave(file.path(fp_fig, 'map_crop_proportion.png'), map_crop_proportion, height = 3.5, width = 4.5, dpi = 400)
ggsave(file.path(fp_fig, 'map_pasture_total.png'), map_pasture_total, height = 3.5, width = 4.5, dpi = 400)
ggsave(file.path(fp_fig, 'map_pasture_proportion.png'), map_pastureland_proportion, height = 3.5, width = 4.5, dpi = 400)

ggsave(file.path(fp_fig, 'map_grain_outbound.png'), map_grain_outbound, height = 3.5, width = 4.5, dpi = 400)
ggsave(file.path(fp_fig, 'map_poultry_outbound.png'), map_poultry_outbound, height = 3.5, width = 4.5, dpi = 400)
ggsave(file.path(fp_fig, 'map_bread_outbound.png'), map_bread_outbound, height = 3.5, width = 4.5, dpi = 400)
ggsave(file.path(fp_fig, 'map_beer_outbound.png'), map_beer_outbound, height = 3.5, width = 4.5, dpi = 400)
ggsave(file.path(fp_fig, 'map_cheese_outbound.png'), map_cheese_outbound, height = 3.5, width = 4.5, dpi = 400)

# 2x2 maps
ggsave(file.path(fp_fig, 'maps_outbound_cattle_4scenarios.png'), maps_outbound_cattle_4scenarios, height = 6, width = 6, dpi = 400)
ggsave(file.path(fp_fig, 'maps_inbound_cattle_4scenarios.png'), maps_inbound_cattle_4scenarios, height = 6, width = 6, dpi = 400)
ggsave(file.path(fp_fig, 'maps_cfs_outbound_land_4scenarios.png'), maps_cfs_outbound_land_4scenarios, height = 6, width = 6, dpi = 400)
ggsave(file.path(fp_fig, 'maps_cfs_inbound_land_4scenarios.png'), maps_cfs_inbound_land_4scenarios, height = 6, width = 6, dpi = 400)
ggsave(file.path(fp_fig, 'maps_tnc_outbound_land_4scenarios.png'), maps_tnc_outbound_land_4scenarios, height = 6, width = 6, dpi = 400)
ggsave(file.path(fp_fig, 'maps_tnc_inbound_land_4scenarios.png'), maps_tnc_inbound_land_4scenarios, height = 6, width = 6, dpi = 400)

# 1x3 maps
ggsave(file.path(fp_fig, 'maps_tnc_threat_reduction.png'), maps_tnc_threat_reduction, height = 3, width = 9, dpi = 400)

# test network diagram ----------------------------------------------------

#https://cran.r-project.org/web/packages/stplanr/vignettes/stplanr-od.html

# Convert landflows to line geometry
landflows_tnc_lines <- landflowstnc %>%
  filter(!TNC_orig %in% zones_remove, !TNC_dest %in% zones_remove) %>%
  group_by(scenario) %>%
  mutate(cropland_flow_norm = cropland_flow/mean(cropland_flow),
         pastureland_flow_norm = pastureland_flow/mean(pastureland_flow)) %>%
  group_map(~ od2line(flow = ., zones = tnc_map)) %>%
  setNames(unique(landflowstnc$scenario))

# Plot with tmap.
tm_shape(tnc_map48) + 
  tm_borders() +
  tm_shape(landflows_tnc_lines$baseline %>% arrange(cropland_flow) %>% filter(cropland_flow >= 1000)) + 
  tm_lines(palette = 'plasma', 
           lwd = 'cropland_flow', 
           col = 'cropland_flow',
           scale = 10, 
           alpha = 0.5,
           legend.lwd.show = FALSE)
